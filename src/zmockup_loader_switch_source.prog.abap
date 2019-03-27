*/--------------------------------------------------------------------------------\
*| This file is part of Mockup loader                                             |
*|                                                                                |
*| The MIT License (MIT)                                                          |
*|                                                                                |
*| Copyright (c) 2015 SBCG Team (www.sbcg.com.ua), Alexander Tsybulsky            |
*|                                                                                |
*| Permission is hereby granted, free of charge, to any person obtaining a copy   |
*| of this software and associated documentation files (the "Software"), to deal  |
*| in the Software without restriction, including without limitation the rights   |
*| to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      |
*| copies of the Software, and to permit persons to whom the Software is          |
*| furnished to do so, subject to the following conditions:                       |
*|                                                                                |
*| The above copyright notice and this permission notice shall be included in all |
*| copies or substantial portions of the Software.                                |
*|                                                                                |
*| THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     |
*| IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       |
*| FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    |
*| AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         |
*| LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  |
*| OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  |
*| SOFTWARE.                                                                      |
*\--------------------------------------------------------------------------------/
*/--------------------------------------------------------------------------------\
*| CONTRIBUTORS                                                                   |
*|--------------------------------------------------------------------------------|
*| Leading developers : Alexander Tsybulsky (atsybulsky@sbcg.com.ua)              |
*|                      Svetlana Shlapak    (sshlapak@sbcg.com.ua)                |
*| Testing and ideas:   Bohdan Petruschak   (b.petrushchak@sbcg.com.ua)           |
*|--------------------------------------------------------------------------------|
*| project homepage: https://github.com/sbcgua/mockup_loader                      |
*\--------------------------------------------------------------------------------/


report zmockup_loader_switch_source.
tables sscrfields.

*&---------------------------------------------------------------------*
*&      Class lib (part of w3mime poller)
*&---------------------------------------------------------------------*

class lcx_error definition
  inheriting from cx_static_check
  final
  create public .

  public section.
    data msg type string read-only .

    methods constructor
      importing
        !msg type string optional .
    class-methods raise
      importing
        !msg type string
      raising
        lcx_error .
endclass.

class lcx_error implementation.
  method constructor.
    super->constructor( ).
    me->msg = msg .
  endmethod.

  method raise.
    raise exception type lcx_error
      exporting
        msg    = msg.
  endmethod.
endclass.


class lcl_mime_storage definition
  final
  create public .

  public section.

    class-methods check_obj_exists
      importing
        !iv_key type wwwdata-objid
        !iv_type type wwwdata-relid default 'MI'
      returning
        value(rv_yes) type abap_bool .
    class-methods update_object
      importing
        !iv_key type wwwdata-objid
        !iv_type type wwwdata-relid default 'MI'
        !it_data type lvc_t_mime
        !iv_size type i
      raising
        lcx_error .
    class-methods get_object_info
      importing
        !iv_key type wwwdata-objid
        !iv_type type wwwdata-relid default 'MI'
      returning
        value(rs_object) type wwwdatatab
      raising
        lcx_error .
    class-methods read_object_single_meta
      importing
        !iv_param type w3_name
        !iv_key type wwwdata-objid
        !iv_type type wwwdata-relid default 'MI'
      returning
        value(rv_value) type w3_qvalue
      raising
        lcx_error .
    class-methods update_object_single_meta
      importing
        !iv_param type w3_name
        !iv_value type w3_qvalue
        !iv_key type wwwdata-objid
        !iv_type type wwwdata-relid default 'MI'
      raising
        lcx_error .
endclass.

class lcl_mime_storage implementation.

  method check_obj_exists.

    data dummy type wwwdata-relid.

    select single relid into dummy
      from wwwdata
      where relid = iv_type
      and   objid = iv_key
      and   srtf2 = 0.

    rv_yes = boolc( sy-subrc = 0 ).

  endmethod.  " check_obj_exists.

  method get_object_info.

    select single * into corresponding fields of rs_object
      from wwwdata
      where relid = iv_type
      and   objid = iv_key
      and   srtf2 = 0.

    if sy-subrc > 0.
      lcx_error=>raise( 'Cannot read W3xx info' ). "#EC NOTEXT
    endif.

  endmethod.  " get_object_info.

  method read_object_single_meta.

    assert iv_type = 'MI' or iv_type = 'HT'.

    call function 'WWWPARAMS_READ'
      exporting
        relid = iv_type
        objid = iv_key
        name  = iv_param
      importing
        value = rv_value
      exceptions
        others = 1.

    if sy-subrc > 0.
      lcx_error=>raise( |Cannot read W3xx metadata: { iv_param }| ). "#EC NOTEXT
    endif.

  endmethod.

  method update_object.

    data: lv_temp   type wwwparams-value,
          ls_object type wwwdatatab.

    " update file size
    lv_temp = iv_size.
    condense lv_temp.
    update_object_single_meta(
      iv_type  = iv_type
      iv_key   = iv_key
      iv_param = 'filesize'
      iv_value = lv_temp ).

    " update version
    try .
      lv_temp = read_object_single_meta(
        iv_type  = iv_type
        iv_key   = iv_key
        iv_param = 'version' ).

      if lv_temp is not initial and strlen( lv_temp ) = 5 and lv_temp+0(5) co '1234567890'.
        data lv_version type numc_5.
        lv_version = lv_temp.
        lv_version = lv_version + 1.
        lv_temp    = lv_version.
        update_object_single_meta(
          iv_type  = iv_type
          iv_key   = iv_key
          iv_param = 'version'
          iv_value = lv_temp ).
      endif.

    catch lcx_error.
      " ignore errors
      clear lv_temp.
    endtry.

    " update data
    ls_object = get_object_info( iv_key = iv_key iv_type = iv_type ).
    ls_object-chname = sy-uname.
    ls_object-tdate  = sy-datum.
    ls_object-ttime  = sy-uzeit.

    call function 'WWWDATA_EXPORT'
      exporting
        key               = ls_object
      tables
        mime              = it_data
      exceptions
        wrong_object_type = 1
        export_error      = 2.

    if sy-subrc > 0.
      lcx_error=>raise( 'Cannot upload W3xx data' ). "#EC NOTEXT
    endif.

  endmethod.  " update_object.


  method update_object_single_meta.

    data: ls_param  type wwwparams,
          ls_object type wwwdatatab.

    assert iv_type = 'MI' or iv_type = 'HT'.

    ls_param-relid = iv_type.
    ls_param-objid = iv_key.
    ls_param-name  = iv_param.
    ls_param-value = iv_value.

    call function 'WWWPARAMS_MODIFY_SINGLE'
      exporting
        params = ls_param
      exceptions
        others = 1.

    if sy-subrc > 0.
      lcx_error=>raise( |Cannot update W3xx metadata { iv_param }| ). "#EC NOTEXT
    endif.

  endmethod.

endclass.


class lcl_fs definition
  final
  create public .

  public section.
    class-methods read_file
      importing
        !iv_filename type string
      exporting
        !et_data type lvc_t_mime
        !ev_size type i
      raising
        lcx_error .

    class-methods choose_file_dialog
      returning
        value(rv_path) type char255 .

endclass.

class lcl_fs implementation.

  method read_file.
    clear: et_data, ev_size.

    cl_gui_frontend_services=>gui_upload(
      exporting
        filename   = iv_filename
        filetype   = 'BIN'
      importing
        filelength = ev_size
      changing
        data_tab   = et_data
      exceptions
        others     = 1 ).

    if sy-subrc > 0.
      lcx_error=>raise( 'Cannot read file' ). "#EC NOTEXT
    endif.

  endmethod.  " read_file.

  method choose_file_dialog.
    data:
          lt_files type filetable,
          lv_rc    type i,
          lv_uact  type i.

    field-symbols <file> like line of lt_files.

    cl_gui_frontend_services=>file_open_dialog(
      changing
        file_table  = lt_files
        rc          = lv_rc
        user_action = lv_uact
      exceptions others = 4 ).

    if sy-subrc > 0 OR lv_uact <> cl_gui_frontend_services=>action_ok.
      return. " Empty value
    endif.

    read table lt_files assigning <file> index 1.
    if sy-subrc = 0.
      rv_path = <file>-filename.
    endif.

  endmethod.


endclass.


class lcl_utils definition
  final
  create public .

  public section.

    class-methods upload
      importing
        iv_filename type string
        iv_key  type wwwdata-objid
        iv_type type wwwdata-relid default 'MI'
      raising lcx_error.

endclass.

class lcl_utils implementation.
  method upload.

    data: lt_data type lvc_t_mime,
          lv_size type i.

    if abap_false = lcl_mime_storage=>check_obj_exists( iv_type = iv_type iv_key = iv_key ).
      lcx_error=>raise( 'MIME object does not exist' ). "#EC NOTEXT
    endif.

    lcl_fs=>read_file(
      exporting
        iv_filename = iv_filename
      importing
        et_data     = lt_data
        ev_size     = lv_size ).

    lcl_mime_storage=>update_object(
      iv_type  = iv_type
      iv_key   = iv_key
      it_data  = lt_data
      iv_size  = lv_size ).

  endmethod.  " upload.
endclass.


*&---------------------------------------------------------------------*
*&      Selection screen
*&---------------------------------------------------------------------*
selection-screen begin of block b1 with frame title txt_b1.
selection-screen begin of line.
parameters p_undef type char1 radiobutton group gr1 user-command gr1.
selection-screen comment (12) txt_und  for field p_undef.
selection-screen end of line.

selection-screen begin of line.
parameters p_mime type char1 radiobutton group gr1.
selection-screen comment (10) txt_mime for field p_mime.
selection-screen end of line.

selection-screen begin of line.
parameters p_file type char1 radiobutton group gr1.
selection-screen comment (10) txt_file for field p_file.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (15) txt_fp for field p_file.
parameters p_fpath type char128.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment (15) txt_mp for field p_file .
parameters: p_mpath type char40.
selection-screen comment (17) txt_mp2 for field p_file .
selection-screen end of line.
selection-screen end of block b1.

selection-screen: function key 1.
selection-screen: function key 2.


*&---------------------------------------------------------------------*
*&      Screen events
*&---------------------------------------------------------------------*
initialization.

  txt_b1   = 'Source type (switch saves parameter immediately)'. "#EC NOTEXT
  txt_und  = 'No override'.                                 "#EC NOTEXT
  txt_mime = 'MIME'.                                        "#EC NOTEXT
  txt_file = 'FILE'.                                        "#EC NOTEXT
  txt_fp   = 'File path'.                                   "#EC NOTEXT
  txt_mp   = 'MIME object'.                                 "#EC NOTEXT
  txt_mp2  = '(for upload only)'.                           "#EC NOTEXT

  sscrfields-functxt_01 = 'Get SU3 value'.                  "#EC NOTEXT
  sscrfields-functxt_02 = 'Upload to MIME'.                 "#EC NOTEXT

  perform get_stype.

at selection-screen output.
  perform insert_into_excl(rsdbrunt) using 'ONLI'. "exclude Execute button
  perform set_stype.

at selection-screen on radiobutton group gr1.
  perform set_stype.

at selection-screen on value-request for p_fpath.
  perform f4_file_path changing p_fpath.

at selection-screen on value-request for p_mpath.
  perform f4_mime_path changing p_mpath.

at selection-screen on p_fpath.
  if p_file is not initial.
    set parameter id 'ZMOCKUP_LOADER_SPATH' field p_fpath.
  endif.

at selection-screen on p_mpath.
  if p_mime is not initial.
    set parameter id 'ZMOCKUP_LOADER_SMIME' field p_mpath.
  endif.

at selection-screen.
  case sy-ucomm.
    when 'FC01'.          "Get SU3 value
      perform get_su3_value.
    when 'FC02'.
      perform upload_mime.
  endcase.

*&---------------------------------------------------------------------*
*&      Form  set_stype
*&---------------------------------------------------------------------*
form set_stype.
  data: l_stype type char4.

  if p_file is not initial.
    l_stype = 'FILE'.
    loop at screen.
      case screen-name.
        when 'P_MPATH' or 'TXT_MP' or 'TXT_MP2'.
          screen-active = 1.
        when 'P_FPATH' or 'TXT_FP'.
          screen-active = 1.
      endcase.
      modify screen.
    endloop.

  elseif p_mime is not initial.
    l_stype = 'MIME'.
    loop at screen.
      case screen-name.
        when 'P_MPATH' or 'TXT_MP'.
          screen-active = 1.
        when 'P_FPATH' or 'TXT_FP' or 'TXT_MP2'.
          screen-active = 0.
      endcase.
      modify screen.
    endloop.

  elseif p_undef is not initial.
    clear: l_stype, p_mpath, p_fpath.
    loop at screen.
      case screen-name.
        when 'P_MPATH' or 'P_FPATH' or 'TXT_FP' or 'TXT_MP' or 'TXT_MP2'.
          screen-active = 0.
      endcase.
      modify screen.
    endloop.
  endif.

  set parameter id 'ZMOCKUP_LOADER_STYPE' field l_stype.

endform.                    "set_stype

*&---------------------------------------------------------------------*
*&      Form  get_stype
*&---------------------------------------------------------------------*
form get_stype.
  data: l_stype type char4,
        l_smime type char128,
        l_spath type char128.

  get parameter id 'ZMOCKUP_LOADER_STYPE' field l_stype.
  get parameter id 'ZMOCKUP_LOADER_SPATH' field l_spath.
  get parameter id 'ZMOCKUP_LOADER_SMIME' field l_smime.
  clear: p_fpath, p_mpath.

  case l_stype.
    when 'FILE'.
      p_file  = 'X'.
      p_mpath = l_smime.
      p_fpath = l_spath.
    when 'MIME'.
      p_mime  = 'X'.
      p_mpath = l_smime.
    when others.
      p_undef = 'X'.
  endcase.
endform.                    "get_stype

*&---------------------------------------------------------------------*
*&      Form  set_file_path
*&---------------------------------------------------------------------*
form f4_file_path changing c_path.
  data: l_path type localfile.

  l_path = lcl_fs=>choose_file_dialog( ).

  c_path = l_path.
  set parameter id 'ZMOCKUP_LOADER_SPATH' field l_path.
endform.                    "set_file_path

*&---------------------------------------------------------------------*
*&      Form  set_mime_path
*&---------------------------------------------------------------------*
form f4_mime_path changing c_path.
  types:
    begin of t_data,
      objid type wwwdata-objid,
      text  type wwwdata-text,
    end of t_data.

  data:
        ls_return type ddshretval,
        lt_data   type table of t_data,
        lt_return type table of ddshretval.

  select distinct objid text from wwwdata
    into corresponding fields of table lt_data
    where relid = 'MI'
    and   objid like 'Z%'
    order by objid.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'OBJID'
      dynprofield     = 'P_MPATH'
      value_org       = 'S'
    tables
      value_tab       = lt_data
      return_tab      = lt_return
    exceptions
      parameter_error = 1
      no_values_found = 2
      others          = 3.

  if sy-subrc is not initial.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  read table lt_return into ls_return index 1.
  p_mpath = ls_return-fieldval.
  set parameter id 'ZMOCKUP_LOADER_SMIME' field p_mpath.
endform.                    "set_file_path

*&---------------------------------------------------------------------*
*&      Form  get_su3_value
*&---------------------------------------------------------------------*
form get_su3_value.
  data l_param type usr05-parva.

  call function 'G_GET_USER_PARAMETER'
    exporting parameter_id    = 'ZMOCKUP_LOADER_SPATH'
    importing parameter_value = l_param.

  p_fpath = l_param.
  set parameter id 'ZMOCKUP_LOADER_SPATH' field l_param.

  call function 'G_GET_USER_PARAMETER'
    exporting parameter_id    = 'ZMOCKUP_LOADER_SMIME'
    importing parameter_value = l_param.

  p_mpath = l_param.
  set parameter id 'ZMOCKUP_LOADER_SMIME' field l_param.

endform.                    "get_su3_value

*&---------------------------------------------------------------------*
*&      Form  get_su3_value
*&---------------------------------------------------------------------*
form upload_mime.
  if p_file is initial.
    message 'Upload only work in FILE mode' type 'E'. "#EC NOTEXT
  endif.
  if p_mpath is initial.
    message 'Please enter MIME name' type 'E'. "#EC NOTEXT
  endif.
  if p_fpath is initial.
    message 'Please enter file path' type 'E'. "#EC NOTEXT
  endif.

  data l_ccat type t000-cccategory.

  select single cccategory into l_ccat from t000 where mandt = sy-mandt.
  if l_ccat = 'P' or l_ccat = 'T'. " Production or test
    message 'Client is productive or QA, upload is disabled' type 'E'. "#EC NOTEXT
    return.
  endif.

  data lx type ref to lcx_error.

  try.
    lcl_utils=>upload(
      iv_filename = |{ p_fpath }|
      iv_key      = p_mpath ).
  catch lcx_error into lx.
    message lx->msg type 'E'.
    return.
  endtry.

  message 'Upload successful' type 'S'. "#EC NOTEXT

endform.                    "upload_mime
