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

types:
  ty_source_type type c length 4.

constants:
  begin of gc_source_type,
    undef type ty_source_type value '',
    file type ty_source_type value 'FILE',
    mime type ty_source_type value 'MIME',
  end of gc_source_type.

types:
  begin of ty_variant,
    report type varid-report,
    variant type varid-variant,
    user type varid-ename,
    text type varit-vtext,
    type type ty_source_type,
    mime type w3objid,
    file type char255,
  end of ty_variant,
  tt_variants type standard table of ty_variant with default key.

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
      and objid = iv_key
      and srtf2 = 0.

    rv_yes = boolc( sy-subrc = 0 ).

  endmethod.

  method get_object_info.

    select single * into corresponding fields of rs_object
      from wwwdata
      where relid = iv_type
      and objid = iv_key
      and srtf2 = 0.

    if sy-subrc > 0.
      lcx_error=>raise( 'Cannot read W3xx info' ). "#EC NOTEXT
    endif.

  endmethod.

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

  endmethod.


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

  endmethod.

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

    if sy-subrc > 0 or lv_uact <> cl_gui_frontend_services=>action_ok.
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

  endmethod.
endclass.

**********************************************************************
* VARIANT DIALOG
**********************************************************************
class lcl_variants_dialog definition final.
  public section.
    class-methods create
      returning
        value(ro_instance) type ref to lcl_variants_dialog.
    methods popup
      importing
        i_own_only type abap_bool default abap_false
      returning
        value(rs_selected) type ty_variant.
  private section.
    methods display_popup
      importing
        it_variants type tt_variants
      returning
        value(rs_selected) type ty_variant.
    methods select_variants
      importing
        i_own_only type abap_bool default abap_false
      returning
        value(rt_variants) type tt_variants.
    methods select_variant_values
      changing
        cs_variant type ty_variant.
endclass.

class lcl_variants_dialog implementation.

  method create.
    create object ro_instance.
  endmethod.

  method popup.

    data lt_variants type tt_variants.
    lt_variants = select_variants( i_own_only ).
    rs_selected = display_popup( lt_variants ).

  endmethod.

  method select_variants.

    select h~report h~variant h~ename as user t~vtext as text
      into corresponding fields of table rt_variants
      from varid as h left outer join varit as t on
        h~report = t~report
        and h~variant = t~variant
        and t~langu = sy-langu
      where h~report = sy-cprog.

    if i_own_only = abap_true.
      delete rt_variants where user <> sy-uname.
    endif.

    field-symbols <v> like line of rt_variants.
    loop at rt_variants assigning <v>.
      select_variant_values( changing cs_variant = <v> ).
    endloop.

  endmethod.

  method select_variant_values.

    data lt_values type table of rsparamsl_255.
    data ls_val like line of lt_values.

    call function 'RS_VARIANT_VALUES_TECH_DAT_255'
      exporting
        report  = cs_variant-report
        variant = cs_variant-variant
      tables
        variant_values = lt_values
      exceptions
        others = 1.

    if sy-subrc <> 0.
      return. " hmmm, refactor
    endif.

    " detect type
    cs_variant-type = gc_source_type-undef.
    read table lt_values into ls_val with key selname = 'P_FILE'.
    if sy-subrc = 0 and ls_val-low = 'X'.
      cs_variant-type = gc_source_type-file.
    else.
      read table lt_values into ls_val with key selname = 'P_MIME'.
      if sy-subrc = 0 and ls_val-low = 'X'.
        cs_variant-type = gc_source_type-mime.
      endif.
    endif.

    read table lt_values into ls_val with key selname = 'P_FPATH'.
    if sy-subrc = 0.
      cs_variant-file = ls_val-low.
    endif.

    read table lt_values into ls_val with key selname = 'P_MPATH'.
    if sy-subrc = 0.
      cs_variant-mime = ls_val-low.
    endif.

  endmethod.

  method display_popup.

    data lt_fieldcat type slis_t_fieldcat_alv.
    data ls_selfield type slis_selfield.
    data lv_exit type c length 1.
    field-symbols <f> like line of lt_fieldcat.

    append initial line to lt_fieldcat assigning <f>.
    <f>-col_pos   = 1.
    <f>-fieldname = 'VARIANT'.
    <f>-seltext_m = 'Variant'.
    <f>-ddictxt   = 'M'.
    <f>-outputlen = 15.

    append initial line to lt_fieldcat assigning <f>.
    <f>-col_pos   = 2.
    <f>-fieldname = 'TEXT'.
    <f>-seltext_m = 'Description'.
    <f>-ddictxt   = 'M'.
    <f>-outputlen = 15.

    append initial line to lt_fieldcat assigning <f>.
    <f>-col_pos   = 3.
    <f>-fieldname = 'USER'.
    <f>-seltext_m = 'Created by'.
    <f>-ddictxt   = 'M'.

    append initial line to lt_fieldcat assigning <f>.
    <f>-col_pos   = 4.
    <f>-fieldname = 'TYPE'.
    <f>-seltext_m = 'Source'.
    <f>-ddictxt   = 'M'.
    <f>-outputlen = 6.

    append initial line to lt_fieldcat assigning <f>.
    <f>-col_pos   = 5.
    <f>-fieldname = 'MIME'.
    <f>-seltext_m = 'Target mime'.
    <f>-ddictxt   = 'M'.
    <f>-outputlen = 20.

    append initial line to lt_fieldcat assigning <f>.
    <f>-col_pos   = 6.
    <f>-fieldname = 'FILE'.
    <f>-seltext_l = 'Source file'.
    <f>-ddictxt   = 'L'.
    <f>-outputlen = 40.

    call function 'REUSE_ALV_POPUP_TO_SELECT'
      exporting
        i_title               = 'Select variant'
        i_zebra               = 'X'
        i_screen_start_column = 5
        i_screen_start_line   = 5
        i_tabname             = '1'
        it_fieldcat           = lt_fieldcat
      importing
        es_selfield           = ls_selfield
        e_exit                = lv_exit
      tables
        t_outtab              = it_variants
      exceptions
        program_error         = 1
        others                = 2.

    if lv_exit is not initial.
      return.
    endif.

    read table it_variants into rs_selected index ls_selfield-tabindex.
    assert sy-subrc = 0.

  endmethod.

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
selection-screen: function key 3.


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
  txt_mp2  = '(to redirect)'.                               "#EC NOTEXT

  data ls_btn type smp_dyntxt.

  ls_btn-icon_id   = icon_variants.
  ls_btn-icon_text = 'Variants'.
  ls_btn-quickinfo = 'Select from saved variants and apply'.
  sscrfields-functxt_01 = ls_btn.

  ls_btn-icon_id   = icon_visit.
  ls_btn-icon_text = 'My variants'.
  ls_btn-quickinfo = 'Select from OWN saved variants and apply'.
  sscrfields-functxt_02 = ls_btn.

  ls_btn-icon_id   = icon_import.
  ls_btn-icon_text = 'Upload to MIME'.
  ls_btn-quickinfo = 'Upload file to to MIME storage (smw0)'.
  sscrfields-functxt_03 = ls_btn.

  " Hide MIME option, seems to be not used. Also need refactoring in terms of target/source mime (2 fields)
  loop at screen.
    if screen-name = 'P_MIME' or screen-name = 'TXT_MIME'.
      screen-active = 0.
    endif.
    modify screen.
  endloop.

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
  if p_mime is not initial or p_file is not initial.
    set parameter id 'ZMOCKUP_LOADER_SMIME' field p_mpath.
  endif.

at selection-screen.
  case sy-ucomm.
    when 'FC01'.
      perform get_variants using abap_false.
    when 'FC02'.
      perform get_variants using abap_true.
    when 'FC03'.
      perform upload_mime.
  endcase.

*&---------------------------------------------------------------------*
*&      Form  set_stype
*&---------------------------------------------------------------------*
form set_stype.

  data l_stype type ty_source_type.

  if p_file is not initial.
    l_stype = 'FILE'.
    loop at screen.
      case screen-name.
        when 'P_MPATH' or 'TXT_MP' or 'TXT_MP2'.
          screen-active = 1.
        when 'P_FPATH' or 'TXT_FP'.
          screen-active = 1.
        when 'P_MIME' or 'TXT_MIME'.
          screen-active = 0.
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
        when 'P_MIME' or 'TXT_MIME'.
          screen-active = 0.
      endcase.
      modify screen.
    endloop.
  endif.

  set parameter id 'ZMOCKUP_LOADER_STYPE' field l_stype.

endform.

*&---------------------------------------------------------------------*
*&      Form  get_stype
*&---------------------------------------------------------------------*
form get_stype.

  data:
    l_stype type ty_source_type,
    l_smime type w3objid,
    l_spath type char255.

  get parameter id 'ZMOCKUP_LOADER_STYPE' field l_stype.
  get parameter id 'ZMOCKUP_LOADER_SPATH' field l_spath.
  get parameter id 'ZMOCKUP_LOADER_SMIME' field l_smime.
  clear: p_fpath, p_mpath, p_undef, p_file, p_mime.

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

endform.

*&---------------------------------------------------------------------*
*&      Form  set_file_path
*&---------------------------------------------------------------------*
form f4_file_path changing c_path type char128.
  c_path = lcl_fs=>choose_file_dialog( ).
  set parameter id 'ZMOCKUP_LOADER_SPATH' field c_path.
endform.

*&---------------------------------------------------------------------*
*&      Form  set_mime_path
*&---------------------------------------------------------------------*
form f4_mime_path changing c_path type char40.
  types:
    begin of lty_data,
      objid type wwwdata-objid,
      text  type wwwdata-text,
    end of lty_data.

  data:
    ls_return type ddshretval,
    lt_data   type table of lty_data,
    lt_return type table of ddshretval.

  select distinct objid text from wwwdata
    into corresponding fields of table lt_data
    where relid = 'MI'
    and objid like 'Z%'
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

endform.

*&---------------------------------------------------------------------*
*&      Form  get_su3_value
*&---------------------------------------------------------------------*
form get_variants using p_own_only type abap_bool.

  data ls_variant type ty_variant.
  data msg type string.

  ls_variant = lcl_variants_dialog=>create( )->popup( i_own_only = p_own_only ).
  if ls_variant is not initial.
    msg = |{ ls_variant-variant } selected|.
    message msg type 'S'.
  else.
    return.
  endif.

  set parameter id 'ZMOCKUP_LOADER_STYPE' field ls_variant-type.
  set parameter id 'ZMOCKUP_LOADER_SPATH' field ls_variant-file.
  set parameter id 'ZMOCKUP_LOADER_SMIME' field ls_variant-mime.

  perform get_stype.

endform.

*&---------------------------------------------------------------------*
*&      Form  upload_mime
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

endform.
