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

  call function 'F4_FILENAME'
    importing
      file_name = l_path.

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
    message 'Upload only work in FILE mode' type 'E' display like 'S'. "#EC NOTEXT
  endif.
  if p_mpath is initial.
    message 'Please enter MIME name' type 'E' display like 'S'. "#EC NOTEXT
  endif.
  if p_fpath is initial.
    message 'Please enter file path' type 'E' display like 'S'. "#EC NOTEXT
  endif.

  data:
        lx     type ref to cx_root,
        l_str  type string,
        lo_obj type ref to object.
  try.
    create object lo_obj type ('ZCL_W3MIME_UTILS').
  catch cx_sy_create_error into lx.
    l_str = 'Install ZW3MIMEPOLL for this feature to work. https://github.com/sbcgua/abap_w3mi_poller'. "#EC NOTEXT
    message l_str type 'E' display like 'S'.
    return.
  endtry.

  try.
    call method ('ZCL_W3MIME_UTILS')=>('UPLOAD')
      exporting
        iv_filename = |{ p_fpath }|
        iv_key      = p_mpath.
  catch cx_static_check into lx.
    l_str = lx->get_text( ).
    message l_str type 'E' display like 'S'.
    return.
  endtry.

  message 'Upload successful' type 'S'. "#EC NOTEXT

endform.                    "upload_mime
