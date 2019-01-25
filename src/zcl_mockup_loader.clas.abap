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
class ZCL_MOCKUP_LOADER definition
  public
  final
  create private .

public section.
  type-pools ABAP .

  class-methods CREATE
    importing
      !I_PATH type STRING
      !I_TYPE type CHAR4 default 'MIME'
      !I_AMT_FORMAT type CHAR2 optional
      !I_ENCODING type ABAP_ENCODING optional
      !I_DATE_FORMAT type CHAR4 optional
      !I_BEGIN_COMMENT type CHAR1 optional
    returning
      value(RO_INSTANCE) type ref to ZCL_MOCKUP_LOADER
    raising
      ZCX_MOCKUP_LOADER_ERROR .
  class-methods CREATE_FROM_SYS_SETTINGS
    importing
      !I_PATH type STRING
      !I_TYPE type CHAR4 default 'MIME'
    returning
      value(RO_INSTANCE) type ref to ZCL_MOCKUP_LOADER
    raising
      ZCX_MOCKUP_LOADER_ERROR .
  class-methods CHECK_VERSION_FITS
    importing
      !I_REQUIRED_VERSION type STRING
    returning
      value(R_FITS) type ABAP_BOOL .

  methods LOAD_RAW
    importing
      !I_OBJ type STRING
      !I_EXT type STRING optional
    exporting
      !E_CONTENT type XSTRING
    raising
      ZCX_MOCKUP_LOADER_ERROR .
  methods LOAD_AND_STORE
    importing
      !I_OBJ type STRING
      !I_STRICT type ABAP_BOOL default ABAP_TRUE
      !I_NAME type CHAR40
      !I_TYPE type CSEQUENCE optional
      !I_TABKEY type ABAP_COMPNAME optional
      !I_TYPE_DESC type ref to CL_ABAP_TYPEDESCR optional
    raising
      ZCX_MOCKUP_LOADER_ERROR .
  methods LOAD_DATA
    importing
      !I_OBJ type STRING
      !I_STRICT type ABAP_BOOL default ABAP_TRUE
      !I_WHERE type ANY optional
    exporting
      !E_CONTAINER type ANY
    raising
      ZCX_MOCKUP_LOADER_ERROR .
  methods SET_PARAMS
    importing
      !I_AMT_FORMAT type CHAR2 optional
      !I_ENCODING type ABAP_ENCODING optional
      !I_DATE_FORMAT type CHAR4 optional
      !I_BEGIN_COMMENT type CHAR1 optional .
protected section.
private section.

  data O_ZIP type ref to CL_ABAP_ZIP .
  data MV_AMT_FORMAT type CHAR2 .
  data MV_ENCODING type ABAP_ENCODING .
  data MV_DATE_FORMAT type CHAR4 .
  data mv_begin_comment type char1.

  methods INITIALIZE
    importing
      !I_PATH type STRING
      !I_TYPE type CHAR4
    raising
      ZCX_MOCKUP_LOADER_ERROR .
  methods PARSE_DATA
    importing
      !I_RAWDATA type STRING
      !I_STRICT type ABAP_BOOL default ABAP_TRUE
      !I_WHERE type ANY optional
    exporting
      !E_CONTAINER type ANY
    raising
      ZCX_MOCKUP_LOADER_ERROR .
  methods READ_ZIP
    importing
      !I_NAME type STRING
    exporting
      !E_RAWDATA type STRING
    raising
      ZCX_MOCKUP_LOADER_ERROR .
  methods CONSTRUCTOR
    raising
      ZCX_MOCKUP_LOADER_ERROR .
ENDCLASS.



CLASS ZCL_MOCKUP_LOADER IMPLEMENTATION.


method CHECK_VERSION_FITS.

  r_fits = zcl_text2tab_utils=>check_version_fits(
    i_current_version  = zif_mockup_loader_constants=>version
    i_required_version = i_required_version ).

endmethod.


method CONSTRUCTOR.

  data lv_required_text2tab_ver type string value 'v2.2.4'.
  if zcl_text2tab_parser=>check_version_fits( lv_required_text2tab_ver ) = abap_false.
    zcx_mockup_loader_error=>raise(
      msg  = |text2tab version ({ zif_text2tab_constants=>version }) is lower than required ({ lv_required_text2tab_ver })|
      code = 'VL' ). "#EC NOTEXT
  endif.

endmethod.


method CREATE.

  create object ro_instance.

  ro_instance->set_params(
    i_amt_format  = i_amt_format
    i_encoding    = i_encoding
    i_date_format = i_date_format
    i_begin_comment = i_begin_comment ).

  data:
        l_src_type  type char4,
        l_src_path  type string,
        l_type_tmp  type char4,
        l_path_tmp  type char128.

  l_src_type = i_type.
  l_src_path = i_path.

  " Get re-direction settings from session memory
  get parameter id 'ZMOCKUP_LOADER_STYPE' field l_type_tmp.
  if l_type_tmp is not initial.
    if l_type_tmp = 'MIME'.
      get parameter id 'ZMOCKUP_LOADER_SMIME' field l_path_tmp.
    elseif l_type_tmp = 'FILE'.
      get parameter id 'ZMOCKUP_LOADER_SPATH' field l_path_tmp.
    endif.
    if l_path_tmp is not initial.
      l_src_type = l_type_tmp.
      l_src_path = l_path_tmp.
    endif.
  endif.

  ro_instance->initialize(
    i_type = l_src_type
    i_path = l_src_path ).

endmethod.


  method create_from_sys_settings.
    types: begin of lt_settings,
             amt_format  type char2,
             codepage    type abap_encoding,
             date_format type char4,
             comment     type char1,
           end of lt_settings.
    types: begin of lt_var,
             name type rvari_vnam,
             low  type rvari_val_255,
           end of lt_var.
    data: l_variable type lt_var,
          l_settings type lt_settings.

    " read system settings (amt_format, encoding, date_format, begin_comment)
    " from table tvarvc
    select name low from tvarvc into l_variable
      where name in ('ZMOCKUP_LOADER_AMT_FORMAT',
        'ZMOCKUP_LOADER_CODEPAGE',
        'ZMOCKUP_LOADER_DATE_FORMAT',
        'ZMOCKUP_LOADER_COMMENT' ).

      transfer_setting amt_format  'ZMOCKUP_LOADER_AMT_FORMAT'.
      transfer_setting codepage    'ZMOCKUP_LOADER_CODEPAGE'.
      transfer_setting date_format 'ZMOCKUP_LOADER_DATE_FORMAT'.
      transfer_setting comment     'ZMOCKUP_LOADER_COMMENT'.

    endselect.

    ro_instance = create(
      i_path          = i_path
      i_type          = i_type
      i_amt_format    = l_settings-amt_format
      i_encoding      = l_settings-codepage
      i_date_format   = l_settings-date_format
      i_begin_comment = l_settings-comment ).

  endmethod.


method INITIALIZE.
  data: l_key       type wwwdatatab,
        l_xstring   type xstring,
        l_size      type int4,
        lt_w3mime   type table of w3mime,
        ls_w3mime   type w3mime.

  " Load data
  case i_type.
  when 'MIME'. " Load from SMW0
    l_key-relid = 'MI'.
    l_key-objid = i_path.

    call function 'WWWDATA_IMPORT'
      exporting
        key    = l_key
      tables
        mime   = lt_w3mime[]
      exceptions
        others = 1.

    if sy-subrc is not initial.
      zcx_mockup_loader_error=>raise( msg = 'SMW0 data import error' code = 'RE' ).  "#EC NOTEXT
    endif.

    describe table lt_w3mime lines l_size.
    l_size = sy-tleng * sy-tfill.

  when 'FILE'. " Load from frontend
    call function 'GUI_UPLOAD'
    exporting
      filename   = i_path
      filetype   = 'BIN'
    importing
      filelength = l_size
    tables
      data_tab   = lt_w3mime
    exceptions
      others = 1.

    if sy-subrc is not initial.
      zcx_mockup_loader_error=>raise( msg = |Cannot upload file: { i_path }| code = 'RE' ). "#EC NOTEXT
    endif.

  when others.
    if sy-subrc is not initial.
      zcx_mockup_loader_error=>raise( msg = 'Wrong source type' code = 'WS' ). "#EC NOTEXT
    endif.

  endcase.

  " Convert to XString
  call function 'SCMS_BINARY_TO_XSTRING'
    exporting
      input_length = l_size
    importing
      buffer       = l_xstring
    tables
      binary_tab   = lt_w3mime[]
    exceptions
      failed       = 1.

  if sy-subrc is not initial.
    zcx_mockup_loader_error=>raise( 'Binary to string error' ). "#EC NOTEXT
  endif.

  " Extract zip
  if o_zip is initial.
    create object o_zip.
  endif.

  o_zip->load(
    exporting  zip    = l_xstring
    exceptions others = 4 ).

  if sy-subrc is not initial or lines( o_zip->files ) = 0.
    zcx_mockup_loader_error=>raise( msg = 'ZIP load failed' code = 'ZE' ).  "#EC NOTEXT
  endif.
endmethod.


method load_and_store.
  data:
        lo_type  type ref to cl_abap_typedescr,
        lo_dtype type ref to cl_abap_datadescr,
        lr_data  type ref to data.

  field-symbols <data> type data.

  if boolc( i_type is supplied ) = boolc( i_type_desc is supplied ).
    zcx_mockup_loader_error=>raise( msg = 'Supply one of i_type or i_type_desc' code = 'TD' ). "#EC NOTEXT
  endif.

  if i_type is supplied.
    cl_abap_typedescr=>describe_by_name(
      exporting  p_name      = i_type
      receiving  p_descr_ref = lo_type
      exceptions others      = 4 ).
  else.
    lo_type = i_type_desc.
  endif.

  if sy-subrc is not initial.
    zcx_mockup_loader_error=>raise( msg = |Type { i_type } not found|  code = 'WT' ). "#EC NOTEXT
  endif.

  " Create container to load zip data to
  lo_dtype ?= lo_type.
  create data lr_data type handle lo_dtype.

  " Load from zip and store
  me->load_data(
    exporting
      i_obj       = i_obj
      i_strict    = i_strict
    importing
      e_container = lr_data ).

  zcl_mockup_loader_store=>get_instance( )->_store(
    i_name     = i_name
    i_data_ref = lr_data
    i_tabkey   = i_tabkey ).

endmethod.


method load_data.
  data l_rawdata  type string.

  if e_container is not supplied.
    zcx_mockup_loader_error=>raise( msg = 'No container supplied' code = 'NC' ). "#EC NOTEXT
  endif.

  me->read_zip(
    exporting
      i_name    = i_obj && '.txt'
    importing
      e_rawdata = l_rawdata ).

  me->parse_data(
    exporting
      i_rawdata   = l_rawdata
      i_strict    = i_strict
      i_where     = i_where
    importing
      e_container = e_container ).

endmethod.


method load_raw.
  data l_filename type string.

  if e_content is not supplied.
    zcx_mockup_loader_error=>raise( msg = 'No container supplied' code = 'NC' ). "#EC NOTEXT
  endif.

  if i_ext is initial.
    l_filename = i_obj && '.txt'.
  else.
    l_filename = i_obj && i_ext.
  endif.

  o_zip->get(
    exporting name    = l_filename
    importing content = e_content ).

endmethod.


method parse_data.
  data:
        lx_dp          type ref to zcx_text2tab_error,
        lo_type_descr  type ref to cl_abap_typedescr,
        lo_table_descr type ref to cl_abap_tabledescr,
        lo_struc_descr type ref to cl_abap_structdescr,
        ld_temp_tab    type ref to data.
  field-symbols:
        <container>     type any,
        <temp_tab>      type standard table.

  " Handle data reference container (use exporting value ???)
  lo_type_descr = cl_abap_typedescr=>describe_by_data( e_container ).
  if lo_type_descr->type_kind = cl_abap_typedescr=>typekind_dref.
    lo_type_descr = cl_abap_typedescr=>describe_by_data_ref( e_container ).
    assign e_container->* to <container>.
  else.
    assign e_container to <container>.
  endif.
  clear <container>.

  " Identify container type and create temp container
  case lo_type_descr->kind.
    when 'T'. " Table
      lo_table_descr ?= lo_type_descr.
      lo_struc_descr ?= lo_table_descr->get_table_line_type( ).
    when 'S'. " Structure
      lo_struc_descr ?= lo_type_descr.
    when others. " Not a table or structure ?
      zcx_mockup_loader_error=>raise( msg = 'Table or structure containers only' code = 'DT' ). "#EC NOTEXT
  endcase.

  lo_table_descr ?= cl_abap_tabledescr=>create(
    p_line_type  = lo_struc_descr
    p_table_kind = cl_abap_tabledescr=>tablekind_std ).
  create data ld_temp_tab type handle lo_table_descr.
  assign ld_temp_tab->* to <temp_tab>.

  try.
    data lo_parser type ref to zcl_text2tab_parser.
    lo_parser = zcl_text2tab_parser=>create(
      i_pattern       = <container>
      i_amount_format = mv_amt_format
      i_date_format   = mv_date_format
      i_begin_comment = mv_begin_comment ).

    lo_parser->parse(
      exporting
        i_data     = i_rawdata
        i_strict   = i_strict
        i_has_head = abap_true " assume head always, maybe change later
      importing
        e_container = <temp_tab> ).
  catch zcx_text2tab_error into lx_dp.
    zcx_mockup_loader_error=>raise( msg = lx_dp->get_text( ) code = 'XE' ).
  endtry.

  " Build filter hash if supplied
  if i_where is not initial.
    zcl_mockup_loader_utils=>filter_table(
      exporting
        i_where     = i_where
        i_tab       = <temp_tab>
      importing
        e_container = <container> ).
  else. " Copy all
    if lo_type_descr->kind = 'S'. " Structure
      read table <temp_tab> into <container> index 1.
    else. " Table
      <container> = <temp_tab>.
    endif.
  endif.

endmethod.


method read_zip.
  data:
        l_xstring type xstring,
        lo_conv   type ref to cl_abap_conv_in_ce,
        l_ex      type ref to cx_root.

  o_zip->get( exporting  name            = i_name
              importing  content         = l_xstring
              exceptions zip_index_error = 1 ).

  if sy-subrc is not initial.
    zcx_mockup_loader_error=>raise( msg = |Cannot read { i_name }| code = 'ZF' ). "#EC NOTEXT
  endif.

  " Remove unicode signatures
  case mv_encoding.
    when zif_mockup_loader_constants=>encoding_utf8.
      shift l_xstring left deleting leading  cl_abap_char_utilities=>byte_order_mark_utf8 in byte mode.
    when zif_mockup_loader_constants=>encoding_utf16.
      shift l_xstring left deleting leading  cl_abap_char_utilities=>byte_order_mark_little in byte mode.
  endcase.

  try.
    lo_conv = cl_abap_conv_in_ce=>create( encoding = mv_encoding ).
    lo_conv->convert( exporting input = l_xstring importing data = e_rawdata ).
  catch cx_root into l_ex.
    zcx_mockup_loader_error=>raise( msg = 'Codepage conversion error' code = 'CP' ). "#EC NOTEXT
  endtry.

endmethod.


method SET_PARAMS.

  if i_amt_format is initial or i_amt_format+1(1) is initial. " Empty param or decimal separator
    me->mv_amt_format = ' ,'. " Defaults
  else.
    me->mv_amt_format = i_amt_format.
  endif.

  if i_encoding is initial.
    me->mv_encoding = zif_mockup_loader_constants=>encoding_utf16.
  else.
    me->mv_encoding = i_encoding.
  endif.

  if i_date_format is initial
    or not i_date_format+3(1) co ' ./-'
    or not ( i_date_format+0(3) = 'DMY'
      or i_date_format+0(3)     = 'MDY'
      or i_date_format+0(3)     = 'YMD' ).
    me->mv_date_format = 'DMY.'. " DD.MM.YYYY
  else.
    me->mv_date_format = i_date_format.
  endif.

  me->mv_begin_comment = i_begin_comment.

endmethod.
ENDCLASS.
