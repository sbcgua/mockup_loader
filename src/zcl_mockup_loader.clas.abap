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
    type-pools abap .

    interfaces zif_mockup_loader.
    aliases:
      load_blob for zif_mockup_loader~load_blob,
      load_data for zif_mockup_loader~load_data.

    class-methods create
      importing
        !i_path type string
        !i_type type char4 default 'MIME'
        !i_amt_format type char2 optional
        !i_encoding type abap_encoding optional
        !i_date_format type char4 optional
        !i_begin_comment type char1 optional
      returning
        value(ro_instance) type ref to zcl_mockup_loader
      raising
        zcx_mockup_loader_error .
    class-methods create_from_sys_settings
      importing
        !i_path type string
        !i_type type char4 default 'MIME'
      returning
        value(ro_instance) type ref to zcl_mockup_loader
      raising
        zcx_mockup_loader_error .
    class-methods assert_version
      importing
        !i_required_version type string
      raising
        zcx_mockup_loader_error .
    class-methods check_version_fits
      importing
        !i_required_version type string
      returning
        value(r_fits) type abap_bool .
    methods load_raw
      importing
        !i_obj type string
        !i_ext type string optional
      exporting
        !e_content type xstring
      raising
        zcx_mockup_loader_error .
    methods load_and_store
      importing
        !i_obj type string
        !i_strict type abap_bool default abap_true
        !i_name type char40
        !i_type type csequence optional
        !i_tabkey type abap_compname optional
        !i_type_desc type ref to cl_abap_typedescr optional
      raising
        zcx_mockup_loader_error .
    methods set_params
      importing
        !i_amt_format type char2 optional
        !i_encoding type abap_encoding optional
        !i_date_format type char4 optional
        !i_begin_comment type char1 optional .
  protected section.
  private section.

    data mo_zip type ref to cl_abap_zip .
    data mv_amt_format type char2 .
    data mv_encoding type abap_encoding .
    data mv_date_format type char4 .
    data mv_begin_comment type char1.

    methods initialize
      importing
        !i_path type string
        !i_type type char4
      raising
        zcx_mockup_loader_error .
    methods parse_data
      importing
        !i_rawdata type string
        !i_strict type abap_bool default abap_true
        !i_corresponding type abap_bool default abap_false
        !i_deep type abap_bool default abap_false
        !i_where type any optional
      exporting
        !e_container type any
      raising
        zcx_mockup_loader_error .
    methods read_zip
      importing
        !i_name type string
      exporting
        !e_rawdata type string
      raising
        zcx_mockup_loader_error .
    methods constructor
      raising
        zcx_mockup_loader_error .
    class-methods redirect_source
      changing
        c_src_type      type char4
        c_src_path      type string.
    class-methods build_table_type
      importing
        io_type_descr   type ref to cl_abap_typedescr
        it_filter       type zcl_mockup_loader_utils=>tt_filter
        i_corresponding type abap_bool default abap_false
      returning
        value(ro_table_descr) type ref to cl_abap_tabledescr
      raising
        zcx_mockup_loader_error .
ENDCLASS.



CLASS ZCL_MOCKUP_LOADER IMPLEMENTATION.


  method assert_version.

    data lv_version_ok type abap_bool.

    lv_version_ok = zcl_text2tab_utils=>check_version_fits(
      i_current_version  = zif_mockup_loader_constants=>version
      i_required_version = i_required_version ).

    if lv_version_ok = abap_false.
      zcx_mockup_loader_error=>raise(
        |mockup loader version ({ zif_mockup_loader_constants=>version
        }) < required ({ i_required_version })| ). "#EC NOTEXT
    endif.

  endmethod.


  method build_table_type.

    data lo_table_descr type ref to cl_abap_tabledescr.
    data lo_struc_descr type ref to cl_abap_structdescr.

    case io_type_descr->kind.
      when 'T'. " Table
        lo_table_descr ?= io_type_descr.
        lo_struc_descr ?= lo_table_descr->get_table_line_type( ).
      when 'S'. " Structure
        lo_struc_descr ?= io_type_descr.
      when others. " Not a table or structure ?
        zcx_mockup_loader_error=>raise( msg = 'Table or structure containers only' code = 'DT' ). "#EC NOTEXT
    endcase.

    if i_corresponding = abap_true and lines( it_filter ) > 0.

      data lt_components type cl_abap_structdescr=>component_table.
      data lt_components_to_add type cl_abap_structdescr=>component_table.
      data ld_value type ref to cl_abap_typedescr.
      data ld_range_tab type ref to cl_abap_tabledescr.
      data ld_range_line type ref to cl_abap_structdescr.

      lt_components = lo_struc_descr->get_components( ).
      sort lt_components by name.

      field-symbols <f> like line of it_filter.
      field-symbols <addcomp> like line of lt_components_to_add.

      loop at it_filter assigning <f>.
        read table lt_components transporting no fields
          binary search
          with key name = <f>-name.
        if sy-subrc is initial.
          continue. " found, no need to add
        endif.
        ld_value = cl_abap_typedescr=>describe_by_data_ref( <f>-valref ).
        if <f>-type = zcl_mockup_loader_utils=>c_filter_type-range.
          ld_range_tab ?= ld_value.
          ld_range_line ?= ld_range_tab->get_table_line_type( ).
          ld_value = ld_range_line->get_component_type( 'LOW' ).
        endif.
        append initial line to lt_components_to_add assigning <addcomp>.
        <addcomp>-name = to_upper( <f>-name ).
        <addcomp>-type ?= ld_value.
      endloop.

      append lines of lt_components_to_add to lt_components.
      lo_struc_descr = cl_abap_structdescr=>get( lt_components ).

    endif.

    ro_table_descr = cl_abap_tabledescr=>create( lo_struc_descr ).

  endmethod.


  method check_version_fits.

    r_fits = zcl_text2tab_utils=>check_version_fits(
      i_current_version  = zif_mockup_loader_constants=>version
      i_required_version = i_required_version ).

  endmethod.


  method constructor.

    data lv_required_text2tab_ver type string value 'v2.3.2'.
    if zcl_text2tab_parser=>check_version_fits( lv_required_text2tab_ver ) = abap_false.
      zcx_mockup_loader_error=>raise(
        msg  = |text2tab version ({ zif_text2tab_constants=>version
          }) is lower than required ({ lv_required_text2tab_ver })|
        code = 'VL' ). "#EC NOTEXT
    endif.

  endmethod.


  method create.

    create object ro_instance.

    ro_instance->set_params(
      i_amt_format  = i_amt_format
      i_encoding    = i_encoding
      i_date_format = i_date_format
      i_begin_comment = i_begin_comment ).

    data l_src_type type char4.
    data l_src_path type string.

    l_src_type = i_type.
    l_src_path = i_path.

    redirect_source(
      changing
        c_src_type = l_src_type
        c_src_path = l_src_path ).

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


  method initialize.
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
    if mo_zip is initial.
      create object mo_zip.
    endif.

    mo_zip->load(
      exporting  zip    = l_xstring
      exceptions others = 4 ).

    if sy-subrc is not initial or lines( mo_zip->files ) = 0.
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

    mo_zip->get(
      exporting name    = l_filename
      importing content = e_content ).

  endmethod.


  method parse_data.
    data:
      lx_dp          type ref to zcx_text2tab_error,
      lo_type_descr  type ref to cl_abap_typedescr,
      lt_filter      type zcl_mockup_loader_utils=>tt_filter,
      lo_table_descr type ref to cl_abap_tabledescr,
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

    " Build filter
    if i_where is not initial.
      lt_filter = zcl_mockup_loader_utils=>build_filter( i_where ).
    endif.

    " Identify container type and create temp container
    lo_table_descr = build_table_type(
      i_corresponding = i_corresponding
      it_filter       = lt_filter
      io_type_descr   = lo_type_descr ).
    create data ld_temp_tab type handle lo_table_descr.
    assign ld_temp_tab->* to <temp_tab>.

    try.
      data lo_parser type ref to zcl_text2tab_parser.
      data lo_deep_provider type ref to zcl_mockup_loader_deep_providr.

      if i_deep = abap_true.
        create object lo_deep_provider exporting ii_ml_instance = me.
      endif.

      lo_parser = zcl_text2tab_parser=>create(
        i_pattern       = <temp_tab>
        i_amount_format = mv_amt_format
        i_date_format   = mv_date_format
        i_deep_provider = lo_deep_provider
        i_begin_comment = mv_begin_comment ).

      lo_parser->parse(
        exporting
          i_data     = i_rawdata
          i_strict   = i_strict
          i_corresponding = i_corresponding
          i_has_head = abap_true " assume head always, maybe change later
        importing
          e_container = <temp_tab> ).
    catch zcx_text2tab_error into lx_dp.
      zcx_mockup_loader_error=>raise( msg = lx_dp->get_text( ) code = 'XE' ).
    endtry.

    zcl_mockup_loader_utils=>filter_table(
      exporting
        i_where     = lt_filter
        i_tab       = <temp_tab>
        i_corresponding = i_corresponding
      importing
        e_container = <container> ).

  endmethod.


  method read_zip.
    data:
          l_xstring type xstring,
          lo_conv   type ref to cl_abap_conv_in_ce,
          l_ex      type ref to cx_root.

    mo_zip->get( exporting  name            = i_name
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


  method redirect_source.

    data:
      l_redirect_type type char4,
      l_redirect_mime type char128,
      l_redirect_file type char128.

    " Get re-direction settings from session memory
    get parameter id 'ZMOCKUP_LOADER_STYPE' field l_redirect_type.
    if l_redirect_type is initial.
      return.
    endif.

    get parameter id 'ZMOCKUP_LOADER_SMIME' field l_redirect_mime.
    get parameter id 'ZMOCKUP_LOADER_SPATH' field l_redirect_file.

    if l_redirect_type = 'MIME' and l_redirect_mime is not initial.
      " Just redirect always - maybe not good, maybe refactor
      " or maybe remove feature at all ? not used, at least by me :)
      c_src_type = l_redirect_type.
      c_src_path = l_redirect_mime.
    elseif l_redirect_type = 'FILE' and l_redirect_file is not initial and l_redirect_mime = c_src_path.
      " Redirect only if redirect mime = original mime
      c_src_type = l_redirect_type.
      c_src_path = l_redirect_file.
    endif.

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


  method zif_mockup_loader~load_blob.

    mo_zip->get(
      exporting
        name    = i_obj_path
      importing
        content = r_content ).

  endmethod.


  method zif_mockup_loader~load_data.

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
        i_corresponding = i_corresponding
        i_deep      = i_deep
        i_where     = i_where
      importing
        e_container = e_container ).

  endmethod.
ENDCLASS.
