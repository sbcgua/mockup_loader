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

  types:
    tt_string type standard table of string with default key.
  types:
    begin of ty_store,
      name    type char40,
      tabkey  type abap_compname,
      data    type ref to data,
    end of ty_store .
  types:
    tt_store type standard table of ty_store with key name .
  types:
    begin of ty_filter,
      name  type string,
      range type ref to data,
      type  type char1,
    end of ty_filter .
  types:
    tt_filter type standard table of ty_filter with key name .
  types:
    begin of ty_where,
      name  type string,
      range type ref to data,
    end of ty_where .
  types:
    tt_where type standard table of ty_where with key name .

  class-methods CLASS_CONSTRUCTOR .
  class-methods CLASS_SET_SOURCE
    importing
      !I_PATH type STRING
      !I_TYPE type CHAR4 .
  type-pools ABAP .
  class-methods CLASS_SET_PARAMS
    importing
      !I_AMT_FORMAT type CHAR2 optional
      !I_ENCODING type ABAP_ENCODING optional
      !I_DATE_FORMAT type CHAR4 optional .
  class-methods GET_INSTANCE
    returning
      value(RO_INSTANCE) type ref to ZCL_MOCKUP_LOADER
    raising
      CX_STATIC_CHECK .
  class-methods FREE_INSTANCE .
  methods LOAD_RAW
    importing
      !I_OBJ type STRING
      !I_EXT type STRING optional
    exporting
      !E_CONTENT type XSTRING
    raising
      CX_STATIC_CHECK .
  methods LOAD_AND_STORE
    importing
      !I_OBJ type STRING
      !I_STRICT type ABAP_BOOL default ABAP_TRUE
      !I_NAME type CHAR40
      !I_TYPE type CSEQUENCE
      !I_TABKEY type ABAP_COMPNAME optional
    raising
      CX_STATIC_CHECK .
  methods LOAD_DATA
    importing
      !I_OBJ type STRING
      !I_STRICT type ABAP_BOOL default ABAP_TRUE
      !I_WHERE type ANY optional
    exporting
      !E_CONTAINER type ANY
    raising
      CX_STATIC_CHECK .
  methods PURGE
    importing
      !I_NAME type CHAR40 .
  class-methods RETRIEVE
    importing
      !I_NAME type CHAR40
      !I_SIFT type CLIKE optional
      !I_WHERE type ANY optional
    exporting
      !E_DATA type ANY
    exceptions
      RETRIEVE_ERROR .
  methods STORE
    importing
      !I_NAME type CHAR40
      !I_DATA type ANY
      !I_TABKEY type ABAP_COMPNAME optional
    raising
      CX_STATIC_CHECK .
protected section.
private section.

  class-data GO_INSTANCE type ref to ZCL_MOCKUP_LOADER .
  data O_ZIP type ref to CL_ABAP_ZIP .
  data AT_STORE type TT_STORE .
  class-data G_MOCKUP_SRC_PATH type STRING .
  class-data G_MOCKUP_SRC_TYPE type CHAR4 .
  class-data G_AMT_FORMAT type CHAR2 .
  type-pools ABAP .
  class-data G_ENCODING type ABAP_ENCODING .
  class-data G_DATE_FORMAT type CHAR4 .

  methods INITIALIZE
    raising
      CX_STATIC_CHECK .
  methods MAP_FILE_STRUCTURE
    importing
      !I_LINE type STRING
      !IO_STRUC_DESCR type ref to CL_ABAP_STRUCTDESCR
      !I_STRICT type ABAP_BOOL
    exporting
      !ET_MAP type INT4_TABLE
    raising
      CX_STATIC_CHECK .
  methods PARSE_APPLY_EXIT
    importing
      !I_DATA type STRING
      !I_CONVEXIT type STRING
    exporting
      !E_FIELD type ANY
    raising
      CX_STATIC_CHECK .
  methods PARSE_DATA
    importing
      !I_RAWDATA type STRING
      !I_STRICT type ABAP_BOOL default ABAP_TRUE
      !I_WHERE type ANY optional
    exporting
      !E_CONTAINER type ANY
    raising
      CX_STATIC_CHECK .
  methods PARSE_LINE
    importing
      !I_LINE type STRING
      !IT_MAP type INT4_TABLE
      !IO_STRUC_DESCR type ref to CL_ABAP_STRUCTDESCR
      !I_INDEX type INT4
    exporting
      !ES_CONTAINER type ANY
    raising
      CX_STATIC_CHECK .
  methods PARSE_FIELD
    importing
      !IS_COMPONENT type ABAP_COMPDESCR
      !I_DATA type STRING
    exporting
      !E_FIELD type ANY
    raising
      CX_STATIC_CHECK .
  methods PARSE_DATE
    importing
      !I_VALUE type STRING
    exporting
      !E_FIELD type D
    raising
      CX_STATIC_CHECK .
  methods BUILD_FILTER
    importing
      !I_WHERE type ANY
    exporting
      !E_FILTER type TT_FILTER
    raising
      CX_STATIC_CHECK .
  methods DOES_LINE_FIT_FILTER
    importing
      !I_LINE type ANY
      !I_FILTER type TT_FILTER
    returning
      value(R_YESNO) type ABAP_BOOL .
  methods READ_ZIP
    importing
      !I_NAME type STRING
    exporting
      !E_RAWDATA type STRING
    raising
      CX_STATIC_CHECK .
  methods _STORE
    importing
      !I_NAME type CHAR40
      !I_DATA_REF type ref to DATA
      !I_TABKEY type ABAP_COMPNAME optional
    raising
      CX_STATIC_CHECK .
  methods _RETRIEVE
    importing
      !I_NAME type CHAR40
      !I_SIFT type CLIKE optional
      !I_WHERE type ANY optional
    exporting
      !E_DATA type ANY
    raising
      CX_STATIC_CHECK .
  class-methods BREAK_TO_LINES
    importing
      !I_TEXT type STRING
    returning
      value(RT_TAB) type TT_STRING .
ENDCLASS.



CLASS ZCL_MOCKUP_LOADER IMPLEMENTATION.


method break_to_lines.
  data:
        l_found type i,
        l_break type string value cl_abap_char_utilities=>cr_lf.

  " Detect line break
  l_found = find( val = i_text sub = cl_abap_char_utilities=>cr_lf ).
  if l_found < 0.
    l_found = find( val = i_text sub = cl_abap_char_utilities=>form_feed ).
    if l_found >= 0.
      l_break = cl_abap_char_utilities=>form_feed.
    endif.
  endif.

  split i_text at l_break into table rt_tab.

endmethod.


method BUILD_FILTER.
  data dy_templ      type ref to cl_abap_tabledescr.
  data dy_type       type ref to cl_abap_typedescr.
  data dy_struc      type ref to cl_abap_structdescr.
  data dy_table      type ref to cl_abap_tabledescr.

  data l_filter      type ty_filter.
  data l_where       type ty_where.
  data lt_filter     type tt_filter.
  data lt_components type cl_abap_structdescr=>component_table.
  data l_component   like line of lt_components.

  field-symbols <ftable> type any table.
  field-symbols <cond>   type string.

  clear     e_filter.
  dy_type   = cl_abap_typedescr=>describe_by_data( i_where ).
  dy_templ ?= cl_abap_typedescr=>describe_by_name( 'SVER_TABLE_TYPE_VERI_RANGE' ).

  try.
    case dy_type->type_kind.
    when cl_abap_typedescr=>typekind_table. " Table -> expect tt_where
      dy_table ?= dy_type.
      dy_struc ?= dy_table->get_table_line_type( ).
      if not dy_struc->absolute_name cs '\CLASS=ZCL_MOCKUP_LOADER\TYPE=TY_WHERE'.
        lcx_error=>raise( msg = |I_WHERE table must be of TT_WHERE type| code = 'WT' ).   "#EC NOTEXT
      endif.

      assign i_where to <ftable>.
      loop at <ftable> into l_where.
        l_filter-name  = l_where-name.
        l_filter-range = l_where-range.
        l_filter-type  = 'R'. " Range
        dy_table ?= cl_abap_typedescr=>describe_by_data_ref( l_filter-range ). " Assume table, cast_error otherwise
        if dy_table->key ne dy_templ->key. " Not range ?
          lcx_error=>raise( msg = |I_WHERE-RANGE must be a range table| code = 'RT' ).   "#EC NOTEXT
        endif.
        append l_filter to lt_filter.
      endloop.

    when cl_abap_typedescr=>typekind_struct2.
      dy_struc      ?= dy_type.

      if dy_struc->absolute_name = '\CLASS=ZCL_MOCKUP_LOADER\TYPE=TY_WHERE'.
        l_where        = i_where.
        l_filter-name  = l_where-name.
        l_filter-range = l_where-range.
        l_filter-type  = 'R'. " Range
        dy_table ?= cl_abap_typedescr=>describe_by_data_ref( l_filter-range ). " Assume table, cast_error otherwise
        if dy_table->key ne dy_templ->key. " Not range ?
          lcx_error=>raise( msg = |I_WHERE-RANGE must be a range table| code = 'RT' ).   "#EC NOTEXT
        endif.
        append l_filter to lt_filter.

      else.                      " structure with named components per range
        lt_components  = dy_struc->get_components( ).
        loop at lt_components into l_component.
          if l_component-type->kind <> cl_abap_typedescr=>kind_table.
            lcx_error=>raise( msg = |I_WHERE must be a structure of ranges or TY_WHERE| code = 'WS' ).   "#EC NOTEXT
          endif.

          dy_table ?= l_component-type.
          if dy_table->key ne dy_templ->key. " Not range-like structure ?
            lcx_error=>raise( msg = |I_WHERE must be a structure of ranges or TY_WHERE| code = 'WS' ).   "#EC NOTEXT
          endif.

          l_filter-name = l_component-name.
          l_filter-type = 'R'. " Range
          assign component l_component-name of structure i_where to <ftable>.
          get reference of <ftable> into l_filter-range.
          append l_filter to lt_filter.
        endloop.
      endif.

    when cl_abap_typedescr=>typekind_char or cl_abap_typedescr=>typekind_string.
      l_filter-type = 'S'. " String
      create data l_filter-range type string.
      assign l_filter-range->* to <cond>.

      split i_where at '=' into l_filter-name <cond>.
      shift l_filter-name right deleting trailing space.
      shift l_filter-name left  deleting leading space.
      shift <cond>        right deleting trailing space.
      shift <cond>        left  deleting leading space.
      translate l_filter-name to upper case.

      if l_filter-name is initial or <cond> is initial.
        lcx_error=>raise( msg = |Incorrect I_WHERE string pattern| code = 'SP' ).   "#EC NOTEXT
      endif.

      append l_filter to lt_filter.

    when others.
      lcx_error=>raise( msg = |Unsupported type { dy_type->absolute_name } of I_WHERE| code = 'UT' ).   "#EC NOTEXT
    endcase.

  catch cx_sy_move_cast_error.
    lcx_error=>raise( msg = |CX_SY_MOVE_CAST_ERROR @BUILD_FILTER()| code = 'CE' ).   "#EC NOTEXT
  endtry.

  e_filter = lt_filter.

endmethod.


method class_constructor.
  class_set_source( i_type = 'MIME' i_path = '' ). " Defaults
  class_set_params( i_amt_format = '' i_encoding = '4103' ). " Defaults, UTF16
endmethod.


method CLASS_SET_PARAMS.
  if i_amt_format is initial or g_amt_format+1(1) is initial. " Empty param or decimal separator
    g_amt_format = ' ,'. " Defaults
  else.
    g_amt_format = i_amt_format.
  endif.

  if i_encoding is initial.
    g_encoding = '4103'. " UTF16
  else.
    g_encoding = i_encoding.
  endif.

  if i_date_format is initial
    or not i_date_format+3(1) co ' ./-'
    or not ( i_date_format+0(3) = 'DMY'
      or i_date_format+0(3)     = 'MDY'
      or i_date_format+0(3)     = 'YMD' ).
    g_date_format = 'DMY.'.
  else.
    g_date_format = i_date_format.
  endif.

endmethod.


method class_set_source.
  if i_type = 'MIME' or i_type = 'FILE'. "TODO some more sophisticated error handling
    g_mockup_src_type = i_type.
    g_mockup_src_path = i_path.
  endif.
endmethod.


method DOES_LINE_FIT_FILTER.
  data l_filter         type ty_filter.
  field-symbols <field> type any.
  field-symbols <range> type any table.
  field-symbols <cond>  type string.

  r_yesno = abap_true.

  loop at i_filter into l_filter.
    assign component l_filter-name of structure i_line to <field>.
    check <field> is assigned. " Just skip irrelevant ranges

    if l_filter-type = 'R'.               " Range
      assign l_filter-range->* to <range>.
    else.                                 " String
      assign l_filter-range->* to <cond>.
    endif.

    if <range> is assigned and not <field> in <range>
    or <cond>  is assigned and not <field> = <cond>. " cx_sy_conversion_error does not catch that :(
      r_yesno = abap_false.
      exit.
    endif.

    unassign: <field>, <range>, <cond>.
  endloop.

endmethod.


method free_instance.
  if go_instance is not initial.
    free go_instance.
  endif.
endmethod.


method get_instance.

  if go_instance is initial.
    create object go_instance.
    go_instance->initialize( ).
  endif.

  ro_instance = go_instance.

endmethod.


method INITIALIZE.
  data: l_key       type wwwdatatab,
        l_xstring   type xstring,
        l_size      type int4,
        lt_w3mime   type table of w3mime,
        ls_w3mime   type w3mime.

  data:
        l_src_type  type char4,
        l_src_path  type string,
        l_type_tmp  type char4,
        l_path_tmp  type char128.

  l_src_type = g_mockup_src_type.
  l_src_path = g_mockup_src_path.

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

  " Load data
  case l_src_type.
  when 'MIME'. " Load from SMW0
    l_key-relid = 'MI'.
    l_key-objid = l_src_path.

    call function 'WWWDATA_IMPORT'
      exporting
        key    = l_key
      tables
        mime   = lt_w3mime[]
      exceptions
        others = 1.

    if sy-subrc is not initial.
      lcx_error=>raise( msg = 'SMW0 data import error' code = 'RE' ).  "#EC NOTEXT
    endif.

    describe table lt_w3mime lines l_size.
    l_size = sy-tleng * sy-tfill.

  when 'FILE'. " Load from frontend
    call function 'GUI_UPLOAD'
    exporting
      filename   = l_src_path
      filetype   = 'BIN'
    importing
      filelength = l_size
    tables
      data_tab   = lt_w3mime
    exceptions
      others = 1.

    if sy-subrc is not initial.
      lcx_error=>raise( msg = |Cannot upload file: { l_src_path }| code = 'RE' ). "#EC NOTEXT
    endif.

  when others.
    if sy-subrc is not initial.
      lcx_error=>raise( msg = 'Wrong source type' code = 'WS' ). "#EC NOTEXT
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
    lcx_error=>raise( 'Binary to string error' ). "#EC NOTEXT
  endif.

  " Extract zip
  if o_zip is initial.
    create object o_zip.
  endif.

  o_zip->load( exporting  zip    = l_xstring
               exceptions others = 4 ).

  if sy-subrc is not initial or lines( o_zip->files ) = 0.
    lcx_error=>raise( msg = 'ZIP load failed' code = 'ZE' ).  "#EC NOTEXT
  endif.
endmethod.


method load_and_store.
  data:
        lo_type  type ref to cl_abap_typedescr,
        lo_dtype type ref to cl_abap_datadescr,
        lr_data  type ref to data.

  field-symbols <data> type data.

  " Create container to load zip data to
  cl_abap_typedescr=>describe_by_name(
    exporting  p_name      = i_type
    receiving  p_descr_ref = lo_type
    exceptions others      = 4 ).

  if sy-subrc is not initial.
    lcx_error=>raise( msg = |Type { i_type } not found|  code = 'WT' ). "#EC NOTEXT
  endif.

  lo_dtype ?= lo_type.

  create data lr_data type handle lo_dtype.
  assign lr_data->* to <data>.

  " Load from zip and store
  me->load_data( exporting i_obj       = i_obj
                           i_strict    = i_strict
                 importing e_container = <data> ).

  me->_store( i_name     = i_name
              i_data_ref = lr_data
              i_tabkey   = i_tabkey ).

endmethod.


method load_data.
  data l_rawdata  type string.

  if e_container is not supplied.
    lcx_error=>raise( msg = 'No container supplied' code = 'NC' ). "#EC NOTEXT
  endif.

  me->read_zip( exporting i_name    = i_obj && '.txt'
                importing e_rawdata = l_rawdata ).

  me->parse_data( exporting i_rawdata   = l_rawdata
                            i_strict    = i_strict
                            i_where     = i_where
                  importing e_container = e_container ).

endmethod.


method load_raw.
  data l_filename type string.

  if e_content is not supplied.
    lcx_error=>raise( msg = 'No container supplied' code = 'NC' ). "#EC NOTEXT
  endif.

  if i_ext is initial.
    l_filename = i_obj && '.txt'.
  else.
    l_filename = i_obj && i_ext.
  endif.

  o_zip->get( exporting name    = l_filename
              importing content = e_content ).

endmethod.


method map_file_structure.
  data:
        l_tabcnt     type i,
        l_fieldcnt   type i,
        lt_fields    type tt_string,
        l_field_name type string,
        lt_dupcheck  type tt_string,
        l_struc_name type string.

  l_struc_name = io_struc_descr->get_relative_name( ).
  split i_line at cl_abap_char_utilities=>horizontal_tab into table lt_fields.

  " Check if the line ends with TAB
  find all occurrences of cl_abap_char_utilities=>horizontal_tab in i_line match count l_tabcnt.
  if l_tabcnt = lines( lt_fields ). " Line ends with TAB, last empty field is not added to table, see help for 'split'
    lcx_error=>raise( msg = |Empty field at the end @{ l_struc_name }| code = 'EN' ).   "#EC NOTEXT
  endif.

  " Compare number of fields, check structure similarity
  if i_strict = abap_true.
    l_fieldcnt = lines( lt_fields ).

    " MANDT field may be skipped
    read table io_struc_descr->components with key name = 'MANDT' transporting no fields.
    if sy-subrc is initial. " Found in strcuture components
      read table lt_fields with key table_line = 'MANDT' transporting no fields.
      if sy-subrc is not initial. " But not found in the file
        add 1 to l_fieldcnt.
      endif.
    endif.

    if l_fieldcnt <> lines( io_struc_descr->components ).
      lcx_error=>raise( msg = |Different columns number @{ l_struc_name }| code = 'CN' ).   "#EC NOTEXT
    endif.
  endif.

  " Check duplicate field names in incoming structure
  lt_dupcheck[] = lt_fields[].
  sort lt_dupcheck[].
  delete adjacent duplicates from lt_dupcheck[].
  if lines( lt_dupcheck ) <> lines( lt_fields ).
    lcx_error=>raise( msg = |Duplicate field names found @{ l_struc_name }| code = 'DN' ).   "#EC NOTEXT
  endif.

  " Compare columns names and make map
  loop at lt_fields into l_field_name.
    if l_field_name is initial. " Check empty fields
      lcx_error=>raise( msg = |Empty field name found @{ l_struc_name }| code = 'EN' ).   "#EC NOTEXT
    endif.

    read table io_struc_descr->components with key name = l_field_name transporting no fields.
    if sy-subrc is initial.
      append sy-tabix to et_map.
    else.
      lcx_error=>raise( msg = |{ l_field_name } not found in structure @{ l_struc_name }| code = 'MC' ). "#EC NOTEXT
    endif.
  endloop.

endmethod.                    "analyse_structure


method parse_apply_exit.
  data l_fmname type rs38l_fnam value 'CONVERSION_EXIT_XXXXX_INPUT'.

  replace first occurrence of 'XXXXX' in l_fmname with i_convexit.

  call function 'FUNCTION_EXISTS'
    exporting
      funcname           = l_fmname
    exceptions
      function_not_exist = 1
      others             = 2.

  if sy-subrc <> 0.
    lcx_error=>raise( msg = 'Conversion exit not found' code = 'EM' ). "#EC NOTEXT
  endif.

  call function l_fmname
    exporting  input  = i_data
    importing  output = e_field
    exceptions others = 1.

  if sy-subrc <> 0.
    lcx_error=>raise( msg = 'Conversion exit failed' code = 'EF' ). "#EC NOTEXT
  endif.

endmethod.


method parse_data.
  data:
        lt_lines       type table of string,
        ls_line        type string,
        lt_map         type int4_table,
        lt_filter      type tt_filter,

        lo_type_descr  type ref to cl_abap_typedescr,
        lo_table_descr type ref to cl_abap_tabledescr,
        lo_struc_descr type ref to cl_abap_structdescr,
        ref_tab_line   type ref to data.

  field-symbols:
                 <table>      type any table,
                 <container>  type any.

  clear e_container.

  " Identify container type and create temp container
  lo_type_descr = cl_abap_typedescr=>describe_by_data( e_container ).
  case lo_type_descr->kind.
  when 'T'. " Table
    lo_table_descr ?= lo_type_descr.
    lo_struc_descr ?= lo_table_descr->get_table_line_type( ).
    create data ref_tab_line type handle lo_struc_descr.
    assign ref_tab_line->* to <container>.
    assign e_container     to <table>.
  when 'S'. " Structure
    lo_struc_descr ?= lo_type_descr.
    assign e_container to <container>.
  when others. " Not a table or structure ?
    lcx_error=>raise( msg = 'Table or structure containers only' code = 'DT' ). "#EC NOTEXT
  endcase.

  " Build filter hash if supplied
  if i_where is not initial.
    me->build_filter( exporting i_where  = i_where
                      importing e_filter = lt_filter ).
  endif.

  " Read and process header line
  lt_lines = break_to_lines( i_rawdata ).
  read table lt_lines into ls_line index 1.
  if sy-subrc <> 0.
    lcx_error=>raise( msg = 'No header line found in the file' code = 'NH' ). "#EC NOTEXT
  endif.
  if ls_line is initial.
    lcx_error=>raise( msg = 'Header line is empty'  code = 'HE' ). "#EC NOTEXT
  endif.

  delete lt_lines index 1.

  me->map_file_structure( exporting i_line         = ls_line
                                    io_struc_descr = lo_struc_descr
                                    i_strict       = i_strict
                          importing et_map         = lt_map ).

  " Main data parsing loop
  loop at lt_lines into ls_line.
    if ls_line is initial. " Check empty lines
      check sy-tabix < lines( lt_lines ). " Last line of a file may be empty, others - not
      lcx_error=>raise( msg = |Empty line { sy-tabix + 1 } cannot be parsed|  code = 'LE' ). "#EC NOTEXT
    endif.

    me->parse_line( exporting i_line         = ls_line
                              io_struc_descr = lo_struc_descr
                              it_map         = lt_map
                              i_index        = sy-tabix + 1
                    importing es_container   = <container> ).

    if does_line_fit_filter( i_line = <container> i_filter = lt_filter ) = abap_true.
      if lo_type_descr->kind = 'S'. " Structure
        exit. " Only first line goes to structure and then exits
      else. " Table
        insert <container> into table <table>.
      endif.
    endif.

  endloop.

endmethod.


method PARSE_DATE.

  data: l_cursor  type i,
        l_iter    type i,
        l_part    type c,
        l_size    type i,
        l_offs    type i,
        l_home    type i,
        l_pad     type i,
        l_stencil type numc4,
        l_rawdate type char8,
        l_charset type char11 value '0123456789',
        l_sep     type c.

  clear e_field.
  l_sep           = g_date_format+3(1).
  l_charset+10(1) = l_sep.

  if i_value is initial or i_value co ` `. " Empty string -> empty date
    return.
  endif.

  if not i_value co l_charset.  " Check wrong symbols
    lcx_error=>raise( msg = 'Date contains invalid symbols' code = 'DY' ). "#EC NOTEXT
  endif.

  " Not separated date must be 8 chars, separated not more than 10
  if l_sep <> space and strlen( i_value ) > 10  or l_sep = space and strlen( i_value ) <> 8.
    lcx_error=>raise( msg = 'Incorrect date length' code = 'DL' ). "#EC NOTEXT
  endif.

  do 3 times.
    l_iter = sy-index - 1.
    l_part = g_date_format+l_iter(1).

    case l_part.
      when 'D'.
        l_size = 2.
        l_home = 6.
      when 'M'.
        l_size = 2.
        l_home = 4.
      when 'Y'.
        l_size = 4.
        l_home = 0.
      when others.
        lcx_error=>raise( msg = 'Wrong date format' ). "#EC NOTEXT
    endcase.

    if l_sep is initial. " No seps
      l_rawdate+l_home(l_size) = i_value+l_cursor(l_size).
      l_cursor                 = l_cursor + l_size.
    else.
      if l_iter = 2. " Last part
        l_offs = strlen( i_value+l_cursor ).
      else.
        find first occurrence of l_sep in i_value+l_cursor match offset l_offs.
      endif.
      if sy-subrc <> 0.
        lcx_error=>raise( msg = 'Date separator is missing' code = 'DS' ). "#EC NOTEXT
      endif.
      if l_offs > l_size.
        lcx_error=>raise( msg = 'Too long date part' code = 'DP' ). "#EC NOTEXT
      endif.
      l_stencil                = i_value+l_cursor(l_offs).
      l_pad                    = 4 - l_size. " Offset within stencil
      l_rawdate+l_home(l_size) = l_stencil+l_pad(l_size).
      l_cursor                 = l_cursor + l_offs + 1. " Including separator
    endif.

  enddo.

  " Native convert
  try.
    cl_abap_datfm=>conv_date_ext_to_int(
      exporting
        im_datext   = l_rawdate
        im_datfmdes = '4' " YYYY.MM.DD
      importing
        ex_datint   = e_field ).
    catch cx_abap_datfm.
      lcx_error=>raise( msg = 'Date format unknown' code = 'DU' ). "#EC NOTEXT
  endtry.

endmethod.


method parse_field.
  data:
        l_mask     type string,
        l_tmp      type string,
        l_unquoted type string,
        l_regex    type string,
        l_len      type i.

  clear e_field.

  " Unquote field
  l_len = strlen( i_data ).
  if l_len >= 2
     and substring( val = i_data off = 0         len = 1 ) = '"'
     and substring( val = i_data off = l_len - 1 len = 1 ) = '"'.
    l_unquoted = substring( val = i_data off = 1 len = l_len - 2 ).
  else.
    l_unquoted = i_data.
  endif.
  clear l_len.

  " Parse depending on output type
  case is_component-type_kind.
    when 'D'. " Date
      parse_date( exporting  i_value    = l_unquoted
                  importing  e_field    = e_field ).

    when 'C'. " Char + convexits
      describe field e_field edit mask l_mask.
      if l_mask is initial.
        e_field = l_unquoted.
      else.
        shift l_mask left deleting leading '='.
        me->parse_apply_exit( exporting i_data     = l_unquoted
                                        i_convexit = l_mask
                              importing e_field    = e_field ).
      endif.

    when 'g'. " String
      e_field = l_unquoted.

    when 'P'. " Amount
      try .
        e_field = l_unquoted. " Try native format first - xxxx.xx

      catch cx_sy_arithmetic_error cx_sy_conversion_error.
        l_tmp   = l_unquoted.
        l_regex = '^-?\d{1,3}(T\d{3})*(\D\d{1,C})?$'. "#EC NOTEXT
        condense l_tmp no-gaps.
        replace 'C' in l_regex with |{ is_component-decimals }|.

        " Validate number
        find first occurrence of g_amt_format+0(1) in l_tmp.
        if sy-subrc is initial. " Found
          replace 'T' in l_regex with g_amt_format+0(1).
        else.
          replace 'T' in l_regex with ''.
        endif.

        replace 'D' in l_regex with g_amt_format+1(1).
        find all occurrences of regex l_regex in l_tmp match count sy-tabix.

        if sy-tabix = 1.
          if not g_amt_format+0(1) is initial.  " Remove thousand separators
            replace all occurrences of g_amt_format+0(1) in l_tmp with ''.
          endif.

          if g_amt_format+1(1) <> '.'.          " Replace decimal separator
            replace g_amt_format+1(1) in l_tmp with '.'.
          endif.

          try. " Try converting again
            clear sy-subrc.
            e_field = l_tmp.
          catch cx_sy_arithmetic_error cx_sy_conversion_error.
            sy-subrc = 4.
          endtry.
        else. " Not matched
          sy-subrc = 4.
        endif.

      endtry.

    when 'N' or 'I'. " Integer number
      if l_unquoted co '0123456789'.
        e_field = l_unquoted.
      else.
        sy-subrc = 4.
      endif.

    when 'X'.        " Raw
      try .
        e_field = l_unquoted.
      catch cx_sy_conversion_no_raw cx_sy_conversion_error.
        sy-subrc = 4.
      endtry.

  endcase.

  if sy-subrc is not initial.
    lcx_error=>raise( msg = |Field: { is_component-name }| code = 'PF' ). "#EC NOTEXT
  endif.

endmethod.


method parse_line.
  data:
        l_tabcnt       type i,
        lt_fields      type table of string,
        ls_field       type string,
        ls_component   type abap_compdescr,
        l_index        type int4.

  field-symbols <field> type any.

  clear es_container.
  split i_line at cl_abap_char_utilities=>horizontal_tab into table lt_fields.

  " Count TABs, if line ends with TAB last empty field is not added to table, see help for 'split'
  find all occurrences of cl_abap_char_utilities=>horizontal_tab in i_line match count l_tabcnt.
  add 1 to l_tabcnt. " Number of fields in the line

  " Check field number is the same as in header
  if l_tabcnt > lines( it_map ).
    lcx_error=>raise( msg = |More fields than in header @{ i_index }| code = '>H' ). "#EC NOTEXT
  elseif l_tabcnt < lines( it_map ).
    lcx_error=>raise( msg = |Less fields than in header @{ i_index }| code = '<H' ). "#EC NOTEXT
  endif.

  " Move data to table line
  loop at lt_fields into ls_field.
    read table it_map into l_index index sy-tabix. " Read map

    read table io_struc_descr->components into ls_component index l_index. " Get component
    if sy-subrc is not initial.
      lcx_error=>raise( 'No component found?!' ). "#EC NOTEXT
    endif.

    check ls_component-name ne 'MANDT'. " Skip client fields

    unassign <field>.
    assign component ls_component-name of structure es_container to <field>.
    if <field> is not assigned.
      lcx_error=>raise( 'Field assign failed?!' ). "#EC NOTEXT
    endif.

    me->parse_field( exporting is_component = ls_component
                               i_data       = ls_field
                     importing e_field      = <field> ).

  endloop.

endmethod.


method purge.
  data l_store type ty_store.

  if i_name = '*'. " Delete all
    loop at at_store into l_store.
      free l_store-data.
    endloop.
    clear at_store.

  else.            " Delete specific record
    read table at_store with key name = i_name into l_store.
    if sy-subrc is initial.
      delete at_store index sy-tabix.
      free l_store-data.
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
    lcx_error=>raise( msg = |Cannot read { i_name }| code = 'ZF' ). "#EC NOTEXT
  endif.

  " Remove unicide signatures
  case g_encoding.
    when '4110'. " UTF-8
      shift l_xstring left deleting leading  cl_abap_char_utilities=>byte_order_mark_utf8 in byte mode.
    when '4103'. " UTF-16LE
      shift l_xstring left deleting leading  cl_abap_char_utilities=>byte_order_mark_little in byte mode.
  endcase.

  try.
    lo_conv = cl_abap_conv_in_ce=>create( encoding = g_encoding ).
    lo_conv->convert( exporting input = l_xstring importing data = e_rawdata ).
  catch cx_root into l_ex.
    lcx_error=>raise( msg = 'Codepage conversion error' code = 'CP' ). "#EC NOTEXT
  endtry.

endmethod.


method retrieve.
  data lx_error type ref to lcx_error.
  data lx_unexp type ref to cx_static_check.

  try .
    get_instance( )->_retrieve( exporting i_name  = i_name
                                          i_sift  = i_sift
                                          i_where = i_where
                                importing e_data  = e_data ).

  catch lcx_error into lx_error.

    " Switch to non-class exceptions to ensure better code readability
    " and compatibility with substituted select results
    " e.g. zcl_mockup_loader=>retrieve( ... ). if sy_subrc is not initial ...
    cl_message_helper=>set_msg_vars_for_if_t100_msg( text = lx_error ).
    message id sy-msgid type sy-msgty number sy-msgno
      raising retrieve_error
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  catch cx_static_check into lx_unexp.

    sy-msgid = 'SY'.
    sy-msgty = 'E'.
    sy-msgno = '499'. " & & & &
    sy-msgv1 = 'ZCL_MOCKUP_LOADER'.
    sy-msgv2 = 'RETRIEVE()'.
    sy-msgv3 = lx_unexp->get_text( ).

    message id sy-msgid type sy-msgty number sy-msgno
      raising retrieve_error
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  endtry.

endmethod.


method store.
  data          lr_data type ref to data.
  field-symbols <data>  type any.

  " Create clone container
  create data lr_data like i_data.
  assign lr_data->* to <data>.

  <data> = i_data. " Copy data to container

  me->_store( i_name     = i_name
              i_data_ref = lr_data
              i_tabkey   = i_tabkey ).

endmethod.


method _retrieve.
  data:
        l_store     type ty_store,
        lt_filter   type tt_filter,
        r_data_tab  type ref to data,
        ld_src      type ref to cl_abap_typedescr,
        ld_dst      type ref to cl_abap_typedescr,
        ld_tab      type ref to cl_abap_tabledescr,
        ld_src_line type ref to cl_abap_structdescr,
        ld_dst_line type ref to cl_abap_structdescr.

  field-symbols:
        <line>    type any,
        <tabkey>  type any,
        <src_tab> type any table,
        <tmp_tab> type standard table,
        <data>    type any.

  clear e_data.

  " Validate parameters
  if i_sift is not initial and i_where is not initial.
    lcx_error=>raise( msg = |Pass just one filter param| code = 'WP' ). "#EC NOTEXT
  endif.

  " Find store
  read table at_store with key name = i_name into l_store.
  if sy-subrc is not initial.
    lcx_error=>raise( msg = |Cannot find store { i_name }| code = 'NF' ). "#EC NOTEXT
  endif.

  assign l_store-data->* to <data>.

  " Build filter
  if i_sift is not initial.
    if l_store-tabkey is initial.
      lcx_error=>raise( msg = 'Tabkey field not found' code = 'FM' ). "#EC NOTEXT
    endif.
    build_filter( exporting i_where  = l_store-tabkey && '=' && i_sift
                  importing e_filter = lt_filter ).
  elseif i_where is not initial.
    build_filter( exporting i_where  = i_where
                  importing e_filter = lt_filter ).
  endif.

  " Ensure types are the same
  ld_src = cl_abap_typedescr=>describe_by_data( <data> ).
  ld_dst = cl_abap_typedescr=>describe_by_data( e_data ).

  if ld_src->kind = 'T'.
    ld_tab      ?= ld_src.
    ld_src_line ?= ld_tab->get_table_line_type( ).
  endif.

  " Ensure filter is applied to a table
  if lt_filter is not initial and ld_src->kind <> 'T'.
    lcx_error=>raise( msg = 'Filtering is relevant for tables only' code = 'TO' ). "#EC NOTEXT
  endif.

  " If types are not equal try going deeper to table line structure
  if ld_src->absolute_name ne ld_dst->absolute_name.
    if ld_src->kind = 'T' and ld_dst->kind = 'T'. " Table => Table
      ld_tab      ?= ld_dst.
      ld_dst_line ?= ld_tab->get_table_line_type( ).
    elseif lt_filter is not initial and ld_src->kind = 'T' and ld_dst->kind = 'S'. " Table + filter => Structure
      ld_dst_line ?= ld_dst.
    else.
      lcx_error=>raise( msg = |Types differ for store { i_name }| code = 'TT' ). "#EC NOTEXT
    endif.

    if ld_src_line->absolute_name ne ld_dst_line->absolute_name.
      lcx_error=>raise( msg = |Types differ for store { i_name }| code = 'TS' ). "#EC NOTEXT
    endif.
  endif.

  " Copy or sift (filter with tabkey) values
  if lt_filter is initial.
    e_data = <data>.

  else. " Assuming ld_src->kind = 'T' -> see STORE
    assert ld_dst->kind ca 'ST'.
    assign l_store-data->* to <src_tab>.

    if ld_dst->kind = 'T'.
      ld_tab = cl_abap_tabledescr=>create( p_line_type  = ld_src_line
                                           p_table_kind = cl_abap_tabledescr=>tablekind_std
                                           p_unique     = abap_false ).

      create data r_data_tab type handle ld_tab.
      assign r_data_tab->* to <tmp_tab>.
    endif.

    loop at <src_tab> assigning <line>.
      if does_line_fit_filter( i_line = <line> i_filter = lt_filter ) = abap_true.
        if ld_dst->kind = 'S'. " Structure
          e_data = <line>.
          exit. " Only first line goes to structure and then exits
        else. " Table
          append <line> to <tmp_tab>.
        endif.
      endif.
    endloop.

    if ld_dst->kind = 'T'.
      e_data = <tmp_tab>.
      free r_data_tab.
    endif.

  endif.

  if e_data is initial.
    lcx_error=>raise( msg = 'No data returned' code = '04' ). "#EC NOTEXT
  endif.

endmethod.


method _store.
  data:
        l_store       type ty_store,
        lo_type       type ref to cl_abap_typedescr,
        lo_tab_type   type ref to cl_abap_tabledescr,
        lo_str_type   type ref to cl_abap_structdescr.

  " Check if tabkey exists
  if i_tabkey is not initial.
    lo_type = cl_abap_typedescr=>describe_by_data_ref( i_data_ref ).
    if lo_type->kind <> 'T'. " Not table ?
      lcx_error=>raise( msg = 'Tabkey is relevant for tables only' code = 'TO' ). "#EC NOTEXT
    endif.

    lo_tab_type ?= lo_type.
    lo_str_type ?= lo_tab_type->get_table_line_type( ).

    read table lo_str_type->components with key name = i_tabkey transporting no fields.
    if sy-subrc <> 0.
      lcx_error=>raise( msg = 'Tabkey field not found' code = 'FM' ). "#EC NOTEXT
    endif.
  endif.

  " Store data
  l_store-name   = i_name.
  l_store-tabkey = i_tabkey.
  l_store-data   = i_data_ref.

  me->purge( i_name ).
  append l_store to me->at_store.

endmethod.
ENDCLASS.
