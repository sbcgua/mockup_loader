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
    tt_string type table of string .
  types:
    begin of ty_store,
      name    type char40,
      tabkey  type abap_compname,
      data    type ref to data,
    end of ty_store .
  types:
    tt_store type table of ty_store with key name .
  types:
    begin of ty_filter,
      name  type string,
      range type ref to data,
    end of ty_filter .
  types:
    tt_filter type standard table of ty_filter with key name .

  class-methods CLASS_CONSTRUCTOR .
  class-methods CLASS_SET_SOURCE
    importing
      !I_PATH type STRING
      !I_TYPE type CHAR4 .
  class-methods CLASS_SET_PARAMS
    importing
      !I_AMT_FORMAT type CHAR2 .
  class-methods GET_INSTANCE
    returning
      value(RO_INSTANCE) type ref to ZCL_MOCKUP_LOADER
    raising
      ZCX_MOCKUP_LOADER_ERROR .
  class-methods FREE_INSTANCE .
  methods LOAD_RAW
    importing
      !I_OBJ type STRING
      !I_EXT type STRING optional
    exporting
      !E_CONTENT type XSTRING
    raising
      ZCX_MOCKUP_LOADER_ERROR .
  type-pools ABAP .
  methods LOAD_AND_STORE
    importing
      !I_OBJ type STRING
      !I_STRICT type ABAP_BOOL default ABAP_TRUE
      !I_NAME type CHAR40
      !I_TYPE type CSEQUENCE
      !I_TABKEY type ABAP_COMPNAME optional
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
  methods PURGE
    importing
      !I_NAME type CHAR40 .
  class-methods RETRIEVE
    importing
      !I_NAME type CHAR40
      !I_SIFT type CLIKE optional
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
      ZCX_MOCKUP_LOADER_ERROR .
protected section.
private section.

  class-data G_AMT_FORMAT type CHAR2 .
  class-data GO_INSTANCE type ref to ZCL_MOCKUP_LOADER .
  data O_ZIP type ref to CL_ABAP_ZIP .
  data AT_STORE type TT_STORE .
  class-data G_MOCKUP_SRC_PATH type STRING .
  class-data G_MOCKUP_SRC_TYPE type CHAR4 .

  methods INITIALIZE
    raising
      ZCX_MOCKUP_LOADER_ERROR .
  methods MAP_FILE_STRUCTURE
    importing
      !I_LINE type STRING
      !IO_STRUC_DESCR type ref to CL_ABAP_STRUCTDESCR
      !I_STRICT type ABAP_BOOL
    exporting
      !ET_MAP type INT4_TABLE
    raising
      ZCX_MOCKUP_LOADER_ERROR .
  methods PARSE_APPLY_EXIT
    importing
      !I_DATA type STRING
      !I_CONVEXIT type STRING
    exporting
      !E_FIELD type ANY
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
  methods PARSE_LINE
    importing
      !I_LINE type STRING
      !IT_MAP type INT4_TABLE
      !IO_STRUC_DESCR type ref to CL_ABAP_STRUCTDESCR
      !I_INDEX type INT4
    exporting
      !ES_CONTAINER type ANY
    raising
      ZCX_MOCKUP_LOADER_ERROR .
  methods PARSE_FIELD
    importing
      !IS_COMPONENT type ABAP_COMPDESCR
      !I_DATA type STRING
    exporting
      !E_FIELD type ANY
    raising
      ZCX_MOCKUP_LOADER_ERROR .
  methods BUILD_FILTER
    importing
      !I_WHERE type ANY
    exporting
      !E_FILTER type TT_FILTER
    raising
      ZCX_MOCKUP_LOADER_ERROR .
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
      ZCX_MOCKUP_LOADER_ERROR .
  methods _STORE
    importing
      !I_NAME type CHAR40
      !I_DATA_REF type ref to DATA
      !I_TABKEY type ABAP_COMPNAME optional
    raising
      ZCX_MOCKUP_LOADER_ERROR .
  methods _RETRIEVE
    importing
      !I_NAME type CHAR40
      !I_SIFT type CLIKE optional
    exporting
      !E_DATA type ANY
    raising
      ZCX_MOCKUP_LOADER_ERROR .
ENDCLASS.



CLASS ZCL_MOCKUP_LOADER IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_MOCKUP_LOADER->BUILD_FILTER
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_WHERE                        TYPE        ANY
* | [<---] E_FILTER                       TYPE        TT_FILTER
* | [!CX!] ZCX_MOCKUP_LOADER_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
method BUILD_FILTER.
  data ld_where type ref to cl_abap_structdescr.
  data lt_comps type cl_abap_structdescr=>component_table.
  data l_comp   like line of lt_comps.

  data r_templ  type range of c.
  data ld_rtemp type ref to cl_abap_tabledescr.
  data ld_rcomp type ref to cl_abap_tabledescr.

  data l_filter  type ty_filter.
  field-symbols <range> type any table.

  " Check if filter is set of ranges
  try.
    ld_rtemp ?= cl_abap_typedescr=>describe_by_data( r_templ ).
    ld_where ?= cl_abap_typedescr=>describe_by_data( i_where ). " Expect structure, cast_error otherwise
    lt_comps  = ld_where->get_components( ).

    loop at lt_comps into l_comp.
      if l_comp-type->kind <> cl_abap_typedescr=>kind_table.
        zcx_mockup_loader_error=>raise( msg = |I_WHERE must be a structure of ranges only| code = 'RO' ).   "#EC NOTEXT
      endif.

      ld_rcomp ?= l_comp-type.
      if ld_rcomp->key ne ld_rtemp->key. " Not range-like structure ?
        zcx_mockup_loader_error=>raise( msg = |I_WHERE must be a structure of ranges only| code = 'RO' ).   "#EC NOTEXT
      endif.

      l_filter-name = l_comp-name.
      assign component l_comp-name of structure i_where to <range>.
      get reference of <range> into l_filter-range.
      append l_filter to e_filter.
    endloop.

  catch cx_sy_move_cast_error.
    zcx_mockup_loader_error=>raise( msg = |CX_SY_MOVE_CAST_ERROR @BUILD_FILTER()| code = 'CE' ).   "#EC NOTEXT
  endtry.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_MOCKUP_LOADER=>CLASS_CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method class_constructor.
  class_set_source( i_type = 'MIME' i_path = '' ). " Defaults
  class_set_params( i_amt_format = '' ). " Defaults
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_MOCKUP_LOADER=>CLASS_SET_PARAMS
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_AMT_FORMAT                   TYPE        CHAR2
* +--------------------------------------------------------------------------------------</SIGNATURE>
method CLASS_SET_PARAMS.
  if i_amt_format is initial or g_amt_format+1(1) is initial. " Empty param or decimal separator
    g_amt_format = ' ,'. " Defaults
  else.
    g_amt_format = i_amt_format.
  endif.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_MOCKUP_LOADER=>CLASS_SET_SOURCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_PATH                         TYPE        STRING
* | [--->] I_TYPE                         TYPE        CHAR4
* +--------------------------------------------------------------------------------------</SIGNATURE>
method class_set_source.
  check i_type = 'MIME' or i_type = 'FILE'. "TODO some more sophisticated error handling

  g_mockup_src_type = i_type.
  g_mockup_src_path = i_path.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_MOCKUP_LOADER->DOES_LINE_FIT_FILTER
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_LINE                         TYPE        ANY
* | [--->] I_FILTER                       TYPE        TT_FILTER
* | [<-()] R_YESNO                        TYPE        ABAP_BOOL
* +--------------------------------------------------------------------------------------</SIGNATURE>
method DOES_LINE_FIT_FILTER.
  field-symbols <field> type any.
  field-symbols <range> type any table.
  data l_filter type ty_filter.

  r_yesno = abap_true.

  loop at i_filter into l_filter.
    assign component l_filter-name of structure i_line to <field>.
    check <field> is assigned. " Just skip irrelevant ranges
    assign l_filter-range->* to <range>.

    if not <field> in <range>.
      r_yesno = abap_false.
      exit.
    endif.
    unassign <field>.
  endloop.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_MOCKUP_LOADER=>FREE_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
method free_instance.
  if go_instance is not initial.
    free go_instance.
  endif.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_MOCKUP_LOADER=>GET_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RO_INSTANCE                    TYPE REF TO ZCL_MOCKUP_LOADER
* | [!CX!] ZCX_MOCKUP_LOADER_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
method get_instance.

  if go_instance is initial.
    create object go_instance.
    call method go_instance->initialize.
  endif.

  ro_instance = go_instance.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_MOCKUP_LOADER->INITIALIZE
* +-------------------------------------------------------------------------------------------------+
* | [!CX!] ZCX_MOCKUP_LOADER_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
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
        l_path_tmp  type char40.

  " Get re-direction settings from session memory
  get parameter id 'ZMOCKUP_LOADER_STYPE' field l_type_tmp.
  get parameter id 'ZMOCKUP_LOADER_SPATH' field l_path_tmp.

  if l_type_tmp is not initial and l_path_tmp is not initial.
    l_src_type = l_type_tmp.
    l_src_path = l_path_tmp.
  else.
    l_src_type = g_mockup_src_type.
    l_src_path = g_mockup_src_path.
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
      zcx_mockup_loader_error=>raise( msg = 'SMW0 data import error' code = 'RE' ).  "#EC NOTEXT
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
      zcx_mockup_loader_error=>raise( msg = |Cannot upload file: { l_src_path }| code = 'RE' ). "#EC NOTEXT
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

  call method o_zip->load
    exporting  zip    = l_xstring
    exceptions others = 4.

  if sy-subrc is not initial or lines( o_zip->files ) = 0.
    zcx_mockup_loader_error=>raise( msg = 'ZIP load failed' code = 'ZE' ).  "#EC NOTEXT
  endif.
endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MOCKUP_LOADER->LOAD_AND_STORE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_OBJ                          TYPE        STRING
* | [--->] I_STRICT                       TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [--->] I_NAME                         TYPE        CHAR40
* | [--->] I_TYPE                         TYPE        CSEQUENCE
* | [--->] I_TABKEY                       TYPE        ABAP_COMPNAME(optional)
* | [!CX!] ZCX_MOCKUP_LOADER_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
method load_and_store.
  data:
        lo_type  type ref to cl_abap_typedescr,
        lo_dtype type ref to cl_abap_datadescr,
        lr_data  type ref to data.

  field-symbols <data> type data.

  " Create container to load zip data to
  call method cl_abap_typedescr=>describe_by_name
    exporting  p_name      = i_type
    receiving  p_descr_ref = lo_type
    exceptions others      = 4.

  if sy-subrc is not initial.
    zcx_mockup_loader_error=>raise( msg = |Type { i_type } not found|  code = 'WT' ). "#EC NOTEXT
  endif.

  lo_dtype ?= lo_type.

  create data lr_data type handle lo_dtype.
  assign lr_data->* to <data>.
  if <data> is not assigned.
    zcx_mockup_loader_error=>raise( 'Data cannot be assigned' ). "#EC NOTEXT
  endif.

  " Load from zip and store
  call method load_data
    exporting
      i_obj       = i_obj
      i_strict    = i_strict
    importing
      e_container = <data>.

  call method _store
    exporting
      i_name     = i_name
      i_data_ref = lr_data
      i_tabkey   = i_tabkey.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MOCKUP_LOADER->LOAD_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_OBJ                          TYPE        STRING
* | [--->] I_STRICT                       TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [--->] I_WHERE                        TYPE        ANY(optional)
* | [<---] E_CONTAINER                    TYPE        ANY
* | [!CX!] ZCX_MOCKUP_LOADER_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
method load_data.
  data l_rawdata  type string.

  if e_container is not supplied.
    zcx_mockup_loader_error=>raise( msg = 'No container supplied' code = 'NC' ). "#EC NOTEXT
  endif.

  call method me->read_zip
    exporting i_name    = i_obj && '.txt'
    importing e_rawdata = l_rawdata.

  call method me->parse_data
    exporting
      i_rawdata   = l_rawdata
      i_strict    = i_strict
      i_where     = i_where
    importing
      e_container = e_container.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MOCKUP_LOADER->LOAD_RAW
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_OBJ                          TYPE        STRING
* | [--->] I_EXT                          TYPE        STRING(optional)
* | [<---] E_CONTENT                      TYPE        XSTRING
* | [!CX!] ZCX_MOCKUP_LOADER_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
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

  call method o_zip->get
    exporting name    = l_filename
    importing content = e_content.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_MOCKUP_LOADER->MAP_FILE_STRUCTURE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_LINE                         TYPE        STRING
* | [--->] IO_STRUC_DESCR                 TYPE REF TO CL_ABAP_STRUCTDESCR
* | [--->] I_STRICT                       TYPE        ABAP_BOOL
* | [<---] ET_MAP                         TYPE        INT4_TABLE
* | [!CX!] ZCX_MOCKUP_LOADER_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
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
    zcx_mockup_loader_error=>raise( msg = |Empty field at the end @{ l_struc_name }| code = 'EN' ).   "#EC NOTEXT
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
      zcx_mockup_loader_error=>raise( msg = |Different columns number @{ l_struc_name }| code = 'CN' ).   "#EC NOTEXT
    endif.
  endif.

  " Check duplicate field names in incoming structure
  lt_dupcheck[] = lt_fields[].
  sort lt_dupcheck[].
  delete adjacent duplicates from lt_dupcheck[].
  if lines( lt_dupcheck ) <> lines( lt_fields ).
    zcx_mockup_loader_error=>raise( msg = |Duplicate field names found @{ l_struc_name }| code = 'DN' ).   "#EC NOTEXT
  endif.

  " Compare columns names and make map
  loop at lt_fields into l_field_name.
    if l_field_name is initial. " Check empty fields
      zcx_mockup_loader_error=>raise( msg = |Empty field name found @{ l_struc_name }| code = 'EN' ).   "#EC NOTEXT
    endif.

    read table io_struc_descr->components with key name = l_field_name transporting no fields.
    if sy-subrc is initial.
      append sy-tabix to et_map.
    else.
      zcx_mockup_loader_error=>raise( msg = |{ l_field_name } not found in structure @{ l_struc_name }| code = 'MC' ). "#EC NOTEXT
    endif.
  endloop.

endmethod.                    "analyse_structure


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_MOCKUP_LOADER->PARSE_APPLY_EXIT
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_DATA                         TYPE        STRING
* | [--->] I_CONVEXIT                     TYPE        STRING
* | [<---] E_FIELD                        TYPE        ANY
* | [!CX!] ZCX_MOCKUP_LOADER_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
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
    zcx_mockup_loader_error=>raise( msg = 'Conversion exit not found' code = 'EM' ). "#EC NOTEXT
  endif.

  call function l_fmname
    exporting  input  = i_data
    importing  output = e_field
    exceptions others = 1.

  if sy-subrc <> 0.
    zcx_mockup_loader_error=>raise( msg = 'Conversion exit failed' code = 'EF' ). "#EC NOTEXT
  endif.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_MOCKUP_LOADER->PARSE_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_RAWDATA                      TYPE        STRING
* | [--->] I_STRICT                       TYPE        ABAP_BOOL (default =ABAP_TRUE)
* | [--->] I_WHERE                        TYPE        ANY(optional)
* | [<---] E_CONTAINER                    TYPE        ANY
* | [!CX!] ZCX_MOCKUP_LOADER_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
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
    zcx_mockup_loader_error=>raise( msg = 'Table or structure containers only' code = 'DT' ). "#EC NOTEXT
  endcase.

  " Build filter hash if supplied
  if i_where is not initial.
    call method build_filter
      exporting i_where  = i_where
      importing e_filter = lt_filter.
  endif.

  " Read and process header line
  split i_rawdata at cl_abap_char_utilities=>cr_lf into table lt_lines.
  read table lt_lines into ls_line index 1.
  if sy-subrc <> 0.
    zcx_mockup_loader_error=>raise( msg = 'No header line found in the file' code = 'NH' ). "#EC NOTEXT
  endif.
  if ls_line is initial.
    zcx_mockup_loader_error=>raise( msg = 'Header line is empty'  code = 'HE' ). "#EC NOTEXT
  endif.

  delete lt_lines index 1.

  call method me->map_file_structure
    exporting
      i_line         = ls_line
      io_struc_descr = lo_struc_descr
      i_strict       = i_strict
    importing
      et_map         = lt_map.

  " Main data parsing loop
  loop at lt_lines into ls_line.
    if ls_line is initial. " Check empty lines
      check sy-tabix < lines( lt_lines ). " Last line of a file may be empty, others - not
      zcx_mockup_loader_error=>raise( msg = |Empty line { sy-tabix + 1 } cannot be parsed|  code = 'LE' ). "#EC NOTEXT
    endif.

    call method parse_line
      exporting
        i_line         = ls_line
        io_struc_descr = lo_struc_descr
        it_map         = lt_map
        i_index        = sy-tabix + 1
      importing
        es_container   = <container>.

    if does_line_fit_filter( i_line = <container> i_filter = lt_filter ) = abap_true.
      if lo_type_descr->kind = 'S'. " Structure
        exit. " Only first line goes to structure and then exits
      else. " Table
        insert <container> into table <table>.
      endif.
    endif.

  endloop.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_MOCKUP_LOADER->PARSE_FIELD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_COMPONENT                   TYPE        ABAP_COMPDESCR
* | [--->] I_DATA                         TYPE        STRING
* | [<---] E_FIELD                        TYPE        ANY
* | [!CX!] ZCX_MOCKUP_LOADER_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
method parse_field.
  data:
        l_mask  type string,
        l_tmp   type string,
        l_regex type string.

  case is_component-type_kind.
    when 'D'. " Date
      call function 'CONVERT_DATE_TO_INTERNAL'
        exporting
          date_external            = i_data
          accept_initial_date      = 'X'
        importing  date_internal   = e_field
        exceptions date_external_is_invalid = 4.

    when 'C'. " Char + convexits
      describe field e_field edit mask l_mask.
      if l_mask is initial.
        e_field = i_data.
      else.
        shift l_mask left deleting leading '='.
        call method me->parse_apply_exit
          exporting
            i_data     = i_data
            i_convexit = l_mask
          importing
            e_field    = e_field.
      endif.

    when 'g'. " String
      e_field = i_data.

    when 'P'. " Amount
      try .
        e_field = i_data. " Try native format first - xxxx.xx

      catch cx_sy_arithmetic_error cx_sy_conversion_error.
        l_tmp   = i_data.
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
      if i_data co '0123456789'.
        e_field = i_data.
      else.
        sy-subrc = 4.
      endif.

    when 'X'.        " Raw
      try .
        e_field = i_data.
      catch cx_sy_conversion_no_raw cx_sy_conversion_error.
        sy-subrc = 4.
      endtry.

  endcase.

  if sy-subrc is not initial.
    zcx_mockup_loader_error=>raise( msg = |Field: { is_component-name }| code = 'PF' ). "#EC NOTEXT
  endif.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_MOCKUP_LOADER->PARSE_LINE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_LINE                         TYPE        STRING
* | [--->] IT_MAP                         TYPE        INT4_TABLE
* | [--->] IO_STRUC_DESCR                 TYPE REF TO CL_ABAP_STRUCTDESCR
* | [--->] I_INDEX                        TYPE        INT4
* | [<---] ES_CONTAINER                   TYPE        ANY
* | [!CX!] ZCX_MOCKUP_LOADER_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
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
    zcx_mockup_loader_error=>raise( msg = |More fields than in header @{ i_index }| code = '>H' ). "#EC NOTEXT
  elseif l_tabcnt < lines( it_map ).
    zcx_mockup_loader_error=>raise( msg = |Less fields than in header @{ i_index }| code = '<H' ). "#EC NOTEXT
  endif.

  " Move data to table line
  loop at lt_fields into ls_field.
    read table it_map into l_index index sy-tabix. " Read map

    read table io_struc_descr->components into ls_component index l_index. " Get component
    if sy-subrc is not initial.
      zcx_mockup_loader_error=>raise( 'No component found?!' ). "#EC NOTEXT
    endif.

    check ls_component-name ne 'MANDT'. " Skip client fields

    unassign <field>.
    assign component ls_component-name of structure es_container to <field>.
    if <field> is not assigned.
      zcx_mockup_loader_error=>raise( 'Field assign failed?!' ). "#EC NOTEXT
    endif.

    call method parse_field
      exporting
        is_component = ls_component
        i_data       = ls_field
      importing
        e_field      = <field>.

  endloop.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MOCKUP_LOADER->PURGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_NAME                         TYPE        CHAR40
* +--------------------------------------------------------------------------------------</SIGNATURE>
method purge.
  data l_store type ty_store.

  if i_name = '*'. " Delete all
    loop at at_store into l_store.
      free l_store-data.
    endloop.
    clear at_store.

  else.            " Delete specific record
    read table at_store with key name = i_name into l_store.
    check sy-subrc is initial.
    delete at_store index sy-tabix.
    free l_store-data.
  endif.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_MOCKUP_LOADER->READ_ZIP
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_NAME                         TYPE        STRING
* | [<---] E_RAWDATA                      TYPE        STRING
* | [!CX!] ZCX_MOCKUP_LOADER_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
method read_zip.
  data:
        l_xstring type xstring,
        lo_conv   type ref to cl_abap_conv_in_ce,
        l_ex      type ref to cx_root.

  call method o_zip->get
    exporting  name            = i_name
    importing  content         = l_xstring
    exceptions zip_index_error = 1.

  if sy-subrc is not initial.
    zcx_mockup_loader_error=>raise( msg = |Cannot read { i_name }| code = 'ZF' ). "#EC NOTEXT
  endif.

  shift l_xstring left deleting leading  cl_abap_char_utilities=>byte_order_mark_little in byte mode.

  try.
    lo_conv = cl_abap_conv_in_ce=>create( encoding = '4103' ). " UTF16
    lo_conv->convert( exporting input = l_xstring importing data = e_rawdata ).
  catch cx_root into l_ex.
    zcx_mockup_loader_error=>raise( msg = 'Codepage conversion error' code = 'CP' ). "#EC NOTEXT
  endtry.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_MOCKUP_LOADER=>RETRIEVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_NAME                         TYPE        CHAR40
* | [--->] I_SIFT                         TYPE        CLIKE(optional)
* | [<---] E_DATA                         TYPE        ANY
* | [EXC!] RETRIEVE_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
method retrieve.
  data lo_ex type ref to zcx_mockup_loader_error.

  try .
    get_instance( )->_retrieve(
      exporting i_name = i_name
                i_sift = i_sift
      importing e_data = e_data ).

  catch zcx_mockup_loader_error into lo_ex.

    " Switch to non-class exceptions to ensure better code readability
    " and compatibility with substituted select results
    " e.g. zcl_mockup_loader=>retrieve( ... ). if sy_subrc is not initial ...
    call method cl_message_helper=>set_msg_vars_for_if_t100_msg exporting text = lo_ex.
    message id sy-msgid type sy-msgty number sy-msgno raising retrieve_error
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  endtry.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_MOCKUP_LOADER->STORE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_NAME                         TYPE        CHAR40
* | [--->] I_DATA                         TYPE        ANY
* | [--->] I_TABKEY                       TYPE        ABAP_COMPNAME(optional)
* | [!CX!] ZCX_MOCKUP_LOADER_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
method store.
  data          lr_data type ref to data.
  field-symbols <data>  type any.

  " Create clone container
  create data lr_data like i_data.
  assign lr_data->* to <data>.
  if <data> is not assigned.
    zcx_mockup_loader_error=>raise( 'Data cannot be assigned' ). "#EC NOTEXT
  endif.

  <data> = i_data. " Copy data to container

  call method _store
    exporting
      i_name     = i_name
      i_data_ref = lr_data
      i_tabkey   = i_tabkey.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_MOCKUP_LOADER->_RETRIEVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_NAME                         TYPE        CHAR40
* | [--->] I_SIFT                         TYPE        CLIKE(optional)
* | [<---] E_DATA                         TYPE        ANY
* | [!CX!] ZCX_MOCKUP_LOADER_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
method _retrieve.
  data:
        l_store     type ty_store,
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

  " Find store
  read table at_store with key name = i_name into l_store.
  if sy-subrc is not initial.
    zcx_mockup_loader_error=>raise( msg = |Cannot find store { i_name }| code = 'NF' ). "#EC NOTEXT
  endif.

  assign l_store-data->* to <data>.
  if <data> is not assigned.
    zcx_mockup_loader_error=>raise( 'Data cannot be assigned' ). "#EC NOTEXT
  endif.

  " Ensure types are the same
  ld_src = cl_abap_typedescr=>describe_by_data( <data> ).
  ld_dst = cl_abap_typedescr=>describe_by_data( e_data ).

  if ld_src->kind = 'T'.
    ld_tab      ?= ld_src.
    ld_src_line ?= ld_tab->get_table_line_type( ).
  endif.

  " If types are not equal try going deeper to table line structure
  if ld_src->absolute_name ne ld_dst->absolute_name.
    if ld_src->kind = 'T' and ld_dst->kind = 'T'. " Table => Table
      ld_tab      ?= ld_dst.
      ld_dst_line ?= ld_tab->get_table_line_type( ).
    elseif i_sift is not initial and ld_src->kind = 'T' and ld_dst->kind = 'S'. " Table + filter => Structure
      ld_dst_line ?= ld_dst.
    else.
      zcx_mockup_loader_error=>raise( msg = |Types differ for store { i_name }| code = 'TT' ). "#EC NOTEXT
    endif.

    if ld_src_line->absolute_name ne ld_dst_line->absolute_name.
      zcx_mockup_loader_error=>raise( msg = |Types differ for store { i_name }| code = 'TS' ). "#EC NOTEXT
    endif.
  endif.

  " Copy or sift (filter with tabkey) values
  if i_sift is initial.
    e_data = <data>.

  else. " Assuming ld_src->kind = 'T' -> see STORE
    assert ld_src->kind = 'T'.
    assert ld_dst->kind ca 'ST'.
    assign l_store-data->* to <src_tab>.

    case ld_dst->kind.
    when 'T'. " Table
      " Create temporary table (needed because DST table can be hashed or sorted)
      ld_tab = cl_abap_tabledescr=>create(
                  p_line_type  = ld_src_line
                  p_table_kind = cl_abap_tabledescr=>tablekind_std
                  p_unique     = abap_false ).

      create data r_data_tab type handle ld_tab.
      assign r_data_tab->* to <tmp_tab>.

      loop at <src_tab> assigning <line>.
        assign component l_store-tabkey of structure <line> to <tabkey>.
        if <tabkey> is not assigned.
          zcx_mockup_loader_error=>raise( msg = 'Tabkey field not found' code = 'FM' ). "#EC NOTEXT
        endif.
        check <tabkey> = i_sift.
        append <line> to <tmp_tab>.
      endloop.

      e_data = <tmp_tab>.
      free r_data_tab.

    when 'S'. " Structure
      read table <src_tab> into e_data with key (l_store-tabkey) = i_sift. "#EC CI_ANYSEQ
    endcase.
  endif.

  if e_data is initial.
    zcx_mockup_loader_error=>raise( msg = 'No data returned' code = '04' ). "#EC NOTEXT
  endif.

endmethod.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method ZCL_MOCKUP_LOADER->_STORE
* +-------------------------------------------------------------------------------------------------+
* | [--->] I_NAME                         TYPE        CHAR40
* | [--->] I_DATA_REF                     TYPE REF TO DATA
* | [--->] I_TABKEY                       TYPE        ABAP_COMPNAME(optional)
* | [!CX!] ZCX_MOCKUP_LOADER_ERROR
* +--------------------------------------------------------------------------------------</SIGNATURE>
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
      zcx_mockup_loader_error=>raise( msg = 'Tabkey is relevant for tables only' code = 'TO' ). "#EC NOTEXT
    endif.

    lo_tab_type ?= lo_type. lo_str_type ?= lo_tab_type->get_table_line_type( ).

    read table lo_str_type->components with key name = i_tabkey transporting no fields.
    if sy-subrc <> 0.
      zcx_mockup_loader_error=>raise( msg = 'Tabkey field not found' code = 'FM' ). "#EC NOTEXT
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
