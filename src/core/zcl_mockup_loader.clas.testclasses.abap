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
*| project homepage: https://github.com/sbcgua/mockup_loader                      |
*\--------------------------------------------------------------------------------/

**********************************************************************
* MACRO
**********************************************************************
define test_parse.
  clear dummy.
  read table lo_struc_descr->components into ls_component with key name = '&1'.
  o->parse_field(
    exporting
      is_component = ls_component
      i_data       = &2
    importing
      e_field      = dummy-&1 ).
end-of-definition.

define test_parse_positive.
  clear lo_ex.
  try.
    test_parse &1 &2.
  catch zcx_mockup_loader_error into lo_ex.
    cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
  endtry.
  cl_abap_unit_assert=>assert_equals( act = dummy-&1 exp = &3 msg = 'Parse field positive:' && &2 ).
end-of-definition.

define test_parse_negative.
  clear lo_ex.
  try.
    test_parse &1 &2.
  catch zcx_mockup_loader_error into lo_ex.
  endtry.
  cl_abap_unit_assert=>assert_not_initial( act = lo_ex msg = 'Parse field negative:' && &2 ).
  cl_abap_unit_assert=>assert_equals( exp = &3 act = lo_ex->code ).
end-of-definition.

define append_dummy.
  e_dummy_struc-tdate    = &1.
  e_dummy_struc-tchar    = &2.
  e_dummy_struc-tstring  = &3.
  e_dummy_struc-tdecimal = &4.
  e_dummy_struc-tnumber  = &5.
  if i_strict = abap_true.
    e_dummy_struc-traw     = &6.
    e_dummy_struc-tinteger = &7.
    e_dummy_struc-talpha   = &8.
  endif.
  append e_dummy_struc to e_dummy_tab.
end-of-definition.

define assert_excode.
  cl_abap_unit_assert=>assert_not_initial( act = lo_ex ).
  cl_abap_unit_assert=>assert_equals( exp = &1 act = lo_ex->code ).
end-of-definition.

**********************************************************************
* DEPENDENCIES CHECK
**********************************************************************

class ltcl_dependencies_version definition final
  for testing
  duration short
  risk level harmless.

  private section.
    methods check_text2tab_version for testing.
endclass.

class ltcl_dependencies_version implementation.
  method check_text2tab_version.

    constants lc_required_ver type string value zif_mockup_loader=>c_required_text2tab_ver.

    if abap_false = zcl_text2tab_parser=>check_version_fits( lc_required_ver ).
      cl_abap_unit_assert=>fail( |data parser version ({
        zif_text2tab=>version }) is lower than required ({
        lc_required_ver })| ). "#EC NOTEXT
    endif.

  endmethod.
endclass.


**********************************************************************
* Test Class definition
**********************************************************************

class ltcl_test_mockup_loader definition for testing
  duration short
  risk level harmless.

  public section.

    types:
      begin of ty_dummy,
        mandt    type mandt,
        tdate    type datum,
        tchar    type c length 8,
        traw     type thraw1,
        tstring  type string,
        talpha   type veri_alpha,
        tdecimal type dmbtr,
        tnumber  type n length 4,
        tinteger type i,
      end of ty_dummy,
      tt_dummy type table of ty_dummy with default key.

    types:
      begin of ty_dummy_corresponding,
        tdate    type datum,
        tchar    type c length 8,
        _another type i,
      end of ty_dummy_corresponding,
      tt_dummy_corresponding type standard table of ty_dummy_corresponding with default key.

    types:
      begin of ty_deep_line,
        docid  type i,
        lineid type i,
        text   type string,
      end of ty_deep_line,
      tt_deep_line type standard table of ty_deep_line with key docid lineid,

      begin of ty_deep_head,
        docid type i,
        lines type tt_deep_line,
        first_line type ty_deep_line,
      end of ty_deep_head,
      tt_deep_head type standard table of ty_deep_head with key docid.

  private section.
    constants c_tab  like cl_abap_char_utilities=>horizontal_tab value cl_abap_char_utilities=>horizontal_tab.
    constants c_crlf like cl_abap_char_utilities=>cr_lf value cl_abap_char_utilities=>cr_lf.
    constants c_lf   like cl_abap_char_utilities=>newline value cl_abap_char_utilities=>newline.

    data o type ref to zcl_mockup_loader.  "class under test

    class-methods class_setup.
    methods setup.

    methods read_zip                 for testing.
    methods integrated_test          for testing.
    methods source_redirect_test     for testing.
    methods utf16_encoding           for testing.

    methods parse_data               for testing.
    methods load_blob                for testing raising zcx_mockup_loader_error.
    methods load_data_to_ref         for testing.
    methods load_deep                for testing raising zcx_mockup_loader_error.
    methods load_deep_negative       for testing.

    methods load_corresponding       for testing raising zcx_mockup_loader_error.
    methods load_corresponding_w_filter for testing raising zcx_mockup_loader_error.
    methods load_w_renames           for testing raising zcx_mockup_loader_error.

    methods assert_version           for testing.
    methods zip_cache                for testing raising zcx_mockup_loader_error.
    methods cd                       for testing raising zcx_mockup_loader_error.
    methods to_container             for testing raising zcx_mockup_loader_error.
    methods into_container           for testing raising zcx_mockup_loader_error.

    methods get_dummy_data
      importing
        i_strict       type abap_bool default abap_true
      exporting
        e_dummy_struc  type ty_dummy
        e_dummy_tab    type tt_dummy
        e_dummy_string type string.

    methods create_default
      returning
        value(r_o) type ref to zcl_mockup_loader
      raising
        zcx_mockup_loader_error.

endclass.

* Friends
class zcl_mockup_loader definition local friends ltcl_test_mockup_loader.

**********************************************************************
* Implementation
**********************************************************************

class ltcl_test_mockup_loader implementation.

**********************************************************************
* Setup methods
**********************************************************************
  method class_setup.
    data l_type_tmp type zif_mockup_loader=>ty_src_type.
    get parameter id 'ZMOCKUP_LOADER_STYPE' field l_type_tmp.
    if l_type_tmp is not initial.
      cl_abap_unit_assert=>fail(
        quit = 2 "cancel-class
        msg  = 'Load source is redirected,'
        && ' please reset with ZMOCKUP_LOADER_SWITCH_SOURCE before running the test' ). "#EC NOTEXT
    endif.
  endmethod.

  method create_default.
    r_o = zcl_mockup_loader=>create(
      i_type       = 'MIME'
      i_path       = 'ZMOCKUP_LOADER_UNIT_TEST'
      i_amt_format = ' .'
      i_encoding   = zif_mockup_loader=>encoding_utf8 ). " utf8
  endmethod.

  method setup.
    data lo_ex type ref to zcx_mockup_loader_error.

    try.
      o = create_default( ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.
  endmethod.

**********************************************************************
* Dummy data generation
**********************************************************************
  method get_dummy_data.
    data l_string type string.

    if i_strict = abap_true.
      l_string = 'MANDT\tTDATE\tTCHAR\tTRAW\tTSTRING\tTALPHA\tTDECIMAL\tTNUMBER\tTINTEGER\n'
              && '\t01.01.2015\tTrololo1\t8A\tString1\t100000\t1234567.81\t2015\t1111\n'
              && '\t02.01.2016\tTrololo2\t8B\tString2\t200000\t1234567.82\t2016\t2222\n'
              && '\t03.01.2016\tTrololo3\t8C\tString3\t300000\t1234567.83\t2015\t3333\n' .
    else.
      l_string = 'TDATE\tTSTRING\tTCHAR\tTDECIMAL\tTNUMBER\n'
              && '01.01.2015\tString1\tTrololo1\t1234567.81\t2015\n'
              && '02.01.2016\tString2\tTrololo2\t1234567.82\t2016\n'
              && '03.01.2016\tString3\tTrololo3\t1234567.83\t2015\n' .
    endif.

    replace all occurrences of '\t' in l_string with c_tab.
    replace all occurrences of '\n' in l_string with c_crlf.

    clear e_dummy_tab.

    "             TDATE      TCHAR      TSTRING   TDECIMAL    TNUM TRAW  TINT  TALPHA
    append_dummy '20150101' 'Trololo1' 'String1' '1234567.81' 2015 '8A'  1111 '0000100000'.
    append_dummy '20160102' 'Trololo2' 'String2' '1234567.82' 2016 '8B'  2222 '0000200000'.
    append_dummy '20160103' 'Trololo3' 'String3' '1234567.83' 2015 '8C'  3333 '0000300000'.

    read table e_dummy_tab into e_dummy_struc index 1.
    e_dummy_string = l_string.

  endmethod.

**********************************************************************
* Simple integrated test - most basic usage of the class
**********************************************************************
  method integrated_test.
    data:
          dummy_act      type ty_dummy,
          dummy_exp      type ty_dummy,
          dummy_tab_act  type tt_dummy,
          dummy_tab_exp  type tt_dummy,
          lo_ex          type ref to zcx_mockup_loader_error.

    get_dummy_data(
      importing
        e_dummy_struc = dummy_exp
        e_dummy_tab   = dummy_tab_exp ).

    " Strict ********************************************************
    try.
      o->load_data(
        exporting
          i_obj       = 'unit_tests/test_complete'
          i_strict    = abap_true
        importing
          e_container = dummy_tab_act ).

      cl_abap_unit_assert=>assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

      o->load_data(
        exporting
          i_obj       = 'unit_tests/test_complete'
          i_strict    = abap_true
        importing
          e_container = dummy_act ).

      cl_abap_unit_assert=>assert_equals( act = dummy_act exp = dummy_exp ).

      o->load_data( " No MANDT field in file
        exporting
          i_obj       = 'unit_tests/test_no_mandt'
          i_strict    = abap_true
        importing
          e_container = dummy_tab_act ).

      cl_abap_unit_assert=>assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    " NOT Strict ****************************************************
    get_dummy_data(
      exporting i_strict    = abap_false
      importing e_dummy_tab = dummy_tab_exp ).

    try.
      o->load_data(
        exporting
          i_obj       = 'unit_tests/test_no_strict'
          i_strict    = abap_false
        importing
          e_container = dummy_tab_act ).

      cl_abap_unit_assert=>assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    " With where ****************************************************
    get_dummy_data( importing e_dummy_tab   = dummy_tab_exp ).

    read table dummy_tab_exp into dummy_exp with key tnumber = '2016'.
    delete dummy_tab_exp where tnumber <> '2016'.

    try.
      o->load_data(
        exporting
          i_obj       = 'unit_tests/test_complete'
          i_where     = 'TNUMBER = 2016'
        importing
          e_container = dummy_tab_act ).

      cl_abap_unit_assert=>assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

      o->load_data(
        exporting
          i_obj       = 'unit_tests/test_complete'
          i_where     = 'TNUMBER = 2016'
        importing
          e_container = dummy_act ).

      cl_abap_unit_assert=>assert_equals( act = dummy_act exp = dummy_exp ).

    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    " No container ***************************************************
    try.
      o->load_data( i_obj = 'unit_tests/test_complete' ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'NC'.

  endmethod.

**********************************************************************
* Source redirection via SET/GET parameters test
**********************************************************************
  method source_redirect_test.

    constants:
      lc_getset_type type memoryid value 'ZMOCKUP_LOADER_STYPE',
      lc_getset_mime type memoryid value 'ZMOCKUP_LOADER_SMIME',
      lc_getset_file type memoryid value 'ZMOCKUP_LOADER_SPATH'.

    data:
      lx_ut      type ref to cx_aunit_fail,
      l_mime_bak type c length 40,
      l_file_bak type c length 40.

    data:
      lo_ex      type ref to zcx_mockup_loader_error,
      l_type     type zif_mockup_loader=>ty_src_type,
      l_path     type c length 40.

    " "DANGEROUS" TEST AS MODIFIES SET/GET PARAMS
    define _bak_params.
      get parameter id lc_getset_file field l_file_bak.
      get parameter id lc_getset_mime field l_mime_bak.
    end-of-definition.
    define _restore_params.
      clear l_type.
      set parameter id lc_getset_type field l_type.
      set parameter id lc_getset_file field l_file_bak.
      set parameter id lc_getset_mime field l_mime_bak.
    end-of-definition.

    cl_abap_unit_assert=>assert_false( o->zif_mockup_loader~is_redirected( ) ).

    " MIME
    _bak_params.

    l_type = 'MIME'.
    set parameter id lc_getset_type field l_type.
    l_path = 'ZMOCKUP_LOADER_WRONG_OBJECT'.
    set parameter id lc_getset_mime field l_path.

    clear lo_ex.
    try.
      o = create_default( ).
      cl_abap_unit_assert=>assert_true( o->zif_mockup_loader~is_redirected( ) ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    _restore_params.
    assert_excode 'RE'.

    " FILE
    _bak_params.

    l_type = 'FILE'.
    set parameter id lc_getset_type field l_type.
    l_path = 'ZMOCKUP_LOADER_WRONG_OBJECT'.
    set parameter id lc_getset_file field l_path.
    l_path = 'ZMOCKUP_LOADER_UNIT_TEST'.
    set parameter id lc_getset_mime field l_path. " replace the mock which was in setup

    clear lo_ex.
    try.
      o = create_default( ).
      cl_abap_unit_assert=>assert_true( o->zif_mockup_loader~is_redirected( ) ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    _restore_params.
    assert_excode 'RE'.

    " FILE, BUT FOR ANOTHER MIME
    _bak_params.

    l_type = 'FILE'.
    set parameter id lc_getset_type field l_type.
    l_path = 'ZMOCKUP_LOADER_WRONG_OBJECT'.
    set parameter id lc_getset_file field l_path.
    l_path = 'ZMOCKUP_LOADER_NOT_EXISTING'.
    set parameter id lc_getset_mime field l_path. " replace the mock which was in setup

    clear lo_ex.
    try.
      o = create_default( ).
      cl_abap_unit_assert=>assert_false( o->zif_mockup_loader~is_redirected( ) ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    _restore_params.
    cl_abap_unit_assert=>assert_initial( lo_ex ).
    cl_abap_unit_assert=>assert_false( o->zif_mockup_loader~is_redirected( ) ).

  endmethod.

**********************************************************************
* Check Unicode encoding parsing
**********************************************************************
  method utf16_encoding.

    data dummy_tab_act  type tt_dummy.
    data dummy_tab_exp  type tt_dummy.
    data lo_ex          type ref to zcx_mockup_loader_error.
    data li             type ref to zif_mockup_loader.

    get_dummy_data( importing e_dummy_tab   = dummy_tab_exp ).

    try.
      li = o->set_params(
        i_amt_format = ' ,'
        i_encoding   = zif_mockup_loader=>encoding_utf16 ).
      cl_abap_unit_assert=>assert_not_initial( li ).

      o->load_data(
        exporting i_obj       = 'unit_tests/test_complete_utf16'
        importing e_container = dummy_tab_act ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

  endmethod.

**********************************************************************
* Test of data parser - dummy data is supplied to the tested method
**********************************************************************
  method parse_data.
    data:
          dummy_val      type c length 40,
          dummy_act      type ty_dummy,
          dummy_tab_act  type tt_dummy,
          dummy_htab     type hashed table of ty_dummy with unique key tdate,
          dummy_stab     type sorted table of ty_dummy with unique key tdate,
          dummy_exp      type ty_dummy,
          dummy_tab_exp  type tt_dummy,
          l_string       type string,
          lo_ex          type ref to zcx_mockup_loader_error.

    " Strict parsing *********************************
    get_dummy_data(
      importing
        e_dummy_struc  = dummy_exp
        e_dummy_tab    = dummy_tab_exp
        e_dummy_string = l_string ).

    try.
      o->parse_data(
        exporting
          i_rawdata   = l_string
          i_strict    = abap_true
        importing
          e_container = dummy_act ).

      o->parse_data(
        exporting
          i_rawdata   = l_string
          i_strict    = abap_true
        importing
          e_container = dummy_tab_act ).

    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals( act = dummy_act     exp = dummy_exp ).
    cl_abap_unit_assert=>assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

    " Parse to sorted and hashed tables ***************
    try.
      o->parse_data(
        exporting
          i_rawdata   = l_string
          i_strict    = abap_true
        importing
          e_container = dummy_stab ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals( act = dummy_stab exp = dummy_tab_exp ).

    try.
      o->parse_data(
        exporting
          i_strict    = abap_true
          i_rawdata   = l_string
        importing
          e_container = dummy_htab ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals( act = dummy_htab exp = dummy_tab_exp ).

    " NOT STRICT parsing ******************************
    get_dummy_data(
      exporting
        i_strict       = abap_false
      importing
        e_dummy_tab    = dummy_tab_exp
        e_dummy_string = l_string ).

    try.
      o->parse_data(
        exporting
          i_rawdata   = l_string
          i_strict    = abap_false
        importing
          e_container = dummy_tab_act ).

    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

    " Catch parser errors, e.g. unknown field
    clear lo_ex.
    get_dummy_data( importing e_dummy_string = l_string ).
    replace first occurrence of 'TDATE' in l_string with 'UNKNOWN_FIELD'.

    try.
      o->parse_data(
        exporting
          i_rawdata   = l_string
          i_strict    = abap_true
        importing
          e_container = dummy_tab_act ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'XE'.

    " Parse to field (not table or structure) *************
    clear lo_ex.
    get_dummy_data( importing e_dummy_string = l_string ).

    try.
      o->parse_data(
        exporting
          i_rawdata   = l_string
          i_strict    = abap_true
        importing
          e_container = dummy_val ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'DT'.

    " Parse empty file ************************************
    clear lo_ex.
    clear l_string.

    try.
      o->parse_data(
        exporting
          i_rawdata   = l_string
          i_strict    = abap_true
        importing
          e_container = dummy_tab_act ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'XE'. " parser error


  endmethod.

**********************************************************************
* ZIP file read test - gets data from SMW0 ZMOCKUP_LOADER_UNIT_TEST
**********************************************************************
  method read_zip.
    data:
          l_str      type string,
          lo_ex      type ref to zcx_mockup_loader_error.

    cl_abap_unit_assert=>assert_not_initial( act = lines( o->mo_zip->files ) ).

    " Positive ***************************************
    try.
      l_str = o->read_zip( i_name = 'unit_tests/test_complete.txt' ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.
    cl_abap_unit_assert=>assert_not_initial( act = l_str ).

    " NEGATIVE - wrong file name **********************
    clear lo_ex.
    try.
      l_str = o->read_zip( i_name = 'unit_tests/wrong_filename.xyz' ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'ZF'.

    " NEGATIVE - wrong code page **********************
    clear lo_ex.
    try.
      l_str = o->read_zip( i_name = 'unit_tests/test_complete_utf16.txt' ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'CP'.

  endmethod.

**********************************************************************
* LOAD_RAW - test load only method e.g. for XMLs
**********************************************************************
  method load_blob.
    data:
          lo_ex      type ref to zcx_mockup_loader_error,
          l_str_exp  type string,
          l_xstr_act type xstring,
          l_str_act  type string,
          lo_conv    type ref to cl_abap_conv_in_ce.

    l_str_exp = '<?xml version="1.0"?><mytag>mydata</mytag>'.

    " .XML
    lo_conv = cl_abap_conv_in_ce=>create( encoding = zif_mockup_loader=>encoding_utf8 ).
    l_xstr_act = o->load_blob( 'unit_tests/test_raw.xml' ).
    lo_conv->convert(
      exporting
        input = l_xstr_act
      importing
        data  = l_str_act ).

    cl_abap_unit_assert=>assert_equals(
      act = l_str_act
      exp = l_str_exp ).

    " .TXT
    l_xstr_act = o->load_blob( 'unit_tests/test_raw.txt' ).
    lo_conv->convert(
      exporting
        input = l_xstr_act
      importing
        data  = l_str_act ).

    cl_abap_unit_assert=>assert_equals(
      act = l_str_act
      exp = l_str_exp ).

    " Missing file
    try .
      o->load_blob( 'unit_tests/no-file-like-this' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>assert_equals(
        act = lo_ex->code
        exp = 'ZF' ).
    endtry.

    " Case insensitive path
    lo_conv = cl_abap_conv_in_ce=>create( encoding = zif_mockup_loader=>encoding_utf8 ).
    l_xstr_act = o->load_blob( 'unit_tests/test_raw.xml' ).
    lo_conv->convert(
      exporting
        input = l_xstr_act
      importing
        data  = l_str_act ).

    cl_abap_unit_assert=>assert_equals(
      act = l_str_act
      exp = l_str_exp ).

  endmethod.

  method load_data_to_ref.
    data:
      dummy_tab_act  type tt_dummy,
      dummy_tab_exp  type tt_dummy,
      lo_ex          type ref to zcx_mockup_loader_error.

    data lr_data type ref to data.
    create data lr_data type tt_dummy.
    field-symbols <act> type tt_dummy.

    get_dummy_data( importing e_dummy_tab = dummy_tab_exp ).

    try.
      o->load_data(
        exporting i_obj       = 'unit_tests/test_complete'
        importing e_container = lr_data ).
      assign lr_data->* to <act>.
      cl_abap_unit_assert=>assert_equals( act = <act> exp = dummy_tab_exp ).

    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.
  endmethod.  " load_data_to_ref.

  method assert_version.

    data lx type ref to zcx_mockup_loader_error.

    try .
      zcl_mockup_loader=>assert_version( zif_mockup_loader=>version ).
    catch zcx_mockup_loader_error into lx.
    endtry.
    cl_abap_unit_assert=>assert_not_bound( lx ).

    try .
      zcl_mockup_loader=>assert_version( 'v999.999.999' ).
    catch zcx_mockup_loader_error into lx.
      cl_abap_unit_assert=>assert_char_cp(
        act = lx->msg
        exp = '*loader version*' ).
    endtry.
    cl_abap_unit_assert=>assert_bound( lx ).

  endmethod.

  method load_deep.

    " DATA
    data lt_docs_exp type tt_deep_head.
    field-symbols <h> like line of lt_docs_exp.
    field-symbols <l> like line of <h>-lines.

    append initial line to lt_docs_exp assigning <h>.
    <h>-docid = 1.
    <h>-first_line-docid  = 1.
    <h>-first_line-lineid = 1.
    <h>-first_line-text   = 'Hello'.
    append initial line to <h>-lines assigning <l>.
    <l>-docid  = 1.
    <l>-lineid = 1.
    <l>-text   = 'Hello'.
    append initial line to <h>-lines assigning <l>.
    <l>-docid  = 1.
    <l>-lineid = 2.
    <l>-text   = 'Mockup'.

    append initial line to lt_docs_exp assigning <h>.
    <h>-docid = 2.
    <h>-first_line-docid  = 2.
    <h>-first_line-lineid = 1.
    <h>-first_line-text   = 'Loader'.
    append initial line to <h>-lines assigning <l>.
    <l>-docid  = 2.
    <l>-lineid = 1.
    <l>-text   = 'Loader'.

    append initial line to lt_docs_exp assigning <h>.
    <h>-docid = 3.

    append initial line to lt_docs_exp assigning <h>.
    <h>-docid = 4.
    <h>-first_line-docid  = 99.
    <h>-first_line-lineid = 1.
    <h>-first_line-text   = '!!!'.
    append initial line to <h>-lines assigning <l>.
    <l>-docid  = 99.
    <l>-lineid = 1.
    <l>-text   = '!!!'.

    append initial line to lt_docs_exp assigning <h>.
    <h>-docid = 5.

    " TEST
    data lt_docs_act like lt_docs_exp.
    o->load_data(
      exporting
        i_obj  = 'unit_tests/deep_head'
        i_deep = abap_true
      importing
        e_container = lt_docs_act ).

    cl_abap_unit_assert=>assert_equals( act = lt_docs_act exp = lt_docs_exp ).

  endmethod.

  method load_deep_negative.

    data lt_docs_act type tt_deep_head.
    data lx type ref to zcx_mockup_loader_error.

    try .
      o->load_data(
        exporting
          i_obj  = 'unit_tests/deep_head_with_wrong_path'
          i_deep = abap_true
        importing
          e_container = lt_docs_act ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'XE' ).
    endtry.

    try .
      o->load_data(
        exporting
          i_obj  = 'unit_tests/deep_head_with_wrong_field'
          i_deep = abap_true
        importing
          e_container = lt_docs_act ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'XE' ).
    endtry.

  endmethod.

  method load_corresponding.
    data:
      dummy_tab_act      type tt_dummy_corresponding,
      dummy_tab_exp      type tt_dummy_corresponding,
      dummy_tab_exp_full type tt_dummy.

    field-symbols <full> like line of dummy_tab_exp_full.
    field-symbols <exp> like line of dummy_tab_exp.

    get_dummy_data( importing e_dummy_tab = dummy_tab_exp_full ).
    loop at dummy_tab_exp_full assigning <full>.
      append initial line to dummy_tab_exp assigning <exp>.
      move-corresponding <full> to <exp>.
    endloop.

    o->load_data(
      exporting
        i_obj           = 'unit_tests/test_complete'
        i_strict        = abap_false
        i_corresponding = abap_true
      importing
        e_container = dummy_tab_act ).
    cl_abap_unit_assert=>assert_equals(
      act = dummy_tab_act
      exp = dummy_tab_exp ).

    delete dummy_tab_exp where tchar <> 'Trololo3'.
    clear dummy_tab_act.

    o->load_data(
      exporting
        i_obj           = 'unit_tests/test_complete'
        i_strict        = abap_false
        i_corresponding = abap_true
        i_where         = 'tchar = Trololo3'
      importing
        e_container = dummy_tab_act ).
    cl_abap_unit_assert=>assert_equals(
      act = dummy_tab_act
      exp = dummy_tab_exp ).

  endmethod.

  method load_corresponding_w_filter.
    data:
      dummy_tab_act      type tt_dummy_corresponding,
      dummy_tab_exp      type tt_dummy_corresponding,
      dummy_tab_exp_full type tt_dummy.

    field-symbols <full> like line of dummy_tab_exp_full.
    field-symbols <exp> like line of dummy_tab_exp.

    get_dummy_data( importing e_dummy_tab = dummy_tab_exp_full ).
    loop at dummy_tab_exp_full assigning <full>.
      check <full>-tnumber = '2015'.
      append initial line to dummy_tab_exp assigning <exp>.
      move-corresponding <full> to <exp>.
    endloop.

    o->load_data(
      exporting
        i_obj           = 'unit_tests/test_complete'
        i_strict        = abap_false
        i_corresponding = abap_true
        i_where         = 'tnumber = 2015'
      importing
        e_container = dummy_tab_act ).
    cl_abap_unit_assert=>assert_equals(
      act = dummy_tab_act
      exp = dummy_tab_exp ).

    data lt_filter type zif_mockup_loader=>tt_filter.
    data lt_range type range of ty_dummy-tnumber.

    " Filter table
    field-symbols <f> like line of lt_filter.
    append initial line to lt_filter assigning <f>.
    <f>-name = 'TNUMBER'.
    <f>-type = 'R'.
    get reference of lt_range into <f>-valref.
    field-symbols <r> like line of lt_range.
    append initial line to lt_range assigning <r>.
    <r>-sign   = 'I'.
    <r>-option = 'EQ'.
    <r>-low    = '2015'.

    clear dummy_tab_act.
    o->load_data(
      exporting
        i_obj           = 'unit_tests/test_complete'
        i_strict        = abap_false
        i_corresponding = abap_true
        i_where         = lt_filter
      importing
        e_container = dummy_tab_act ).
    cl_abap_unit_assert=>assert_equals(
      act = dummy_tab_act
      exp = dummy_tab_exp ).

  endmethod.

  method load_w_renames.

    types:
      begin of lty_dummy_w_renames,
        tdate    type datum,
        ychar    type c length 8,
      end of lty_dummy_w_renames.

    data:
      dummy_act type lty_dummy_w_renames,
      dummy_exp type lty_dummy_w_renames.

    o->load_data(
      exporting
        i_obj           = 'unit_tests/test_complete'
        i_strict        = abap_false
        i_rename_fields = 'tchar:ychar'
        i_corresponding = abap_true
      importing
        e_container = dummy_act ).

    dummy_exp-tdate = '20150101'.
    dummy_exp-ychar = 'Trololo1'.
    cl_abap_unit_assert=>assert_equals(
      act = dummy_act
      exp = dummy_exp ).

  endmethod.

  method zip_cache.

    data ls_cache like line of zcl_mockup_loader=>gt_zip_cache.
    data lv_reuse_count_at_step1 type i.

    cl_abap_unit_assert=>assert_initial( zcl_mockup_loader=>gt_zip_cache ).

    zcl_mockup_loader=>create(
      i_type       = 'MIME'
      i_path       = 'ZMOCKUP_LOADER_UNIT_TEST'
      i_cache_timeout = 5
      i_amt_format = ''     " default
      i_encoding   = zif_mockup_loader=>encoding_utf8 ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( zcl_mockup_loader=>gt_zip_cache )
      exp = 1 ).
    read table zcl_mockup_loader=>gt_zip_cache
      with key key = 'MIME:ZMOCKUP_LOADER_UNIT_TEST'
      into ls_cache.
    cl_abap_unit_assert=>assert_subrc( ).
    lv_reuse_count_at_step1 = zcl_mockup_loader=>gv_cache_reuse_count.

    zcl_mockup_loader=>create(
      i_type       = 'MIME'
      i_path       = 'ZMOCKUP_LOADER_UNIT_TEST'
      i_cache_timeout = 5
      i_amt_format = ''     " default
      i_encoding   = zif_mockup_loader=>encoding_utf8 ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( zcl_mockup_loader=>gt_zip_cache )
      exp = 1 ).
    read table zcl_mockup_loader=>gt_zip_cache
      with key key = 'MIME:ZMOCKUP_LOADER_UNIT_TEST'
      into ls_cache.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_mockup_loader=>gv_cache_reuse_count - lv_reuse_count_at_step1
      exp = 1 ).

  endmethod.

  method cd.

    data ls_act1 type ty_dummy.
    data ls_act2 type ty_dummy.
    data lx type ref to zcx_mockup_loader_error.

    o->load_data(
      exporting
        i_obj       = 'unit_tests/test_complete'
      importing
        e_container = ls_act1 ).
    cl_abap_unit_assert=>assert_not_initial( ls_act1 ).

    try.
      o->load_data(
        exporting
          i_obj       = './test_complete'
        importing
          e_container = ls_act2 ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->code
        exp = 'ZF' ).
    endtry.

    data li type ref to zif_mockup_loader.
    li = o->zif_mockup_loader~cd( 'unit_tests' ).
    cl_abap_unit_assert=>assert_not_initial( li ).
    o->load_data(
      exporting
        i_obj       = './test_complete'
      importing
        e_container = ls_act2 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_act2
      exp = ls_act1 ).

  endmethod.

  method to_container.

    data li type ref to zif_mockup_loader.
    data lx type ref to zcx_mockup_loader_error.
    data ls_act1 type ty_dummy.
    data ls_act2 type ty_dummy.
    data lr type ref to ty_dummy.

    li = o.
    get reference of ls_act2 into lr.

    li->load_data(
      exporting
        i_obj       = 'unit_tests/test_complete'
      importing
        e_container = ls_act1 ).
    cl_abap_unit_assert=>assert_not_initial( ls_act1 ).

    try.
      li->load_data( 'unit_tests/test_complete' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'NC' ).
    endtry.

    li->to( lr )->load_data( 'unit_tests/test_complete' ).
    cl_abap_unit_assert=>assert_equals( act = ls_act2 exp = ls_act1 ).

  endmethod.

  method into_container.

    data li type ref to zif_mockup_loader.
    data lx type ref to zcx_mockup_loader_error.
    data ls_exp type ty_dummy.
    data ls_act type ty_dummy.
    data lt_exp type tt_dummy.
    data lt_act type tt_dummy.

    li = o.

    " Usual load
    li->load_data(
      exporting
        i_obj       = 'unit_tests/test_complete'
      importing
        e_container = ls_exp ).
    cl_abap_unit_assert=>assert_not_initial( ls_exp ).

    li->load_data(
      exporting
        i_obj       = 'unit_tests/test_complete'
      importing
        e_container = lt_exp ).
    cl_abap_unit_assert=>assert_not_initial( lt_exp ).

    " ideaomatic load
    li->load( 'unit_tests/test_complete' )->into( changing data = ls_act ).
    cl_abap_unit_assert=>assert_equals( act = ls_act exp = ls_exp ).

    " load table
    li->load( 'unit_tests/test_complete' )->into( changing data = lt_act ).
    cl_abap_unit_assert=>assert_equals( act = lt_act exp = lt_exp ).

    " filter
    " No deep test, because it just calls load_data insede which is well tested
    li->load(
      i_obj   = 'unit_tests/test_complete'
      i_where = 'tnumber = 2016' )->into( changing data = lt_act ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_exp ) exp = 3 ).
    cl_abap_unit_assert=>assert_equals( act = lines( lt_act ) exp = 1 ).

    " Renames
    data:
      begin of ls_act_renamed,
        ychar type c length 8,
      end of ls_act_renamed.
    li->load(
      i_obj           = 'unit_tests/test_complete'
      i_corresponding = abap_true
      i_rename_fields = 'tchar:ychar' )->into( changing data = ls_act_renamed ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_act_renamed-ychar
      exp = 'Trololo1' ).

  endmethod.

endclass.
