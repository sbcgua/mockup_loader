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

  define add_range.
    r_&1-sign   = &2.
    r_&1-option = &3.
    r_&1-low    = &4.
    append r_&1 to l_where-t&1.
  end-of-definition.

  define assert_excode.
    cl_abap_unit_assert=>assert_not_initial( act = lo_ex ).
    cl_abap_unit_assert=>assert_equals( exp = &1 act = lo_ex->code ).
  end-of-definition.


**********************************************************************
* Test Class definition
**********************************************************************

class lcl_test_mockup_loader definition for testing
  duration short
  risk level harmless.

* ================
  public section.

    types:
      begin of ty_dummy,
        mandt    type mandt,
        tdate    type datum,
        tchar    type veri_c08,
        traw     type veri_x1,
        tstring  type string,
        talpha   type veri_alpha,
        tdecimal type veri_cur13,
        tnumber  type veri_n04,
        tinteger type i,
      end of ty_dummy,
      tt_dummy type table of ty_dummy with default key.

* ================
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
    methods break_to_lines           for testing.

    methods parse_data               for testing.
    methods parse_field              for testing.
    methods empty_lines              for testing.
    methods map_file_structure       for testing.
    methods range_filtering          for testing.

    methods load_and_store           for testing.
    methods load_raw                 for testing.
    methods parse_apply_exit         for testing.

    methods get_dummy_data
      importing
        i_strict       type abap_bool default abap_true
      exporting
        e_dummy_struc  type ty_dummy
        e_dummy_tab    type tt_dummy
        e_dummy_string type string.

    methods filter_helper
      importing
        i_tab        type tt_dummy
        i_filter     type zcl_mockup_loader=>tt_filter
      returning value(e_tab) type tt_dummy.

endclass.       "lcl_test_mockup_loader

* Friends
class zcl_mockup_loader definition local friends lcl_test_mockup_loader.

**********************************************************************
* Implementation
**********************************************************************

class lcl_test_mockup_loader implementation.

**********************************************************************
* Setup methods
**********************************************************************
  method class_setup.
    data l_type_tmp type char4.

    get parameter id 'ZMOCKUP_LOADER_STYPE' field l_type_tmp.
    if l_type_tmp is not initial.
      cl_abap_unit_assert=>fail( quit = 2 "calcel-class
            msg  = 'Load source is redirected, please reset with ZMOCKUP_LOADER_SWITCH_SOURCE before running the test' ). "#EC NOTEXT
    endif.

    zcl_mockup_loader=>class_set_source( i_type = 'MIME' i_path = 'ZMOCKUP_LOADER_UNIT_TEST' ).
    zcl_mockup_loader=>class_set_params( i_amt_format = '' i_encoding = '4110' ). " fmt = default, enc = utf-8
  endmethod.       "class_setup

  method setup.
    data lo_ex type ref to zcx_mockup_loader_error.

    try.
      o = zcl_mockup_loader=>get_instance( ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.
  endmethod.       "setup

**********************************************************************
* Dummy data generation
**********************************************************************
  method get_dummy_data.
    data l_string type string.

    if i_strict = abap_true.
      l_string = 'MANDT\tTDATE\tTCHAR\tTRAW\tTSTRING\tTALPHA\tTDECIMAL\tTNUMBER\tTINTEGER\n'
              && '\t01.01.2015\tTrololo1\t8A\tString1\t100000\t1234567,81\t2015\t1111\n'
              && '\t02.01.2016\tTrololo2\t8B\tString2\t200000\t1234567,82\t2016\t2222\n'
              && '\t03.01.2016\tTrololo3\t8C\tString3\t300000\t1234567,83\t2015\t3333\n' .
    else.
      l_string = 'TDATE\tTSTRING\tTCHAR\tTDECIMAL\tTNUMBER\n'
              && '01.01.2015\tString1\tTrololo1\t1234567,81\t2015\n'
              && '02.01.2016\tString2\tTrololo2\t1234567,82\t2016\n'
              && '03.01.2016\tString3\tTrololo3\t1234567,83\t2015\n' .
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

  endmethod.       " get_dummy_data

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
        exporting i_obj       = 'testdir/testfile_complete'
        importing e_container = dummy_tab_act ).

      cl_abap_unit_assert=>assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

      o->load_data(
        exporting i_obj       = 'testdir/testfile_complete'
        importing e_container = dummy_act ).

      cl_abap_unit_assert=>assert_equals( act = dummy_act exp = dummy_exp ).

      o->load_data( " No MANDT field in file
        exporting i_obj       = 'testdir/testfile_no_mandt'
        importing e_container = dummy_tab_act ).

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
          i_obj       = 'testdir/testfile_no_strict'
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
          i_obj       = 'testdir/testfile_complete'
          i_where     = 'TNUMBER = 2016'
        importing
          e_container = dummy_tab_act ).

      cl_abap_unit_assert=>assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

      o->load_data(
        exporting
          i_obj       = 'testdir/testfile_complete'
          i_where     = 'TNUMBER = 2016'
        importing
          e_container = dummy_act ).

      cl_abap_unit_assert=>assert_equals( act = dummy_act exp = dummy_exp ).

    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    " No container ***************************************************
    try.
      o->load_data( i_obj = 'testdir/testfile_complete' ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'NC'.

  endmethod.       " integrated_test

**********************************************************************
* Source redirection via SET/GET parameters test
**********************************************************************
  method source_redirect_test.
    data:
          lo_ex      type ref to zcx_mockup_loader_error,
          l_type     type char4,
          l_path     type char40,
          l_path_tmp type char40.

    get parameter id 'ZMOCKUP_LOADER_SPATH' field l_path_tmp. " Preserve

    l_type = 'FILE'.
    l_path = 'ZMOCKUP_LOADER_WRONG_OBJECT'.
    set parameter id 'ZMOCKUP_LOADER_STYPE' field l_type.
    set parameter id 'ZMOCKUP_LOADER_SPATH' field l_path.
    o->free_instance( ).

    try.
      o = zcl_mockup_loader=>get_instance( ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    clear l_type.
    set parameter id 'ZMOCKUP_LOADER_STYPE' field l_type.
    set parameter id 'ZMOCKUP_LOADER_SPATH' field l_path_tmp.
    o->free_instance( ).

    assert_excode 'RE'.

    " MIME
    get parameter id 'ZMOCKUP_LOADER_SMIME' field l_path_tmp. " Preserve

    l_type = 'MIME'.
    l_path = 'ZMOCKUP_LOADER_WRONG_OBJECT'.
    set parameter id 'ZMOCKUP_LOADER_STYPE' field l_type.
    set parameter id 'ZMOCKUP_LOADER_SMIME' field l_path.
    o->free_instance( ).

    try.
      o = zcl_mockup_loader=>get_instance( ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    clear l_type.
    set parameter id 'ZMOCKUP_LOADER_STYPE' field l_type.
    set parameter id 'ZMOCKUP_LOADER_SMIME' field l_path_tmp.
    o->free_instance( ).

    assert_excode 'RE'.

  endmethod.       " source_redirect_test

**********************************************************************
* Check Unicode encoding parsing
**********************************************************************
  method utf16_encoding.
    data:
          dummy_tab_act  type tt_dummy,
          dummy_tab_exp  type tt_dummy,
          lo_ex          type ref to zcx_mockup_loader_error.

    get_dummy_data( importing e_dummy_tab   = dummy_tab_exp ).

    try.
      zcl_mockup_loader=>class_set_params( i_amt_format = '' i_encoding = '4103' ). " UTF16
      o->load_data(
        exporting i_obj       = 'testdir/testfile_complete_utf16'
        importing e_container = dummy_tab_act ).
      zcl_mockup_loader=>class_set_params( i_amt_format = '' i_encoding = '4110' ). " Back to SETUP defaults
    catch zcx_mockup_loader_error into lo_ex.
      zcl_mockup_loader=>class_set_params( i_amt_format = '' i_encoding = '4110' ). " Back to SETUP defaults
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

  endmethod. "utf16_encoding

**********************************************************************
* Break to lines, line break detection
**********************************************************************
  method break_to_lines.
    data:
          lt_act type string_table,
          lt_exp type string_table.

    append 'line1' to lt_exp.
    append 'line2' to lt_exp.

    lt_act = zcl_mockup_loader=>break_to_lines( 'line1' && c_crlf && 'line2' ).
    cl_abap_unit_assert=>assert_equals( act = lt_act exp = lt_exp ).
    lt_act = zcl_mockup_loader=>break_to_lines( 'line1' && c_lf && 'line2' ).
    cl_abap_unit_assert=>assert_equals( act = lt_act exp = lt_exp ).

  endmethod.  " break_to_lines

**********************************************************************
* Test of data parser - dummy data is supplied to the tested method
**********************************************************************
  method parse_data.
    data:
          dummy_val      type char40,
          dummy_act      type ty_dummy,
          dummy_tab_act  type tt_dummy,
          dummy_htab     type hashed table of ty_dummy with unique key tdate,
          dummy_stab     type sorted table of ty_dummy with unique key tdate,
          dummy_exp      type ty_dummy,
          dummy_tab_exp  type tt_dummy,
          l_string       type string,
          lo_ex          type ref to zcx_mockup_loader_error.

    " Strict parsing *********************************
    get_dummy_data( importing e_dummy_struc  = dummy_exp
                              e_dummy_tab    = dummy_tab_exp
                              e_dummy_string = l_string ).

    try.
      o->parse_data(
        exporting i_rawdata   = l_string
        importing e_container = dummy_act ).

      o->parse_data(
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act ).

    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals( act = dummy_act     exp = dummy_exp ).
    cl_abap_unit_assert=>assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

    " Parse to sorted and hashed tables ***************
    try.
      o->parse_data(
        exporting i_rawdata   = l_string
        importing e_container = dummy_stab ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals( act = dummy_stab exp = dummy_tab_exp ).

    try.
      o->parse_data(
        exporting i_rawdata   = l_string
        importing e_container = dummy_htab ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals( act = dummy_htab exp = dummy_tab_exp ).

    " NOT STRICT parsing ******************************
    get_dummy_data( exporting i_strict       = abap_false
                    importing e_dummy_tab    = dummy_tab_exp
                              e_dummy_string = l_string ).

    try.
      o->parse_data(
        exporting
          i_rawdata   = l_string
          i_strict    = ''
        importing
          e_container = dummy_tab_act ).

    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

    " Fields out of bound (more fields than in header) ***
    clear lo_ex.
    get_dummy_data( importing e_dummy_string = l_string ).
    replace first occurrence of '1111' in l_string with '1111' && c_tab && 'EXCESS_FIELD'.

    try.
      o->parse_data(
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode '>H'.

    " Fields out of bound (less fields than in header) ***
    clear lo_ex.
    get_dummy_data( importing e_dummy_string = l_string ).
    replace first occurrence of c_tab && '1111' in l_string with ''.

    try.
      o->parse_data(
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode '<H'.

    " Parse to field (not table or structure) *************
    clear lo_ex.
    get_dummy_data( importing e_dummy_string = l_string ).

    try.
      o->parse_data(
        exporting i_rawdata   = l_string
        importing e_container = dummy_val ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'DT'.

    " Parse empty file ************************************
    clear lo_ex.
    clear l_string.

    try.
      o->parse_data(
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'NH'.


  endmethod.       "parse_Data

**********************************************************************
* Individual field parsing test
**********************************************************************
  method parse_field.
    data:
          dummy          type ty_dummy,
          lo_struc_descr type ref to cl_abap_structdescr,
          ls_component   type abap_compdescr,
          lo_ex          type ref to zcx_mockup_loader_error.

    lo_struc_descr ?= cl_abap_structdescr=>describe_by_data( dummy ).

    " Positive tests ******************************
    try.
      test_parse_positive TDATE    '01.01.2015'      '20150101'.
      test_parse_positive TDATE    '1.2.2015'        '20150201'.
      test_parse_positive TCHAR    'ABC'             'ABC'.
      test_parse_positive TSTRING  'The string test' 'The string test'.
      test_parse_positive TALPHA   '100000'          '0000100000'.
      test_parse_positive TNUMBER  '2015'            '2015'.
      test_parse_positive TINTEGER '123'             123.
      test_parse_positive TRAW     '8E'              '8E'.
      test_parse_positive TNUMBER  '"2015"'          '2015'. " Quoted
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    " Negative tests ******************************

    test_parse_negative TNUMBER  '20ha' 'PF'.

    " Decimal converion tests *********************
    try.
      test_parse_positive TDECIMAL '1234.12'         '1234.12'. " Native ABAP format
      test_parse_positive TDECIMAL '-1234.12'        '-1234.12'." Native ABAP format

      zcl_mockup_loader=>class_set_params( i_amt_format = '' ). " Set defaults
      test_parse_positive TDECIMAL '-1234,12'        '-1234.12'.
      test_parse_positive TDECIMAL '1234,12'         '1234.12'.
      test_parse_positive TDECIMAL '1 234,12'        '1234.12'.
      test_parse_positive TDECIMAL '14,12'           '14.12'.
      test_parse_positive TDECIMAL '1 234 567,12'    '1234567.12'.

      zcl_mockup_loader=>class_set_params( i_amt_format = '.,' ).
      test_parse_positive TDECIMAL '1234,12'         '1234.12'.
      test_parse_positive TDECIMAL '1 234,12'        '1234.12'.
      test_parse_positive TDECIMAL '1.234,12'        '1234.12'.
      test_parse_positive TDECIMAL '14,12'           '14.12'.
      test_parse_positive TDECIMAL '1.234.567,12'    '1234567.12'.

      zcl_mockup_loader=>class_set_params( i_amt_format = ',.' ).
      test_parse_positive TDECIMAL '1234.12'         '1234.12'.
      test_parse_positive TDECIMAL '1 234.12'        '1234.12'.
      test_parse_positive TDECIMAL '1,234.12'        '1234.12'.
      test_parse_positive TDECIMAL '14.12'           '14.12'.
      test_parse_positive TDECIMAL '1,234,567.12'    '1234567.12'.

    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    zcl_mockup_loader=>class_set_params( i_amt_format = '' ). " Set defaults
    test_parse_negative TDECIMAL '1 234.12' 'PF'.
    test_parse_negative TDECIMAL '1 234_12' 'PF'.
    test_parse_negative TDECIMAL '1234,123' 'PF'. " 3 decimal digits into amount which has just 2
    test_parse_negative TDECIMAL '1234,12_' 'PF'.
    test_parse_negative TDECIMAL 'Not-a-number' 'PF'.
    zcl_mockup_loader=>class_set_params( i_amt_format = '.,' ).
    test_parse_negative TDECIMAL '1 234.12' 'PF'.
    test_parse_negative TDECIMAL '1,234.12' 'PF'.
    zcl_mockup_loader=>class_set_params( i_amt_format = ',.' ).
    test_parse_negative TDECIMAL '1 234,12' 'PF'.
    test_parse_negative TDECIMAL '1.234,12' 'PF'.

    " Date tests **********************************

    zcl_mockup_loader=>class_set_params( i_date_format = 'MDY').
    test_parse_positive TDATE    '02012015'    '20150201'.
    zcl_mockup_loader=>class_set_params( i_date_format = 'YMD').
    test_parse_positive TDATE    '20150201'    '20150201'.
    test_parse_negative TDATE    '2015020'     'DL'.  " Too short
    zcl_mockup_loader=>class_set_params( i_date_format = 'YMD-').
    test_parse_positive TDATE    '2015-02-01'  '20150201'.
    test_parse_positive TDATE    '2015-2-1'    '20150201'.
    test_parse_positive TDATE    `        `    '00000000'.
    test_parse_positive TDATE    ''            '00000000'.
    zcl_mockup_loader=>class_set_params( i_date_format = 'DMY.').

    " Negative tests
    test_parse_negative TDATE    'AB022015'    'DY'. " Wrong symbols
    test_parse_negative TDATE    '01.02-2015'  'DY'. " Wrong separators
    test_parse_negative TDATE    '01.02.20156' 'DL'. " Too long
    test_parse_negative TDATE    '1.2.201567'  'DP'. " Wrong part length
    test_parse_negative TDATE    '123.2.2015'  'DP'. " Wrong part length
    test_parse_negative TDATE    '01022015'    'DS'. " No separators
    test_parse_negative TDATE    '01.012015'   'DS'. " No second separator
    test_parse_negative TDATE    '40.01.2015'  'DU'. " Incorrect day
    test_parse_negative TDATE    '01.13.2015'  'DU'. " Incorrect month

    zcl_mockup_loader=>class_set_params( i_amt_format = '' i_encoding = '4110' ). " Set defaults back

  endmethod.       "parse_Field

**********************************************************************
* ZIP file read test - gets data from SMW0 ZMOCKUP_LOADER_UNIT_TEST
**********************************************************************
  method read_zip.
    data:
          l_str      type string,
          lo_ex      type ref to zcx_mockup_loader_error.

    cl_abap_unit_assert=>assert_not_initial( act = lines( o->o_zip->files ) ).

    " Positive ***************************************
    try.
      o->read_zip(
        exporting i_name    = 'testdir/testfile_complete.txt'
        importing e_rawdata = l_str ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.
    cl_abap_unit_assert=>assert_not_initial( act = l_str ).

    " NEGATIVE - wrong file name **********************
    clear lo_ex.
    try.
      o->read_zip(
        exporting i_name    = 'testdir/wrong_filename.xyz'
        importing e_rawdata = l_str ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'ZF'.

    " NEGATIVE - wrong code page **********************
    clear lo_ex.
    try.
      o->read_zip(
        exporting i_name    = 'testdir/testfile_complete_utf16.txt'
        importing e_rawdata = l_str ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'CP'.

  endmethod.        " read_zip

**********************************************************************
* Identification of empty lines in the text files
**********************************************************************
  method empty_lines.
    data:
          dummy_tab_exp type tt_dummy,
          dummy_tab_act type tt_dummy,
          l_string      type string,
          lo_ex         type ref to zcx_mockup_loader_error.

    get_dummy_data( importing e_dummy_tab    = dummy_tab_exp
                              e_dummy_string = l_string ).

    " Add empty line at the end *****************************
    l_string = l_string && c_crlf.

    try.
      o->parse_data(
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

    " Add empty line in the middle ***************************
    replace first occurrence of c_crlf in l_string with c_crlf && c_crlf.
    clear lo_ex.

    try.
      o->parse_data(
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'LE'.

    " Add empty line at the beginning ************************
    l_string = c_crlf && l_string.
    clear lo_ex.

    try.
      o->parse_data(
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'HE'.

  endmethod.       "empty_lines

**********************************************************************
* Test check of in and out data types
**********************************************************************
  method map_file_structure.
    data:
          dummy_tab_exp type tt_dummy,
          dummy_tab_act type tt_dummy,
          l_string      type string,
          l_string_bak  type string,
          lo_ex         type ref to zcx_mockup_loader_error.

    get_dummy_data( importing e_dummy_tab    = dummy_tab_exp
                              e_dummy_string = l_string_bak ).

    " Duplicate field names *******************************
    clear lo_ex.
    l_string = l_string_bak.
    replace first occurrence of 'TCHAR' in l_string with 'TDATE'.

    try.
      o->parse_data(
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'DN'.

    " Empty field names ***********************************
    clear lo_ex.
    l_string = l_string_bak.
    replace first occurrence of 'TCHAR' in l_string with ''.

    try.
      o->parse_data(
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'EN'.

    " Unknown field in text *******************************
    clear lo_ex.
    l_string = l_string_bak.
    replace first occurrence of 'TCHAR' in l_string with 'UNKNOWN'.

    try.
      o->parse_data(
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'MC'.

    " More fields than in target structure ****************
    clear lo_ex.
    l_string = l_string_bak.
    replace first occurrence of 'TINTEGER' in l_string with 'TINTEGER' && c_tab && 'EXCESS_FIELD'.

    try.
      o->parse_data(
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'CN'.

    " Empty field at the end ******************************
    clear lo_ex.
    l_string = l_string_bak.
    replace first occurrence of 'TINTEGER' in l_string with 'TINTEGER' && c_tab.

    try.
      o->parse_data(
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'EN'.

    " Empty field at the end of data line ******************
    clear lo_ex.
    l_string = l_string_bak.
    replace first occurrence of '1111' in l_string with '1111' && c_tab.

    try.
      o->parse_data(
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode '>H'.

  endmethod.       "map_file_structure

**********************************************************************
* LOAD AND STORE at once
**********************************************************************
  method load_and_store.
    data:
          dummy_tab_exp  type tt_dummy,
          dummy_tab_act  type tt_dummy,
          lo_ex          type ref to zcx_mockup_loader_error.

    get_dummy_data( importing e_dummy_tab   = dummy_tab_exp ).

    " Positive test ************************************
    try.
      o->load_and_store(
        exporting i_obj       = 'testdir/testfile_complete'
                  i_name      = 'TAB'
                  i_type      = 'LCL_TEST_MOCKUP_LOADER=>TT_DUMMY' ).

      zcl_mockup_loader_store=>retrieve(
        exporting i_name   = 'TAB'
        importing e_data   = dummy_tab_act ).

    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals( act = dummy_tab_act  exp = dummy_tab_exp ).

    " Negative: type that not exists ********************
    clear lo_ex.
    try.
      o->load_and_store(
        exporting i_obj       = 'testdir/testfile_complete'
                  i_name      = 'TAB'
                  i_type      = '************' ).

    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'WT'.

  endmethod.       "load_and_store

**********************************************************************
* RANGE_FILTERING - test build_filter and does_line_fit_filter
**********************************************************************
  method range_filtering.
    data:
          dummy_tab_src  type tt_dummy,
          dummy_tab_exp  type tt_dummy,
          dummy          type ty_dummy,

          l_filter       type zcl_mockup_loader=>tt_filter,
          lo_ex          type ref to zcx_mockup_loader_error,

          lt_tywhere     type zcl_mockup_loader=>tt_where,
          l_tywhere      type zcl_mockup_loader=>ty_where,

          begin of l_where_err1,
            tnumber  type range of veri_n04,
            tdate    type range of datum,
            tother   type tt_dummy,
          end of l_where_err1,

          begin of l_where_err2,
            tnumber  type range of veri_n04,
            tdate    type range of datum,
            tother   type c,
          end of l_where_err2,

          begin of l_where,
            tnumber  type range of veri_n04,
            tdate    type range of datum,
            tother   type range of c,
          end of l_where,

          r_number  like line of l_where-tnumber,
          r_date    like line of l_where-tdate,
          r_other   like line of l_where-tother.

    get_dummy_data( importing e_dummy_tab = dummy_tab_src ).

    " Negative tests --------------------------------------------------------------------

    " Component is not a range table
    try.
      l_filter = o->build_filter( l_where_err1 ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'WS'.

    " Component is not a table
    try.
      l_filter = o->build_filter( l_where_err2 ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'WS'.

    " Unexpected types of filters
    l_tywhere-name = 'TNUMBER'.
    get reference of l_where into l_tywhere-range.
    try.
      l_filter = o->build_filter( l_tywhere ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'CE'.

    " Filter is not range
    l_tywhere-name = 'TNUMBER'.
    get reference of dummy_tab_exp into l_tywhere-range.
    try.
      l_filter = o->build_filter( l_tywhere ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'RT'.

    " Filter is not range + TABLE
    l_tywhere-name = 'TNUMBER'.
    get reference of dummy_tab_exp into l_tywhere-range.
    append l_tywhere to lt_tywhere.
    try.
      l_filter = o->build_filter( lt_tywhere ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    clear lt_tywhere.
    assert_excode 'RT'.

    " Wrong type of table
    try.
      l_filter = o->build_filter( dummy_tab_exp ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'WT'.

    " Parameter is an unsupported type
    try.
      l_filter = o->build_filter( lo_ex ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'UT'.

    " parameter is incorrect string pattern
    try.
      l_filter = o->build_filter( 'TNUMBER??' ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'SP'.


    " Positive test RANGE STRUCTURE -----------------------------------------------------
    dummy_tab_exp[] = dummy_tab_src[].
    delete dummy_tab_exp where tnumber <> '2015' or tdate < '20160101'.

    add_range number 'I' 'EQ' '2015'.
    add_range date   'I' 'GE' '20160101'.
    add_range other  'I' 'GE' 'A'.

    try.
      l_filter = o->build_filter( l_where ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals(
      act = filter_helper( i_tab = dummy_tab_src i_filter = l_filter )
      exp = dummy_tab_exp ).

    " Positive test TY_WHERE TABLE --------------------------------------------------
    " REUSE dummy_tab_exp and ranges from above
    l_tywhere-name = 'TNUMBER'.
    get reference of l_where-tnumber into l_tywhere-range.
    append l_tywhere to lt_tywhere.

    l_tywhere-name = 'TDATE'.
    get reference of l_where-tdate into l_tywhere-range.
    append l_tywhere to lt_tywhere.

    try .
      l_filter = o->build_filter( lt_tywhere ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals(
      act = filter_helper( i_tab = dummy_tab_src i_filter = l_filter )
      exp = dummy_tab_exp ).

    " Positive test STRING --------------------------------------------------------------
    dummy_tab_exp[] = dummy_tab_src[].
    delete dummy_tab_exp where tnumber <> '2015'.

    try .
      l_filter = o->build_filter( 'TNUMBER = 2015' ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals( act = filter_helper( i_tab = dummy_tab_src i_filter = l_filter )
                   exp = dummy_tab_exp ).

    " Same but with lower case name
    try .
      l_filter = o->build_filter( 'TnumBER = 2015' ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals(
      act = filter_helper( i_tab = dummy_tab_src i_filter = l_filter )
      exp = dummy_tab_exp ).


    " Positive test TY_WHERE STRUCTURE --------------------------------------------------
    " REUSE dummy_tab_exp and ranges from above
    l_tywhere-name = 'TNUMBER'.
    get reference of l_where-tnumber into l_tywhere-range.

    try .
      l_filter = o->build_filter( l_tywhere ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals(
      act = filter_helper( i_tab = dummy_tab_src i_filter = l_filter )
      exp = dummy_tab_exp ).

  endmethod.       "range_filtering

  method filter_helper.
    data dummy type ty_dummy.
    loop at i_tab into dummy.
      if o->does_line_fit_filter( i_line = dummy i_filter = i_filter ) = abap_true.
        append dummy to e_tab.
      endif.
    endloop.
  endmethod.       " filter_helper

**********************************************************************
* LOAD_RAW - test load only method e.g. for XMLs
**********************************************************************
  method load_raw.
    data:
          lo_exr     type ref to cx_root,
          lo_ex      type ref to zcx_mockup_loader_error,
          l_str_exp  type string,
          l_xstr_act type xstring,
          l_str_act  type string,
          lo_conv    type ref to cl_abap_conv_in_ce.

    l_str_exp = '<?xml version="1.0"?><mytag>mydata</mytag>'.

    try. " .XML
      lo_conv = cl_abap_conv_in_ce=>create( encoding = '4110' ).
      o->load_raw(
        exporting
          i_obj = 'testdir/test_raw'
          i_ext = '.xml'
        importing
          e_content = l_xstr_act ).
      lo_conv->convert(
        exporting input = l_xstr_act
        importing data  = l_str_act ).
    catch cx_root into lo_exr.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals( act = l_str_act exp = l_str_exp ).

    try. " .TXT
      o->load_raw(
        exporting i_obj = 'testdir/test_raw'
        importing e_content = l_xstr_act ).
      lo_conv->convert(
        exporting input = l_xstr_act
        importing data  = l_str_act ).
    catch cx_root into lo_exr.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals( act = l_str_act exp = l_str_exp ).

    try. " No container
      o->load_raw( i_obj = 'testdir/test_raw' ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'NC'.

  endmethod. "load_raw

**********************************************************************
* PARSE_APPLY_EXIT - apply exit test
**********************************************************************
  method parse_apply_exit.
    data:
          l_dummy  type ty_dummy,
          lo_ex    type ref to zcx_mockup_loader_error.

    try .
      o->parse_apply_exit(
        exporting
          i_convexit = 'ALPHA'
          i_data     = '123'
        importing
          e_field    = l_dummy-talpha ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals( act = l_dummy-talpha exp = '0000000123' ).

    " Check wrong exit
    clear lo_ex.
    try .
      o->parse_apply_exit(
        exporting
          i_convexit = 'NONAME'
          i_data     = '123'
        importing
          e_field    = l_dummy-talpha ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'EM'.

  endmethod. "parse_apply_exit

endclass.       "lcl_Test_Mockup_Loader
