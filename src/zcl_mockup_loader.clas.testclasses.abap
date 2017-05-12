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

**********************************************************************
* MACRO
**********************************************************************
  define test_parse.
    clear dummy.
    read table lo_struc_descr->components into ls_component with key name = '&1'.
    call method o->parse_field
      exporting
        is_component = ls_component
        i_data       = &2
      importing
        e_field      = dummy-&1.
  end-of-definition.

  define test_parse_positive.
    clear lo_ex.
    try.
      test_parse &1 &2.
    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.
    assert_equals( act = dummy-&1 exp = &3 msg = 'Parse field positive:' && &2 ).
  end-of-definition.

  define test_parse_negative.
    clear lo_ex.
    try.
      test_parse &1 &2.
    catch cx_static_check into lo_ex.
    endtry.
    assert_not_initial( act = lo_ex msg = 'Parse field negative:' && &2 ).
    assert_equals( exp = &3 act = get_excode( lo_ex ) ).
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
    assert_not_initial( act = lo_ex ).
    assert_equals( exp = &1 act = get_excode( lo_ex ) ).
  end-of-definition.


**********************************************************************
* Test Class definition
**********************************************************************

class lcl_test_mockup_loader definition for testing
  duration short
  inheriting from cl_aunit_assert  risk level harmless.

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
      end of ty_dummy.

    types: tt_dummy type table of ty_dummy with default key.

* ================
  private section.
    constants c_tab  like cl_abap_char_utilities=>horizontal_tab value cl_abap_char_utilities=>horizontal_tab.
    constants c_crlf like cl_abap_char_utilities=>cr_lf value cl_abap_char_utilities=>cr_lf.

    data o type ref to zcl_mockup_loader.  "class under test

    class-methods class_setup.
    methods setup.

    methods read_zip                 for testing.
    methods integrated_test          for testing.
    methods source_redirect_test     for testing.
    methods utf16_encoding           for testing.

    methods parse_data               for testing.
    methods parse_field              for testing.
    methods empty_lines              for testing.
    methods map_file_structure       for testing.
    methods range_filtering          for testing.

    methods store_retrieve           for testing.
    methods retrieve_types           for testing.
    methods store_retrieve_with_key  for testing.
    methods store_retrieve_with_where for testing.
    methods load_and_store           for testing.
    methods load_raw                 for testing.
    methods parse_apply_exit         for testing.

    methods get_dummy_data importing i_strict       type abap_bool default abap_true
                           exporting e_dummy_struc  type ty_dummy
                                     e_dummy_tab    type tt_dummy
                                     e_dummy_string type string.

    methods filter_helper  importing i_tab        type tt_dummy
                                     i_filter     type zcl_mockup_loader=>tt_filter
                           returning value(e_tab) type tt_dummy.

    class-methods get_excode importing ix_error type ref to cx_static_check
                             returning value(rv_code) like lcx_error=>code.

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
      fail( quit = class
            msg  = 'Load source is redirected, please reset with ZMOCKUP_LOADER_SWITCH_SOURCE before running the test' ). "#EC NOTEXT
    endif.

    zcl_mockup_loader=>class_set_source( i_type = 'MIME' i_path = 'ZMOCKUP_LOADER_UNIT_TEST' ).
    zcl_mockup_loader=>class_set_params( i_amt_format = '' i_encoding = '4110' ). " fmt = default, enc = utf-8
  endmethod.       "class_setup

  method setup.
    data lo_ex type ref to cx_static_check.

    try.
      o = zcl_mockup_loader=>get_instance( ).
    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.
  endmethod.       "setup

  method get_excode.
    data lo_error type ref to lcx_error.
    lo_error ?= ix_error.
    rv_code   = lo_error->code.
  endmethod.

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
          lo_ex          type ref to cx_static_check.

    get_dummy_data( importing e_dummy_struc = dummy_exp
                              e_dummy_tab   = dummy_tab_exp ).

    " Strict ********************************************************
    try.
      call method o->load_data
        exporting i_obj       = 'testdir/testfile_complete'
        importing e_container = dummy_tab_act.

      assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

      call method o->load_data
        exporting i_obj       = 'testdir/testfile_complete'
        importing e_container = dummy_act.

      assert_equals( act = dummy_act exp = dummy_exp ).

      call method o->load_data " No MANDT field in file
        exporting i_obj       = 'testdir/testfile_no_mandt'
        importing e_container = dummy_tab_act.

      assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    " NOT Strict ****************************************************
    get_dummy_data( exporting i_strict    = abap_false
                    importing e_dummy_tab = dummy_tab_exp ).

    try.
      call method o->load_data
        exporting i_obj       = 'testdir/testfile_no_strict'
                  i_strict    = abap_false
        importing e_container = dummy_tab_act.

      assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    " With where ****************************************************
    get_dummy_data( importing e_dummy_tab   = dummy_tab_exp ).

    read table dummy_tab_exp into dummy_exp with key tnumber = '2016'.
    delete dummy_tab_exp where tnumber <> '2016'.

    try.
      call method o->load_data
        exporting i_obj       = 'testdir/testfile_complete'
                  i_where     = 'TNUMBER = 2016'
        importing e_container = dummy_tab_act.

      assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

      call method o->load_data
        exporting i_obj       = 'testdir/testfile_complete'
                  i_where     = 'TNUMBER = 2016'
        importing e_container = dummy_act.

      assert_equals( act = dummy_act exp = dummy_exp ).

    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    " No container ***************************************************
    try.
      call method o->load_data exporting i_obj = 'testdir/testfile_complete'.
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'NC'.

  endmethod.       " integrated_test

**********************************************************************
* Source redirection via SET/GET parameters test
**********************************************************************
  method source_redirect_test.
    data:
          lo_ex      type ref to cx_static_check,
          l_type     type char4,
          l_path     type char40,
          l_path_tmp type char40.

    get parameter id 'ZMOCKUP_LOADER_SPATH' field l_path_tmp. " Preserve

    l_type = 'MIME'.
    l_path = 'ZMOCKUP_LOADER_WRONG_OBJECT'.
    set parameter id 'ZMOCKUP_LOADER_STYPE' field l_type.
    set parameter id 'ZMOCKUP_LOADER_SPATH' field l_path.
    o->free_instance( ).

    try.
      o = zcl_mockup_loader=>get_instance( ).
    catch cx_static_check into lo_ex.
    endtry.

    clear l_type.
    set parameter id 'ZMOCKUP_LOADER_STYPE' field l_type.
    set parameter id 'ZMOCKUP_LOADER_SPATH' field l_path_tmp.
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
          lo_ex          type ref to cx_static_check.

    get_dummy_data( importing e_dummy_tab   = dummy_tab_exp ).

    try.
      zcl_mockup_loader=>class_set_params( i_amt_format = '' i_encoding = '4103' ). " UTF16
      call method o->load_data
        exporting i_obj       = 'testdir/testfile_complete_utf16'
        importing e_container = dummy_tab_act.
      zcl_mockup_loader=>class_set_params( i_amt_format = '' i_encoding = '4110' ). " Back to SETUP defaults
    catch cx_static_check into lo_ex.
      zcl_mockup_loader=>class_set_params( i_amt_format = '' i_encoding = '4110' ). " Back to SETUP defaults
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

  endmethod. "utf16_encoding

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
          lo_ex          type ref to cx_static_check.

    " Strict parsing *********************************
    get_dummy_data( importing e_dummy_struc  = dummy_exp
                              e_dummy_tab    = dummy_tab_exp
                              e_dummy_string = l_string ).

    try.
      call method o->parse_data
        exporting i_rawdata   = l_string
        importing e_container = dummy_act.

      call method o->parse_data
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act.

    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_equals( act = dummy_act     exp = dummy_exp ).
    assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

    " Parse to sorted and hashed tables ***************
    try.
      call method o->parse_data
        exporting i_rawdata   = l_string
        importing e_container = dummy_stab.
    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_equals( act = dummy_stab exp = dummy_tab_exp ).

    try.
      call method o->parse_data
        exporting i_rawdata   = l_string
        importing e_container = dummy_htab.
    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_equals( act = dummy_htab exp = dummy_tab_exp ).

    " NOT STRICT parsing ******************************
    get_dummy_data( exporting i_strict       = abap_false
                    importing e_dummy_tab    = dummy_tab_exp
                              e_dummy_string = l_string ).

    try.
      call method o->parse_data
        exporting i_rawdata   = l_string
                  i_strict    = ''
        importing e_container = dummy_tab_act.

    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

    " Fields out of bound (more fields than in header) ***
    clear lo_ex.
    get_dummy_data( importing e_dummy_string = l_string ).
    replace first occurrence of '1111' in l_string with '1111' && c_tab && 'EXCESS_FIELD'.

    try.
      call method o->parse_data
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act.
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode '>H'.

    " Fields out of bound (less fields than in header) ***
    clear lo_ex.
    get_dummy_data( importing e_dummy_string = l_string ).
    replace first occurrence of c_tab && '1111' in l_string with ''.

    try.
      call method o->parse_data
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act.
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode '<H'.

    " Parse to field (not table or structure) *************
    clear lo_ex.
    get_dummy_data( importing e_dummy_string = l_string ).

    try.
      call method o->parse_data
        exporting i_rawdata   = l_string
        importing e_container = dummy_val.
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'DT'.

    " Parse empty file ************************************
    clear lo_ex.
    clear l_string.

    try.
      call method o->parse_data
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act.
    catch cx_static_check into lo_ex.
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
          lo_ex          type ref to cx_static_check.

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
    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
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

    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
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
          lo_ex      type ref to cx_static_check.

    assert_not_initial( act = lines( o->o_zip->files ) ).

    " Positive ***************************************
    try.
      call method o->read_zip
        exporting i_name    = 'testdir/testfile_complete.txt'
        importing e_rawdata = l_str.
    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.
    assert_not_initial( act = l_str ).

    " NEGATIVE - wrong file name **********************
    clear lo_ex.
    try.
      call method o->read_zip
        exporting i_name    = 'testdir/wrong_filename.xyz'
        importing e_rawdata = l_str.
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'ZF'.

    " NEGATIVE - wrong code page **********************
    clear lo_ex.
    try.
      call method o->read_zip
        exporting i_name    = 'testdir/testfile_complete_utf16.txt'
        importing e_rawdata = l_str.
    catch cx_static_check into lo_ex.
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
          lo_ex         type ref to cx_static_check.

    get_dummy_data( importing e_dummy_tab    = dummy_tab_exp
                              e_dummy_string = l_string ).

    " Add empty line at the end *****************************
    l_string = l_string && c_crlf.

    try.
      call method o->parse_data
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act.
    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

    " Add empty line in the middle ***************************
    replace first occurrence of c_crlf in l_string with c_crlf && c_crlf.
    clear lo_ex.

    try.
      call method o->parse_data
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act.
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'LE'.

    " Add empty line at the beginning ************************
    l_string = c_crlf && l_string.
    clear lo_ex.

    try.
      call method o->parse_data
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act.
    catch cx_static_check into lo_ex.
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
          lo_ex         type ref to cx_static_check.

    get_dummy_data( importing e_dummy_tab    = dummy_tab_exp
                              e_dummy_string = l_string_bak ).

    " Duplicate field names *******************************
    clear lo_ex. l_string = l_string_bak.
    replace first occurrence of 'TCHAR' in l_string with 'TDATE'.

    try. o->parse_data( exporting i_rawdata   = l_string importing e_container = dummy_tab_act ).
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'DN'.

    " Empty field names ***********************************
    clear lo_ex. l_string = l_string_bak.
    replace first occurrence of 'TCHAR' in l_string with ''.

    try. o->parse_data( exporting i_rawdata   = l_string importing e_container = dummy_tab_act ).
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'EN'.

    " Unknown field in text *******************************
    clear lo_ex. l_string = l_string_bak.
    replace first occurrence of 'TCHAR' in l_string with 'UNKNOWN'.

    try. o->parse_data( exporting i_rawdata   = l_string importing e_container = dummy_tab_act ).
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'MC'.

    " More fields than in target structure ****************
    clear lo_ex. l_string = l_string_bak.
    replace first occurrence of 'TINTEGER' in l_string with 'TINTEGER' && c_tab && 'EXCESS_FIELD'.

    try. o->parse_data( exporting i_rawdata   = l_string importing e_container = dummy_tab_act ).
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'CN'.

    " Empty field at the end ******************************
    clear lo_ex. l_string = l_string_bak.
    replace first occurrence of 'TINTEGER' in l_string with 'TINTEGER' && c_tab.

    try. o->parse_data( exporting i_rawdata   = l_string importing e_container = dummy_tab_act ).
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'EN'.

    " Empty field at the end of data line ******************
    clear lo_ex. l_string = l_string_bak.
    replace first occurrence of '1111' in l_string with '1111' && c_tab.

    try. o->parse_data( exporting i_rawdata   = l_string importing e_container = dummy_tab_act ).
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode '>H'.

  endmethod.       "map_file_structure

**********************************************************************
* STORE tests - basic functionality
**********************************************************************
  method store_retrieve.
    data:
          dummy_exp      type ty_dummy,
          dummy_tab_exp  type tt_dummy,
          dummy_act      type ty_dummy,
          dummy_tab_act  type tt_dummy,
          lo_ex          type ref to cx_static_check.

    get_dummy_data( importing e_dummy_struc = dummy_exp
                              e_dummy_tab   = dummy_tab_exp ).

    " Instance method ********************************
    try.
      o->store( i_name = 'STRUC' i_data = dummy_exp ).
      o->store( i_name = 'TAB'   i_data = dummy_tab_exp ).

      call method o->_retrieve
        exporting i_name  = 'STRUC'
        importing e_data = dummy_act.

      call method o->_retrieve
        exporting i_name  = 'TAB'
        importing e_data = dummy_tab_act.

    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_equals( act = dummy_act      exp = dummy_exp ).
    assert_equals( act = dummy_tab_act  exp = dummy_tab_exp ).

    " Instance method - NEGATIVE **********************
    try.
      call method o->_retrieve
        exporting i_name  = 'NOT_EXISTING'
        importing e_data = dummy_act.
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'NF'.

    " Static method ***********************************
    clear: dummy_act, dummy_tab_act.

    call method zcl_mockup_loader=>retrieve
      exporting i_name  = 'STRUC'
      importing e_data = dummy_act
      exceptions others = 4.
    assert_subrc( act = sy-subrc ).

    call method zcl_mockup_loader=>retrieve
      exporting i_name  = 'TAB'
      importing e_data = dummy_tab_act
      exceptions others = 4.
    assert_subrc( act = sy-subrc ).

    assert_equals( act = dummy_act      exp = dummy_exp ).
    assert_equals( act = dummy_tab_act  exp = dummy_tab_exp ).

    " Static method - NEGATIVE ************************
    call method zcl_mockup_loader=>retrieve
      exporting i_name  = 'NOT_EXISTING'
      importing e_data = dummy_act
      exceptions others = 4.

    assert_subrc(  act = sy-subrc       exp = 4 ).
    assert_equals( act = sy-msgno       exp = 499 ). " SY(499) -> & & & &

    " Purge tests
    try.
      o->store( i_name = 'ANOTHER_STRUC' i_data = dummy_exp ).
    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_equals( act = lines( o->at_store ) exp = 3 ).
    o->purge( 'ANOTHER_STRUC' ).
    assert_equals( act = lines( o->at_store ) exp = 2 ).
    o->purge( '*' ).
    assert_equals( act = lines( o->at_store ) exp = 0 ).

  endmethod.       "store_retrieve

**********************************************************************
* STORE RETRIEVE types checking test
**********************************************************************
  method retrieve_types.
    data:
          lo_ex        type ref to cx_static_check,
          lt_src       type scarr_tab,
          lt_dst_tab   type scarr_tab,
          lt_dst_tt    type table of scarr,
          lt_dst_ts    type sorted table of scarr with unique key carrid,
          l_dst_struc  type scarr,
          lt_dst_diff  type table of sflight.

    append initial line to lt_src.
    try. o->store( i_name = 'DATA' i_data = lt_src ).
    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    " Store to different kinds of same table *******************
    try.
      call method o->_retrieve exporting i_name  = 'DATA' importing e_data = lt_dst_tab.
      call method o->_retrieve exporting i_name  = 'DATA' importing e_data = lt_dst_tt.
      call method o->_retrieve exporting i_name  = 'DATA' importing e_data = lt_dst_ts.
    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    " Store to structure ***************************************
    try. call method o->_retrieve exporting i_name  = 'DATA' importing e_data = l_dst_struc.
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'TT'.

    " Store to table with different structure ******************
    clear lo_ex.
    try. call method o->_retrieve exporting i_name  = 'DATA' importing e_data = lt_dst_diff.
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'TS'.

  endmethod.       "retrieve_types

**********************************************************************
* STORE RETRIEVE with table key and sieving (filtering)
**********************************************************************
  method store_retrieve_with_key.
    data:
          dummy_exp      type ty_dummy,
          dummy_tab_exp  type tt_dummy,
          dummy_act      type ty_dummy,
          dummy_tab_act  type tt_dummy,
          ls_wrong       type sflight,
          lo_ex          type ref to cx_static_check.

    get_dummy_data( importing e_dummy_struc = dummy_exp
                              e_dummy_tab   = dummy_tab_exp ).

    try. o->store( i_name = 'TAB' i_data = dummy_tab_exp i_tabkey = 'TNUMBER' ).
    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    delete dummy_tab_exp where tnumber <> '2016'.

    " Positive *****************************************
    try.
      call method o->_retrieve
        exporting i_name   = 'TAB'
                  i_sift   = '2015'
        importing e_data   = dummy_act.

      call method o->_retrieve
        exporting i_name   = 'TAB'
                  i_sift   = '2016'
        importing e_data   = dummy_tab_act.

    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_equals( act = dummy_act      exp = dummy_exp ).
    assert_equals( act = dummy_tab_act  exp = dummy_tab_exp ).

    " Retrieve to wrong structure **********************
    clear lo_ex.
    try.
      call method o->_retrieve
        exporting i_name   = 'TAB'
                  i_sift   = '2015'
        importing e_data   = ls_wrong.
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'TS'.

    " Retrieve - no data selected **********************
    clear lo_ex.
    try.
      call method o->_retrieve
        exporting i_name   = 'TAB'
                  i_sift   = '2000'
        importing e_data   = dummy_tab_act. " Table
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode '04'.

    clear lo_ex.
    try.
      call method o->_retrieve
        exporting i_name   = 'TAB'
                  i_sift   = '2000'
        importing e_data   = dummy_act.    " Structure
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode '04'.

    " Save structure ************************************
    clear lo_ex.
    try. o->store( i_name = 'STRUC' i_data = dummy_exp i_tabkey = 'TNUMBER' ).
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'TO'.

    " Tab key that does not exist in the structure ******
    clear lo_ex.
    try. o->store( i_name = 'TAB' i_data = dummy_tab_exp i_tabkey = 'UNDEFINED' ).
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'FM'.

  endmethod.       "store_retrieve_with_key

**********************************************************************
* LOAD AND STORE at once
**********************************************************************
  method load_and_store.
    data:
          dummy_tab_exp  type tt_dummy,
          dummy_tab_act  type tt_dummy,
          lo_ex          type ref to cx_static_check.

    get_dummy_data( importing e_dummy_tab   = dummy_tab_exp ).

    " Positive test ************************************
    try.
      call method o->load_and_store
        exporting i_obj       = 'testdir/testfile_complete'
                  i_name      = 'TAB'
                  i_type      = 'LCL_TEST_MOCKUP_LOADER=>TT_DUMMY'.

      call method o->_retrieve
        exporting i_name   = 'TAB'
        importing e_data   = dummy_tab_act.

    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_equals( act = dummy_tab_act  exp = dummy_tab_exp ).

    " Negative: type that not exists ********************
    clear lo_ex.
    try.
      call method o->load_and_store
        exporting i_obj       = 'testdir/testfile_complete'
                  i_name      = 'TAB'
                  i_type      = '************'.

    catch cx_static_check into lo_ex.
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
          lo_ex          type ref to cx_static_check,

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
    try.  o->build_filter( exporting i_where = l_where_err1 importing e_filter = l_filter ).
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'WS'.

    " Component is not a table
    try.  o->build_filter( exporting i_where = l_where_err2 importing e_filter = l_filter ).
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'WS'.

    " Unexpected types of filters
    l_tywhere-name = 'TNUMBER'.
    get reference of l_where into l_tywhere-range.
    try.  o->build_filter( exporting i_where = l_tywhere importing e_filter = l_filter ).
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'CE'.

    " Filter is not range
    l_tywhere-name = 'TNUMBER'.
    get reference of dummy_tab_exp into l_tywhere-range.
    try.  o->build_filter( exporting i_where = l_tywhere importing e_filter = l_filter ).
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'RT'.

    " Filter is not range + TABLE
    l_tywhere-name = 'TNUMBER'.
    get reference of dummy_tab_exp into l_tywhere-range.
    append l_tywhere to lt_tywhere.
    try.  o->build_filter( exporting i_where = lt_tywhere importing e_filter = l_filter ).
    catch cx_static_check into lo_ex.
    endtry.
    clear lt_tywhere.
    assert_excode 'RT'.

    " Wrong type of table
    try.  o->build_filter( exporting i_where = dummy_tab_exp importing e_filter = l_filter ).
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'WT'.

    " Parameter is an unsupported type
    try.  o->build_filter( exporting i_where = lo_ex importing e_filter = l_filter ).
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'UT'.

    " parameter is incorrect string pattern
    try.  o->build_filter( exporting i_where = 'TNUMBER??' importing e_filter = l_filter ).
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'SP'.


    " Positive test RANGE STRUCTURE -----------------------------------------------------
    dummy_tab_exp[] = dummy_tab_src[].
    delete dummy_tab_exp where tnumber <> '2015' or tdate < '20160101'.

    add_range number 'I' 'EQ' '2015'.
    add_range date   'I' 'GE' '20160101'.
    add_range other  'I' 'GE' 'A'.

    try.
      o->build_filter( exporting i_where = l_where importing e_filter = l_filter ).
    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_equals( act = filter_helper( i_tab = dummy_tab_src i_filter = l_filter )
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
      o->build_filter( exporting i_where = lt_tywhere importing e_filter = l_filter ).
    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_equals( act = filter_helper( i_tab = dummy_tab_src i_filter = l_filter )
                   exp = dummy_tab_exp ).

    " Positive test STRING --------------------------------------------------------------
    dummy_tab_exp[] = dummy_tab_src[].
    delete dummy_tab_exp where tnumber <> '2015'.

    try .
      o->build_filter( exporting i_where = 'TNUMBER = 2015' importing e_filter = l_filter ).
    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_equals( act = filter_helper( i_tab = dummy_tab_src i_filter = l_filter )
                   exp = dummy_tab_exp ).

    " Same but with lower case name
    try .
      o->build_filter( exporting i_where = 'TnumBER = 2015' importing e_filter = l_filter ).
    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_equals( act = filter_helper( i_tab = dummy_tab_src i_filter = l_filter )
                   exp = dummy_tab_exp ).


    " Positive test TY_WHERE STRUCTURE --------------------------------------------------
    " REUSE dummy_tab_exp and ranges from above
    l_tywhere-name = 'TNUMBER'.
    get reference of l_where-tnumber into l_tywhere-range.

    try .
      o->build_filter( exporting i_where = l_tywhere importing e_filter = l_filter ).
    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_equals( act = filter_helper( i_tab = dummy_tab_src i_filter = l_filter )
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
          lo_ex      type ref to cx_static_check,
          l_str_exp  type string,
          l_xstr_act type xstring,
          l_str_act  type string,
          lo_conv    type ref to cl_abap_conv_in_ce.

    l_str_exp = '<?xml version="1.0"?><mytag>mydata</mytag>'.

    try. " .XML
      lo_conv = cl_abap_conv_in_ce=>create( encoding = '4110' ).
      o->load_raw( exporting i_obj = 'testdir/test_raw'
                             i_ext = '.xml'
                   importing e_content = l_xstr_act ).
      lo_conv->convert( exporting input = l_xstr_act importing data = l_str_act ).
    catch cx_root into lo_exr.
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_equals( act = l_str_act exp = l_str_exp ).

    try. " .TXT
      o->load_raw( exporting i_obj = 'testdir/test_raw'
                   importing e_content = l_xstr_act ).
      lo_conv->convert( exporting input = l_xstr_act importing data = l_str_act ).
    catch cx_root into lo_exr.
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_equals( act = l_str_act exp = l_str_exp ).

    try. " No container
      o->load_raw( exporting i_obj = 'testdir/test_raw' ).
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'NC'.

  endmethod. "load_raw

**********************************************************************
* PARSE_APPLY_EXIT - apply exit test
**********************************************************************
  method parse_apply_exit.
    data:
          l_dummy  type ty_dummy,
          lo_ex    type ref to cx_static_check.

    try .
      o->parse_apply_exit( exporting i_convexit = 'ALPHA'
                                     i_data     = '123'
                           importing e_field    = l_dummy-talpha ).
    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_equals( act = l_dummy-talpha exp = '0000000123' ).

    " Check wrong exit
    clear lo_ex.
    try .
      o->parse_apply_exit( exporting i_convexit = 'NONAME'
                                     i_data     = '123'
                           importing e_field    = l_dummy-talpha ).
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'EM'.

  endmethod. "parse_apply_exit

**********************************************************************
* STORE_RETRIEVE_WITH_WHERE - New logic with i_where in store
**********************************************************************
  method store_retrieve_with_where.
    data:
          dummy_exp      type ty_dummy,
          dummy_tab_exp  type tt_dummy,
          dummy_act      type ty_dummy,
          lo_ex          type ref to cx_static_check.

    get_dummy_data( importing e_dummy_tab   = dummy_tab_exp ).
    read table dummy_tab_exp into dummy_exp with key tnumber = '2016'.

    " POSITIVE - use i_where
    try .
      o->store( i_name = 'STRUC' i_data = dummy_exp ).
      o->store( i_name = 'TAB'   i_data = dummy_tab_exp ).

      o->_retrieve( exporting i_name = 'TAB' i_where = 'TNUMBER=2016'
                    importing e_data = dummy_act ).
    catch cx_static_check into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.
    assert_equals( act = dummy_act exp = dummy_exp ).

    " NEGATIVE - Pass both filter simultaneously
    clear lo_ex.
    try .
      o->_retrieve( exporting i_name = 'TAB' i_sift = 'X' i_where = 'Y'
                    importing e_data = dummy_act ).
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'WP'.

    " NEGATIVE - Store without tab key
    clear lo_ex.
    try .
      o->_retrieve( exporting i_name = 'TAB' i_sift = 'X'
                    importing e_data = dummy_act ).
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'FM'.

    " NEGATIVE - Filter tables only
    clear lo_ex.
    try .
      o->_retrieve( exporting i_name = 'STRUC' i_where = 'TNUMBER=2016'
                    importing e_data = dummy_act ).
    catch cx_static_check into lo_ex.
    endtry.
    assert_excode 'TO'.


  endmethod. "store_retrieve_with_where

endclass.       "lcl_Test_Mockup_Loader
