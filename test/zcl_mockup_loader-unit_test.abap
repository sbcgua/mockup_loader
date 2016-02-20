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
    test_parse &1 &2.
    assert_equals( act = dummy-&1 exp = &3 msg = 'Parse field positive:' && &2 ).
  end-of-definition.

  define test_parse_negative.
    clear lo_ex.
    try.
      test_parse &1 &2.
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( act = lo_ex msg = 'Parse field negative:' && &2 ).
    assert_equals( exp = 'PF' act = lo_ex->code ).
  end-of-definition.

  define append_dummy.
    e_dummy_struc-tdate    = &1.
    e_dummy_struc-tchar    = &2.
    e_dummy_struc-tstring  = &3.
    e_dummy_struc-tdecimal = &4.
    e_dummy_struc-tnumber  = &5.
    if i_strict = abap_true.
      e_dummy_struc-tinteger = &6.
      e_dummy_struc-talpha   = &7.
    endif.
    append e_dummy_struc to e_dummy_tab.
  end-of-definition.

  define add_range.
    r_&1-sign   = &2.
    r_&1-option = &3.
    r_&1-low    = &4.
    append r_&1 to l_where-t&1.
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
        tstring  type string,
        talpha   type veri_alpha,
        tdecimal type veri_cur13,
        tnumber  type veri_n04,
        tinteger type i,
      end of ty_dummy.

    types:
      tt_dummy type table of ty_dummy.

* ================
  private section.

    data o type ref to zcl_mockup_loader.  "class under test

    class-methods: class_setup.
    methods: setup.

    methods: read_zip for testing.
    methods: integrated_test for testing.
    methods: source_redirect_test for testing.

    methods: parse_data for testing.
    methods: parse_field for testing.
    methods: empty_lines for testing.
    methods: map_file_structure for testing.
    methods: range_filtering for testing.

    methods: store_retrieve for testing.
    methods: retrieve_types for testing.
    methods: store_retrieve_with_key for testing.
    methods: load_and_store for testing.

    methods: get_dummy_data
              importing
                i_strict       type abap_bool default abap_true
              exporting
                e_dummy_struc  type ty_dummy
                e_dummy_tab    type tt_dummy
                e_dummy_string type string.

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
  endmethod.       "class_setup

  method setup.
    data lo_ex type ref to zcx_mockup_loader_error.

    try.
      o = zcl_mockup_loader=>get_instance( ).
    catch zcx_mockup_loader_error into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.
  endmethod.       "setup

**********************************************************************
* Dummy data generation
**********************************************************************
  method get_dummy_data.
    data l_string type string.

    if i_strict = abap_true.
      l_string = 'MANDT\tTDATE\tTCHAR\tTSTRING\tTALPHA\tTDECIMAL\tTNUMBER\tTINTEGER\n'
              && '\t01.01.2015\tTrololo1\tString1\t100000\t1234567,81\t2015\t1111\n'
              && '\t02.01.2016\tTrololo2\tString2\t200000\t1234567,82\t2016\t2222\n'
              && '\t03.01.2016\tTrololo3\tString3\t300000\t1234567,83\t2015\t3333\n' .
    else.
      l_string = 'TDATE\tTSTRING\tTCHAR\tTDECIMAL\tTNUMBER\n'
              && '01.01.2015\tString1\tTrololo1\t1234567,81\t2015\n'
              && '02.01.2016\tString2\tTrololo2\t1234567,82\t2016\n'
              && '03.01.2016\tString3\tTrololo3\t1234567,83\t2015\n' .
    endif.

    replace all occurrences of '\t' in l_string with cl_abap_char_utilities=>horizontal_tab.
    replace all occurrences of '\n' in l_string with cl_abap_char_utilities=>cr_lf.

    clear e_dummy_tab.

    "             TDATE      TCHAR      TSTRING   TDECIMAL    TNUM TINT  TALPHA
    append_dummy '20150101' 'Trololo1' 'String1' '1234567.81' 2015 1111 '0000100000'.
    append_dummy '20160102' 'Trololo2' 'String2' '1234567.82' 2016 2222 '0000200000'.
    append_dummy '20160103' 'Trololo3' 'String3' '1234567.83' 2015 3333 '0000300000'.

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

    call method get_dummy_data
      importing
        e_dummy_struc = dummy_exp
        e_dummy_tab   = dummy_tab_exp.

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

    catch zcx_mockup_loader_error into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    " NOT Strict ****************************************************
    call method get_dummy_data
      exporting i_strict    = abap_false
      importing e_dummy_tab = dummy_tab_exp.

    try.
      call method o->load_data
        exporting i_obj       = 'testdir/testfile_no_strict'
                  i_strict    = abap_false
        importing e_container = dummy_tab_act.

      assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

    catch zcx_mockup_loader_error into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    " With where ****************************************************
    data:
          begin of l_where,
            tnumber  type range of veri_n04,
          end of l_where,
          r_num like line of l_where-tnumber.

    call method get_dummy_data
      importing e_dummy_tab   = dummy_tab_exp.

    read table dummy_tab_exp into dummy_exp with key tnumber = '2016'.
    delete dummy_tab_exp where tnumber <> '2016'.

    r_num-sign   = 'I'.
    r_num-option = 'EQ'.
    r_num-low    = '2016'.
    append r_num to l_where-tnumber.

    try.
      call method o->load_data
        exporting i_obj       = 'testdir/testfile_complete'
                  i_where     = l_where
        importing e_container = dummy_tab_act.

      assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

      call method o->load_data
        exporting i_obj       = 'testdir/testfile_complete'
                  i_where     = l_where
        importing e_container = dummy_act.

      assert_equals( act = dummy_act exp = dummy_exp ).

    catch zcx_mockup_loader_error into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

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

    l_type = 'MIME'.
    l_path = 'ZMOCKUP_LOADER_WRONG_OBJECT'.
    set parameter id 'ZMOCKUP_LOADER_STYPE' field l_type.
    set parameter id 'ZMOCKUP_LOADER_SPATH' field l_path.

    try.
      o->free_instance( ).
      o = zcl_mockup_loader=>get_instance( ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    clear l_type.
    set parameter id 'ZMOCKUP_LOADER_STYPE' field l_type.
    set parameter id 'ZMOCKUP_LOADER_SPATH' field l_path_tmp.

    assert_not_initial( lo_ex ).
    assert_equals( exp = 'RE' act = lo_ex->code ).

  endmethod.       " source_redirect_test

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
    call method get_dummy_data
      importing
        e_dummy_struc  = dummy_exp
        e_dummy_tab    = dummy_tab_exp
        e_dummy_string = l_string.

    try.
      call method o->parse_data
        exporting i_rawdata   = l_string
        importing e_container = dummy_act.

      call method o->parse_data
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act.

    catch zcx_mockup_loader_error into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_equals( act = dummy_act     exp = dummy_exp ).
    assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

    " Parse to sorted and hashed tables ***************
    try.
      call method o->parse_data
        exporting i_rawdata   = l_string
        importing e_container = dummy_stab.
    catch zcx_mockup_loader_error into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_equals( act = dummy_stab exp = dummy_tab_exp ).

    try.
      call method o->parse_data
        exporting i_rawdata   = l_string
        importing e_container = dummy_htab.
    catch zcx_mockup_loader_error into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_equals( act = dummy_htab exp = dummy_tab_exp ).

    " NOT STRICT parsing ******************************
    call method get_dummy_data
      exporting
        i_strict       = abap_false
      importing
        e_dummy_tab    = dummy_tab_exp
        e_dummy_string = l_string.

    try.
      call method o->parse_data
        exporting i_rawdata   = l_string
                  i_strict    = ''
        importing e_container = dummy_tab_act.

    catch zcx_mockup_loader_error into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

    " Fields out of bound (more fields than in header) ***
    clear lo_ex.
    call method get_dummy_data importing e_dummy_string = l_string.
    replace first occurrence of '1111' in l_string
      with '1111' && cl_abap_char_utilities=>horizontal_tab && 'EXCESS_FIELD'.

    try.
      call method o->parse_data
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act.
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( lo_ex ).
    assert_equals( exp = '>H' act = lo_ex->code ).

    " Fields out of bound (less fields than in header) ***
    clear lo_ex.
    call method get_dummy_data importing e_dummy_string = l_string.
    replace first occurrence of cl_abap_char_utilities=>horizontal_tab && '1111'
      in l_string with ''.

    try.
      call method o->parse_data
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act.
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( lo_ex ).
    assert_equals( exp = '<H' act = lo_ex->code ).

    " Parse to field (not table or structure) *************
    clear lo_ex.
    call method get_dummy_data importing e_dummy_string = l_string.

    try.
      call method o->parse_data
        exporting i_rawdata   = l_string
        importing e_container = dummy_val.
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( lo_ex ).
    assert_equals( exp = 'DT' act = lo_ex->code ).

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
      test_parse_positive TCHAR    'ABC'             'ABC'.
      test_parse_positive TSTRING  'The string test' 'The string test'.
      test_parse_positive TALPHA   '100000'          '0000100000'.
      test_parse_positive TNUMBER  '2015'            '2015'.
      test_parse_positive TINTEGER '123'             123.
    catch zcx_mockup_loader_error into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    " Negative tests ******************************

    test_parse_negative TDATE    '01.012015'.
    test_parse_negative TNUMBER  '20ha'.

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
      fail( lo_ex->get_text( ) ).
    endtry.

    zcl_mockup_loader=>class_set_params( i_amt_format = '' ). " Set defaults
    test_parse_negative TDECIMAL '1 234.12'.
    test_parse_negative TDECIMAL '1 234_12'.
    test_parse_negative TDECIMAL '1234,123'. " 3 decimal digits into amount which has just 2
    test_parse_negative TDECIMAL '1234,12_'.
    test_parse_negative TDECIMAL 'Not-a-number'.
    zcl_mockup_loader=>class_set_params( i_amt_format = '.,' ).
    test_parse_negative TDECIMAL '1 234.12'.
    test_parse_negative TDECIMAL '1,234.12'.
    zcl_mockup_loader=>class_set_params( i_amt_format = ',.' ).
    test_parse_negative TDECIMAL '1 234,12'.
    test_parse_negative TDECIMAL '1.234,12'.

    zcl_mockup_loader=>class_set_params( i_amt_format = '' ). " Set defaults back


  endmethod.       "parse_Field

**********************************************************************
* ZIP file read test - gets data from SMW0 ZMOCKUP_LOADER_UNIT_TEST
**********************************************************************
  method read_zip.
    data:
          l_filename type string,
          l_str      type string,
          lo_ex      type ref to zcx_mockup_loader_error.

    assert_not_initial( act = lines( o->o_zip->files ) ).

    l_filename = 'testdir/testfile_complete.txt'.

    " Positive ***************************************
    try.
      call method o->read_zip
        exporting i_name    = l_filename
        importing e_rawdata = l_str.
    catch zcx_mockup_loader_error into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_not_initial( act = l_str ).

    " Negative ***************************************
    l_filename = l_filename && 'XYZ'.
    try.
      call method o->read_zip
        exporting i_name    = l_filename
        importing e_rawdata = l_str.
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( lo_ex ).
    assert_equals( exp = 'ZF' act = lo_ex->code ).

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

    call method get_dummy_data
      importing
        e_dummy_tab    = dummy_tab_exp
        e_dummy_string = l_string.

    " Add empty line at the end *****************************
    l_string = l_string && cl_abap_char_utilities=>cr_lf.

    try.
      call method o->parse_data
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act.
    catch zcx_mockup_loader_error into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_equals( act = dummy_tab_act exp = dummy_tab_exp ).

    " Add empty line in the middle ***************************
    replace first occurrence of cl_abap_char_utilities=>cr_lf in l_string
      with cl_abap_char_utilities=>cr_lf && cl_abap_char_utilities=>cr_lf.
    clear lo_ex.

    try.
      call method o->parse_data
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act.
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( lo_ex ).
    assert_equals( exp = 'LE' act = lo_ex->code ).

    " Add empty line at the beginning ************************
    l_string = cl_abap_char_utilities=>cr_lf && l_string.
    clear lo_ex.

    try.
      call method o->parse_data
        exporting i_rawdata   = l_string
        importing e_container = dummy_tab_act.
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( lo_ex ).
    assert_equals( exp = 'HE' act = lo_ex->code ).

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

    call method get_dummy_data
      importing
        e_dummy_tab    = dummy_tab_exp
        e_dummy_string = l_string_bak.

    " Duplicate field names *******************************
    clear lo_ex. l_string = l_string_bak.
    replace first occurrence of 'TCHAR' in l_string with 'TDATE'.

    try. o->parse_data( exporting i_rawdata   = l_string importing e_container = dummy_tab_act ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( lo_ex ).
    assert_equals( exp = 'DN' act = lo_ex->code ).

    " Empty field names ***********************************
    clear lo_ex. l_string = l_string_bak.
    replace first occurrence of 'TCHAR' in l_string with ''.

    try. o->parse_data( exporting i_rawdata   = l_string importing e_container = dummy_tab_act ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( lo_ex ).
    assert_equals( exp = 'EN' act = lo_ex->code ).

    " Unknown field in text *******************************
    clear lo_ex. l_string = l_string_bak.
    replace first occurrence of 'TCHAR' in l_string with 'UNKNOWN'.

    try. o->parse_data( exporting i_rawdata   = l_string importing e_container = dummy_tab_act ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( lo_ex ).
    assert_equals( exp = 'MC' act = lo_ex->code ).

    " More fields than in target structure ****************
    clear lo_ex. l_string = l_string_bak.
    replace first occurrence of 'TINTEGER' in l_string
      with 'TINTEGER' && cl_abap_char_utilities=>horizontal_tab && 'EXCESS_FIELD'.

    try. o->parse_data( exporting i_rawdata   = l_string importing e_container = dummy_tab_act ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( lo_ex ).
    assert_equals( exp = 'CN' act = lo_ex->code ).

    " Empty field at the end ******************************
    clear lo_ex. l_string = l_string_bak.
    replace first occurrence of 'TINTEGER' in l_string
      with 'TINTEGER' && cl_abap_char_utilities=>horizontal_tab.

    try. o->parse_data( exporting i_rawdata   = l_string importing e_container = dummy_tab_act ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( lo_ex ).
    assert_equals( exp = 'EN' act = lo_ex->code ).

    " Empty field at the end of data line ******************
    clear lo_ex. l_string = l_string_bak.
    replace first occurrence of '1111' in l_string
      with '1111' && cl_abap_char_utilities=>horizontal_tab.

    try. o->parse_data( exporting i_rawdata   = l_string importing e_container = dummy_tab_act ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( lo_ex ).
    assert_equals( exp = '>H' act = lo_ex->code ).


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
          lo_ex          type ref to zcx_mockup_loader_error.

    call method get_dummy_data
      importing
        e_dummy_struc = dummy_exp
        e_dummy_tab   = dummy_tab_exp.

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

    catch zcx_mockup_loader_error into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    assert_equals( act = dummy_act      exp = dummy_exp ).
    assert_equals( act = dummy_tab_act  exp = dummy_tab_exp ).

    " Instance method - NEGATIVE **********************
    try.
      call method o->_retrieve
        exporting i_name  = 'NOT_EXISTING'
        importing e_data = dummy_act.
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( act = lo_ex ).
    assert_equals( exp = 'NF' act = lo_ex->code ).

    " Static method ***********************************
    clear: dummy_act, dummy_tab_act.

    call method zcl_mockup_loader=>retrieve
      exporting i_name  = 'STRUC'
      importing e_data = dummy_act
      exceptions others = 4.

    call method zcl_mockup_loader=>retrieve
      exporting i_name  = 'TAB'
      importing e_data = dummy_tab_act
      exceptions others = 4.

    assert_subrc(  act = sy-subrc ).
    assert_equals( act = dummy_act      exp = dummy_exp ).
    assert_equals( act = dummy_tab_act  exp = dummy_tab_exp ).

    " Static method - NEGATIVE ************************
    call method zcl_mockup_loader=>retrieve
      exporting i_name  = 'NOT_EXISTING'
      importing e_data = dummy_act
      exceptions others = 4.

    assert_subrc(  act = sy-subrc       exp = 4 ).
    assert_equals( act = sy-msgno       exp = 499 ). " SY(499) -> & & & &

  endmethod.       "store_retrieve

**********************************************************************
* STORE RETRIEVE types checking test
**********************************************************************
  method retrieve_types.
    data:
          lo_ex        type ref to zcx_mockup_loader_error,
          lt_src       type scarr_tab,
          lt_dst_tab   type scarr_tab,
          lt_dst_tt    type table of scarr,
          lt_dst_ts    type sorted table of scarr with unique key carrid,
          l_dst_struc  type scarr,
          lt_dst_diff  type table of sflight.

    append initial line to lt_src.
    try. o->store( i_name = 'DATA' i_data = lt_src ).
    catch zcx_mockup_loader_error into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    " Store to different kinds of same table *******************
    try.
      call method o->_retrieve exporting i_name  = 'DATA' importing e_data = lt_dst_tab.
      call method o->_retrieve exporting i_name  = 'DATA' importing e_data = lt_dst_tt.
      call method o->_retrieve exporting i_name  = 'DATA' importing e_data = lt_dst_ts.
    catch zcx_mockup_loader_error into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    " Store to structure ***************************************
    try. call method o->_retrieve exporting i_name  = 'DATA' importing e_data = l_dst_struc.
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( act = lo_ex ).
    assert_equals( exp = 'TT' act = lo_ex->code ).

    " Store to table with different structure ******************
    clear lo_ex.
    try. call method o->_retrieve exporting i_name  = 'DATA' importing e_data = lt_dst_diff.
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( act = lo_ex ).
    assert_equals( exp = 'TS' act = lo_ex->code ).

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
          lo_ex          type ref to zcx_mockup_loader_error.

    call method get_dummy_data
      importing
        e_dummy_struc = dummy_exp
        e_dummy_tab   = dummy_tab_exp.

    try. o->store( i_name = 'TAB' i_data = dummy_tab_exp i_tabkey = 'TNUMBER' ).
    catch zcx_mockup_loader_error into lo_ex.
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

    catch zcx_mockup_loader_error into lo_ex.
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
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( act = lo_ex ).
    assert_equals( exp = 'TS' act = lo_ex->code ).

    " Retrieve - no data selected **********************
    clear lo_ex.
    try.
      call method o->_retrieve
        exporting i_name   = 'TAB'
                  i_sift   = '2000'
        importing e_data   = dummy_tab_act. " Table
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( act = lo_ex ).
    assert_equals( exp = '04' act = lo_ex->code ).

    clear lo_ex.
    try.
      call method o->_retrieve
        exporting i_name   = 'TAB'
                  i_sift   = '2000'
        importing e_data   = dummy_act.    " Structure
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( act = lo_ex ).
    assert_equals( exp = '04' act = lo_ex->code ).


    " Save structure ************************************
    clear lo_ex.
    try. o->store( i_name = 'STRUC' i_data = dummy_exp i_tabkey = 'TNUMBER' ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( act = lo_ex ).
    assert_equals( exp = 'TO' act = lo_ex->code ).

    " Tab key that does not exist in the structure ******
    clear lo_ex.
    try. o->store( i_name = 'TAB' i_data = dummy_tab_exp i_tabkey = 'UNDEFINED' ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( act = lo_ex ).
    assert_equals( exp = 'FM' act = lo_ex->code ).

  endmethod.       "store_retrieve_with_key

**********************************************************************
* LOAD AND STORE at once
**********************************************************************
  method load_and_store.
    data:
          dummy_tab_exp  type tt_dummy,
          dummy_tab_act  type tt_dummy,
          lo_ex          type ref to zcx_mockup_loader_error.

    call method get_dummy_data
      importing e_dummy_tab   = dummy_tab_exp.

    " Positive test ************************************

    try.
      call method o->load_and_store
        exporting i_obj       = 'testdir/testfile_complete'
                  i_name      = 'TAB'
                  i_type      = 'LCL_TEST_MOCKUP_LOADER=>TT_DUMMY'.

      call method o->_retrieve
        exporting i_name   = 'TAB'
        importing e_data   = dummy_tab_act.

    catch zcx_mockup_loader_error into lo_ex.
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

    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( act = lo_ex ).
    assert_equals( exp = 'WT' act = lo_ex->code ).

  endmethod.       "load_and_store

**********************************************************************
* RANGE_FILTERING - test build_filter and does_line_fit_filter
**********************************************************************
  method range_filtering.
    data:
          dummy_tab_src  type tt_dummy,
          dummy_tab_act  type tt_dummy,
          dummy_tab_exp  type tt_dummy,
          dummy          type ty_dummy,

          l_filter       type zcl_mockup_loader=>tt_filter,
          lo_ex          type ref to zcx_mockup_loader_error,

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

    " Negative test ---------------------------------------------------------------------
    try . " Component is not a range table
      o->build_filter( exporting i_where = l_where_err1 importing e_filter = l_filter ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( act = lo_ex ).
    assert_equals( exp = 'RO' act = lo_ex->code ).

    try . " Component is not a table
      o->build_filter( exporting i_where = l_where_err2 importing e_filter = l_filter ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( act = lo_ex ).
    assert_equals( exp = 'RO' act = lo_ex->code ).

    try . " Parameter is not a structure
      o->build_filter( exporting i_where = l_filter importing e_filter = l_filter ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.

    assert_not_initial( act = lo_ex ).
    assert_equals( exp = 'CE' act = lo_ex->code ).


    " Positive test ---------------------------------------------------------------------
    call method get_dummy_data importing e_dummy_tab = dummy_tab_src.

    dummy_tab_exp[] = dummy_tab_src[].
    delete dummy_tab_exp where tnumber <> '2015' or tdate < '20160101'.

    add_range number 'I' 'EQ' '2015'.
    add_range date   'I' 'GE' '20160101'.
    add_range other  'I' 'GE' 'A'.

    try .
      o->build_filter( exporting i_where = l_where importing e_filter = l_filter ).
    catch zcx_mockup_loader_error into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    loop at dummy_tab_src into dummy.
      if o->does_line_fit_filter( i_line = dummy i_filter = l_filter ) = abap_true.
        append dummy to dummy_tab_act.
      endif.
    endloop.

    assert_equals( exp = dummy_tab_exp  act = dummy_tab_act ).

  endmethod.       "range_filtering

endclass.       "lcl_Test_Mockup_Loader
