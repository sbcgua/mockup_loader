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
define append_dummy.
  e_dummy_struc-tdate    = &1.
  e_dummy_struc-tchar    = &2.
  e_dummy_struc-tstring  = &3.
  e_dummy_struc-tdecimal = &4.
  e_dummy_struc-tnumber  = &5.
  e_dummy_struc-traw     = &6.
  e_dummy_struc-tinteger = &7.
  e_dummy_struc-talpha   = &8.
  append e_dummy_struc to e_dummy_tab.
end-of-definition.

define assert_excode.
  cl_abap_unit_assert=>assert_not_initial( act = lo_ex ).
  cl_abap_unit_assert=>assert_equals( exp = &1 act = lo_ex->code ).
end-of-definition.

**********************************************************************
* Test Class definition
**********************************************************************

class ltcl_test_mockup_store definition for testing
  duration short
  risk level harmless.

* ================
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

* ================
  private section.
    data o type ref to zcl_mockup_loader_store.  "class under test

    methods setup.
    methods store_retrieve            for testing.
    methods retrieve_types            for testing.
    methods store_retrieve_with_key   for testing.
    methods store_retrieve_with_where for testing.
    methods load_and_store            for testing raising zcx_mockup_loader_error.

    methods get_dummy_data
      exporting
        e_dummy_struc type ty_dummy
        e_dummy_tab   type tt_dummy.

endclass.       "lcl_test_mockup_loader

* Friends
class zcl_mockup_loader_store definition local friends ltcl_test_mockup_store.

**********************************************************************
* Implementation
**********************************************************************

class ltcl_test_mockup_store implementation.

**********************************************************************
* Setup methods
**********************************************************************
  method setup.
    data lo_ex type ref to zcx_mockup_loader_error.

    try.
      zcl_mockup_loader_store=>free_instance( ).
      o = zcl_mockup_loader_store=>get_instance( ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.
  endmethod.       "setup

**********************************************************************
* Dummy data generation
**********************************************************************
  method get_dummy_data.
    clear e_dummy_tab.

    "             TDATE      TCHAR      TSTRING   TDECIMAL    TNUM TRAW  TINT  TALPHA
    append_dummy '20150101' 'Trololo1' 'String1' '1234567.81' 2015 '8A'  1111 '0000100000'.
    append_dummy '20160102' 'Trololo2' 'String2' '1234567.82' 2016 '8B'  2222 '0000200000'.
    append_dummy '20160103' 'Trololo3' 'String3' '1234567.83' 2015 '8C'  3333 '0000300000'.

    read table e_dummy_tab into e_dummy_struc index 1.

  endmethod.       " get_dummy_data

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

    get_dummy_data(
      importing
        e_dummy_struc = dummy_exp
        e_dummy_tab   = dummy_tab_exp ).

    " Instance method ********************************
    try.
      o->store( i_name = 'STRUC' i_data = dummy_exp ).
      o->store( i_name = 'TAB'   i_data = dummy_tab_exp ).

      o->_retrieve(
        exporting i_name  = 'STRUC'
        importing e_data = dummy_act ).

      o->_retrieve(
        exporting i_name  = 'TAB'
        importing e_data = dummy_tab_act ).

    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals( act = dummy_act      exp = dummy_exp ).
    cl_abap_unit_assert=>assert_equals( act = dummy_tab_act  exp = dummy_tab_exp ).

    " Instance method - NEGATIVE **********************
    try.
      o->_retrieve(
        exporting i_name = 'NOT_EXISTING'
        importing e_data = dummy_act ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'NF'.

    " Static method ***********************************
    clear: dummy_act, dummy_tab_act.

    zcl_mockup_loader_store=>retrieve(
      exporting i_name  = 'STRUC'
      importing e_data = dummy_act
      exceptions others = 4 ).
    cl_abap_unit_assert=>assert_subrc( act = sy-subrc ).

    zcl_mockup_loader_store=>retrieve(
      exporting i_name  = 'TAB'
      importing e_data = dummy_tab_act
      exceptions others = 4 ).
    cl_abap_unit_assert=>assert_subrc( act = sy-subrc ).

    cl_abap_unit_assert=>assert_equals( act = dummy_act      exp = dummy_exp ).
    cl_abap_unit_assert=>assert_equals( act = dummy_tab_act  exp = dummy_tab_exp ).

    " Static method - NEGATIVE ************************
    zcl_mockup_loader_store=>retrieve(
      exporting i_name  = 'NOT_EXISTING'
      importing e_data = dummy_act
      exceptions others = 4 ).

    cl_abap_unit_assert=>assert_subrc(  act = sy-subrc       exp = 4 ).
    cl_abap_unit_assert=>assert_equals( act = sy-msgno       exp = 499 ). " SY(499) -> & & & &

    " Purge tests
    try.
      o->store( i_name = 'ANOTHER_STRUC' i_data = dummy_exp ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals( act = lines( o->mt_store ) exp = 3 ).
    o->purge( 'ANOTHER_STRUC' ).
    cl_abap_unit_assert=>assert_equals( act = lines( o->mt_store ) exp = 2 ).
    o->purge( '*' ).
    cl_abap_unit_assert=>assert_equals( act = lines( o->mt_store ) exp = 0 ).

  endmethod.       "store_retrieve

**********************************************************************
* STORE RETRIEVE types checking test
**********************************************************************
  method retrieve_types.
    data:
          lo_ex        type ref to zcx_mockup_loader_error,
          lt_src       type standard table of scarr,
          lt_dst_tab   type standard table of scarr,
          lt_dst_tt    type table of scarr,
          lt_dst_ts    type sorted table of scarr with unique key carrid,
          l_dst_struc  type scarr,
          lt_dst_diff  type table of sflight.

    append initial line to lt_src.
    try.
      o->store( i_name = 'DATA' i_data = lt_src ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    " Store to different kinds of same table *******************
    try.
      o->_retrieve( exporting i_name  = 'DATA' importing e_data = lt_dst_tab ).
      o->_retrieve( exporting i_name  = 'DATA' importing e_data = lt_dst_tt ).
      o->_retrieve( exporting i_name  = 'DATA' importing e_data = lt_dst_ts ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    " Store to structure ***************************************
    try.
      o->_retrieve( exporting i_name  = 'DATA' importing e_data = l_dst_struc ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'TT'.

    " Store to table with different structure ******************
    clear lo_ex.
    try.
      o->_retrieve( exporting i_name  = 'DATA' importing e_data = lt_dst_diff ).
    catch zcx_mockup_loader_error into lo_ex.
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
          lo_ex          type ref to zcx_mockup_loader_error.

    get_dummy_data(
      importing
        e_dummy_struc = dummy_exp
        e_dummy_tab   = dummy_tab_exp ).

    try.
      o->store( i_name = 'TAB' i_data = dummy_tab_exp i_tabkey = 'TNUMBER' ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    delete dummy_tab_exp where tnumber <> '2016'.

    " Positive *****************************************
    try.
      o->_retrieve(
        exporting
          i_name   = 'TAB'
          i_sift   = '2015'
        importing
          e_data   = dummy_act ).

      o->_retrieve(
        exporting
          i_name   = 'TAB'
          i_sift   = '2016'
        importing
          e_data   = dummy_tab_act ).

    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals( act = dummy_act      exp = dummy_exp ).
    cl_abap_unit_assert=>assert_equals( act = dummy_tab_act  exp = dummy_tab_exp ).

    " Retrieve to wrong structure **********************
    clear lo_ex.
    try.
      o->_retrieve(
        exporting
          i_name   = 'TAB'
          i_sift   = '2015'
        importing
          e_data   = ls_wrong ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'TS'.

    " Retrieve - no data selected **********************
    clear lo_ex.
    try.
      o->_retrieve(
        exporting
          i_name   = 'TAB'
          i_sift   = '2000'
        importing
          e_data   = dummy_tab_act ). " Table
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode '04'.

    clear lo_ex.
    try.
      o->_retrieve(
        exporting
          i_name   = 'TAB'
          i_sift   = '2000'
        importing
          e_data   = dummy_act ).    " Structure
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode '04'.

    " Save structure ************************************
    clear lo_ex.
    try.
      o->store( i_name = 'STRUC' i_data = dummy_exp i_tabkey = 'TNUMBER' ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'TO'.

    " Tab key that does not exist in the structure ******
    clear lo_ex.
    try.
      o->store( i_name = 'TAB' i_data = dummy_tab_exp i_tabkey = 'UNDEFINED' ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'FM'.

  endmethod.       "store_retrieve_with_key

**********************************************************************
* STORE_RETRIEVE_WITH_WHERE - New logic with i_where in store
**********************************************************************
  method store_retrieve_with_where.
    data:
          dummy_exp      type ty_dummy,
          dummy_tab_exp  type tt_dummy,
          dummy_act      type ty_dummy,
          lo_ex          type ref to zcx_mockup_loader_error.

    get_dummy_data( importing e_dummy_tab = dummy_tab_exp ).
    read table dummy_tab_exp into dummy_exp with key tnumber = '2016'.

    " POSITIVE - use i_where
    try .
      o->store( i_name = 'STRUC' i_data = dummy_exp ).
      o->store( i_name = 'TAB'   i_data = dummy_tab_exp ).

      o->_retrieve(
        exporting
          i_name = 'TAB'
          i_where = 'TNUMBER=2016'
        importing
          e_data = dummy_act ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.
    cl_abap_unit_assert=>assert_equals( act = dummy_act exp = dummy_exp ).

    " NEGATIVE - Pass both filter simultaneously
    clear lo_ex.
    try .
      o->_retrieve(
        exporting
          i_name = 'TAB'
          i_sift = 'X' i_where = 'Y'
        importing
          e_data = dummy_act ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'WP'.

    " NEGATIVE - Store without tab key
    clear lo_ex.
    try .
      o->_retrieve(
        exporting
          i_name = 'TAB'
          i_sift = 'X'
        importing
          e_data = dummy_act ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'FM'.

    " NEGATIVE - Filter tables only
    clear lo_ex.
    try .
      o->_retrieve(
        exporting
          i_name = 'STRUC'
          i_where = 'TNUMBER=2016'
        importing
          e_data = dummy_act ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'TO'.

  endmethod. "store_retrieve_with_where

**********************************************************************
* LOAD AND STORE at once
**********************************************************************
  method load_and_store.
    data:
          lo_type_desc   type ref to cl_abap_typedescr,
          dummy_tab_exp  type tt_dummy,
          dummy_tab_act  type tt_dummy,
          lo_ml          type ref to zcl_mockup_loader,
          lo_ex          type ref to zcx_mockup_loader_error.

    lo_ml = zcl_mockup_loader=>create(
      i_type       = 'MIME'
      i_path       = 'ZMOCKUP_LOADER_UNIT_TEST'
      i_amt_format = ''
      i_encoding   = zif_mockup_loader_constants=>encoding_utf8 ).

    get_dummy_data( importing e_dummy_tab   = dummy_tab_exp ).

    lo_type_desc = cl_abap_typedescr=>describe_by_name( 'LTCL_TEST_MOCKUP_STORE=>TT_DUMMY' ).

    " Positive test ************************************
    try.
      o->load_and_store(
        io_ml       = lo_ml
        i_obj       = 'testdir/testfile_complete'
        i_name      = 'TAB'
        i_type      = 'LTCL_TEST_MOCKUP_STORE=>TT_DUMMY' ).

      zcl_mockup_loader_store=>retrieve(
        exporting i_name   = 'TAB'
        importing e_data   = dummy_tab_act ).
      cl_abap_unit_assert=>assert_equals( act = dummy_tab_act  exp = dummy_tab_exp ).

      o->load_and_store(
        io_ml       = lo_ml
        i_obj       = 'testdir/testfile_complete'
        i_name      = 'TAB'
        i_type_desc = lo_type_desc ).
      clear dummy_tab_act.
      zcl_mockup_loader_store=>retrieve(
        exporting i_name   = 'TAB'
        importing e_data   = dummy_tab_act ).
      cl_abap_unit_assert=>assert_equals( act = dummy_tab_act  exp = dummy_tab_exp ).

    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.


    " Negative: type that not exists ********************
    clear lo_ex.
    try.
      o->load_and_store(
        io_ml       = lo_ml
        i_obj       = 'testdir/testfile_complete'
        i_name      = 'TAB'
        i_type      = '************' ).

    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'WT'.

    " Negative: one type descriptor only ****************
    clear lo_ex.
    try.
      o->load_and_store(
        io_ml       = lo_ml
        i_obj       = 'testdir/testfile_complete'
        i_name      = 'TAB'
        i_type_desc = lo_type_desc
        i_type      = '************' ).

    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'TD'.


  endmethod.       "load_and_store


endclass.
