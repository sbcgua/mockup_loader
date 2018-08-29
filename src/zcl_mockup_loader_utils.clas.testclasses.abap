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

class lcl_test_mockup_utils definition for testing
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

    methods setup.
    methods range_filtering          for testing.

    methods get_dummy_data
      importing
        i_strict       type abap_bool default abap_true
      exporting
        e_dummy_struc  type ty_dummy
        e_dummy_tab    type tt_dummy.

    methods filter_helper
      importing
        i_tab        type tt_dummy
        i_filter     type zcl_mockup_loader_utils=>tt_filter
      returning value(e_tab) type tt_dummy.

    methods filter_table for testing.
    methods filter_table_neg for testing.
    methods does_line_fit_filter for testing.

    methods build_filter_with_value for testing.
    methods build_filter for testing.

endclass.       "lcl_test_mockup_loader

**********************************************************************
* Implementation
**********************************************************************

class lcl_test_mockup_utils implementation.

**********************************************************************
* Setup methods
**********************************************************************
  method setup.
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
* RANGE_FILTERING - test build_filter and does_line_fit_filter
**********************************************************************
  method range_filtering.
    data:
          dummy_tab_src  type tt_dummy,
          dummy_tab_exp  type tt_dummy,
          dummy          type ty_dummy,

          l_filter       type zcl_mockup_loader_utils=>tt_filter,
          lo_ex          type ref to zcx_mockup_loader_error,

          lt_tywhere     type zcl_mockup_loader_utils=>tt_where,
          l_tywhere      type zcl_mockup_loader_utils=>ty_where,

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
    append initial line to l_where_err1-tnumber.
    append initial line to l_where_err2-tnumber.
    append initial line to dummy_tab_exp.

    " Negative tests --------------------------------------------------------------------

    " Component is not a range table
    try.
      l_filter = zcl_mockup_loader_utils=>build_filter( i_where = l_where_err1 ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'WS'.

    " Component is not a table
    try.
      l_filter = zcl_mockup_loader_utils=>build_filter( i_where = l_where_err2 ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'WS'.

    " Unexpected types of filters
    l_tywhere-name = 'TNUMBER'.
    get reference of l_where into l_tywhere-range.
    try.
      l_filter = zcl_mockup_loader_utils=>build_filter( i_where = l_tywhere ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'CE'.

    " Filter is not range
    l_tywhere-name = 'TNUMBER'.
    get reference of dummy_tab_exp into l_tywhere-range.
    try.
      l_filter = zcl_mockup_loader_utils=>build_filter( i_where = l_tywhere ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'RT'.

    " Filter is not range + TABLE
    l_tywhere-name = 'TNUMBER'.
    get reference of dummy_tab_exp into l_tywhere-range.
    append l_tywhere to lt_tywhere.
    try.
      l_filter = zcl_mockup_loader_utils=>build_filter( i_where = lt_tywhere ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    clear lt_tywhere.
    assert_excode 'RT'.

    " Wrong type of table
    try.
      l_filter = zcl_mockup_loader_utils=>build_filter( i_where = dummy_tab_exp ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'WT'.

    " Parameter is an unsupported type
    try.
      l_filter = zcl_mockup_loader_utils=>build_filter( i_where = lo_ex ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'UT'.

    " parameter is incorrect string pattern
    try.
      l_filter = zcl_mockup_loader_utils=>build_filter( i_where = 'TNUMBER??' ).
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
      l_filter = zcl_mockup_loader_utils=>build_filter( i_where = l_where ).
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
      l_filter = zcl_mockup_loader_utils=>build_filter( i_where = lt_tywhere ).
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
      l_filter = zcl_mockup_loader_utils=>build_filter( i_where = 'TNUMBER = 2015' ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals(
      act = filter_helper( i_tab = dummy_tab_src i_filter = l_filter )
      exp = dummy_tab_exp ).

    " Same but with lower case name
    try .
      l_filter = zcl_mockup_loader_utils=>build_filter( i_where = 'TnumBER = 2015' ).
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
      l_filter = zcl_mockup_loader_utils=>build_filter( i_where = l_tywhere ).
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
      if zcl_mockup_loader_utils=>does_line_fit_filter( i_line = dummy i_filter = i_filter ) = abap_true.
        append dummy to e_tab.
      endif.
    endloop.
  endmethod.       " filter_helper

  method filter_table.
    data:
          dummy_tab_src type tt_dummy,
          lt_act        type tt_dummy,
          lt_exp        type tt_dummy,
          ls_act        type ty_dummy,
          ls_exp        type ty_dummy,
          l_str         type string,
          lt_filter     type zcl_mockup_loader_utils=>tt_filter,
          lo_ex         type ref to zcx_mockup_loader_error.

    field-symbols <f> like line of lt_filter.

    get_dummy_data( importing e_dummy_tab = dummy_tab_src ).

    " Filter table
    l_str = '2015'.
    append initial line to lt_filter assigning <f>.
    <f>-name = 'TNUMBER'.
    <f>-type = 'S'.
    get reference of l_str into <f>-valref.

    try .
      zcl_mockup_loader_utils=>filter_table(
        exporting
          i_filter    = lt_filter
          i_tab       = dummy_tab_src
        importing
          e_container = lt_act ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    cl_abap_unit_assert=>assert_equals( act = lines( lt_act ) exp = 2 ).
    read table dummy_tab_src into ls_exp index 1.
    read table lt_act into ls_act index 1.
    cl_abap_unit_assert=>assert_equals( act = ls_act exp = ls_exp ).
    read table dummy_tab_src into ls_exp index 3.
    read table lt_act into ls_act index 2.
    cl_abap_unit_assert=>assert_equals( act = ls_act exp = ls_exp ).

    " Filter structure
    l_str = '2016'. " second line !
    try .
      zcl_mockup_loader_utils=>filter_table(
        exporting
          i_filter    = lt_filter
          i_tab       = dummy_tab_src
        importing
          e_container = ls_act ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.
    read table dummy_tab_src into ls_exp index 2.
    cl_abap_unit_assert=>assert_equals( act = ls_act exp = ls_exp ).

    " Filter with i_where (second line !)
    try .
      zcl_mockup_loader_utils=>filter_table(
        exporting
          i_where     = 'TNUMBER=2016'
          i_tab       = dummy_tab_src
        importing
          e_container = ls_act ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.
    read table dummy_tab_src into ls_exp index 2.
    cl_abap_unit_assert=>assert_equals( act = ls_act exp = ls_exp ).


  endmethod.  " filter_table.

  method filter_table_neg.
    data:
          dummy_tab_src type tt_dummy,
          ls_act        type ty_dummy,
          lt_filter     type zcl_mockup_loader_utils=>tt_filter,
          ls_filter     like line of lt_filter,
          lo_ex         type ref to zcx_mockup_loader_error.

    clear lo_ex.
    try . " Different table line types
      zcl_mockup_loader_utils=>filter_table(
        exporting
          i_tab       = dummy_tab_src
          i_filter    = lt_filter
        importing
          e_container = lt_filter ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'LT'.

    clear lo_ex.
    try . " Different struc line types
      zcl_mockup_loader_utils=>filter_table(
        exporting
          i_tab       = dummy_tab_src
          i_filter    = lt_filter
        importing
          e_container = ls_filter ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'LT'.

    clear lo_ex.
    try . " Wrong container type
      zcl_mockup_loader_utils=>filter_table(
        exporting
          i_tab       = dummy_tab_src
          i_filter    = lt_filter
        importing
          e_container = lo_ex ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'WT'.

    clear lo_ex.
    try . " Wrong container type
      zcl_mockup_loader_utils=>filter_table(
        exporting
          i_tab       = dummy_tab_src
          i_filter    = lt_filter
          i_where     = 'a=b'
        importing
          e_container = lo_ex ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'OO'.

  endmethod.  " filter_table_neg.

  method does_line_fit_filter.
    data:
          lv_fit    type abap_bool,
          ls_line   type ty_dummy,
          lt_filter type zcl_mockup_loader_utils=>tt_filter,
          l_str     type string.

    data:
          begin of l_where,
            tnumber  type range of veri_n04,
            tdate    type range of datum,
          end of l_where,

          r_number  like line of l_where-tnumber,
          r_date    like line of l_where-tdate.


    field-symbols <f> like line of lt_filter.

    get_dummy_data( importing e_dummy_struc = ls_line ).

    " Filter by string
    append initial line to lt_filter assigning <f>.
    <f>-name = 'TNUMBER'.
    <f>-type = 'S'.
    get reference of l_str into <f>-valref.
    l_str = '2015'.

    ls_line-tnumber = 2015.
    lv_fit = zcl_mockup_loader_utils=>does_line_fit_filter( i_line = ls_line i_filter = lt_filter ).
    cl_abap_unit_assert=>assert_true( lv_fit ).

    ls_line-tnumber = 2016.
    lv_fit = zcl_mockup_loader_utils=>does_line_fit_filter( i_line = ls_line i_filter = lt_filter ).
    cl_abap_unit_assert=>assert_false( lv_fit ).

    " Filter by range
    clear lt_filter.
    add_range number 'I' 'EQ' '2015'.
    add_range date   'I' 'GE' '20160101'.

    append initial line to lt_filter assigning <f>.
    <f>-name = 'TNUMBER'.
    <f>-type = 'R'.
    get reference of l_where-tnumber into <f>-valref.

    append initial line to lt_filter assigning <f>.
    <f>-name = 'TDATE'.
    <f>-type = 'R'.
    get reference of l_where-tdate into <f>-valref.

    ls_line-tnumber = 2015.
    ls_line-tdate   = '20160101'.
    lv_fit = zcl_mockup_loader_utils=>does_line_fit_filter( i_line = ls_line i_filter = lt_filter ).
    cl_abap_unit_assert=>assert_true( lv_fit ).

    ls_line-tnumber = 2015.
    ls_line-tdate   = '20160108'.
    lv_fit = zcl_mockup_loader_utils=>does_line_fit_filter( i_line = ls_line i_filter = lt_filter ).
    cl_abap_unit_assert=>assert_true( lv_fit ).

    ls_line-tnumber = 2015.
    ls_line-tdate   = '20150101'.
    lv_fit = zcl_mockup_loader_utils=>does_line_fit_filter( i_line = ls_line i_filter = lt_filter ).
    cl_abap_unit_assert=>assert_false( lv_fit ).

    ls_line-tnumber = 2016.
    ls_line-tdate   = '20160108'.
    lv_fit = zcl_mockup_loader_utils=>does_line_fit_filter( i_line = ls_line i_filter = lt_filter ).
    cl_abap_unit_assert=>assert_false( lv_fit ).

  endmethod.  " does_line_fit_filter.

  method build_filter_with_value.

    data:
          l_val         type string,
          lt_filter     type zcl_mockup_loader_utils=>tt_filter,
          ls_filter     like line of lt_filter,
          lo_ex         type ref to zcx_mockup_loader_error.

    clear lo_ex.
    try . " i_where not string
      lt_filter = zcl_mockup_loader_utils=>build_filter(
          i_where        = 123
          i_single_value = '234' ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'PN'.

    clear lo_ex.
    try . " i_value - elementary type
      lt_filter = zcl_mockup_loader_utils=>build_filter(
          i_where        = '123'
          i_single_value = lt_filter ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'ET'.

    clear lo_ex.
    try . " positive test
      l_val = 'dummy'.
      lt_filter = zcl_mockup_loader_utils=>build_filter(
          i_where        = 'fld'
          i_single_value = l_val ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( ).
    endtry.

    cl_abap_unit_assert=>assert_equals( act = lines( lt_filter ) exp = 1 ).
    read table lt_filter into ls_filter index 1.
    cl_abap_unit_assert=>assert_equals( act = ls_filter-name exp = 'FLD' ).
    cl_abap_unit_assert=>assert_equals( act = ls_filter-type exp = 'V' ).
    cl_abap_unit_assert=>assert_bound( act = ls_filter-valref ).
    l_val = 'something else'.
    field-symbols <val> type string.
    assign ls_filter-valref->* to <val>.
    cl_abap_unit_assert=>assert_equals( act = <val> exp = 'dummy' ).

  endmethod.  " build_filter_with_value.

  method build_filter.

    data:
          lt_filter     type zcl_mockup_loader_utils=>tt_filter,
          lt_filter_act type zcl_mockup_loader_utils=>tt_filter,
          ls_filter     like line of lt_filter,
          lo_ex         type ref to zcx_mockup_loader_error.

    ls_filter-name = 'AAA'.
    ls_filter-type = 'V'.
    append ls_filter to lt_filter.

    clear lo_ex.
    try . " tt_filter pass through
      lt_filter_act = zcl_mockup_loader_utils=>build_filter( i_where = lt_filter ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( ).
    endtry.

    cl_abap_unit_assert=>assert_equals( act = lt_filter_act exp = lt_filter ).



  endmethod.  " build_filter.

endclass.
