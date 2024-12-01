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

class ltcl_test_mockup_utils definition for testing
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
        tnumber  type n length 4,
        _another type i,
      end of ty_dummy_corresponding,
      tt_dummy_corresponding type standard table of ty_dummy_corresponding with default key.

  private section.

    methods range_filtering for testing raising zcx_mockup_loader_error.

    methods get_dummy_data
      importing
        i_strict       type abap_bool default abap_true
      exporting
        value(e_dummy_struc) type ty_dummy
        value(e_dummy_tab)   type tt_dummy.

    methods filter_helper
      importing
        i_tab        type tt_dummy
        i_filter     type zif_mockup_loader=>tt_filter
      returning
        value(r_tab) type tt_dummy.

    methods assert_filter_equals
      importing
        i_act type zif_mockup_loader=>ty_filter
        i_exp type zif_mockup_loader=>ty_filter.

    methods assert_filter_tab_equals
      importing
        i_act type zif_mockup_loader=>tt_filter
        i_exp type zif_mockup_loader=>tt_filter.

    methods filter_table for testing raising zcx_mockup_loader_error.
    methods filter_table_neg for testing raising zcx_mockup_loader_error.
    methods filter_table_corresponding for testing raising zcx_mockup_loader_error.
    methods filter_full_copy for testing raising zcx_mockup_loader_error.
    methods does_line_fit_filter for testing raising zcx_mockup_loader_error.

    methods build_filter_with_value for testing raising zcx_mockup_loader_error.
    methods build_filter for testing raising zcx_mockup_loader_error.
    methods build_filter_w_nc for testing raising zcx_mockup_loader_error.

    methods conv_single_val_to_filter for testing raising zcx_mockup_loader_error .
    methods conv_string_to_filter     for testing raising zcx_mockup_loader_error .
    methods conv_where_to_filter      for testing raising zcx_mockup_loader_error .
    methods conv_nc_struc_to_filter   for testing raising zcx_mockup_loader_error .
    methods conv_range_to_filter      for testing raising zcx_mockup_loader_error .

    methods test_and                  for testing raising zcx_mockup_loader_error .

endclass.

**********************************************************************
* Implementation
**********************************************************************

class ltcl_test_mockup_utils implementation.

**********************************************************************
* Dummy data generation
**********************************************************************
  method get_dummy_data.
    "             TDATE      TCHAR      TSTRING   TDECIMAL    TNUM TRAW  TINT  TALPHA
    append_dummy '20150101' 'Trololo1' 'String1' '1234567.81' 2015 '8A'  1111 '0000100000'.
    append_dummy '20160102' 'Trololo2' 'String2' '1234567.82' 2016 '8B'  2222 '0000200000'.
    append_dummy '20160103' 'Trololo3' 'String3' '1234567.83' 2015 '8C'  3333 '0000300000'.

    read table e_dummy_tab into e_dummy_struc index 1.
  endmethod.


**********************************************************************
* RANGE_FILTERING - test build_filter and does_line_fit_filter
**********************************************************************
  method range_filtering.
    data:
      dummy_tab_src  type tt_dummy,
      dummy_tab_exp  type tt_dummy,
      dummy          type ty_dummy,

      l_filter       type zif_mockup_loader=>tt_filter,
      lo_ex          type ref to zcx_mockup_loader_error,

      lt_tywhere     type zif_mockup_loader=>tt_where,
      l_tywhere      type zif_mockup_loader=>ty_where,

      begin of l_where_err1,
        tnumber  type range of ty_dummy-tnumber,
        tdate    type range of datum,
        tother   type tt_dummy,
      end of l_where_err1,

      begin of l_where_err2,
        tnumber  type range of ty_dummy-tnumber,
        tdate    type range of datum,
        tother   type ty_dummy,
      end of l_where_err2,

      begin of l_where,
        tnumber  type range of ty_dummy-tnumber,
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
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lo_ex.
      assert_excode 'WS'.
    endtry.

    " Component is not a table
    try.
      l_filter = zcl_mockup_loader_utils=>build_filter( i_where = l_where_err2 ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lo_ex.
      assert_excode 'WS'.
    endtry.

    " Unexpected types of filters
    l_tywhere-name = 'TNUMBER'.
    get reference of l_where into l_tywhere-range.
    try.
      l_filter = zcl_mockup_loader_utils=>build_filter( i_where = l_tywhere ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lo_ex.
      assert_excode 'RT'.
    endtry.

    " Filter is not range
    l_tywhere-name = 'TNUMBER'.
    get reference of dummy_tab_exp into l_tywhere-range.
    try.
      l_filter = zcl_mockup_loader_utils=>build_filter( i_where = l_tywhere ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lo_ex.
      assert_excode 'RT'.
    endtry.

    " Filter is not range + TABLE
    l_tywhere-name = 'TNUMBER'.
    get reference of dummy_tab_exp into l_tywhere-range.
    append l_tywhere to lt_tywhere.
    try.
      l_filter = zcl_mockup_loader_utils=>build_filter( i_where = lt_tywhere ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lo_ex.
      assert_excode 'RT'.
    endtry.
    clear lt_tywhere.

    " Wrong type of table
    try.
      l_filter = zcl_mockup_loader_utils=>build_filter( i_where = dummy_tab_exp ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lo_ex.
      assert_excode 'WT'.
    endtry.

    " Parameter is an unsupported type
    try.
      l_filter = zcl_mockup_loader_utils=>build_filter( i_where = lo_ex ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lo_ex.
      assert_excode 'UT'.
    endtry.

    " parameter is incorrect string pattern
    try.
      l_filter = zcl_mockup_loader_utils=>build_filter( i_where = 'TNUMBER??' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lo_ex.
      assert_excode 'SP'.
    endtry.


    " Positive test RANGE STRUCTURE -----------------------------------------------------
    dummy_tab_exp[] = dummy_tab_src[].
    delete dummy_tab_exp where tnumber <> '2015' or tdate < '20160101'.

    add_range number 'I' 'EQ' '2015'.
    add_range date   'I' 'GE' '20160101'.
    add_range other  'I' 'GE' 'A'.

    l_filter = zcl_mockup_loader_utils=>build_filter( i_where = l_where ).
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

    l_filter = zcl_mockup_loader_utils=>build_filter( i_where = lt_tywhere ).
    cl_abap_unit_assert=>assert_equals(
      act = filter_helper( i_tab = dummy_tab_src i_filter = l_filter )
      exp = dummy_tab_exp ).

    " Positive test STRING --------------------------------------------------------------
    dummy_tab_exp[] = dummy_tab_src[].
    delete dummy_tab_exp where tnumber <> '2015'.

    l_filter = zcl_mockup_loader_utils=>build_filter( i_where = 'TNUMBER = 2015' ).
    cl_abap_unit_assert=>assert_equals(
      act = filter_helper( i_tab = dummy_tab_src i_filter = l_filter )
      exp = dummy_tab_exp ).

    " Same but with lower case name
    l_filter = zcl_mockup_loader_utils=>build_filter( i_where = 'TnumBER = 2015' ).
    cl_abap_unit_assert=>assert_equals(
      act = filter_helper( i_tab = dummy_tab_src i_filter = l_filter )
      exp = dummy_tab_exp ).

    " Positive test TY_WHERE STRUCTURE --------------------------------------------------
    " REUSE dummy_tab_exp and ranges from above
    l_tywhere-name = 'TNUMBER'.
    get reference of l_where-tnumber into l_tywhere-range.

    l_filter = zcl_mockup_loader_utils=>build_filter( i_where = l_tywhere ).
    cl_abap_unit_assert=>assert_equals(
      act = filter_helper( i_tab = dummy_tab_src i_filter = l_filter )
      exp = dummy_tab_exp ).

  endmethod.

  method filter_helper.
    data dummy type ty_dummy.
    loop at i_tab into dummy.
      if zcl_mockup_loader_utils=>does_line_fit_filter( i_line = dummy i_filter = i_filter ) = abap_true.
        append dummy to r_tab.
      endif.
    endloop.
  endmethod.

  method filter_table.
    data:
      dummy_tab_src type tt_dummy,
      lt_act        type tt_dummy,
      lt_exp        type tt_dummy,
      ls_act        type ty_dummy,
      ls_exp        type ty_dummy,
      l_str         type string,
      lt_filter     type zif_mockup_loader=>tt_filter,
      lo_ex         type ref to zcx_mockup_loader_error.

    field-symbols <f> like line of lt_filter.

    get_dummy_data( importing e_dummy_tab = dummy_tab_src ).

    " Filter table
    l_str = '2015'.
    append initial line to lt_filter assigning <f>.
    <f>-name = 'TNUMBER'.
    <f>-type = 'V'.
    get reference of l_str into <f>-valref.

    zcl_mockup_loader_utils=>filter_table(
      exporting
        i_filter    = lt_filter
        i_tab       = dummy_tab_src
      importing
        e_container = lt_act ).

    cl_abap_unit_assert=>assert_equals( act = lines( lt_act ) exp = 2 ).
    read table dummy_tab_src into ls_exp index 1.
    read table lt_act into ls_act index 1.
    cl_abap_unit_assert=>assert_equals( act = ls_act exp = ls_exp ).
    read table dummy_tab_src into ls_exp index 3.
    read table lt_act into ls_act index 2.
    cl_abap_unit_assert=>assert_equals( act = ls_act exp = ls_exp ).

    " Filter structure
    l_str = '2016'. " second line !
    zcl_mockup_loader_utils=>filter_table(
      exporting
        i_filter    = lt_filter
        i_tab       = dummy_tab_src
      importing
        e_container = ls_act ).

    read table dummy_tab_src into ls_exp index 2.
    cl_abap_unit_assert=>assert_equals( act = ls_act exp = ls_exp ).

    " Filter with i_where (second line !)
    zcl_mockup_loader_utils=>filter_table(
      exporting
        i_where     = 'TNUMBER=2016'
        i_tab       = dummy_tab_src
      importing
        e_container = ls_act ).

    read table dummy_tab_src into ls_exp index 2.
    cl_abap_unit_assert=>assert_equals( act = ls_act exp = ls_exp ).

  endmethod.

  method filter_table_neg.
    data:
      dummy_tab_src type tt_dummy,
      ls_act        type ty_dummy,
      lt_filter     type zif_mockup_loader=>tt_filter,
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
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lo_ex.
      assert_excode 'LT'.
    endtry.

    try . " Different struc line types
      zcl_mockup_loader_utils=>filter_table(
        exporting
          i_tab       = dummy_tab_src
          i_filter    = lt_filter
        importing
          e_container = ls_filter ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lo_ex.
      assert_excode 'LT'.
    endtry.

    try . " Wrong container type
      zcl_mockup_loader_utils=>filter_table(
        exporting
          i_tab       = dummy_tab_src
          i_filter    = lt_filter
        importing
          e_container = lo_ex ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lo_ex.
      assert_excode 'WT'.
    endtry.

    clear lo_ex.
    try . " Wrong container type
      zcl_mockup_loader_utils=>filter_table(
        exporting
          i_tab       = dummy_tab_src
          i_filter    = lt_filter
          i_where     = 'a=b'
        importing
          e_container = lo_ex ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lo_ex.
      assert_excode 'OO'.
    endtry.

  endmethod.

  method does_line_fit_filter.
    data:
      lv_fit    type abap_bool,
      ls_line   type ty_dummy,
      lt_filter type zif_mockup_loader=>tt_filter,
      l_str     type string.

    data:
      begin of l_where,
        tnumber  type range of ty_dummy-tnumber,
        tdate    type range of datum,
      end of l_where,

      r_number  like line of l_where-tnumber,
      r_date    like line of l_where-tdate.


    field-symbols <f> like line of lt_filter.

    get_dummy_data( importing e_dummy_struc = ls_line ).

    " Filter by string
    append initial line to lt_filter assigning <f>.
    <f>-name = 'TNUMBER'.
    <f>-type = 'V'.
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

  endmethod.

  method build_filter_with_value.

    data:
      l_val         type string,
      lt_filter     type zif_mockup_loader=>tt_filter,
      ls_filter     like line of lt_filter,
      lo_ex         type ref to zcx_mockup_loader_error.

    try . " i_where not string
      lt_filter = zcl_mockup_loader_utils=>build_filter(
          i_where        = 123
          i_single_value = '234' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lo_ex.
      assert_excode 'PN'.
    endtry.

    try . " i_value - elementary type
      lt_filter = zcl_mockup_loader_utils=>build_filter(
          i_where        = '123'
          i_single_value = lt_filter ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lo_ex.
      assert_excode 'ET'.
    endtry.

    " positive test
    l_val = 'dummy'.
    lt_filter = zcl_mockup_loader_utils=>build_filter(
        i_where        = 'fld'
        i_single_value = l_val ).

    cl_abap_unit_assert=>assert_equals( act = lines( lt_filter ) exp = 1 ).
    read table lt_filter into ls_filter index 1.
    cl_abap_unit_assert=>assert_equals( act = ls_filter-name exp = 'FLD' ).
    cl_abap_unit_assert=>assert_equals( act = ls_filter-type exp = 'V' ).
    cl_abap_unit_assert=>assert_bound( act = ls_filter-valref ).
    l_val = 'something else'.
    field-symbols <val> type string.
    assign ls_filter-valref->* to <val>.
    cl_abap_unit_assert=>assert_equals( act = <val> exp = 'dummy' ).

  endmethod.

  method build_filter.

    data:
      lt_filter     type zif_mockup_loader=>tt_filter,
      lt_filter_act type zif_mockup_loader=>tt_filter,
      ls_filter     like line of lt_filter,
      lo_ex         type ref to zcx_mockup_loader_error.

    ls_filter-name = 'AAA'.
    ls_filter-type = 'V'.
    append ls_filter to lt_filter.

    " tt_filter pass through
    lt_filter_act = zcl_mockup_loader_utils=>build_filter( i_where = lt_filter ).
    cl_abap_unit_assert=>assert_equals( act = lt_filter_act exp = lt_filter ).

    " ty_filter only
    lt_filter_act = zcl_mockup_loader_utils=>build_filter( i_where = ls_filter ).
    cl_abap_unit_assert=>assert_equals( act = lt_filter_act exp = lt_filter ).

  endmethod.

  method build_filter_w_nc.

    data:
      begin of ls_filter,
        a type c length 10,
        b type d,
      end of ls_filter.

    data lt_filter_exp type zif_mockup_loader=>tt_filter.
    data lt_filter_act type zif_mockup_loader=>tt_filter.
    data ls_val_copy like ls_filter.
    field-symbols <f> like line of lt_filter_exp.

    ls_filter-a = 'A123'.
    ls_filter-b = '20210701'.
*    ls_val_copy = ls_filter.

    append initial line to lt_filter_exp assigning <f>.
    <f>-name = 'A'.
    <f>-type = 'V'.
    get reference of ls_filter-a into <f>-valref.

    append initial line to lt_filter_exp assigning <f>.
    <f>-name = 'B'.
    <f>-type = 'V'.
    get reference of ls_filter-b into <f>-valref.

    lt_filter_act = zcl_mockup_loader_utils=>build_filter( ls_filter ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_filter_act
      exp = lt_filter_exp ).

  endmethod.

  method assert_filter_equals.
    field-symbols <act> type any.
    field-symbols <exp> type any.
    cl_abap_unit_assert=>assert_equals( act = i_act-name exp = i_exp-name ).
    cl_abap_unit_assert=>assert_equals( act = i_act-type exp = i_exp-type ).
    assign i_act-valref->* to <act>.
    assign i_exp-valref->* to <exp>.
    cl_abap_unit_assert=>assert_equals( act = <act> exp = <exp> ).
  endmethod.

  method assert_filter_tab_equals.
    field-symbols <act> type any.
    field-symbols <exp> type any.
    cl_abap_unit_assert=>assert_equals( act = lines( i_act ) exp = lines( i_exp ) ).
    loop at i_act assigning <act>.
      read table i_exp index sy-tabix assigning <exp>.
    endloop.
    assert_filter_equals( i_act = <act> i_exp = <exp> ).
  endmethod.

  method conv_single_val_to_filter.

    data ls_filter_act type zif_mockup_loader=>ty_filter.
    data ls_filter_exp type zif_mockup_loader=>ty_filter.

    data lv_str type string value 'efg'.
    ls_filter_exp-name = 'ABC'.
    ls_filter_exp-type = zif_mockup_loader=>c_filter_type-value.
    get reference of lv_str into ls_filter_exp-valref.

    ls_filter_act = zcl_mockup_loader_utils=>conv_single_val_to_filter(
      i_where = 'abc'
      i_value = |efg| ).

    assert_filter_equals(
      i_act = ls_filter_act
      i_exp = ls_filter_exp ).

    " Negative
    data lx type ref to zcx_mockup_loader_error.
    try .
      zcl_mockup_loader_utils=>conv_single_val_to_filter(
        i_where = 'abc'
        i_value = ls_filter_act ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'ET' ).
    endtry.

  endmethod.

  method conv_string_to_filter.

    data ls_filter_act type zif_mockup_loader=>ty_filter.
    data ls_filter_exp type zif_mockup_loader=>ty_filter.

    data lv_str type string value 'efg'.
    ls_filter_exp-name = 'ABC'.
    ls_filter_exp-type = zif_mockup_loader=>c_filter_type-value.
    get reference of lv_str into ls_filter_exp-valref.

    ls_filter_act = zcl_mockup_loader_utils=>conv_string_to_filter( 'abc = efg' ).

    assert_filter_equals(
      i_act = ls_filter_act
      i_exp = ls_filter_exp ).

    " Negative
    data lx type ref to zcx_mockup_loader_error.
    try .
      zcl_mockup_loader_utils=>conv_string_to_filter( 'abcefg' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'SP' ).
    endtry.

    try .
      zcl_mockup_loader_utils=>conv_string_to_filter( '=abcefg' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'SP' ).
    endtry.

  endmethod.

  method conv_where_to_filter.

    data ls_filter_act type zif_mockup_loader=>ty_filter.
    data ls_filter_exp type zif_mockup_loader=>ty_filter.

    data lr_i type range of ty_dummy-tnumber.
    field-symbols <r> like line of lr_i.
    append initial line to lr_i assigning <r>.
    <r>-option = 'EQ'.

    ls_filter_exp-name = 'ABC'.
    ls_filter_exp-type = zif_mockup_loader=>c_filter_type-range.
    get reference of lr_i into ls_filter_exp-valref.

    data ls_where type zif_mockup_loader=>ty_where.
    ls_where-name = 'abc'.
    get reference of lr_i into ls_where-range.

    ls_filter_act = zcl_mockup_loader_utils=>conv_where_to_filter( ls_where ).

    assert_filter_equals(
      i_act = ls_filter_act
      i_exp = ls_filter_exp ).

    " Negative
    data lx type ref to zcx_mockup_loader_error.
    data lt_dummy_tab type string_table.
    try .
      get reference of lt_dummy_tab into ls_where-range.
      zcl_mockup_loader_utils=>conv_where_to_filter( ls_where ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'RT' ).
    endtry.

  endmethod.

  method conv_nc_struc_to_filter.

    data:
      begin of ls_where,
        num  type range of ty_dummy-tnumber,
        char type range of c,
      end of ls_where.

    field-symbols <n> like line of ls_where-num.
    append initial line to ls_where-num assigning <n>.
    <n>-option = 'EQ'.
    field-symbols <c> like line of ls_where-char.
    append initial line to ls_where-char assigning <c>.
    <c>-option = 'EQ'.

    data lt_filter_act type zif_mockup_loader=>tt_filter.
    data lt_filter_exp type zif_mockup_loader=>tt_filter.
    field-symbols <f> like line of lt_filter_exp.

    append initial line to lt_filter_exp assigning <f>.
    <f>-name = 'NUM'.
    <f>-type = zif_mockup_loader=>c_filter_type-range.
    get reference of ls_where-num into <f>-valref.

    append initial line to lt_filter_exp assigning <f>.
    <f>-name = 'CHAR'.
    <f>-type = zif_mockup_loader=>c_filter_type-range.
    get reference of ls_where-char into <f>-valref.

    data lo_type type ref to cl_abap_structdescr.
    lo_type ?= cl_abap_typedescr=>describe_by_data( ls_where ).
    lt_filter_act = zcl_mockup_loader_utils=>conv_nc_struc_to_filter(
      id_struc = lo_type
      i_where  = ls_where ).

    assert_filter_tab_equals(
      i_act = lt_filter_act
      i_exp = lt_filter_exp ).

    " Negative
    data:
      lx type ref to zcx_mockup_loader_error,
      begin of ls_where_err1,
        number  type range of ty_dummy-tnumber,
        other   type tt_dummy,
      end of ls_where_err1,
      begin of ls_where_err2,
        number  type range of ty_dummy-tnumber,
        other   type ty_dummy,
      end of ls_where_err2.

    try .
      zcl_mockup_loader_utils=>conv_nc_struc_to_filter( ls_where_err1 ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'WS' ).
    endtry.

    try .
      zcl_mockup_loader_utils=>conv_nc_struc_to_filter( ls_where_err2 ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'WS' ).
    endtry.

  endmethod.

  method filter_table_corresponding.

    data:
      dummy_tab_src type tt_dummy,
      lv_filter_val type string,
      lt_act        type tt_dummy_corresponding,
      lt_exp        type tt_dummy_corresponding,
      ls_act        type ty_dummy_corresponding,
      ls_exp        type ty_dummy_corresponding,
      lt_filter     type zif_mockup_loader=>tt_filter.

    field-symbols <src> like line of dummy_tab_src.
    field-symbols <exp> like line of lt_exp.
    get_dummy_data( importing e_dummy_tab = dummy_tab_src ).
    loop at dummy_tab_src assigning <src>.
      check <src>-tnumber = '2015'.
      append initial line to lt_exp assigning <exp>.
      move-corresponding <src> to <exp>.
    endloop.
    read table lt_exp into ls_exp index 1.

    " Filter table
    field-symbols <f> like line of lt_filter.
    append initial line to lt_filter assigning <f>.
    <f>-name = 'TNUMBER'.
    <f>-type = 'V'.
    get reference of lv_filter_val into <f>-valref.
    lv_filter_val = '2015'.

    zcl_mockup_loader_utils=>filter_table(
      exporting
        i_filter    = lt_filter
        i_tab       = dummy_tab_src
        i_corresponding = abap_true
      importing
        e_container = lt_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

    zcl_mockup_loader_utils=>filter_table(
      exporting
        i_filter    = lt_filter
        i_tab       = dummy_tab_src
        i_corresponding = abap_true
      importing
        e_container = ls_act ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_act
      exp = ls_exp ).

  endmethod.

  method filter_full_copy.

    data:
      dummy_tab_src type tt_dummy,
      lt_act        type tt_dummy,
      lt_exp        type tt_dummy,
      lt_act_corr   type tt_dummy_corresponding,
      lt_exp_corr   type tt_dummy_corresponding,
      lt_filter     type zif_mockup_loader=>tt_filter. " empty

    field-symbols <src> like line of dummy_tab_src.
    field-symbols <exp> like line of lt_exp_corr.
    get_dummy_data( importing e_dummy_tab = dummy_tab_src ).
    lt_exp = dummy_tab_src.
    loop at dummy_tab_src assigning <src>.
      append initial line to lt_exp_corr assigning <exp>.
      move-corresponding <src> to <exp>.
    endloop.

    zcl_mockup_loader_utils=>filter_table(
      exporting
        i_filter        = lt_filter
        i_tab           = dummy_tab_src
        i_corresponding = abap_false
      importing
        e_container = lt_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

    zcl_mockup_loader_utils=>filter_table(
      exporting
        i_filter        = lt_filter
        i_tab           = dummy_tab_src
        i_corresponding = abap_true
      importing
        e_container = lt_act_corr ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act_corr
      exp = lt_exp_corr ).

  endmethod.

  method conv_range_to_filter.

    data ls_filter_act type zif_mockup_loader=>ty_filter.
    data ls_filter_exp type zif_mockup_loader=>ty_filter.

    data lr_i type range of ty_dummy-tnumber.
    field-symbols <r> like line of lr_i.
    append initial line to lr_i assigning <r>.
    <r>-option = 'EQ'.

    ls_filter_exp-name = 'ABC'.
    ls_filter_exp-type = zif_mockup_loader=>c_filter_type-range.
    get reference of lr_i into ls_filter_exp-valref.

    ls_filter_act = zcl_mockup_loader_utils=>conv_range_to_filter(
      i_where = 'abc'
      i_range = lr_i ).

    assert_filter_equals(
      i_act = ls_filter_act
      i_exp = ls_filter_exp ).

    " Negative
    data lx type ref to zcx_mockup_loader_error.
    data lt_dummy_tab type string_table.
    try .
      ls_filter_act = zcl_mockup_loader_utils=>conv_range_to_filter(
        i_where = 'abc'
        i_range = lt_dummy_tab ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'ER' ).
    endtry.

  endmethod.

  method test_and.

    data lt_filter_act type zif_mockup_loader=>tt_filter.
    data ls_filter like line of lt_filter_act.
    field-symbols <v> type any.

    lt_filter_act = zcl_mockup_loader_utils=>and(
      i_op1 = 'A = B'
      i_op2 = zcl_mockup_loader_utils=>conv_single_val_to_filter( i_where = 'XYZ' i_value = 1 ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_filter_act )
      exp = 2 ).

    read table lt_filter_act into ls_filter index 1.
    cl_abap_unit_assert=>assert_equals(
      act = ls_filter-name
      exp = 'A' ).
    assign ls_filter-valref->* to <v>.
    cl_abap_unit_assert=>assert_equals(
      act = <v>
      exp = 'B' ).

    read table lt_filter_act into ls_filter index 2.
    cl_abap_unit_assert=>assert_equals(
      act = ls_filter-name
      exp = 'XYZ' ).
    assign ls_filter-valref->* to <v>.
    cl_abap_unit_assert=>assert_equals(
      act = <v>
      exp = 1 ).

  endmethod.

endclass.
