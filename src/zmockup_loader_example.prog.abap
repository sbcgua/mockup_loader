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
*| project homepage: https://github.com/sbcgua/mockup_loader                      |
*\--------------------------------------------------------------------------------/

report zmockup_loader_example.

**********************************************************************
* SINGLETON SETTINGS CLASS (a sample way to get test env indicator)
**********************************************************************
class lcl_context definition final create private.

  public section.
    data a_carrid  type sflight-carrid read-only.       " Indicates execution in test environment
    data a_testenv type abap_bool read-only.

    class-methods get_instance
      returning value(ro_instance) type ref to lcl_context.

    methods set_carrid
      importing i_carrid type sflight-carrid.

  private section.
    class-data go_instance type ref to lcl_context.     " Some settings for the production code

endclass.  " lcl_context

class lcl_context implementation.
  method get_instance. " Get sinleton instance
    if go_instance is not bound.
      create object go_instance.
    endif.
    ro_instance = go_instance.
  endmethod.

  method set_carrid. " Setup context for production environment
    clear: me->a_carrid, me->a_testenv.
    me->a_carrid = i_carrid.
    if i_carrid = 'ZZZ'. " Special test env airline - non existing !
      me->a_testenv = abap_true.
    endif.
  endmethod.

endclass.  " lcl_context

**********************************************************************
* SOME BUSINESS LOGIC CLASS - the object to test
**********************************************************************
class lcl_main_logic definition final create public.

  public section.
    methods constructor.
    methods get_price
      importing
        i_connid type sflight-connid
        i_date   type sflight-fldate
      returning value(r_price) type sflight-price
      exceptions not_found.

  private section.
    data o_context type ref to lcl_context.

endclass.  " lcl_main_logic

class lcl_main_logic implementation.
  method constructor.
    o_context = lcl_context=>get_instance( ). " Get context
  endmethod.   "constructor

  method get_price. " Get price of the connection in the context airline
    data ls_flight type sflight.

    if o_context->a_testenv = abap_false. " Production env
      select single price into corresponding fields of ls_flight
        from sflight
        where carrid = o_context->a_carrid
        and connid = i_connid
        and fldate = i_date.
    else.                                  " Test env
      zcl_mockup_loader_store=>retrieve(
        exporting
          i_name = 'SFLIGHT'
          i_sift = i_connid
        importing
          e_data = ls_flight
        exceptions others = 4 ).
    endif.

    if sy-subrc is not initial. " Selection error ?
      raise not_found.
    endif.

    r_price = ls_flight-price.

  endmethod.   "get_price
endclass.  " lcl_main_logic

**********************************************************************
* TEST CLASS
**********************************************************************
class ltcl_test definition for testing duration short
  risk level harmless.

  public section.
    types:
      begin of ty_testcase, " test case structure
        testid   type i,
        type     type c length 1,
        connid   type sflight-connid,
        result   type sflight-price,
        msg      type string,
      end of ty_testcase.

  private section.
    data o    type ref to lcl_main_logic.    " Class being tested
    data o_ml type ref to zcl_mockup_loader. " Mockup loader

    methods: setup.
    methods: get_price for testing.

endclass.   "lcl_test

class ltcl_test implementation.
  method setup. " Initialize instances
    data lo_context type ref to lcl_context.
    data lo_ex      type ref to cx_static_check.

    lo_context = lcl_context=>get_instance( ).
    lo_context->set_carrid( 'ZZZ' ). " Test env airline

    create object o.

    try.
      o_ml = zcl_mockup_loader=>create(
        i_type       = 'MIME'
        i_path       = 'ZMOCKUP_LOADER_EXAMPLE'
        i_amt_format = ' ,' ).
    catch cx_static_check into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

  endmethod.

  method get_price.
    data lt_testcases type table of ty_testcase.
    data ls_case      type ty_testcase.
    data lo_ex        type ref to cx_static_check.
    data l_result     type sflight-price.

    try.
      " Load test cases index for local usage
      o_ml->load_data(
        exporting i_obj       = 'EXAMPLE/testcases'
        importing e_container = lt_testcases ).

      " Load and store flights table
      zcl_mockup_loader_store=>load_and_store(
        io_ml    = o_ml
        i_obj    = 'EXAMPLE/sflight'
        i_name   = 'SFLIGHT'
        i_strict = abap_false
        i_tabkey = 'CONNID'
        i_type   = 'FLIGHTTAB' ).

    catch cx_static_check into lo_ex.
      cl_abap_unit_assert=>fail( lo_ex->get_text( ) ).
    endtry.

    loop at lt_testcases into ls_case. " Loop through test catalog and run tests
      o->get_price(
        exporting
          i_connid = ls_case-connid
          i_date   = '20150101'
        receiving
          r_price  = l_result
        exceptions others = 4 ).

      if ls_case-type = '+'. " Positive test
        cl_abap_unit_assert=>assert_subrc(
          act = sy-subrc
          exp = 0
          msg = |[{ ls_case-testid }] { ls_case-msg }| ).
        cl_abap_unit_assert=>assert_equals(
          act = l_result
          exp = ls_case-result
          msg = |[{ ls_case-testid }] { ls_case-msg }| ).

      else. "'-'             " Negative test
        cl_abap_unit_assert=>assert_subrc(
          act = sy-subrc
          exp = 4
          msg = |[{ ls_case-testid }] { ls_case-msg }| ).
      endif.

    endloop.

  endmethod.

endclass.   "lcl_test
