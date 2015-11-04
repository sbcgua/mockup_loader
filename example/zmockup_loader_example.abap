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
  data a_bukrs   type bukrs     read-only. " Some settings for the production code
  data a_testenv type abap_bool read-only. " Indicates execution in test environment

  class-methods get_instance
    returning value(ro_instance) type ref to lcl_context.

  methods set_bukrs
    importing i_bukrs type bukrs.

private section.
  class-data go_instance type ref to lcl_context.

endclass.  " lcl_context

class lcl_context implementation.
  method get_instance. " Get sinleton instance
    if go_instance is not bound.
      create object go_instance.
    endif.
    ro_instance = go_instance.
  endmethod.

  method set_bukrs. " Setup context for production environment
    clear: me->a_bukrs, me->a_testenv.
    me->a_bukrs = i_bukrs.
    if i_bukrs = 'ZZZZ'. " Special test env company code - non existing !
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
  methods check_doc_is_reversed
    importing
      i_belnr type belnr_d
      i_gjahr type gjahr
    returning value(r_reversed) type abap_bool
    exceptions not_found.

private section.
  data o_context type ref to lcl_context.

endclass.  " lcl_main_logic

class lcl_main_logic implementation.
  method constructor.
    o_context = lcl_context=>get_instance( ). " Get context
  endmethod.   "constructor

  method check_doc_is_reversed. " Checks if FI document is reversed - the method to be tested
    data ls_bkpf type bkpf.

    if o_context->a_testenv = abap_false. " Production env
      select single xreversal into corresponding fields of ls_bkpf
        from bkpf
        where bukrs = o_context->a_bukrs
        and   belnr = i_belnr
        and   gjahr = i_gjahr.
    else.                                  " Test env
      zcl_mockup_loader=>retrieve( exporting i_name = 'BKPF'  i_sift = i_belnr
                                   importing e_data = ls_bkpf exceptions others = 4 ).
    endif.

    if sy-subrc is not initial. " Selection error ?
      raise not_found.
    endif.

    if ls_bkpf-xreversal is initial. " Document reversed ?
      r_reversed = abap_false.
    else.
      r_reversed = abap_true.
    endif.

  endmethod.   "check_doc_is_reversed
endclass.  " lcl_main_logic

**********************************************************************
* TEST CLASS
**********************************************************************
class lcl_test definition for testing duration short
  inheriting from cl_aunit_assert  risk level harmless.

  public section.
    types:
      begin of ty_testcase,
        testid   type i,
        type     type char1,
        belnr    type belnr_d,
        result   type abap_bool,
        msg      type string,
      end of ty_testcase.

  private section.
    data o    type ref to lcl_main_logic.    " Class being tested
    data o_ml type ref to zcl_mockup_loader. " Mockup loader

    class-methods: class_setup.
    methods: setup.
    methods: check_doc_is_reversed for testing.

endclass.   "lcl_test

class lcl_test implementation.
  method class_setup. " Set mockup source -> workstation file
    zcl_mockup_loader=>class_set_source( i_type = 'FILE' i_path = 'c:\sap\example.zip' ).
  endmethod.

  method setup. " Initialize instances
    data lo_context type ref to lcl_context.
    data lo_ex      type ref to zcx_mockup_loader_error.

    lo_context = lcl_context=>get_instance( ).
    lo_context->set_bukrs( 'ZZZZ' ). " Test env bukrs

    create object o.

    try.
      o_ml = zcl_mockup_loader=>get_instance( ).
    catch zcx_mockup_loader_error into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

  endmethod.

  method check_doc_is_reversed.
    data lt_testcases type table of ty_testcase.
    data ls_case      type ty_testcase.
    data lo_ex        type ref to zcx_mockup_loader_error.
    data l_result     type abap_bool.

    try.
      " Load test cases index for local usage
      o_ml->load_data( exporting i_obj       = 'testcases'
                       importing e_container = lt_testcases ).

      " Load and store BKPF table
      o_ml->load_and_store( i_obj    = 'bkpf'
                            i_name   = 'BKPF'
                            i_strict = abap_false
                            i_tabkey = 'BELNR'
                            i_type   = 'BKPF_T' ).

    catch zcx_mockup_loader_error into lo_ex.
      fail( lo_ex->get_text( ) ).
    endtry.

    loop at lt_testcases into ls_case. " Loop through test catalog and run tests
      call method o->check_doc_is_reversed
        exporting
          i_belnr    = ls_case-belnr
          i_gjahr    = '2015'
        receiving
          r_reversed = l_result
        exceptions others = 4.

      if ls_case-type = '+'. " Positive test
        assert_subrc(  act = sy-subrc  exp = 0
                       msg = |[{ ls_case-testid }] { ls_case-msg }| ).
        assert_equals( act = l_result  exp = ls_case-result
                       msg = |[{ ls_case-testid }] { ls_case-msg }| ).

      else. "'-'             " Negative test
        assert_subrc(  act = sy-subrc  exp = 4
                       msg = |[{ ls_case-testid }] { ls_case-msg }| ).
      endif.

    endloop.

  endmethod.

endclass.   "lcl_test
