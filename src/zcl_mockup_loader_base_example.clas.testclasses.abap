class ltcl_test_base definition final
  for testing
  risk level harmless
  duration short.

  private section.

    class-methods class_setup.
    methods check_ml_version for testing.

endclass.

class ltcl_test_base implementation.

  method class_setup.

*          gi_ml->cd( 'testdir' ). " Only if

  endmethod.


  method check_ml_version.

    data lv_version_ok type abap_bool.
    data lv_req_ver    type string value zcl_mockup_loader_base_example=>gc_required_mockup_loader_ver.

    lv_version_ok = zcl_mockup_loader=>check_version_fits( lv_req_ver ).
    if lv_version_ok = abap_false.
       cl_abap_unit_assert=>fail( |mockup loader version ({ zif_mockup_loader=>version }) is lower than required ({ lv_req_ver })| ).
    endif.

  endmethod.

endclass.

***

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

endclass.

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
        i_encoding   = zif_mockup_loader=>encoding_utf16
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
        exporting
          i_obj       = 'EXAMPLE/testcases'
        importing
          e_container = lt_testcases ).

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
        exceptions
          others = 4 ).

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

endclass.
