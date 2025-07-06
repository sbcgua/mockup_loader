class ltcl_ml_cx_test definition for testing
  duration short
  risk level harmless.

  private section.

    methods raise_error for testing.
    methods remsg for testing.
endclass.

class ltcl_ml_cx_test implementation.

  method remsg.

    data lx type ref to zcx_mockup_loader_error.

    try.
      try.
        raise exception type zcx_mockup_loader_error exporting msg = 'A'.
      catch zcx_mockup_loader_error into lx.
        lx->remsg( 'B' ).
        raise exception lx.
      endtry.
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->get_text( ) exp = 'B' ).
    endtry.

  endmethod.


  method raise_error.
    data lcx type ref to zcx_mockup_loader_error.

    try.
      zcx_mockup_loader_error=>raise( msg = 'abc' code = '42' ).
    catch zcx_mockup_loader_error into lcx.
      " skip
    endtry.

    cl_abap_unit_assert=>assert_not_initial( lcx ).
    cl_abap_unit_assert=>assert_equals(
      act = lcx->msg
      exp = 'abc' ).
    cl_abap_unit_assert=>assert_equals(
      act = lcx->code
      exp = '42' ).
    cl_abap_unit_assert=>assert_equals(
      act = lcx->methname
      exp = 'RAISE_ERROR' ).
    cl_abap_unit_assert=>assert_equals(
      act = lcx->get_text( )
      exp = '[RAISE_ERROR] abc' ).

  endmethod.

endclass.
