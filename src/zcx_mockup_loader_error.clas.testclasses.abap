class lcl_ml_cx_test definition for testing
  duration short
  risk level harmless.

  private section.

    methods raise_error for testing.
endclass.

class lcl_ml_cx_test implementation.

  method raise_error.
    data lcx type ref to zcx_mockup_loader_error.

    try.
      zcx_mockup_loader_error=>raise( msg = 'abc' code = '42' ).
    catch zcx_mockup_loader_error into lcx.
      " skip
    endtry.

    cl_abap_unit_assert=>assert_not_initial( lcx ).
    cl_abap_unit_assert=>assert_equals( act = lcx->msg exp = 'abc' ).
    cl_abap_unit_assert=>assert_equals( act = lcx->code exp = '42' ).
    cl_abap_unit_assert=>assert_equals( act = lcx->methname exp = '[RAISE_ERROR]' ).
    cl_abap_unit_assert=>assert_equals( act = lcx->get_text( ) exp = '[RAISE_ERROR] abc' ).

  endmethod.  " raise_error.

endclass.
