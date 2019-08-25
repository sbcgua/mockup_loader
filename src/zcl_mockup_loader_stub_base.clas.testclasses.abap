class ltcl_mockup_stub_base_test definition final
  for testing
  duration short
  risk level harmless.

  private section.
    methods get_mock_data for testing.
endclass.

class lcl_mockup_loader_stub_final definition final
  inheriting from zcl_mockup_loader_stub_base
  friends ltcl_mockup_stub_base_test.
endclass.
class lcl_mockup_loader_stub_final implementation.
endclass.

class ltcl_mockup_stub_base_test implementation.

  method get_mock_data.

    data lo type ref to lcl_mockup_loader_stub_final.
    data lo_ml type ref to zcl_mockup_loader.
    data lt_config type lcl_mockup_loader_stub_final=>tt_mock_config.
    data lo_ex type ref to zcx_mockup_loader_error.
    data ls_conf like line of lt_config.
    data lt_exp type flighttab.
    data lr_act type ref to data.
    field-symbols <act> type flighttab.

    ls_conf-method_name  = 'METHOD_SIMPLE'.
    ls_conf-mock_name    = 'EXAMPLE/sflight'.
    ls_conf-output_type ?= cl_abap_typedescr=>describe_by_name( 'FLIGHTTAB' ).
    ls_conf-mock_tab_key = 'CONNID'.
    append ls_conf to lt_config.

    ls_conf-method_name  = 'METHOD_SIFTED'.
    ls_conf-sift_param   = 'I_CONNID'.
    append ls_conf to lt_config.

    try.
      lo_ml  = zcl_mockup_loader=>create(
        i_type = 'MIME'
        i_path = 'ZMOCKUP_LOADER_EXAMPLE' ).
      create object lo
        exporting
          it_config = lt_config
          io_ml = lo_ml.
      lo_ml->load_data(
        exporting
          i_obj    = 'EXAMPLE/sflight'
          i_strict = abap_false
        importing
          e_container = lt_exp ).

      lr_act = lo->get_mock_data( i_method_name = 'METHOD_SIMPLE' i_sift_value = '1000' ).
      assign lr_act->* to <act>.
      cl_abap_unit_assert=>assert_equals( act = <act> exp = lt_exp ).

      lr_act = lo->get_mock_data( i_method_name = 'METHOD_SIFTED' i_sift_value = '1000' ).
      assign lr_act->* to <act>.
      delete lt_exp index 2.
      cl_abap_unit_assert=>assert_equals( act = <act> exp = lt_exp ).

      lr_act = lo->get_mock_data( i_method_name = 'METHOD_MISSING' i_sift_value = '1000' ).
      cl_abap_unit_assert=>assert_initial( lr_act ).

    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( ).
    endtry.

  endmethod.

endclass.
