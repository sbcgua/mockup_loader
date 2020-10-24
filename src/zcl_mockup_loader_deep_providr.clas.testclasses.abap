class ltcl_mockup_loader_mock definition final
  for testing.

  public section.

    types:
      begin of ty_data,
        data type string,
      end of ty_data.

    interfaces zif_mockup_loader.
    data:
      mv_calls  type i,
      mv_obj    type string,
      mv_strict type abap_bool.
endclass.

class ltcl_mockup_loader_mock implementation.

  method zif_mockup_loader~load_blob.
  endmethod.
  method zif_mockup_loader~set_params.
  endmethod.
  method zif_mockup_loader~is_redirected.
  endmethod.

  method zif_mockup_loader~load_data.
    data ls_data type ty_data.
    ls_data-data = 'RESPONSE'.

    if cl_abap_typedescr=>describe_by_data( e_container )->kind = 'T'.
      field-symbols <tab> type standard table.
      assign e_container to <tab>.
      append ls_data to <tab>.
    else.
      e_container  = ls_data.
    endif.

    mv_obj       = i_obj.
    mv_strict    = i_strict.
    mv_calls     = mv_calls + 1.
  endmethod.

endclass.

class ltcl_deep_provider_test definition final
  for testing
  duration short
  risk level harmless.

  private section.

    types:
      begin of ty_data,
        id   type i,
        data type c length 10,
      end of ty_data,
      tt_data type standard table of ty_data with key id.

    methods select for testing raising zcx_text2tab_error.

endclass.

class ltcl_deep_provider_test implementation.

  method select.

    data lo_cut type ref to zcl_mockup_loader_deep_providr.
    data lo_ml_mock type ref to ltcl_mockup_loader_mock.
    data ls_record_dummy type ty_data.
    data ls_data type ltcl_mockup_loader_mock=>ty_data.
    data ls_exp_data type ltcl_mockup_loader_mock=>ty_data.
    data ls_where type zcl_mockup_loader_utils=>ty_filter.

    create object lo_ml_mock.
    create object lo_cut exporting ii_ml_instance = lo_ml_mock.

    ls_record_dummy-id = 123.

    lo_cut->zif_text2tab_deep_provider~select(
      exporting
        i_address = 'anotherfile[key=@id]'
        i_cursor  = ls_record_dummy
      importing
        e_container = ls_data ).

    ls_exp_data-data = 'RESPONSE'.
    cl_abap_unit_assert=>assert_equals(
      act = ls_data
      exp = ls_exp_data ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ml_mock->mv_calls
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ml_mock->mv_obj
      exp = 'anotherfile' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ml_mock->mv_strict
      exp = abap_false ). " ???

  endmethod.

endclass.
