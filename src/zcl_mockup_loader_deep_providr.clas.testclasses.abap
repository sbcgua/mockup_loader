class ltcl_mockup_loader_mock definition final
  for testing.
  public section.
    interfaces zif_mockup_loader.
endclass.

class ltcl_mockup_loader_mock implementation.

  method zif_mockup_loader~load_raw_x.
  endmethod.

  method zif_mockup_loader~load_data.
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

    methods select for testing.

endclass.

class ltcl_deep_provider_test implementation.

  method select.

    data lo_cut type ref to zcl_mockup_loader_deep_providr.
    data lo_ml_mock type ref to ltcl_mockup_loader_mock.
    data ls_record_dummy type ty_data.
    data lv_data type string.

    create object lo_ml_mock.
    create object lo_cut exporting ii_ml_instance = lo_ml_mock.

    ls_record_dummy-id = 123.

    lo_cut->zif_text2tab_deep_provider~select(
      exporting
        i_address = 'anotherfile[key=@id]'
        i_cursor  = ls_record_dummy
      importing
        e_container = lv_data ). " TODO EXCEPTION ?????

    " validate lv_data
    " validate that ltcl_mockup_loader_mock->load_data
    " was called with
    " I_OBJ  = anotherfile
    " I_STRICT = ???
    " I_where = constrcutd structure of filter "key = 123"

  endmethod.

endclass.
