class ltcl_mockup_stub_factory_test definition final
  for testing
  duration short
  risk level harmless.

  private section.
    methods main_test_stub for testing.
    methods connect_method for testing.
    methods build_config for testing.
    methods instantiation for testing.
    methods forward_method for testing.
    methods proxy_forwarding for testing.
    methods filtering for testing raising zcx_mockup_loader_error.
    methods filtering_w_struc_param for testing raising zcx_mockup_loader_error.

    methods get_ml
      returning
        value(ro_ml) type ref to zcl_mockup_loader
      raising zcx_mockup_loader_error.

    methods get_factory
      importing
        io_ml type ref to zcl_mockup_loader
      returning
        value(ro_factory) type ref to zcl_mockup_loader_stub_factory
      raising zcx_mockup_loader_error.

    methods get_sflights
      importing
        io_ml type ref to zcl_mockup_loader
      returning
        value(rt_tab) type flighttab
      raising zcx_mockup_loader_error.

endclass.

class lcl_test_proxy_target definition.
  public section.
    interfaces zif_mockup_loader_stub_dummy.
endclass.

class lcl_test_proxy_target implementation.
  method zif_mockup_loader_stub_dummy~proxy_test.
    r_val = |{ i_p1 } { i_p2 }|.
  endmethod.
  method zif_mockup_loader_stub_dummy~tab_return.
  endmethod.
  method zif_mockup_loader_stub_dummy~tab_export.
  endmethod.
  method zif_mockup_loader_stub_dummy~tab_change.
  endmethod.
  method zif_mockup_loader_stub_dummy~wrong_return.
  endmethod.
  method zif_mockup_loader_stub_dummy~wrong_sift.
  endmethod.
  method zif_mockup_loader_stub_dummy~gen_param_target.
  endmethod.
endclass.

class lcl_test_base definition final.
  public section.
    class-methods main_test
      importing iv_factory_classname type seoclsname.
endclass.
class lcl_test_base implementation.
  method main_test.

    data lo_dc type ref to zcl_mockup_loader_stub_factory.
    data li_if type ref to zif_mockup_loader_stub_dummy.
    data lo_ml type ref to zcl_mockup_loader.
    data lt_exp type flighttab.
    data lo_ex type ref to zcx_mockup_loader_error.

    try.
      lo_ml  = zcl_mockup_loader=>create(
        i_type = 'MIME'
        i_path = 'ZMOCKUP_LOADER_EXAMPLE' ).

      create object lo_dc type (iv_factory_classname)
        exporting
          io_ml_instance = lo_ml
          i_interface_name = 'ZIF_MOCKUP_LOADER_STUB_DUMMY'.

      lo_dc->connect_method(
        i_sift_param      = 'I_CONNID'
        i_mock_tab_key    = 'CONNID'
        i_method_name     = 'TAB_RETURN'
        i_mock_name       = 'EXAMPLE/sflight' ).

      lo_dc->connect_method(
        i_method_name  = 'TAB_EXPORT'
        i_mock_name    = 'EXAMPLE/sflight'
        i_output_param = 'E_TAB' ).

      lo_dc->connect_method(
        i_method_name  = 'TAB_CHANGE'
        i_mock_name    = 'EXAMPLE/sflight'
        i_output_param = 'C_TAB' ).

      li_if ?= lo_dc->generate_stub( ).

      lo_ml->load_data(
        exporting
          i_obj    = 'EXAMPLE/sflight'
          i_strict = abap_false
        importing
          e_container = lt_exp ).

      delete lt_exp index 2.
      data lt_res type flighttab.
      lt_res = li_if->tab_return( i_connid = '1000' ).
      cl_abap_unit_assert=>assert_equals( act = lt_res exp = lt_exp ).

      lo_ml->load_data(
        exporting
          i_obj    = 'EXAMPLE/sflight'
          i_strict = abap_false
        importing
          e_container = lt_exp ).

      clear lt_res.
      li_if->tab_export( exporting i_connid = '1000' importing e_tab = lt_res ).
      cl_abap_unit_assert=>assert_equals( act = lt_res exp = lt_exp ).

      clear lt_res.
      li_if->tab_change( exporting i_connid = '1000' changing c_tab = lt_res ).
      cl_abap_unit_assert=>assert_equals( act = lt_res exp = lt_exp ).

    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( ).
    endtry.

  endmethod.

endclass.

define assert_excode.
  cl_abap_unit_assert=>assert_not_initial( act = lo_ex ).
  cl_abap_unit_assert=>assert_equals( exp = &1 act = lo_ex->code ).
end-of-definition.

class ltcl_mockup_stub_factory_test implementation.

  method main_test_stub.
    lcl_test_base=>main_test( 'ZCL_MOCKUP_LOADER_STUB_FACTORY' ).
  endmethod.

  method connect_method.
    data lo_dc type ref to zcl_mockup_loader_stub_factory.
    data lo_ml type ref to zcl_mockup_loader.
    data lo_ex type ref to zcx_mockup_loader_error.

    try.

    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( ).
    endtry.

    try.
      lo_ml  = zcl_mockup_loader=>create(
        i_type = 'MIME'
        i_path = 'ZMOCKUP_LOADER_EXAMPLE' ).

      create object lo_dc
        exporting
          io_ml_instance = lo_ml
          i_interface_name = 'ZIF_MOCKUP_LOADER_STUB_DUMMY'.

      lo_dc->connect_method(
        i_method_name     = 'TAB_RETURN'
        i_mock_name       = 'EXAMPLE/sflight' ).
      lo_dc->connect_method(
        i_method_name     = 'TAB_RETURN'
        i_mock_name       = 'EXAMPLE/sflight' ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'MC'.

  endmethod.

  method build_config.
    data ld_if       type ref to cl_abap_objectdescr.
    data ls_conf     type zcl_mockup_loader_stub_base=>ty_mock_config.
    data ls_conf_act type zcl_mockup_loader_stub_base=>ty_mock_config.
    data lo_ex type ref to zcx_mockup_loader_error.

    ld_if ?= cl_abap_typedescr=>describe_by_name( 'ZIF_MOCKUP_LOADER_STUB_DUMMY' ).

    try.
      clear: lo_ex, ls_conf.
      ls_conf-method_name = 'TAB_RETURN'.
      ls_conf-mock_name   = 'EXAMPLE/sflight'.
      ls_conf_act = zcl_mockup_loader_stub_factory=>build_config(
        id_if_desc = ld_if
        i_config   = ls_conf ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( ).
    endtry.
    cl_abap_unit_assert=>assert_equals( act = ls_conf_act-output_param exp = 'R_TAB' ).
    cl_abap_unit_assert=>assert_equals( act = ls_conf_act-output_pkind exp = 'R' ).
    cl_abap_unit_assert=>assert_bound( act = ls_conf_act-output_type ).

    try. " method missing
      clear: lo_ex, ls_conf.
      ls_conf-mock_name   = 'EXAMPLE/sflight'.
      ls_conf_act = zcl_mockup_loader_stub_factory=>build_config(
        id_if_desc = ld_if
        i_config   = ls_conf ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'MM'.

    try. " mock missing
      clear: lo_ex, ls_conf.
      ls_conf-method_name = 'TAB_RETURN'.
      ls_conf_act = zcl_mockup_loader_stub_factory=>build_config(
        id_if_desc = ld_if
        i_config   = ls_conf ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'MK'.

    try. " sift incomplete
      clear: lo_ex, ls_conf.
      ls_conf-method_name = 'TAB_RETURN'.
      ls_conf-mock_name   = 'EXAMPLE/sflight'.
      ls_conf-sift_param  = 'I_CONNID'.
      ls_conf_act = zcl_mockup_loader_stub_factory=>build_config(
        id_if_desc = ld_if
        i_config   = ls_conf ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'MS'.

    try. " sift incomplete
      clear: lo_ex, ls_conf.
      ls_conf-method_name  = 'TAB_RETURN'.
      ls_conf-mock_name    = 'EXAMPLE/sflight'.
      ls_conf-mock_tab_key = 'CONNID'.
      ls_conf_act = zcl_mockup_loader_stub_factory=>build_config(
        id_if_desc = ld_if
        i_config   = ls_conf ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'MS'.

    try. " sift incomplete
      clear: lo_ex, ls_conf.
      ls_conf-method_name  = '???'.
      ls_conf-mock_name    = 'EXAMPLE/sflight'.
      ls_conf_act = zcl_mockup_loader_stub_factory=>build_config(
        id_if_desc = ld_if
        i_config   = ls_conf ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'MF'.

    try. " sift not found
      clear: lo_ex, ls_conf.
      ls_conf-method_name  = 'TAB_RETURN'.
      ls_conf-mock_name    = 'EXAMPLE/sflight'.
      ls_conf-sift_param   = 'X_CONNID'.
      ls_conf-mock_tab_key = 'CONNID'.
      ls_conf_act = zcl_mockup_loader_stub_factory=>build_config(
        id_if_desc = ld_if
        i_config   = ls_conf ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'PF'.

    try. " sift has wrong type
      clear: lo_ex, ls_conf.
      ls_conf-method_name  = 'WRONG_SIFT'.
      ls_conf-mock_name    = 'EXAMPLE/sflight'.
      ls_conf-sift_param   = 'I_CONNID'.
      ls_conf-mock_tab_key = 'CONNID'.
      ls_conf_act = zcl_mockup_loader_stub_factory=>build_config(
        id_if_desc = ld_if
        i_config   = ls_conf ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'PE'.

    try. " no return
      clear: lo_ex, ls_conf.
      ls_conf-method_name = 'TAB_EXPORT'.
      ls_conf-mock_name   = 'EXAMPLE/sflight'.
      ls_conf_act = zcl_mockup_loader_stub_factory=>build_config(
        id_if_desc = ld_if
        i_config   = ls_conf ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'MR'.

    try. " no param
      clear: lo_ex, ls_conf.
      ls_conf-method_name  = 'TAB_RETURN'.
      ls_conf-mock_name    = 'EXAMPLE/sflight'.
      ls_conf-output_param = '???'.
      ls_conf_act = zcl_mockup_loader_stub_factory=>build_config(
        id_if_desc = ld_if
        i_config   = ls_conf ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'PF'.

    try. " no param
      clear: lo_ex, ls_conf.
      ls_conf-method_name  = 'TAB_RETURN'.
      ls_conf-mock_name    = 'EXAMPLE/sflight'.
      ls_conf-output_param = 'I_CONNID'.
      ls_conf_act = zcl_mockup_loader_stub_factory=>build_config(
        id_if_desc = ld_if
        i_config   = ls_conf ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'PI'.

    try. " no param
      clear: lo_ex, ls_conf.
      ls_conf-method_name  = 'WRONG_RETURN'.
      ls_conf-mock_name    = 'EXAMPLE/sflight'.
      ls_conf_act = zcl_mockup_loader_stub_factory=>build_config(
        id_if_desc = ld_if
        i_config   = ls_conf ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'PT'.

  endmethod.

  method instantiation.
    data lo type ref to zcl_mockup_loader_stub_factory.
    data lo_ex type ref to zcx_mockup_loader_error.
    data lo_ml type ref to zcl_mockup_loader.

    try. " wrong interface
      lo_ml  = zcl_mockup_loader=>create(
        i_type = 'MIME'
        i_path = 'ZMOCKUP_LOADER_EXAMPLE' ).

      clear: lo_ex.
      create object lo
        exporting
          io_ml_instance   = lo_ml
          i_interface_name = 'CHAR1'.
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'IF'.

    data lo_proxy_target type ref to lcl_test_proxy_target.
    try. " wrong proxy target
      clear: lo_ex.
      create object lo_proxy_target.
      create object lo
        exporting
          io_ml_instance   = lo_ml
          io_proxy_target  = lo_ml
          i_interface_name = 'ZIF_MOCKUP_LOADER_STUB_DUMMY'.
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'II'.

  endmethod.

  method forward_method.

    data lo type ref to zcl_mockup_loader_stub_factory.
    data lo_ex type ref to zcx_mockup_loader_error.
    data lo_ml type ref to zcl_mockup_loader.
    data lo_proxy_target type ref to lcl_test_proxy_target.
    create object lo_proxy_target.

    try. " no proxy during creation
      clear: lo_ex.
      create object lo
        exporting
          io_ml_instance   = lo_ml
          i_interface_name = 'ZIF_MOCKUP_LOADER_STUB_DUMMY'.
      lo->forward_method( 'PROXY_TEST' ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'PA'.

    try. " missing method
      clear: lo_ex.
      create object lo
        exporting
          io_ml_instance   = lo_ml
          io_proxy_target  = lo_proxy_target
          i_interface_name = 'ZIF_MOCKUP_LOADER_STUB_DUMMY'.
      lo->forward_method( 'PROXY_TEST_XXX' ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'MF'.

    try. " missing method
      clear: lo_ex.
      lo->forward_method( 'PROXY_TEST' ).
      lo->forward_method( 'PROXY_TEST' ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'MC'.

  endmethod.

  method proxy_forwarding.
    data lo type ref to zcl_mockup_loader_stub_factory.
    data lo_ex type ref to zcx_mockup_loader_error.
    data lo_ml type ref to zcl_mockup_loader.
    data lo_proxy_target type ref to lcl_test_proxy_target.
    data li_if type ref to zif_mockup_loader_stub_dummy.
    data l_act type string.
    create object lo_proxy_target.

    try. " missing method
      create object lo
        exporting
          io_ml_instance   = lo_ml
          io_proxy_target  = lo_proxy_target
          i_interface_name = 'ZIF_MOCKUP_LOADER_STUB_DUMMY'.
      lo->forward_method( 'PROXY_TEST' ).
      li_if ?= lo->generate_stub( ).
      l_act = li_if->proxy_test( i_p1 = 'Hello' i_p2 = 123 ).
      cl_abap_unit_assert=>assert_equals( act = l_act exp = 'Hello 123' ).
    catch zcx_mockup_loader_error into lo_ex.
      cl_abap_unit_assert=>fail( ).
    endtry.

  endmethod.

**********************************************************************

  method get_ml.
    ro_ml = zcl_mockup_loader=>create(
      i_type = 'MIME'
      i_path = 'ZMOCKUP_LOADER_EXAMPLE' ).
  endmethod.

  method get_factory.
    create object ro_factory
      exporting
        io_ml_instance   = io_ml " get_ml( )
        i_interface_name = 'ZIF_MOCKUP_LOADER_STUB_DUMMY'.
  endmethod.

  method get_sflights.
    io_ml->load_data(
      exporting
        i_obj    = 'EXAMPLE/sflight'
        i_strict = abap_false
      importing
        e_container = rt_tab ).
  endmethod.

  method filtering.

    data factory type ref to zcl_mockup_loader_stub_factory.
    data stub type ref to zif_mockup_loader_stub_dummy.
    data ml type ref to zcl_mockup_loader.
    data lt_exp type flighttab.
    data lt_act type flighttab.

    ml      = get_ml( ).
    factory = get_factory( ml ).
    lt_exp  = get_sflights( ml ).
    delete lt_exp index 2.

    factory->connect_method(
      i_sift_param      = 'I_CONNID'
      i_mock_tab_key    = 'CONNID'
      i_method_name     = 'TAB_RETURN'
      i_mock_name       = 'EXAMPLE/sflight' ).

    stub ?= factory->generate_stub( ).
    lt_act = stub->tab_return( i_connid = '1000' ).
    cl_abap_unit_assert=>assert_equals( act = lt_act exp = lt_exp ).

  endmethod.

  method filtering_w_struc_param.

    data factory type ref to zcl_mockup_loader_stub_factory.
    data stub type ref to zif_mockup_loader_stub_dummy.
    data ml type ref to zcl_mockup_loader.
    data lt_exp type flighttab.
    data lt_act type flighttab.

    ml      = get_ml( ).
    factory = get_factory( ml ).
    lt_exp  = get_sflights( ml ).
    delete lt_exp index 2.

    factory->connect_method(
      i_sift_param      = 'I_PARAMS-CONNID'
      i_mock_tab_key    = 'CONNID'
      i_method_name     = 'TAB_RETURN_W_STRUC_PARAM'
      i_mock_name       = 'EXAMPLE/sflight' ).

    data call_params type zif_mockup_loader_stub_dummy=>ty_params.
    call_params-connid = '1000'.

    stub ?= factory->generate_stub( ).
    lt_act = stub->tab_return_w_struc_param( i_params = call_params ).
    cl_abap_unit_assert=>assert_equals( act = lt_act exp = lt_exp ).

  endmethod.

endclass.
