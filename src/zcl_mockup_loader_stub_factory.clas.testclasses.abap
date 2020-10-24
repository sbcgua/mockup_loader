class ltcl_mockup_stub_factory_test definition final
  for testing
  duration short
  risk level harmless.

  private section.
    methods main_test_stub for testing.
    methods connect_deep_corresponding for testing raising zcx_mockup_loader_error.
    methods connect_method for testing.
    methods build_config for testing.
    methods instantiation for testing.
    methods forward_method for testing.
    methods proxy_forwarding for testing raising zcx_mockup_loader_error.
    methods filtering for testing raising zcx_mockup_loader_error.
    methods filtering_w_const for testing raising zcx_mockup_loader_error.
    methods filtering_w_struc_param for testing raising zcx_mockup_loader_error.
    methods returning_value for testing raising zcx_mockup_loader_error.
    methods corresponding for testing raising zcx_mockup_loader_error.
    methods filter_by_range_param for testing raising zcx_mockup_loader_error.
    methods controls for testing raising zcx_mockup_loader_error.
    methods const_value for testing raising zcx_mockup_loader_error.
    methods connect_string_positive for testing raising zcx_mockup_loader_error.
    methods parse_string_negative for testing.
    methods parse_string_positive for testing raising zcx_mockup_loader_error.

    " HELPERS
    methods get_ml
      returning
        value(ro_ml) type ref to zcl_mockup_loader
      raising zcx_mockup_loader_error.

    methods get_factory
      importing
        io_ml type ref to zcl_mockup_loader
        io_proxy type ref to zif_mockup_loader_stub_dummy optional
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
    class-methods create returning value(ro_instance) type ref to lcl_test_proxy_target.
endclass.

class lcl_test_proxy_target implementation.
  method create.
    create object ro_instance.
  endmethod.
  method zif_mockup_loader_stub_dummy~proxy_test.
    r_val = |{ i_p1 } { i_p2 }|.
  endmethod.
  method zif_mockup_loader_stub_dummy~tab_return.
  endmethod.
  method zif_mockup_loader_stub_dummy~tab_return_by_range.
  endmethod.
  method zif_mockup_loader_stub_dummy~tab_return_extract_by_date.
  endmethod.
  method zif_mockup_loader_stub_dummy~tab_return_w_struc_param.
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
  method zif_mockup_loader_stub_dummy~return_value.
  endmethod.
  method zif_mockup_loader_stub_dummy~return_deep.
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

      delete lt_exp where connid <> '1000'.
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

class zcl_mockup_loader_stub_factory definition local friends ltcl_mockup_stub_factory_test.

class ltcl_mockup_stub_factory_test implementation.

  method main_test_stub.
    lcl_test_base=>main_test( 'ZCL_MOCKUP_LOADER_STUB_FACTORY' ).
  endmethod.

  method connect_deep_corresponding.

    data lo_ml type ref to zcl_mockup_loader.
    data lo_dc type ref to zcl_mockup_loader_stub_factory.
    data li_if type ref to zif_mockup_loader_stub_dummy.

    data lt_exp type zif_mockup_loader_stub_dummy=>tt_deep.

    lo_ml  = zcl_mockup_loader=>create(
      i_type = 'MIME'
      i_path = 'ZMOCKUP_LOADER_EXAMPLE' ).

    create object lo_dc
      exporting
        io_ml_instance   = lo_ml
        i_interface_name = 'ZIF_MOCKUP_LOADER_STUB_DUMMY'.

    lo_dc->connect_method(
      i_sift_param      = 'I_CONNID'
      i_mock_tab_key    = 'CONNID'
      i_method_name     = 'RETURN_DEEP'
      i_deep            = abap_true
      i_corresponding   = abap_true
      i_mock_name       = 'EXAMPLE/sflight' ).

    li_if ?= lo_dc->generate_stub( ).

    lo_ml->load_data(
      exporting
        i_obj    = 'EXAMPLE/sflight'
        i_strict = abap_false
        i_corresponding = abap_true
        i_deep   = abap_true
      importing
        e_container = lt_exp ).

    delete lt_exp where connid <> '2000'.

    data lt_res like lt_exp.
    lt_res = li_if->return_deep( i_connid = '2000' ).
    cl_abap_unit_assert=>assert_equals( act = lt_res exp = lt_exp ).

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

    try. " field only elementary param
      clear: lo_ex, ls_conf.
      ls_conf-method_name  = 'TAB_RETURN'.
      ls_conf-mock_name    = 'EXAMPLE/sflight'.
      ls_conf-field_only   = 'PRICE'.
      ls_conf_act = zcl_mockup_loader_stub_factory=>build_config(
        id_if_desc = ld_if
        i_config   = ls_conf ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'PL'.

    try. " field only + corresponding
      clear: lo_ex, ls_conf.
      ls_conf-method_name  = 'TAB_RETURN_EXTRACT_BY_DATE'.
      ls_conf-mock_name    = 'EXAMPLE/sflight'.
      ls_conf-corresponding = abap_true.
      ls_conf-field_only   = 'PRICE'.
      ls_conf_act = zcl_mockup_loader_stub_factory=>build_config(
        id_if_desc = ld_if
        i_config   = ls_conf ).
    catch zcx_mockup_loader_error into lo_ex.
    endtry.
    assert_excode 'PC'.

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

    try. " wrong proxy target
      clear: lo_ex.
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
          io_proxy_target  = lcl_test_proxy_target=>create( )
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
    data lo_ml type ref to zcl_mockup_loader.
    data li_if type ref to zif_mockup_loader_stub_dummy.
    data l_act type string.

    create object lo
      exporting
        io_ml_instance   = lo_ml
        io_proxy_target  = lcl_test_proxy_target=>create( )
        i_interface_name = 'ZIF_MOCKUP_LOADER_STUB_DUMMY'.
    lo->forward_method( 'PROXY_TEST' ).
    li_if ?= lo->generate_stub( ).
    l_act = li_if->proxy_test( i_p1 = 'Hello' i_p2 = 123 ).
    cl_abap_unit_assert=>assert_equals( act = l_act exp = 'Hello 123' ).

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
        i_interface_name = 'ZIF_MOCKUP_LOADER_STUB_DUMMY'
        io_proxy_target  = io_proxy.
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
    delete lt_exp where connid <> '1000'.

    factory->connect_method(
      i_sift_param      = 'I_CONNID'
      i_mock_tab_key    = 'CONNID'
      i_method_name     = 'TAB_RETURN'
      i_mock_name       = 'EXAMPLE/sflight' ).

    stub ?= factory->generate_stub( ).
    lt_act = stub->tab_return( i_connid = '1000' ).
    cl_abap_unit_assert=>assert_equals( act = lt_act exp = lt_exp ).

  endmethod.

  method filtering_w_const.

    data factory type ref to zcl_mockup_loader_stub_factory.
    data stub type ref to zif_mockup_loader_stub_dummy.
    data ml type ref to zcl_mockup_loader.
    data lt_exp type flighttab.
    data lt_act type flighttab.

    ml      = get_ml( ).
    factory = get_factory( ml ).
    lt_exp  = get_sflights( ml ).
    delete lt_exp where connid <> '1000'.

    factory->connect_method(
      i_sift_const      = '1000'
      i_mock_tab_key    = 'CONNID'
      i_method_name     = 'TAB_RETURN'
      i_mock_name       = 'EXAMPLE/sflight' ).

    stub ?= factory->generate_stub( ).
    lt_act = stub->tab_return( i_connid = '2000' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

    " Fail on const and param
    data lx type ref to zcx_mockup_loader_error.
    factory = get_factory( ml ).
    try.
      factory->connect_method(
        i_sift_param      = 'I_CONNID'
        i_sift_const      = '1000'
        i_mock_tab_key    = 'CONNID'
        i_method_name     = 'TAB_RETURN'
        i_mock_name       = 'EXAMPLE/sflight' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->code
        exp = 'CS' ).
    endtry.

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
    delete lt_exp where connid <> '1000'.

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

  method returning_value.

    data factory type ref to zcl_mockup_loader_stub_factory.
    data stub type ref to zif_mockup_loader_stub_dummy.
    data ml type ref to zcl_mockup_loader.
    data lt_exp type flighttab.
    data ls_exp like line of lt_exp.

    ml      = get_ml( ).
    factory = get_factory( ml ).
    lt_exp  = get_sflights( ml ).
    read table lt_exp into ls_exp index 1.

    factory->connect_method(
      i_sift_param      = 'I_CONNID'
      i_mock_tab_key    = 'CONNID'
      i_field_only      = 'PRICE'
      i_method_name     = 'RETURN_VALUE'
      i_mock_name       = 'EXAMPLE/sflight' ).

    stub ?= factory->generate_stub( ).
    cl_abap_unit_assert=>assert_equals(
      act = stub->return_value( i_connid = '1000' )
      exp = '100.00' ).
    cl_abap_unit_assert=>assert_equals(
      act = stub->return_value( i_connid = '2000' )
      exp = '200.00' ).
    cl_abap_unit_assert=>assert_equals(
      act = stub->return_value( i_connid = '9999' ) " Missing record, return initial
      exp = '' ).

  endmethod.

  method corresponding.

    data factory type ref to zcl_mockup_loader_stub_factory.
    data stub type ref to zif_mockup_loader_stub_dummy.
    data ml type ref to zcl_mockup_loader.
    data lt_exp_base type flighttab.
    data lt_exp type zif_mockup_loader_stub_dummy=>tt_sflight_extract.
    data lt_act type zif_mockup_loader_stub_dummy=>tt_sflight_extract.
    field-symbols <base> like line of lt_exp_base.
    field-symbols <exp> like line of lt_exp.

    ml          = get_ml( ).
    factory     = get_factory( ml ).
    lt_exp_base = get_sflights( ml ).
    delete lt_exp_base where fldate <> '20150101'.
    loop at lt_exp_base assigning <base>.
      append initial line to lt_exp assigning <exp>.
      move-corresponding <base> to <exp>.
    endloop.

    factory->connect_method(
      i_sift_param      = 'i_fldate'
      i_mock_tab_key    = 'fldate'
      i_method_name     = 'tab_return_extract_by_date'
      i_corresponding   = abap_true
      i_mock_name       = 'EXAMPLE/sflight' ).

    stub ?= factory->generate_stub( ).
    lt_act = stub->tab_return_extract_by_date( i_fldate = '20150101' ).
    cl_abap_unit_assert=>assert_equals( act = lt_act exp = lt_exp ).

  endmethod.


  method filter_by_range_param.

    data factory type ref to zcl_mockup_loader_stub_factory.
    data stub type ref to zif_mockup_loader_stub_dummy.
    data ml type ref to zcl_mockup_loader.
    data lt_exp type flighttab.
    data lt_act type flighttab.

    ml      = get_ml( ).
    factory = get_factory( ml ).
    lt_exp  = get_sflights( ml ).
    delete lt_exp where connid <> '1000'.

    data lr_range type zif_mockup_loader_stub_dummy=>ty_connid_range.
    field-symbols <r> like line of lr_range.
    append initial line to lr_range assigning <r>.
    <r>-sign   = 'I'.
    <r>-option = 'EQ'.
    <r>-low    = '1000'.

    factory->connect_method(
      i_sift_param      = 'ir_connid'
      i_mock_tab_key    = 'connid'
      i_method_name     = 'tab_return_by_range'
      i_mock_name       = 'EXAMPLE/sflight' ).

    stub ?= factory->generate_stub( ).
    lt_act = stub->tab_return_by_range( ir_connid = lr_range ).
    cl_abap_unit_assert=>assert_equals( act = lt_act exp = lt_exp ).

  endmethod.

  method controls.

    data lo_dc type ref to zcl_mockup_loader_stub_factory.
    data li_if type ref to zif_mockup_loader_stub_dummy.
    data lo_ml type ref to zcl_mockup_loader.
    data lt_exp type flighttab.
    data lo_ex type ref to zcx_mockup_loader_error.
    data li_control type ref to zif_mockup_loader_stub_control.

    lo_ml  = zcl_mockup_loader=>create(
      i_type = 'MIME'
      i_path = 'ZMOCKUP_LOADER_EXAMPLE' ).

    create object lo_dc
      exporting
        io_ml_instance   = lo_ml
        io_proxy_target  = lcl_test_proxy_target=>create( )
        i_interface_name = 'ZIF_MOCKUP_LOADER_STUB_DUMMY'.

    lo_dc->connect_method(
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

    lo_dc->forward_method( 'PROXY_TEST' ).

    li_if ?= lo_dc->generate_stub( ).
    li_control ?= li_if.

    data l_act type string.
    data lt_res type flighttab.
    data ls_dummy_res like line of lt_res.
    ls_dummy_res-carrid = 1.

    lo_ml->load_data(
      exporting
        i_obj    = 'EXAMPLE/sflight'
        i_strict = abap_false
      importing
        e_container = lt_exp ).

    " Calls before disable
    l_act = li_if->proxy_test( i_p1 = 'Hello' i_p2 = 123 ).
    cl_abap_unit_assert=>assert_equals( act = l_act exp = 'Hello 123' ).

    lt_res = li_if->tab_return( i_connid = '1000' ).
    cl_abap_unit_assert=>assert_equals( act = lt_res exp = lt_exp ).

    li_if->return_value( '1000' ). " for counter check

    li_control->disable( ).

    " Calls after disable
    lt_res = li_if->tab_return( i_connid = '1000' ).
    cl_abap_unit_assert=>assert_initial( act = lt_res ).

    append ls_dummy_res to lt_res.
    li_if->tab_export( exporting i_connid = '1000' importing e_tab = lt_res ).
    cl_abap_unit_assert=>assert_initial( act = lt_res ).

    append ls_dummy_res to lt_res.
    li_if->tab_change( exporting i_connid = '1000' changing c_tab = lt_res ).
    cl_abap_unit_assert=>assert_initial( act = lt_res ).

    l_act = li_if->proxy_test( i_p1 = 'Hello' i_p2 = 123 ).
    cl_abap_unit_assert=>assert_initial( l_act ).

    " Call counts
    cl_abap_unit_assert=>assert_equals(
      act = li_control->get_call_count( 'TAB_RETURN' )
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = li_control->get_call_count( 'TAB_EXPORT' )
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = li_control->get_call_count( 'TAB_CHANGE' )
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = li_control->get_call_count( 'PROXY_TEST' )
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = li_control->get_call_count( 'RETURN_VALUE' )
      exp = 1 ).

  endmethod.

  method const_value.

    data factory type ref to zcl_mockup_loader_stub_factory.
    data stub type ref to zif_mockup_loader_stub_dummy.
    data ml type ref to zcl_mockup_loader.
    data lv_act type sflight-price.

    ml      = get_ml( ).
    factory = get_factory( ml ).

    factory->connect_method(
      i_method_name = 'return_value'
      i_const_value = '12.34' ).

    stub ?= factory->generate_stub( ).
    lv_act = stub->return_value( i_connid = '1000' ).
    cl_abap_unit_assert=>assert_equals( act = lv_act exp = '12.34' ).

    " NEGATIVES
    data lx type ref to zcx_mockup_loader_error.
    factory = get_factory( ml ).

    try .
      factory->connect_method(
        i_method_name = 'return_value'
        i_mock_name   = 'non-empty'
        i_const_value = '12.34' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'CV' ).
    endtry.

    try .
      factory->connect_method(
        i_method_name = 'tab_return'
        i_const_value = '12.34' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'CE' ).
    endtry.

  endmethod.

  method connect_string_positive.

    data factory type ref to zcl_mockup_loader_stub_factory.
    data ml type ref to zcl_mockup_loader.
    data ls_config like line of factory->mt_config.

    ml      = get_ml( ).
    factory = get_factory(
      io_ml    = ml
      io_proxy = lcl_test_proxy_target=>create( ) ).

    factory->connect( 'tab_return -> mock_dummy' ).
    factory->connect( 'proxy_test -> *' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( factory->mt_config )
      exp = 2 ).

    read table factory->mt_config index 1 into ls_config.
    cl_abap_unit_assert=>assert_equals(
      act = ls_config-method_name
      exp = 'TAB_RETURN' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_config-mock_name
      exp = 'mock_dummy' ).

    read table factory->mt_config index 2 into ls_config.
    cl_abap_unit_assert=>assert_equals(
      act = ls_config-method_name
      exp = 'PROXY_TEST' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_config-as_proxy
      exp = abap_true ).

  endmethod.

  method parse_string_negative.

    data lx type ref to zcx_mockup_loader_error.

    try .
      zcl_mockup_loader_stub_factory=>parse_connect_string( 'tab_return -> mock_dummy (field' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'SF' ).
    endtry.

    try .
      zcl_mockup_loader_stub_factory=>parse_connect_string( 'tab_return -> mock_dummy [filter' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'SF' ).
    endtry.

    try .
      zcl_mockup_loader_stub_factory=>parse_connect_string( 'methodX -> mock_path [ field = "const ]' ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lx.
      cl_abap_unit_assert=>assert_equals( act = lx->code exp = 'SF' ).
    endtry.

  endmethod.

  method parse_string_positive.

    data ls_exp type zcl_mockup_loader_stub_base=>ty_mock_config.

    clear ls_exp.
    ls_exp-method_name   = 'methodX'.
    ls_exp-mock_name     = 'mock_path'.
    ls_exp-corresponding = ''.
    ls_exp-sift_param    = ''.
    ls_exp-mock_tab_key  = ''.
    ls_exp-field_only    = ''.
    ls_exp-const_value   = ''.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_mockup_loader_stub_factory=>parse_connect_string( 'methodX -> mock_path' )
      exp = ls_exp ).

    clear ls_exp.
    ls_exp-method_name   = 'methodX'.
    ls_exp-mock_name     = 'mock_path~txt'.
    ls_exp-corresponding = 'X'.
    ls_exp-sift_param    = ''.
    ls_exp-mock_tab_key  = ''.
    ls_exp-field_only    = ''.
    ls_exp-const_value   = ''.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_mockup_loader_stub_factory=>parse_connect_string( 'methodX -> ~mock_path~txt' )
      exp = ls_exp ).

    clear ls_exp.
    ls_exp-method_name   = 'methodX'.
    ls_exp-mock_name     = 'dir/mock_path'.
    ls_exp-corresponding = 'X'.
    ls_exp-sift_param    = ''.
    ls_exp-mock_tab_key  = ''.
    ls_exp-field_only    = ''.
    ls_exp-const_value   = ''.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_mockup_loader_stub_factory=>parse_connect_string( 'methodX -> dir/~mock_path' )
      exp = ls_exp ).

    clear ls_exp.
    ls_exp-method_name   = 'methodX'.
    ls_exp-mock_name     = 'mock_path'.
    ls_exp-corresponding = ''.
    ls_exp-sift_param    = ''.
    ls_exp-mock_tab_key  = ''.
    ls_exp-field_only    = 'result'.
    ls_exp-const_value   = ''.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_mockup_loader_stub_factory=>parse_connect_string( 'methodX -> mock_path (result)' )
      exp = ls_exp ).

    clear ls_exp.
    ls_exp-method_name   = 'methodX'.
    ls_exp-mock_name     = 'mock_path'.
    ls_exp-corresponding = ''.
    ls_exp-sift_param    = 'param'.
    ls_exp-mock_tab_key  = 'field'.
    ls_exp-field_only    = ''.
    ls_exp-const_value   = ''.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_mockup_loader_stub_factory=>parse_connect_string( 'methodX -> mock_path [ field = param ]' )
      exp = ls_exp ).

    clear ls_exp.
    ls_exp-method_name   = 'methodX'.
    ls_exp-mock_name     = 'mock_path'.
    ls_exp-corresponding = ''.
    ls_exp-sift_const    = 'const'.
    ls_exp-mock_tab_key  = 'field'.
    ls_exp-field_only    = ''.
    ls_exp-const_value   = ''.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_mockup_loader_stub_factory=>parse_connect_string( 'methodX -> mock_path [ field = "const" ]' )
      exp = ls_exp ).

    clear ls_exp.
    ls_exp-method_name   = 'methodX'.
    ls_exp-mock_name     = ''.
    ls_exp-corresponding = ''.
    ls_exp-sift_param    = ''.
    ls_exp-mock_tab_key  = ''.
    ls_exp-field_only    = ''.
    ls_exp-const_value   = 'value'.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_mockup_loader_stub_factory=>parse_connect_string( 'methodX -> =value' )
      exp = ls_exp ).

    clear ls_exp.
    ls_exp-method_name   = 'methodX'.
    ls_exp-mock_name     = '*'.
    ls_exp-corresponding = ''.
    ls_exp-sift_param    = ''.
    ls_exp-mock_tab_key  = ''.
    ls_exp-field_only    = ''.
    ls_exp-const_value   = ''.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_mockup_loader_stub_factory=>parse_connect_string( 'methodX -> *' )
      exp = ls_exp ).

    clear ls_exp.
    ls_exp-method_name   = 'methodX'.
    ls_exp-mock_name     = 'mock_path'.
    ls_exp-corresponding = ''.
    ls_exp-sift_param    = ''.
    ls_exp-mock_tab_key  = ''.
    ls_exp-field_only    = ''.
    ls_exp-const_value   = ''.
    ls_exp-deep          = abap_true.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_mockup_loader_stub_factory=>parse_connect_string( 'methodX -> :deep:mock_path' )
      exp = ls_exp ).

    clear ls_exp.
    ls_exp-method_name   = 'methodX'.
    ls_exp-mock_name     = 'mock_path'.
    ls_exp-corresponding = ''.
    ls_exp-sift_param    = ''.
    ls_exp-mock_tab_key  = ''.
    ls_exp-field_only    = ''.
    ls_exp-const_value   = ''.
    ls_exp-deep          = abap_true.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_mockup_loader_stub_factory=>parse_connect_string( 'methodX -> :deep: mock_path' )
      exp = ls_exp ).

    clear ls_exp.
    ls_exp-method_name   = 'methodX'.
    ls_exp-mock_name     = 'mock_path'.
    ls_exp-corresponding = abap_true.
    ls_exp-sift_param    = ''.
    ls_exp-mock_tab_key  = ''.
    ls_exp-field_only    = ''.
    ls_exp-const_value   = ''.
    ls_exp-deep          = abap_true.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_mockup_loader_stub_factory=>parse_connect_string( 'methodX -> ~:deep:mock_path' )
      exp = ls_exp ).

    clear ls_exp.
    ls_exp-method_name   = 'methodX'.
    ls_exp-mock_name     = 'xxx:deep:mock_path'. " Deep marker is just at the beginning
    ls_exp-corresponding = ''.
    ls_exp-sift_param    = ''.
    ls_exp-mock_tab_key  = ''.
    ls_exp-field_only    = ''.
    ls_exp-const_value   = ''.
    ls_exp-deep          = abap_false.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_mockup_loader_stub_factory=>parse_connect_string( 'methodX -> xxx:deep:mock_path' )
      exp = ls_exp ).

  endmethod.

endclass.
