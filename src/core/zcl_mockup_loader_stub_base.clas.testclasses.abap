class lcl_mockup_loader_stub_final definition deferred.
class ltcl_mockup_stub_base_test definition final
  for testing
  duration short
  risk level harmless.

  private section.
    class-data go_ml type ref to zcl_mockup_loader.
    class-data gt_flights type flighttab.

    data mo_stub_cut type ref to lcl_mockup_loader_stub_final.

    class-methods class_setup raising zcx_mockup_loader_error.
    methods setup raising zcx_mockup_loader_error.

    methods get_mock_data_basic for testing raising zcx_mockup_loader_error.
    methods get_mock_data_sifted for testing raising zcx_mockup_loader_error.
    methods build_filter_negative for testing raising zcx_mockup_loader_error.
    methods get_mock_data_sifted_multi for testing raising zcx_mockup_loader_error.
    methods get_sifted_multi_const for testing raising zcx_mockup_loader_error.

    methods control_disable for testing raising zcx_mockup_loader_error.
    methods control_call_count for testing raising zcx_mockup_loader_error.
    methods control_set_proxy for testing raising zcx_mockup_loader_error.
endclass.

**********************************************************************

class lcl_mockup_loader_stub_final definition final
  inheriting from zcl_mockup_loader_stub_base
  friends ltcl_mockup_stub_base_test.
endclass.

**********************************************************************

class ltcl_mockup_stub_base_test implementation.

  method class_setup.

    go_ml = zcl_mockup_loader=>create(
      i_type     = 'MIME'
      i_path     = 'ZMOCKUP_LOADER_EXAMPLE'
      i_encoding = zif_mockup_loader=>encoding_utf16 ).

    go_ml->load_data(
      exporting
        i_obj    = 'EXAMPLE/sflight'
        i_strict = abap_false
      importing
        e_container = gt_flights ).

  endmethod.

  method setup.

    data lt_config type zif_mockup_loader=>tt_mock_config.
    data ls_conf like line of lt_config.
    data lt_dummy type flighttab.
    field-symbols <filter> like line of ls_conf-filter.

    " Common
    ls_conf-output_type ?= cl_abap_typedescr=>describe_by_data( lt_dummy ).
    ls_conf-mock_name    = 'EXAMPLE/sflight'.

    ls_conf-method_name  = 'METHOD_SIMPLE'.
    append ls_conf to lt_config.

    ls_conf-method_name  = 'METHOD_SIFTED'.
    ls_conf-mock_tab_key = 'CONNID'.
    ls_conf-sift_param   = 'I_CONNID'.
    append ls_conf to lt_config.

    ls_conf-method_name  = 'METHOD_SIFTED_MULTI'.
    clear: ls_conf-mock_tab_key, ls_conf-sift_param.
    append initial line to ls_conf-filter assigning <filter>.
    <filter>-mock_tab_key = 'CONNID'.
    <filter>-sift_param   = 'I_CONNID'.
    append initial line to ls_conf-filter assigning <filter>.
    <filter>-mock_tab_key = 'FLDATE'.
    <filter>-sift_param   = 'I_FLDATE'.
    append ls_conf to lt_config.

    ls_conf-method_name  = 'METHOD_SIFTED_MULTI_C'.
    clear ls_conf-filter.
    append initial line to ls_conf-filter assigning <filter>.
    <filter>-mock_tab_key = 'CONNID'.
    <filter>-sift_param   = 'I_CONNID'.
    append initial line to ls_conf-filter assigning <filter>.
    <filter>-mock_tab_key = 'FLDATE'.
    <filter>-sift_const   = '20150101'.
    append ls_conf to lt_config.

    create object mo_stub_cut
      exporting
        it_config = lt_config
        ii_ml     = go_ml.

  endmethod.

  method get_mock_data_basic.

    data lt_exp type flighttab.
    data lr_act type ref to data.
    field-symbols <act> type flighttab.

    lt_exp = gt_flights.

    lr_act = mo_stub_cut->get_mock_data(
      i_method_name = 'METHOD_SIMPLE'
      i_sift_value  = '1000' ).
    assign lr_act->* to <act>.
    cl_abap_unit_assert=>assert_equals(
      act = <act>
      exp = lt_exp ).

    lr_act = mo_stub_cut->get_mock_data(
      i_method_name = 'METHOD_MISSING'
      i_sift_value  = '1000' ).
    cl_abap_unit_assert=>assert_initial( lr_act ).

  endmethod.

  method get_mock_data_sifted.

    data lt_exp type flighttab.
    data lr_act type ref to data.
    field-symbols <act> type flighttab.

    lt_exp = gt_flights.
    delete lt_exp where connid <> '1000'.
    lr_act = mo_stub_cut->get_mock_data(
      i_method_name = 'METHOD_SIFTED'
      i_sift_value  = '1000' ).
    assign lr_act->* to <act>.
    cl_abap_unit_assert=>assert_equals(
      act = <act>
      exp = lt_exp ).

    lt_exp = gt_flights.
    delete lt_exp where connid <> '2000'.
    lr_act = mo_stub_cut->get_mock_data(
      i_method_name = 'METHOD_SIFTED'
      i_sift_value  = '2000' ).
    assign lr_act->* to <act>.
    cl_abap_unit_assert=>assert_equals(
      act = lines( <act> )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = <act>
      exp = lt_exp ).

  endmethod.

  method get_mock_data_sifted_multi.

    data lt_exp type flighttab.
    data lr_act type ref to data.
    field-symbols <act> type flighttab.
    data lt_filter_refs type zif_mockup_loader=>tty_stub_sift_values.
    data lr like line of lt_filter_refs.
    data ls_filter_values like line of lt_exp.

    lt_exp = gt_flights.
    delete lt_exp where not ( connid = '2000' and fldate = '20150102' ).

    ls_filter_values-connid = '2000'.
    ls_filter_values-fldate = '20150102'.
    get reference of ls_filter_values-connid into lr.
    append lr to lt_filter_refs.
    get reference of ls_filter_values-fldate into lr.
    append lr to lt_filter_refs.

    lr_act = mo_stub_cut->get_mock_data(
      i_method_name = 'METHOD_SIFTED_MULTI'
      i_sift_value  = lt_filter_refs ).

    assign lr_act->* to <act>.
    cl_abap_unit_assert=>assert_equals(
      act = lines( <act> )
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = <act>
      exp = lt_exp ).

  endmethod.

  method get_sifted_multi_const.

    data lt_exp type flighttab.
    data lr_act type ref to data.
    field-symbols <act> type flighttab.
    data lt_filter_refs type zif_mockup_loader=>tty_stub_sift_values.
    data lr like line of lt_filter_refs.
    data ls_filter_values like line of lt_exp.

    lt_exp = gt_flights.
    delete lt_exp where not ( connid = '1000' and fldate = '20150101' ).

    ls_filter_values-connid = '1000'.
    get reference of ls_filter_values-connid into lr.
    append lr to lt_filter_refs.

    lr_act = mo_stub_cut->get_mock_data(
      i_method_name = 'METHOD_SIFTED_MULTI_C'
      i_sift_value  = lt_filter_refs ).

    assign lr_act->* to <act>.
    cl_abap_unit_assert=>assert_equals(
      act = lines( <act> )
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = <act>
      exp = lt_exp ).

    " Test 2

    lt_exp = gt_flights.
    delete lt_exp where not ( connid = '2000' and fldate = '20150101' ).

    clear lt_filter_refs.
    ls_filter_values-connid = '2000'.
    get reference of ls_filter_values-connid into lr.
    append lr to lt_filter_refs.

    lr_act = mo_stub_cut->get_mock_data(
      i_method_name = 'METHOD_SIFTED_MULTI_C'
      i_sift_value  = lt_filter_refs ).

    assign lr_act->* to <act>.
    cl_abap_unit_assert=>assert_equals(
      act = lines( <act> )
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = <act>
      exp = lt_exp ).

  endmethod.

  method build_filter_negative.

    data lx type ref to zcx_mockup_loader_error.
    data ls_dummy like line of gt_flights.
    data lt_values type zif_mockup_loader=>tty_stub_sift_values.

    try.
      mo_stub_cut->get_mock_data(
        i_method_name = 'METHOD_SIFTED'
        i_sift_value  = ls_dummy ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->code
        exp = 'US' ).
    endtry.

* It'd be nice to add this check
* But it requires extra type check in build_filter
* which maybe have some performance impact
* though needs to be measured

*    try.
*      mo_stub_cut->get_mock_data(
*        i_method_name = 'METHOD_SIFTED_MULTI'
*        i_sift_value  = ls_dummy ).
*      cl_abap_unit_assert=>fail( ).
*    catch zcx_mockup_loader_error into lx.
*      cl_abap_unit_assert=>assert_equals(
*        act = lx->code
*        exp = 'UT' ).
*    endtry.

    try.
      mo_stub_cut->get_mock_data(
        i_method_name = 'METHOD_SIFTED_MULTI'
        i_sift_value  = lt_values ).
      cl_abap_unit_assert=>fail( ).
    catch zcx_mockup_loader_error into lx.
      cl_abap_unit_assert=>assert_equals(
        act = lx->code
        exp = 'SC' ).
    endtry.

  endmethod.

  method control_disable.

    data li_control type ref to zif_mockup_loader_stub_control.
    data ls_control like line of mo_stub_cut->mt_control.

    cl_abap_unit_assert=>assert_equals( act = lines( mo_stub_cut->mt_control ) exp = 0 ).

    " Disable all
    li_control ?= mo_stub_cut.
    li_control->disable( ).

    cl_abap_unit_assert=>assert_equals( act = lines( mo_stub_cut->mt_control ) exp = 4 ).
    read table mo_stub_cut->mt_control into ls_control with key method_name = 'METHOD_SIMPLE'.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_control-is_disabled exp = abap_true ).
    read table mo_stub_cut->mt_control into ls_control with key method_name = 'METHOD_SIFTED'.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_control-is_disabled exp = abap_true ).

    " Enable all
    li_control->enable( ).

    cl_abap_unit_assert=>assert_equals( act = lines( mo_stub_cut->mt_control ) exp = 4 ).
    read table mo_stub_cut->mt_control into ls_control with key method_name = 'METHOD_SIMPLE'.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_control-is_disabled exp = abap_false ).
    read table mo_stub_cut->mt_control into ls_control with key method_name = 'METHOD_SIFTED'.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_control-is_disabled exp = abap_false ).

    " Disable one
    li_control->disable( 'METHOD_SIMPLE' ).

    cl_abap_unit_assert=>assert_equals( act = lines( mo_stub_cut->mt_control ) exp = 4 ).
    read table mo_stub_cut->mt_control into ls_control with key method_name = 'METHOD_SIMPLE'.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_control-is_disabled exp = abap_true ).
    read table mo_stub_cut->mt_control into ls_control with key method_name = 'METHOD_SIFTED'.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals( act = ls_control-is_disabled exp = abap_false ).

  endmethod.

  method control_call_count.

    data lt_exp type flighttab.
    data lr_act type ref to data.
    field-symbols <act> type flighttab.
    data li_control type ref to zif_mockup_loader_stub_control.

    li_control ?= mo_stub_cut.
    cl_abap_unit_assert=>assert_equals(
      act = li_control->get_call_count( 'METHOD_SIMPLE' )
      exp = 0 ).

    mo_stub_cut->increment_call_count( 'METHOD_SIMPLE' ).
    cl_abap_unit_assert=>assert_equals(
      act = li_control->get_call_count( 'METHOD_SIMPLE' )
      exp = 1 ).

  endmethod.

  method control_set_proxy.

    cl_abap_unit_assert=>assert_initial( mo_stub_cut->mo_proxy_target ).

    mo_stub_cut->zif_mockup_loader_stub_control~set_proxy_target( me ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_stub_cut->mo_proxy_target
      exp = me ).

  endmethod.
endclass.
