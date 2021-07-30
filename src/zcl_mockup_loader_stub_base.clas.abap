class ZCL_MOCKUP_LOADER_STUB_BASE definition
  public
  abstract
  create public .

  public section.

    interfaces zif_mockup_loader_stub_control.

    methods constructor
      importing
        !it_config type zif_mockup_loader=>tt_mock_config
        !ii_ml type ref to zif_mockup_loader
        !io_proxy_target type ref to object optional .

  protected section.

    types:
      begin of ty_control,
        method_name     type abap_methname,
        is_disabled     type abap_bool,
        call_count      type i,
      end of ty_control .
    types:
      tt_control type standard table of ty_control with key method_name .
    types:
      tty_control_by_meth_name type sorted table of ty_control with unique key method_name .

    methods get_mock_data
      importing
        i_method_name type abap_methname
        i_sift_value  type any optional
      returning
        value(r_data) type ref to data
      raising
        zcx_mockup_loader_error.

    methods is_disabled
      importing
        i_method   type abap_methname
      returning
        value(rv_disabled) type abap_bool.

    methods increment_call_count
      importing
        i_method   type abap_methname.

    data mt_config  type zif_mockup_loader=>tty_mock_config_by_methname.
    data mt_control type tty_control_by_meth_name.
    data mi_ml      type ref to zif_mockup_loader.
    data mo_proxy_target type ref to object.

  private section.

    methods set_disabled
      importing
        i_method   type abap_methname
        i_disabled type abap_bool.

    methods get_control_for_method
      importing
        i_method   type abap_methname
      returning
        value(rv_ref) type ref to ty_control.

    methods build_filter_item
      importing
        is_filter_param type zif_mockup_loader=>ty_stub_filter_param
        ir_sift_value type ref to data
      returning
        value(rs_filter) type zif_mockup_loader=>ty_filter
      raising
        zcx_mockup_loader_error.

    methods build_filter
      importing
        is_conf like line of mt_config
        i_sift_value  type any optional
      returning
        value(rt_filter) type zif_mockup_loader=>tt_filter
      raising
        zcx_mockup_loader_error.

ENDCLASS.



CLASS ZCL_MOCKUP_LOADER_STUB_BASE IMPLEMENTATION.


  method build_filter.

    data ls_filter like line of rt_filter.
    data ls_filter_param like line of is_conf-filter.
    data lr_sift_value type ref to data.

    if is_conf-mock_tab_key is not initial.
      ls_filter_param-mock_tab_key = is_conf-mock_tab_key.
      ls_filter_param-sift_param   = is_conf-sift_param.
      ls_filter_param-sift_const   = is_conf-sift_const.
      get reference of i_sift_value into lr_sift_value.

      ls_filter = build_filter_item(
        is_filter_param = ls_filter_param
        ir_sift_value   = lr_sift_value ).

      if ls_filter is not initial.
        append ls_filter to rt_filter.
      endif.

    elseif is_conf-filter is not initial.

      field-symbols <sift_values> type zif_mockup_loader=>tty_stub_sift_values.
      assign i_sift_value to <sift_values>.
      if sy-subrc <> 0.
        zcx_mockup_loader_error=>raise( msg = 'Unexpected sift param table' code = 'UT' ).
      endif.

      data lv_value_index type i value 1.
      loop at is_conf-filter into ls_filter_param.
        if ls_filter_param-sift_param is not initial.
          read table <sift_values> into lr_sift_value index lv_value_index.
          if sy-subrc <> 0.
            zcx_mockup_loader_error=>raise( msg = 'Sift param count <> filter count' code = 'SC' ).
          endif.
          lv_value_index = lv_value_index + 1.
        endif.

        ls_filter = build_filter_item(
          is_filter_param = ls_filter_param
          ir_sift_value   = lr_sift_value ).

        if ls_filter is not initial.
          append ls_filter to rt_filter.
        endif.
      endloop.

    endif.

  endmethod.


  method build_filter_item.

    data ld_type type ref to cl_abap_typedescr.
    field-symbols <sift_value> type any.

    assign ir_sift_value->* to <sift_value>.

    if is_filter_param-sift_param is not initial.
      ld_type = cl_abap_typedescr=>describe_by_data( <sift_value> ).
      if ld_type->kind = ld_type->kind_elem.
        rs_filter = zcl_mockup_loader_utils=>conv_single_val_to_filter(
          i_where = is_filter_param-mock_tab_key
          i_value = <sift_value> ).
      elseif ld_type->kind = ld_type->kind_table.
        rs_filter = zcl_mockup_loader_utils=>conv_range_to_filter(
          i_where = is_filter_param-mock_tab_key
          i_range = <sift_value> ).
      else.
        zcx_mockup_loader_error=>raise( msg = 'Unexpected sift param type' code = 'US' ).
      endif.
    endif.

    " if sift const, build filter. param xor const is checked in the factory
    if is_filter_param-sift_const is not initial.
      rs_filter = zcl_mockup_loader_utils=>conv_single_val_to_filter(
        i_where = is_filter_param-mock_tab_key
        i_value = is_filter_param-sift_const ).
    endif.

  endmethod.


  method constructor.
    mt_config       = it_config.
    mi_ml           = ii_ml.
    mo_proxy_target = io_proxy_target.
  endmethod.


  method get_control_for_method.

    field-symbols <ctl> like line of mt_control.
    data ls_ctl like line of mt_control.
    data l_method like i_method.
    l_method = to_upper( i_method ).

    read table mt_control assigning <ctl> with key method_name = l_method.
    if sy-subrc <> 0.
      ls_ctl-method_name = l_method.
      insert ls_ctl into table mt_control assigning <ctl>.
    endif.
    get reference of <ctl> into rv_ref.

  endmethod.


  method get_mock_data.
    " find config
    field-symbols <conf> like line of mt_config.
    read table mt_config with key method_name = i_method_name assigning <conf>.
    if <conf> is not assigned.
      return.
    endif.

    " if sift, build filter
    data lt_filter type zif_mockup_loader=>tt_filter.
    lt_filter = build_filter(
      is_conf      = <conf>
      i_sift_value = i_sift_value ).

    " create data container and load mock
    create data r_data type handle <conf>-output_type.
    mi_ml->load_data(
      exporting
        i_obj    = <conf>-mock_name
        i_strict = <conf>-load_strict
        i_corresponding = boolc( <conf>-field_only is not initial or <conf>-corresponding = abap_true )
        i_deep   = <conf>-deep
        i_where  = lt_filter
      importing
        e_container = r_data ).

    if <conf>-field_only is not initial.
      field-symbols <data> type any.
      field-symbols <field> type any.
      assign r_data->* to <data>.
      assign component <conf>-field_only of structure <data> to <field>.
      data lr_field type ref to data.
      create data lr_field like <field>.
      assign lr_field->* to <data>.
      <data> = <field>.
      r_data = lr_field. " smoking upside down in the sky ...
    endif.

  endmethod.


  method increment_call_count.
    data lr_control type ref to ty_control.
    lr_control = get_control_for_method( i_method ).
    lr_control->call_count = lr_control->call_count + 1.
  endmethod.


  method is_disabled.
    data lr_control type ref to ty_control.
    lr_control = get_control_for_method( i_method ).
    rv_disabled = lr_control->is_disabled.
  endmethod.


  method set_disabled.

    data lr_control type ref to ty_control.
    field-symbols <conf> like line of mt_config.

    if i_method is not initial.
      lr_control = get_control_for_method( i_method ).
      lr_control->is_disabled = i_disabled.
    else.
      loop at mt_config assigning <conf>.
        lr_control = get_control_for_method( <conf>-method_name ).
        lr_control->is_disabled = i_disabled.
      endloop.
    endif.

  endmethod.


  method zif_mockup_loader_stub_control~disable.
    set_disabled(
      i_method = i_method
      i_disabled = abap_true ).
  endmethod.


  method zif_mockup_loader_stub_control~enable.
    set_disabled(
      i_method = i_method
      i_disabled = abap_false ).
  endmethod.


  method zif_mockup_loader_stub_control~get_call_count.
    data lr_control type ref to ty_control.
    lr_control = get_control_for_method( i_method ).
    rv_call_count = lr_control->call_count.
  endmethod.


  method zif_mockup_loader_stub_control~set_proxy_target.
    mo_proxy_target = io_proxy_target.
  endmethod.
ENDCLASS.
