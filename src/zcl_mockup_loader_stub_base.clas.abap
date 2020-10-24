class ZCL_MOCKUP_LOADER_STUB_BASE definition
  public
  abstract
  create public .

  public section.

    types:
      begin of ty_mock_config,
        method_name     type abap_methname,
        mock_name       type string,
        load_strict     type abap_bool,
        corresponding   type abap_bool,
        sift_param      type string,
        sift_const      type string,
        mock_tab_key    type abap_compname,
        output_param    type abap_parmname,
        output_pkind    type abap_parmkind,
        output_type     type ref to cl_abap_datadescr,
        as_proxy        type abap_bool,
        field_only      type abap_parmname,
        const_value     type string,
        deep            type abap_bool,
      end of ty_mock_config .
    types:
      tt_mock_config type standard table of ty_mock_config with key method_name .

    types:
      begin of ty_control,
        method_name     type abap_methname,
        is_disabled     type abap_bool,
        call_count      type i,
      end of ty_control .
    types:
      tt_control type standard table of ty_control with key method_name .

    interfaces ZIF_MOCKUP_LOADER_STUB_CONTROL.

    methods constructor
      importing
        !it_config type tt_mock_config
        !io_ml type ref to zcl_mockup_loader
        !io_proxy_target type ref to object optional .
  protected section.
    methods get_mock_data
      importing
        i_method_name type abap_methname
        i_sift_value  type any optional
      returning value(r_data) type ref to data
      raising zcx_mockup_loader_error.

    methods is_disabled
      importing
        i_method   type abap_methname
      returning
        value(rv_disabled) type abap_bool.

    methods increment_call_count
      importing
        i_method   type abap_methname.

    data mt_config type tt_mock_config.
    data mt_control type tt_control.
    data mo_ml      type ref to zcl_mockup_loader.
    data mo_proxy_target type ref to object .
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
ENDCLASS.



CLASS ZCL_MOCKUP_LOADER_STUB_BASE IMPLEMENTATION.


  method constructor.
    mt_config       = it_config.
    mo_ml           = io_ml.
    mo_proxy_target = io_proxy_target.
  endmethod.


  method get_control_for_method.

    field-symbols <ctl> like line of mt_control.
    data l_method like i_method.
    l_method = to_upper( i_method ).

    read table mt_control assigning <ctl> with key method_name = l_method.
    if sy-subrc <> 0.
      append initial line to mt_control assigning <ctl>.
      <ctl>-method_name = l_method.
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
    data ls_filter type zcl_mockup_loader_utils=>ty_filter.
    if <conf>-sift_param is not initial.
      data ld_type type ref to cl_abap_typedescr.
      ld_type = cl_abap_typedescr=>describe_by_data( i_sift_value ).
      if ld_type->kind = ld_type->kind_elem.
        ls_filter = zcl_mockup_loader_utils=>conv_single_val_to_filter(
          i_where = <conf>-mock_tab_key
          i_value = i_sift_value ).
      elseif ld_type->kind = ld_type->kind_table.
        ls_filter = zcl_mockup_loader_utils=>conv_range_to_filter(
          i_where = <conf>-mock_tab_key
          i_range = i_sift_value ).
      else.
        zcx_mockup_loader_error=>raise( msg = 'Unexpected sift param type' code = 'US' ).
      endif.
    endif.

    " if sift const, build filter. param xor const is checked in the factory
    if <conf>-sift_const is not initial.
      ls_filter = zcl_mockup_loader_utils=>conv_single_val_to_filter(
        i_where = <conf>-mock_tab_key
        i_value = <conf>-sift_const ).
    endif.

    " create data container and load mock
    create data r_data type handle <conf>-output_type.
    mo_ml->load_data(
      exporting
        i_obj    = <conf>-mock_name
        i_strict = <conf>-load_strict
        i_corresponding = boolc( <conf>-field_only is not initial or <conf>-corresponding = abap_true )
        i_deep   = <conf>-deep
        i_where  = ls_filter
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
