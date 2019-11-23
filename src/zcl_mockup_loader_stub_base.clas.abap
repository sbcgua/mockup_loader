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
        mock_tab_key    type abap_compname,
        output_param    type abap_parmname,
        output_pkind    type abap_parmkind,
        output_type     type ref to cl_abap_datadescr,
        as_proxy        type abap_bool,
        field_only      type abap_parmname,
        is_disabled     type abap_bool,
      end of ty_mock_config .
    types:
      tt_mock_config type standard table of ty_mock_config with key method_name .

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

    methods set_disabled
      importing
        i_method   type abap_methname
        i_disabled type abap_bool.

    data mt_config type tt_mock_config.
    data mo_ml      type ref to zcl_mockup_loader.
    data mo_proxy_target type ref to object .
  private section.
ENDCLASS.



CLASS ZCL_MOCKUP_LOADER_STUB_BASE IMPLEMENTATION.


  method constructor.
    mt_config       = it_config.
    mo_ml           = io_ml.
    mo_proxy_target = io_proxy_target.
  endmethod.


  method get_mock_data.
    " find config
    field-symbols <conf> like line of mt_config.
    read table mt_config with key method_name = i_method_name assigning <conf>.
    if <conf> is not assigned or <conf>-is_disabled = abap_true.
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

    " create data container and load mock
    create data r_data type handle <conf>-output_type.
    mo_ml->load_data(
      exporting
        i_obj    = <conf>-mock_name
        i_strict = <conf>-load_strict
        i_corresponding = boolc( <conf>-field_only is not initial or <conf>-corresponding = abap_true )
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


  method set_disabled.

    field-symbols <conf> like line of mt_config.

    if i_method is not initial.
      read table mt_config assigning <conf> with key method_name = i_method.
      if sy-subrc = 0.
        <conf>-is_disabled = i_disabled.
      endif.
    else.
      loop at mt_config assigning <conf>.
        <conf>-is_disabled = i_disabled.
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
ENDCLASS.
