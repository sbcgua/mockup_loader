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
        sift_param      type string,
        mock_tab_key    type abap_compname,
        output_param    type abap_parmname,
        output_pkind    type abap_parmkind,
        output_type     type ref to cl_abap_datadescr,
        as_proxy        type abap_bool,
      end of ty_mock_config .
    types:
      tt_mock_config type standard table of ty_mock_config with key method_name .

    methods constructor
      importing
        !it_config type tt_mock_config
        !io_ml type ref to zcl_mockup_loader
        !io_proxy_target type ref to object optional .
  protected section.
    methods get_mock_data
      importing
        i_method_name type abap_methname
        i_sift_value  type simple optional
      returning value(r_data) type ref to data
      raising zcx_mockup_loader_error.

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
    if <conf> is not assigned.
      return.
    endif.

    " if sift, build filter
    if <conf>-sift_param is not initial.
      data lt_filter type zcl_mockup_loader_utils=>tt_filter.
      lt_filter = zcl_mockup_loader_utils=>build_filter(
        i_where        = <conf>-mock_tab_key
        i_single_value = i_sift_value ).
    endif.

    " create data container and load mock
    create data r_data type handle <conf>-output_type.
    mo_ml->load_data(
      exporting
        i_obj    = <conf>-mock_name
        i_strict = <conf>-load_strict
        i_where  = lt_filter
      importing
        e_container = r_data ).

  endmethod.
ENDCLASS.
