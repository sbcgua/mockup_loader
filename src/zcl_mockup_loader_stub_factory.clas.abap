class ZCL_MOCKUP_LOADER_STUB_FACTORY definition
  public .

public section.

  methods CONSTRUCTOR
    importing
      !I_INTERFACE_NAME type SEOCLSNAME
      !IO_ML_INSTANCE type ref to ZCL_MOCKUP_LOADER
    raising
      ZCX_MOCKUP_LOADER_ERROR .
  methods CONNECT_METHOD
    importing
      !I_METHOD_NAME type ABAP_METHNAME
      !I_MOCK_NAME type STRING
      !I_LOAD_STRICT type ABAP_BOOL default ABAP_FALSE
      !I_SIFT_PARAM type ABAP_PARMNAME optional
      !I_MOCK_TAB_KEY type ABAP_COMPNAME optional
      !I_OUTPUT_PARAM type ABAP_PARMNAME optional
    returning
      value(R_INSTANCE) type ref to ZCL_MOCKUP_LOADER_STUB_FACTORY
    raising
      ZCX_MOCKUP_LOADER_ERROR .
  methods GENERATE_STUB
    returning
      value(R_STUB) type ref to OBJECT .
  class-methods GENERATE_PARAMS
    importing
      !ID_IF_DESC type ref to CL_ABAP_OBJECTDESCR
      !I_METHOD type ABAP_METHNAME
    returning
      value(RT_PARAMS) type ABAP_PARMBIND_TAB .
  class-methods BUILD_CONFIG
    importing
      !ID_IF_DESC type ref to CL_ABAP_OBJECTDESCR
      !I_CONFIG type ZCL_MOCKUP_LOADER_STUB_BASE=>TY_MOCK_CONFIG
    returning
      value(R_CONFIG) type ZCL_MOCKUP_LOADER_STUB_BASE=>TY_MOCK_CONFIG
    raising
      ZCX_MOCKUP_LOADER_ERROR .
protected section.

  data MV_INTERFACE_NAME type SEOCLSNAME .
  data MT_CONFIG type ZCL_MOCKUP_LOADER_STUB_BASE=>TT_MOCK_CONFIG .
  data MO_ML type ref to ZCL_MOCKUP_LOADER .
  data MD_IF_DESC type ref to CL_ABAP_OBJECTDESCR .
private section.
ENDCLASS.



CLASS ZCL_MOCKUP_LOADER_STUB_FACTORY IMPLEMENTATION.


method build_config.
  data ld_type type ref to cl_abap_typedescr.

  " Config basic checks
  if i_config-method_name is initial.
    zcx_mockup_loader_error=>raise(
      msg  = 'Specify method_name'
      code = 'MM' ). "#EC NOTEXT
  elseif i_config-mock_name is initial.
    zcx_mockup_loader_error=>raise(
      msg  = 'Specify mock_name'
      code = 'MK' ). "#EC NOTEXT
  elseif boolc( i_config-sift_param is initial ) <> boolc( i_config-mock_tab_key is initial ). " XOR
    zcx_mockup_loader_error=>raise(
      msg  = 'Specify both i_sift_param and i_mock_tab_key'
      code = 'MS' ). "#EC NOTEXT
  endif.

  " find method, check if exists
  field-symbols <method> like line of md_if_desc->methods.
  read table id_if_desc->methods assigning <method> with key name = i_config-method_name.
  if <method> is not assigned.
    zcx_mockup_loader_error=>raise(
      msg  = |Method { i_config-method_name } not found|
      code = 'MF' ). "#EC NOTEXT
  endif.

  " check if sift param
  field-symbols <param> like line of <method>-parameters.
  if i_config-sift_param is not initial.
    read table <method>-parameters with key name = i_config-sift_param assigning <param>.
    if sy-subrc is not initial.
      zcx_mockup_loader_error=>raise(
        msg  = |Param { i_config-sift_param } not found|
        code = 'PF' ). "#EC NOTEXT
    endif.

    ld_type = id_if_desc->get_method_parameter_type(
      p_method_name    = <method>-name
      p_parameter_name = <param>-name ).
    if ld_type->kind <> cl_abap_typedescr=>kind_elem.
      zcx_mockup_loader_error=>raise(
        msg  = |Param { i_config-sift_param } must be elementary|
        code = 'PE' ). "#EC NOTEXT
    endif.
  endif.

  r_config = i_config.

  " Check output param
  if r_config-output_param is initial.
    read table <method>-parameters with key parm_kind = 'R' assigning <param>. " returning
    if sy-subrc is not initial.
      zcx_mockup_loader_error=>raise(
        msg  = 'Method has no returning params and output_param was not specified'
        code = 'MR' ). "#EC NOTEXT
    endif.
    r_config-output_param = <param>-name.
  else.
    read table <method>-parameters with key name = r_config-output_param assigning <param>.
    if sy-subrc is not initial.
      zcx_mockup_loader_error=>raise(
        msg  = |Param { r_config-output_param } not found|
        code = 'PF' ). "#EC NOTEXT
    endif.
  endif.

  if <param>-parm_kind = 'I'.
    zcx_mockup_loader_error=>raise(
      msg  = |Param { r_config-output_param } is importing|
      code = 'PI' ). "#EC NOTEXT
  endif.
  r_config-output_pkind = <param>-parm_kind.

  ld_type = id_if_desc->get_method_parameter_type(
    p_method_name    = <method>-name
    p_parameter_name = <param>-name ).
  if not ld_type->kind ca 'ST'. " Table or structure
    zcx_mockup_loader_error=>raise(
      msg  = |Param { r_config-output_param } must be table or structure|
      code = 'PT' ). "#EC NOTEXT
  endif.
  r_config-output_type ?= ld_type.

endmethod.


method connect_method.
  data ls_config like line of mt_config.
  ls_config-method_name  = to_upper( i_method_name ).
  ls_config-mock_name    = i_mock_name.
  ls_config-load_strict  = i_load_strict.
  ls_config-sift_param   = to_upper( i_sift_param ).
  ls_config-mock_tab_key = to_upper( i_mock_tab_key ).
  ls_config-output_param = to_upper( i_output_param ).

  read table mt_config with key method_name = ls_config-method_name transporting no fields.
  if sy-subrc is initial.
    zcx_mockup_loader_error=>raise(
      msg  = |Method { ls_config-method_name } is already connected|
      code = 'MC' ). "#EC NOTEXT
  endif.

  " Validate and save config
  ls_config = build_config(
    id_if_desc = md_if_desc
    i_config   = ls_config ).
  append ls_config to mt_config.

  r_instance = me.
endmethod.


method constructor.
  data ld_desc type ref to cl_abap_typedescr.
  ld_desc = cl_abap_typedescr=>describe_by_name( i_interface_name ).
  if ld_desc->kind <> cl_abap_typedescr=>kind_intf.
    zcx_mockup_loader_error=>raise(
      msg  = |{ i_interface_name } is not interface|
      code = 'IF' ). "#EC NOTEXT
  endif.

  me->md_if_desc       ?= ld_desc.
  me->mo_ml             = io_ml_instance.
  me->mv_interface_name = i_interface_name.
endmethod.


method generate_params.

  field-symbols <method> like line of id_if_desc->methods.
  read table id_if_desc->methods assigning <method> with key name = i_method.

  data ls_param like line of rt_params.
  data ld_data  type ref to cl_abap_datadescr.
  field-symbols <param> like line of <method>-parameters.

  loop at <method>-parameters assigning <param> where is_optional = abap_false.
    if <param>-parm_kind ca 'IC'. " importing and changing
      if <param>-type_kind ca '~&?#$'. " any, clike, csequence, data, simple
        ld_data ?= cl_abap_typedescr=>describe_by_name( 'C' ).
      else.
        ld_data = id_if_desc->get_method_parameter_type(
          p_method_name    = <method>-name
          p_parameter_name = <param>-name ).
      endif.

      ls_param-name = <param>-name.
      case <param>-parm_kind.
        when 'I'. ls_param-kind = 'E'.
        when 'C'. ls_param-kind = 'C'.
      endcase.
      create data ls_param-value type handle ld_data.
      insert ls_param into table rt_params.
    endif.
  endloop.

endmethod.


method generate_stub.

  define _src.
    append &1 to lt_src.
  end-of-definition.

  data:
      ln            type string,
      lv_message    type string,
      l_prog_name   type string,
      l_class_name  type string,
      lt_src        type string_table.

  field-symbols <method> like line of md_if_desc->methods.
  field-symbols <conf> like line of mt_config.

  _src 'program.'.
  _src 'class lcl_mockup_loader_stub definition final'.
  _src '  inheriting from zcl_mockup_loader_stub_base.'.
  _src '  public section.'.
  ln = |    interfaces { mv_interface_name }.|. _src ln.
  _src 'endclass.'.

  _src 'class lcl_mockup_loader_stub implementation.'.

  loop at md_if_desc->methods assigning <method>.
    read table mt_config assigning <conf> with key method_name = <method>-name.
    if sy-subrc is initial.
      ln = |  method { mv_interface_name }~{ <method>-name }.|. _src ln.
      _src '    data lr_data type ref to data.'.
      _src '    lr_data = get_mock_data('.
      if <conf>-sift_param is not initial.
        ln = |      i_sift_value  = { <conf>-sift_param }|. _src ln.
      endif.
      ln = |      i_method_name = '{ <method>-name }' ).|. _src ln.
      _src '    field-symbols <container> type any.'.
      _src '    assign lr_data->* to <container>.'.
      ln = |    { <conf>-output_param } = <container>.|. _src ln.
      _src '  endmethod.'.
    else.
      ln = |  method { mv_interface_name }~{ <method>-name }.|. _src ln.
      _src '  endmethod.'.
    endif.
  endloop.

  _src 'endclass.'.

  generate subroutine pool lt_src name l_prog_name MESSAGE lv_message. "#EC CI_GENERATE
  l_class_name = |\\PROGRAM={ l_prog_name }\\CLASS=LCL_MOCKUP_LOADER_STUB|.

  create object r_stub type (l_class_name)
    exporting
      it_config = mt_config
      io_ml     = mo_ml.

endmethod.
ENDCLASS.
