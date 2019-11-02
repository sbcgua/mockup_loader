class ZCL_MOCKUP_LOADER_STUB_FACTORY definition
  public
  create public .

  public section.

    methods constructor
      importing
        !i_interface_name type seoclsname
        !io_ml_instance type ref to zcl_mockup_loader
        !io_proxy_target type ref to object optional
      raising
        zcx_mockup_loader_error .
    methods connect_method
      importing
        !i_method_name type abap_methname
        !i_mock_name type string
        !i_load_strict type abap_bool default abap_false
        !i_sift_param type string optional
        !i_mock_tab_key type abap_compname optional
        !i_output_param type abap_parmname optional
      returning
        value(r_instance) type ref to zcl_mockup_loader_stub_factory
      raising
        zcx_mockup_loader_error .
    methods forward_method
      importing
        !i_method_name type abap_methname
      returning
        value(r_instance) type ref to zcl_mockup_loader_stub_factory
      raising
        zcx_mockup_loader_error .
    methods generate_stub
      returning
        value(r_stub) type ref to object .
    class-methods build_config
      importing
        !id_if_desc type ref to cl_abap_objectdescr
        !i_config type zcl_mockup_loader_stub_base=>ty_mock_config
      returning
        value(r_config) type zcl_mockup_loader_stub_base=>ty_mock_config
      raising
        zcx_mockup_loader_error .
  protected section.

    data mv_interface_name type seoclsname .
    data mt_config type zcl_mockup_loader_stub_base=>tt_mock_config .
    data mo_ml type ref to zcl_mockup_loader .
    data md_if_desc type ref to cl_abap_objectdescr .
    data mo_proxy_target type ref to object .

  private section.
    data mt_src type string_table.
    methods _src
      importing
        iv_src_line type string.

    class-methods validate_sift_param
      importing
        id_if_desc type ref to cl_abap_objectdescr
        iv_method_name type abap_methname
        iv_param_name type string
      raising
        zcx_mockup_loader_error .

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
      validate_sift_param(
        id_if_desc = id_if_desc
        iv_method_name = i_config-method_name
        iv_param_name = i_config-sift_param ).
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
    me->mo_proxy_target   = io_proxy_target.

    if io_proxy_target is bound.
      data ld_obj type ref to cl_abap_objectdescr.
      ld_obj ?= cl_abap_typedescr=>describe_by_object_ref( io_proxy_target ).
      read table ld_obj->interfaces transporting no fields with key name = i_interface_name.
      if sy-subrc is not initial.
        zcx_mockup_loader_error=>raise(
          msg  = |io_proxy_target does not implement { i_interface_name } interface|
          code = 'II' ). "#EC NOTEXT
      endif.
    endif.

  endmethod.


  method FORWARD_METHOD.
    if mo_proxy_target is initial.
      zcx_mockup_loader_error=>raise(
        msg  = |Proxy target was not specified during instantiation|
        code = 'PA' ). "#EC NOTEXT
    endif.

    data ls_config like line of mt_config.
    ls_config-method_name = to_upper( i_method_name ).
    ls_config-as_proxy    = abap_true.

    read table md_if_desc->methods transporting no fields with key name = ls_config-method_name.
    if sy-subrc is not initial.
      zcx_mockup_loader_error=>raise(
        msg  = |Method { ls_config-method_name } not found|
        code = 'MF' ). "#EC NOTEXT
    endif.

    read table mt_config with key method_name = ls_config-method_name transporting no fields.
    if sy-subrc is initial.
      zcx_mockup_loader_error=>raise(
        msg  = |Method { ls_config-method_name } is already connected|
        code = 'MC' ). "#EC NOTEXT
    endif.

    append ls_config to mt_config.

    r_instance = me.
  endmethod.


  method generate_stub.

    data:
      lv_message    type string,
      l_prog_name   type string,
      l_class_name  type string.

    field-symbols <method> like line of md_if_desc->methods.
    field-symbols <conf> like line of mt_config.

    clear mt_src.

    _src( 'program.' ).                                        "#EC NOTEXT
    _src( 'class lcl_mockup_loader_stub definition final' ).   "#EC NOTEXT
    _src( '  inheriting from zcl_mockup_loader_stub_base.' ).  "#EC NOTEXT
    _src( '  public section.' ).                               "#EC NOTEXT
    _src( |    interfaces { mv_interface_name }.| ).           "#EC NOTEXT
    _src( 'endclass.' ).                                       "#EC NOTEXT

    _src( 'class lcl_mockup_loader_stub implementation.' ).    "#EC NOTEXT

    loop at md_if_desc->methods assigning <method>.
      unassign <conf>.
      read table mt_config assigning <conf> with key method_name = <method>-name.
      _src( |  method { mv_interface_name }~{ <method>-name }.| ).
      if <conf> is assigned.
        if <conf>-as_proxy = abap_true.
          field-symbols <param> like line of <method>-parameters.
          data l_param_kind type char1.

          _src( '    data lt_params type abap_parmbind_tab.' ). "#EC NOTEXT
          _src( '    data ls_param like line of lt_params.' ).  "#EC NOTEXT

          loop at <method>-parameters assigning <param>.
            l_param_kind = <param>-parm_kind.
            translate l_param_kind using 'IEEICCRR'. " Inporting -> exporting, etc
            _src( |    ls_param-name = '{ <param>-name }'.| ).
            _src( |    ls_param-kind = '{ l_param_kind }'.| ).
            _src( |    get reference of { <param>-name } into ls_param-value.| ).
            _src( '    insert ls_param into table lt_params.' ). "#EC NOTEXT
          endloop.

          _src( |    call method mo_proxy_target->('{ mv_interface_name }~{ <method>-name }')| ). "#EC NOTEXT
          _src( |      parameter-table lt_params.| ).            "#EC NOTEXT

        else.
          _src( '    data lr_data type ref to data.' ).          "#EC NOTEXT
          _src( '    lr_data = get_mock_data(' ).                "#EC NOTEXT
          if <conf>-sift_param is not initial.
            _src( |      i_sift_value  = { <conf>-sift_param }| ).
          endif.
          _src( |      i_method_name = '{ <method>-name }' ).| ).
          _src( '    field-symbols <container> type any.' ).      "#EC NOTEXT
          _src( '    assign lr_data->* to <container>.' ).        "#EC NOTEXT
          _src( |    { <conf>-output_param } = <container>.| ).
        endif.
      endif.
      _src( '  endmethod.' ).                                   "#EC NOTEXT
    endloop.

    _src( 'endclass.' ).                                        "#EC NOTEXT

    generate subroutine pool mt_src name l_prog_name MESSAGE lv_message. "#EC CI_GENERATE
    l_class_name = |\\PROGRAM={ l_prog_name }\\CLASS=LCL_MOCKUP_LOADER_STUB|.

    create object r_stub type (l_class_name)
      exporting
        it_config       = mt_config
        io_proxy_target = mo_proxy_target
        io_ml           = mo_ml.

  endmethod.


  method validate_sift_param.

    data ld_type type ref to cl_abap_typedescr.

    data lv_part1 type abap_parmname.
    data lv_part2 type abap_parmname.
    split iv_param_name at '-' into lv_part1 lv_part2.

    id_if_desc->get_method_parameter_type(
      exporting
        p_method_name    = iv_method_name
        p_parameter_name = lv_part1
      receiving
        p_descr_ref = ld_type
      exceptions
        parameter_not_found = 1 ).
    if sy-subrc = 1.
      zcx_mockup_loader_error=>raise(
        msg  = |Param { lv_part1 } not found|
        code = 'PF' ). "#EC NOTEXT
    endif.

    if lv_part2 is initial. " elementary param
      if ld_type->kind <> cl_abap_typedescr=>kind_elem.
        zcx_mockup_loader_error=>raise(
          msg  = |Param { lv_part1 } must be elementary|
          code = 'PE' ). "#EC NOTEXT
      endif.
    else. " structured param
      if ld_type->kind <> cl_abap_typedescr=>kind_struct. " TODO class ref ?
        zcx_mockup_loader_error=>raise(
          msg  = |Param { lv_part1 } must be a strcture|
          code = 'PE' ). "#EC NOTEXT
      endif.

      data ld_struc type ref to cl_abap_structdescr.
      ld_struc ?= ld_type.

      ld_struc->get_component_type(
        exporting
          p_name = lv_part2
        receiving
          p_descr_ref = ld_type
        exceptions
          component_not_found = 1 ).
      if sy-subrc = 1.
        zcx_mockup_loader_error=>raise(
          msg  = |Param { lv_part2 } not found|
          code = 'PF' ). "#EC NOTEXT
      endif.

      if ld_type->kind <> cl_abap_typedescr=>kind_elem.
        zcx_mockup_loader_error=>raise(
          msg  = |Param { lv_part2 } must be elementary|
          code = 'PE' ). "#EC NOTEXT
      endif.
    endif.

  endmethod.


  method _src.
    append iv_src_line to mt_src. " just to improve readability and linting
  endmethod.
ENDCLASS.
