class ZCL_MOCKUP_LOADER_STUB_FACTORY definition
  public
  final
  create public .

  public section.

    methods constructor
      importing
        !i_interface_name type seoclsname
        !ii_ml_instance type ref to zif_mockup_loader
        !io_proxy_target type ref to object optional
        !i_allow_overrides type abap_bool default abap_false
      raising
        zcx_mockup_loader_error .
    methods connect_method
      importing
        !i_method_name type abap_methname
        !i_mock_name type string optional
        !i_load_strict type abap_bool default abap_false
        !i_corresponding type abap_bool default abap_false
        !i_sift_param type string optional
        !i_sift_const type string optional
        !i_mock_tab_key type abap_compname optional
        !i_field_only type abap_parmname optional
        !i_output_param type abap_parmname optional
        !i_const_value type string optional
        !i_deep type abap_bool default abap_false
        !i_filter type zif_mockup_loader=>tty_stub_filter_params optional
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
    methods connect
      importing
        i_connect_string type string
      returning
        value(r_instance) type ref to zcl_mockup_loader_stub_factory
      raising
        zcx_mockup_loader_error .
    methods generate_stub
      returning
        value(r_stub) type ref to object .
    methods set_default_mock
      importing
        iv_path type csequence.

  protected section.
  private section.

    types:
      begin of ty_filter_type,
        mock_tab_key type abap_compname,
        type type ref to cl_abap_typedescr,
      end of ty_filter_type,
      tty_filter_types type standard table of ty_filter_type with key mock_tab_key.

    data mv_default_mock_root type string.
    data mt_src type string_table.
    data mv_interface_name type seoclsname .
    data mt_config type zif_mockup_loader=>tt_mock_config .
    data mi_ml type ref to zif_mockup_loader .
    data md_if_desc type ref to cl_abap_objectdescr .
    data mo_proxy_target type ref to object .
    data mv_allow_overrides type abap_bool.

    class-methods build_config
      importing
        !id_if_desc type ref to cl_abap_objectdescr
        !i_config type zif_mockup_loader=>ty_mock_config
      returning
        value(r_config) type zif_mockup_loader=>ty_mock_config
      raising
        zcx_mockup_loader_error .

    methods build_stub_source_code.
    methods _src
      importing
        iv_src_line type string.

    methods handle_duplicates
      importing
        is_config like line of mt_config
      raising
        zcx_mockup_loader_error .

    class-methods validate_sift_and_get_type
      importing
        id_if_desc type ref to cl_abap_objectdescr
        iv_method_name type abap_methname
        iv_param_name type string
      returning
        value(rd_sift_type) type ref to cl_abap_typedescr
      raising
        zcx_mockup_loader_error .

    class-methods validate_connect_and_get_types
      importing
        id_if_desc type ref to cl_abap_objectdescr
        i_config type zif_mockup_loader=>ty_mock_config
      exporting
        et_sift_types type tty_filter_types
        ed_output_type type ref to cl_abap_typedescr
        es_output_param type abap_parmdescr
      raising
        zcx_mockup_loader_error .

    class-methods validate_method_and_get_otype
      importing
        id_if_desc type ref to cl_abap_objectdescr
        i_config type zif_mockup_loader=>ty_mock_config
        i_method type abap_methdescr
      exporting
        ed_output_type type ref to cl_abap_typedescr
        es_output_param type abap_parmdescr
      raising
        zcx_mockup_loader_error .

    class-methods validate_filter_and_get_ftype
      importing
        id_if_desc type ref to cl_abap_objectdescr
        i_config type zif_mockup_loader=>ty_mock_config
      returning
        value(rt_sift_types) type tty_filter_types
      raising
        zcx_mockup_loader_error .

    class-methods build_field_only_struc_type
      importing
        i_config type zif_mockup_loader=>ty_mock_config
        id_output_type type ref to cl_abap_typedescr
        it_sift_types type tty_filter_types
      returning
        value(rd_type) type ref to cl_abap_structdescr
      raising
        zcx_mockup_loader_error .

ENDCLASS.



CLASS ZCL_MOCKUP_LOADER_STUB_FACTORY IMPLEMENTATION.


  method build_config.
    data ld_output_type type ref to cl_abap_typedescr.
    data ls_output_param type abap_parmdescr.
    data lt_sift_types type tty_filter_types.

    " TODO: unify filters: merge single filter params into filter tab

    validate_connect_and_get_types(
      exporting
        i_config   = i_config
        id_if_desc = id_if_desc
      importing
        et_sift_types   = lt_sift_types
        es_output_param = ls_output_param
        ed_output_type  = ld_output_type ).

    r_config = i_config.
    r_config-output_param = ls_output_param-name.
    r_config-output_pkind = ls_output_param-parm_kind.

    if i_config-field_only is not initial.
      r_config-output_type ?= build_field_only_struc_type(
        id_output_type = ld_output_type
        it_sift_types  = lt_sift_types
        i_config       = r_config ).
    else.
      r_config-output_type ?= ld_output_type.
    endif.

  endmethod.


  method build_field_only_struc_type.

    data lt_components type cl_abap_structdescr=>component_table.
    field-symbols <c> like line of lt_components.

    append initial line to lt_components assigning <c>.
    <c>-name = i_config-field_only.
    <c>-type ?= id_output_type.

    if <c>-name = '?'. " check existance only
      if id_output_type->type_kind <> cl_abap_typedescr=>typekind_char.
        zcx_mockup_loader_error=>raise(
          msg  = 'Expecter CHAR returning type'
          code = 'CR' ). "#EC NOTEXT
      endif.
      <c>-name = '____DUMMY'. " In hope this is not a real field ever :)
    endif.

    field-symbols <t> like line of it_sift_types.
    loop at it_sift_types assigning <t>.
      append initial line to lt_components assigning <c>.
      <c>-name  = <t>-mock_tab_key.
      <c>-type ?= <t>-type.
    endloop.

    rd_type = cl_abap_structdescr=>get( lt_components ).

  endmethod.


  method build_stub_source_code.

    field-symbols <method> like line of md_if_desc->methods.
    field-symbols <conf> like line of mt_config.
    field-symbols <param> like line of <method>-parameters.
    field-symbols <f> like line of <conf>-filter.

    data lv_data_def_added type abap_bool.
    data l_param_kind like <param>-parm_kind.

    clear mt_src.

    _src( 'program.' ).
    _src( 'class lcl_mockup_loader_stub definition final' ).
    _src( '  inheriting from zcl_mockup_loader_stub_base.' ).
    _src( '  public section.' ).
    _src( |    interfaces { mv_interface_name }.| ).
    _src( 'endclass.' ).

    _src( 'class lcl_mockup_loader_stub implementation.' ).

    loop at md_if_desc->methods assigning <method>.
      _src( |  method { mv_interface_name }~{ <method>-name }.| ).

      lv_data_def_added = abap_false.
      loop at <method>-parameters assigning <param> where parm_kind = 'E' or parm_kind = 'C'.
        _src( |    clear { <param>-name }.| ).
      endloop.
      _src( |    if is_disabled( '{ <method>-name }' ) = abap_true.| ).
      _src( '      return.' ).
      _src( '    endif.' ).
      _src( |    increment_call_count( '{ <method>-name }' ).| ).

      loop at mt_config assigning <conf> where method_name = <method>-name.

        if <conf>-as_proxy = abap_true.

          " Proxy connection can only be one so data will be defined once
          _src( '    data lt_params type abap_parmbind_tab.' ).
          _src( '    data ls_param like line of lt_params.' ).

          loop at <method>-parameters assigning <param>.
            l_param_kind = <param>-parm_kind.
            translate l_param_kind using 'IEEICCRR'. " Importing -> exporting, etc
            _src( |    ls_param-name = '{ <param>-name }'.| ).
            _src( |    ls_param-kind = '{ l_param_kind }'.| ).
            _src( |    get reference of { <param>-name } into ls_param-value.| ).
            _src( '    insert ls_param into table lt_params.' ).
          endloop.

          _src( |    call method mo_proxy_target->('{ mv_interface_name }~{ <method>-name }')| ).
          _src( |      parameter-table lt_params.| ).

        elseif <conf>-const_value is not initial.

          _src( |    { <conf>-output_param } = '{ <conf>-const_value }'.| ).

        else.

          if lv_data_def_added = abap_false.
            _src( '    data lt_sift_values type zif_mockup_loader=>tty_stub_sift_values.' ).
            _src( '    data lr_sift_val type ref to data.' ).
            _src( '    data lr_data type ref to data.' ).
            _src( '    field-symbols <container> type any.' ).
            lv_data_def_added = abap_true.
          endif.

          if <conf>-filter is not initial.
            _src( '    clear lt_sift_values.' ).
            loop at <conf>-filter assigning <f> where sift_param is not initial.
              _src( |    get reference of { <f>-sift_param } into lr_sift_val.| ).
              _src( '    append lr_sift_val to lt_sift_values.' ).
            endloop.
          endif.

          " TODO try catch ?
          _src( '    lr_data = get_mock_data(' ).
          if <conf>-sift_param is not initial.
            _src( |      i_sift_value = { <conf>-sift_param }| ).
          elseif <conf>-filter is not initial.
            _src( '      i_sift_value = lt_sift_values' ).
          endif.
          _src( |      i_output_param = '{ <conf>-output_param }'| ).
          _src( |      i_method_name = '{ <method>-name }' ).| ).
          _src( '    assign lr_data->* to <container>.' ).
          _src( |    { <conf>-output_param } = <container>.| ).

        endif.
      endloop.

      _src( '  endmethod.' ).
    endloop.

    _src( 'endclass.' ).

  endmethod.


  method connect.

    data ls_params type zif_mockup_loader=>ty_mock_config.

    ls_params = lcl_connect_string_parser=>parse( i_connect_string ).

    if ls_params-mock_name = '*'. " Proxy
      forward_method( i_method_name = ls_params-method_name ).
    else.
      connect_method(
        i_sift_param      = ls_params-sift_param
        i_output_param    = ls_params-output_param
        i_mock_tab_key    = ls_params-mock_tab_key
        i_field_only      = ls_params-field_only
        i_method_name     = ls_params-method_name
        i_corresponding   = ls_params-corresponding
        i_const_value     = ls_params-const_value
        i_mock_name       = ls_params-mock_name
        i_filter          = ls_params-filter ).
    endif.

  endmethod.


  method connect_method.
    data ls_config like line of mt_config.
    ls_config-method_name   = to_upper( i_method_name ).
    ls_config-mock_name     = i_mock_name.
    ls_config-load_strict   = i_load_strict.
    ls_config-corresponding = i_corresponding.
    ls_config-sift_param    = to_upper( i_sift_param ).
    ls_config-sift_const    = i_sift_const.
    ls_config-mock_tab_key  = to_upper( i_mock_tab_key ).
    ls_config-output_param  = to_upper( i_output_param ).
    ls_config-field_only    = to_upper( i_field_only ).
    ls_config-const_value   = i_const_value.
    ls_config-deep          = i_deep.
    ls_config-filter        = i_filter.

    if mv_default_mock_root is not initial and strlen( ls_config-mock_name ) >= 2 and ls_config-mock_name+0(2) = './'.
      ls_config-mock_name = replace(
        val  = ls_config-mock_name
        sub  = `.`
        with = mv_default_mock_root
        occ  = 1 ).
    endif.

    field-symbols <f> like line of ls_config-filter.
    loop at ls_config-filter assigning <f>.
      <f>-mock_tab_key = to_upper( <f>-mock_tab_key ).
      <f>-sift_param   = to_upper( <f>-sift_param ).
    endloop.

    " Validate and save config
    ls_config = build_config(
      id_if_desc = md_if_desc
      i_config   = ls_config ).

    handle_duplicates( ls_config ).
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
    me->mi_ml             = ii_ml_instance.
    me->mv_interface_name = i_interface_name.
    me->mo_proxy_target   = io_proxy_target.
    me->mv_allow_overrides = i_allow_overrides.

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


  method forward_method.
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

    handle_duplicates( ls_config ).

    append ls_config to mt_config.

    r_instance = me.
  endmethod.


  method generate_stub.

    data:
      lv_message    type string,
      l_prog_name   type string,
      l_class_name  type string.

    build_stub_source_code( ).

    generate subroutine pool mt_src name l_prog_name message lv_message. "#EC CI_GENERATE
    l_class_name = |\\PROGRAM={ l_prog_name }\\CLASS=LCL_MOCKUP_LOADER_STUB|.

    create object r_stub type (l_class_name)
      exporting
        it_config       = mt_config
        io_proxy_target = mo_proxy_target
        ii_ml           = mi_ml.

    clear mt_src. " for memory

  endmethod.


  method handle_duplicates.

    data lv_raise type abap_bool.
    field-symbols <m> like line of mt_config.

    read table mt_config assigning <m> with key method_name = is_config-method_name.

    if sy-subrc <> 0. " no connection yet - no problem
      return.
    endif.

    if is_config-as_proxy = abap_true or <m>-as_proxy = abap_true.
      if mv_allow_overrides = abap_true.
        delete mt_config where method_name = is_config-method_name.
      else.
        lv_raise = abap_true.
      endif.
    else. " maybe it's connecting another param of the same method
      read table mt_config transporting no fields
        with key
          method_name  = is_config-method_name
          output_param = is_config-output_param.
      if sy-subrc = 0. " found with the same output_param
        if mv_allow_overrides = abap_true.
          delete mt_config index sy-tabix.
        else.
          lv_raise = abap_true.
        endif.
      endif.
    endif.

    if lv_raise = abap_true.
      zcx_mockup_loader_error=>raise(
        msg  = |Method { is_config-method_name } is already connected|
        code = 'MC' ). "#EC NOTEXT
    endif.

  endmethod.


  method set_default_mock.
    mv_default_mock_root = iv_path.
  endmethod.


  method validate_connect_and_get_types.

    " Config basic checks
    if i_config-method_name is initial.
      zcx_mockup_loader_error=>raise(
        msg  = 'Specify method_name'
        code = 'MM' ). "#EC NOTEXT
    elseif i_config-mock_name is initial and i_config-const_value is initial.
      zcx_mockup_loader_error=>raise(
        msg  = 'Specify mock_name'
        code = 'MK' ). "#EC NOTEXT
    elseif i_config-mock_name is not initial and i_config-const_value is not initial.
      zcx_mockup_loader_error=>raise(
        msg  = 'Either mock or const value is allowed'
        code = 'CV' ). "#EC NOTEXT
    elseif i_config-corresponding = abap_true and i_config-field_only is not initial.
      zcx_mockup_loader_error=>raise(
        msg  = 'Cannot combine field_only and corresponding'
        code = 'PC' ). "#EC NOTEXT
    endif.

    " find method, check if exists
    field-symbols <method> like line of id_if_desc->methods.
    read table id_if_desc->methods assigning <method> with key name = i_config-method_name.
    if <method> is not assigned.
      zcx_mockup_loader_error=>raise(
        msg  = |Method { i_config-method_name } not found|
        code = 'MF' ). "#EC NOTEXT
    endif.

    validate_method_and_get_otype(
      exporting
        id_if_desc = id_if_desc
        i_config   = i_config
        i_method   = <method>
      importing
        ed_output_type  = ed_output_type
        es_output_param = es_output_param ).

    et_sift_types = validate_filter_and_get_ftype(
      id_if_desc     = id_if_desc
      i_config       = i_config ).

  endmethod.


  method validate_filter_and_get_ftype.

    " Normalize filters data
    data lt_filter like i_config-filter.
    field-symbols <f> like line of lt_filter.

    append initial line to lt_filter assigning <f>.
    move-corresponding i_config to <f>. " key, param, const
    if <f> is initial and i_config-filter is initial.
      return.
    endif.
    if <f> is not initial and i_config-filter is not initial.
      zcx_mockup_loader_error=>raise(
        msg  = 'Cannot specify single and multi filter at the same time'
        code = 'BF' ). "#EC NOTEXT
    endif.
    if <f> is initial.
      lt_filter = i_config-filter.
    endif.

    " Get filter types
    loop at lt_filter assigning <f>.

      " check filters
      if boolc( <f>-sift_param is initial and <f>-sift_const is initial )
        <> boolc( <f>-mock_tab_key is initial ). " XOR
        zcx_mockup_loader_error=>raise(
          msg  = 'Specify both sift_param/const and mock_tab_key'
          code = 'MS' ). "#EC NOTEXT
      elseif <f>-sift_param is not initial and <f>-sift_const is not initial.
        zcx_mockup_loader_error=>raise(
          msg  = 'Cannot combine sift_param and sift_const'
          code = 'CS' ). "#EC NOTEXT
      endif.

      " duplication check
      read table rt_sift_types transporting no fields with key mock_tab_key = <f>-mock_tab_key.
      if sy-subrc = 0.
        zcx_mockup_loader_error=>raise(
          msg  = |Filter duplication for field { <f>-mock_tab_key }|
          code = 'DF' ). "#EC NOTEXT
      endif.

      " get types
      data ls_filter_type like line of rt_sift_types.
      if <f>-sift_param is not initial.
        ls_filter_type-type = validate_sift_and_get_type(
          id_if_desc     = id_if_desc
          iv_method_name = i_config-method_name
          iv_param_name  = <f>-sift_param ).
      else.
        ls_filter_type-type = cl_abap_typedescr=>describe_by_name( 'STRING' ).
      endif.
      ls_filter_type-mock_tab_key = <f>-mock_tab_key.
      append ls_filter_type to rt_sift_types.

    endloop.

  endmethod.


  method validate_method_and_get_otype.

    " Check output param
    if i_config-output_param is initial.
      read table i_method-parameters with key parm_kind = 'R' into es_output_param. " returning
      if sy-subrc is not initial.
        zcx_mockup_loader_error=>raise(
          msg  = 'Method has no returning params and output_param was not specified'
          code = 'MR' ). "#EC NOTEXT
      endif.
    else.
      read table i_method-parameters with key name = i_config-output_param into es_output_param.
      if sy-subrc is not initial.
        zcx_mockup_loader_error=>raise(
          msg  = |Param { i_config-output_param } not found|
          code = 'PF' ). "#EC NOTEXT
      endif.
    endif.

    if es_output_param-parm_kind = 'I'.
      zcx_mockup_loader_error=>raise(
        msg  = |Param { i_config-output_param } is importing|
        code = 'PI' ). "#EC NOTEXT
    endif.

    ed_output_type = id_if_desc->get_method_parameter_type(
      p_method_name    = i_method-name
      p_parameter_name = es_output_param-name ).

    if i_config-field_only is not initial.
      if ed_output_type->kind <> cl_abap_typedescr=>kind_elem. " Elementary
        zcx_mockup_loader_error=>raise(
          msg  = |Field only param { es_output_param-name } must be elementary|
          code = 'PL' ). "#EC NOTEXT
      endif.
    elseif i_config-const_value is not initial.
      if ed_output_type->kind <> cl_abap_typedescr=>kind_elem. " Elementary
        zcx_mockup_loader_error=>raise(
          msg  = |Const value result { es_output_param-name } must be elementary|
          code = 'CE' ). "#EC NOTEXT
      endif.
    else.
      if not ed_output_type->kind co 'ST'. " Table or structure
        zcx_mockup_loader_error=>raise(
          msg  = |Param { es_output_param-name } must be table or structure|
          code = 'PT' ). "#EC NOTEXT
      endif.
    endif.

  endmethod.


  method validate_sift_and_get_type.

    data ld_type type ref to cl_abap_typedescr.
    data ld_struc type ref to cl_abap_structdescr.

    data lv_part1 type abap_parmname.
    data lv_part2 type abap_parmname.
    data lv_final_param type abap_parmname.
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
      lv_final_param = lv_part1.
    else. " structured param
      if ld_type->kind <> cl_abap_typedescr=>kind_struct. " TODO class ref ?
        zcx_mockup_loader_error=>raise(
          msg  = |Param { lv_part1 } must be a structure|
          code = 'PE' ). "#EC NOTEXT
      endif.

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

      lv_final_param = lv_part2.
    endif.

    if ld_type->kind <> cl_abap_typedescr=>kind_elem.
      if ld_type->kind = cl_abap_typedescr=>kind_table.
        if zcl_mockup_loader_utils=>is_range( ld_type ) = abap_false.
          zcx_mockup_loader_error=>raise(
            msg  = |Param { lv_final_param } must be elementary or range|
            code = 'PE' ). "#EC NOTEXT
        endif.

        data ld_table type ref to cl_abap_tabledescr.
        ld_table ?= ld_type.
        ld_struc ?= ld_table->get_table_line_type( ).
        ld_type   = ld_struc->get_component_type( 'LOW' ).
      else.
        zcx_mockup_loader_error=>raise(
          msg  = |Param { lv_final_param } must be elementary or range|
          code = 'PE' ). "#EC NOTEXT
      endif.
    endif.
    rd_sift_type = ld_type.

  endmethod.


  method _src.
    append iv_src_line to mt_src. " just to improve readability and linting
  endmethod.
ENDCLASS.
