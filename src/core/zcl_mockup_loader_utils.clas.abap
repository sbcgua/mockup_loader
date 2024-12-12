class ZCL_MOCKUP_LOADER_UTILS definition
  public
  final
  create public .

  public section.

    class-methods filter_table
      importing
        !i_filter type zif_mockup_loader=>tt_filter optional
        !i_tab type any table
        !i_where type any optional
        !i_corresponding type abap_bool default abap_false
      exporting
        !e_container type any
      raising
        zcx_mockup_loader_error .
    class-methods build_filter
      importing
        !i_where type any
        !i_single_value type any optional
      returning
        value(r_filter) type zif_mockup_loader=>tt_filter
      raising
        zcx_mockup_loader_error .
    class-methods does_line_fit_filter
      importing
        !i_line type any
        !i_filter type zif_mockup_loader=>tt_filter
      returning
        value(r_yesno) type abap_bool .
    class-methods class_constructor .

    class-methods conv_tt_where_to_filter
      importing
        !i_where type any
      returning
        value(r_filter) type zif_mockup_loader=>tt_filter
      raising
        zcx_mockup_loader_error .
    class-methods conv_nc_struc_to_filter
      importing
        !i_where  type any
        !id_struc type ref to cl_abap_structdescr optional
      returning
        value(rt_filter) type zif_mockup_loader=>tt_filter
      raising
        zcx_mockup_loader_error .
    class-methods conv_single_val_to_filter
      importing
        !i_where type csequence
        !i_value type any
      returning
        value(r_filter) type zif_mockup_loader=>ty_filter
      raising
        zcx_mockup_loader_error .
    class-methods conv_range_to_filter
      importing
        !i_where type csequence
        !i_range type any table
      returning
        value(r_filter) type zif_mockup_loader=>ty_filter
      raising
        zcx_mockup_loader_error .

    class-methods conv_string_to_filter
      importing
        !i_where type clike
      returning
        value(r_filter) type zif_mockup_loader=>ty_filter
      raising
        zcx_mockup_loader_error .
    class-methods conv_where_to_filter
      importing
        !i_where type zif_mockup_loader=>ty_where
      returning
        value(r_filter) type zif_mockup_loader=>ty_filter
      raising
        zcx_mockup_loader_error .
    class-methods and
      importing
        !i_op1 type any
        !i_op2 type any
        !i_op3 type any optional
        !i_op4 type any optional
      returning
        value(r_filter) type zif_mockup_loader=>tt_filter
      raising
        zcx_mockup_loader_error.

    class-methods is_range
      importing
        !id_type type ref to cl_abap_typedescr
      returning
        value(rv_yes) type abap_bool.

  protected section.
  private section.
    class-data g_ty_where_abs_name type abap_abstypename.
    class-data g_range_key type abap_keydescr_tab.
    class-data g_ty_filter_abs_name type abap_abstypename.

    class-methods validate_destination_type
      importing
        i_corresponding type abap_bool
        id_src_type type ref to cl_abap_typedescr
        id_dst_type type ref to cl_abap_typedescr
      returning
        value(rd_dst_struc) type ref to cl_abap_structdescr
      raising
        zcx_mockup_loader_error .

ENDCLASS.



CLASS ZCL_MOCKUP_LOADER_UTILS IMPLEMENTATION.


  method and.

    data lt_filter type zif_mockup_loader=>tt_filter.

    lt_filter = build_filter( i_op1 ).
    append lines of lt_filter to r_filter.

    lt_filter = build_filter( i_op2 ).
    append lines of lt_filter to r_filter.

    if i_op3 is not initial.
      lt_filter = build_filter( i_op3 ).
      append lines of lt_filter to r_filter.
    endif.

    if i_op4 is not initial.
      lt_filter = build_filter( i_op4 ).
      append lines of lt_filter to r_filter.
    endif.

  endmethod.


  method build_filter.
    data dy_type       type ref to cl_abap_typedescr.
    data dy_struc      type ref to cl_abap_structdescr.
    data dy_table      type ref to cl_abap_tabledescr.
    data l_filter      type zif_mockup_loader=>ty_filter.
    data lt_filter     type zif_mockup_loader=>tt_filter.

    if i_where is initial.
      return.
    endif.

    dy_type = cl_abap_typedescr=>describe_by_data( i_where ).

    if i_single_value is supplied and not boolc( dy_type->type_kind ca 'Cg' ) = abap_true. " Char or string
      zcx_mockup_loader_error=>raise(
        msg  = 'I_WHERE must be a parameter name for supplied I_SINGLE_VALUE'
        code = 'PN' ). "#EC NOTEXT
    endif.

    try.
      case dy_type->type_kind.
        when cl_abap_typedescr=>typekind_table. " Table -> expect tt_where
          dy_table ?= dy_type.
          dy_struc ?= dy_table->get_table_line_type( ).

          if dy_struc->absolute_name = g_ty_filter_abs_name.
            r_filter = i_where.
            return.
          endif.

          if dy_struc->absolute_name <> g_ty_where_abs_name.
            zcx_mockup_loader_error=>raise( msg = |I_WHERE table must be of TT_WHERE type| code = 'WT' ).   "#EC NOTEXT
          endif.

          lt_filter = conv_tt_where_to_filter( i_where ).


        when cl_abap_typedescr=>typekind_struct1 or cl_abap_typedescr=>typekind_struct2.
          " structure can be ty_where or "named components"
          dy_struc ?= dy_type.

          if dy_struc->absolute_name = g_ty_where_abs_name. " ty_where
            l_filter = conv_where_to_filter( i_where ).
            append l_filter to lt_filter.
          elseif dy_struc->absolute_name = g_ty_filter_abs_name. " ty_filter
            append i_where to lt_filter.
          else.                      " structure with named components per range
            lt_filter = conv_nc_struc_to_filter(
              i_where  = i_where
              id_struc = dy_struc ).
          endif.

        when cl_abap_typedescr=>typekind_char or cl_abap_typedescr=>typekind_string.
          if i_single_value is supplied.
            l_filter = conv_single_val_to_filter(
              i_where = i_where
              i_value = i_single_value ).
          else.
            l_filter = conv_string_to_filter( i_where ).
          endif.
          append l_filter to lt_filter.

        when others.
          zcx_mockup_loader_error=>raise(
            msg  = |Unsupported type { dy_type->absolute_name } of I_WHERE|
            code = 'UT' ). "#EC NOTEXT
      endcase.

    catch cx_sy_move_cast_error.
      zcx_mockup_loader_error=>raise( msg = |CX_SY_MOVE_CAST_ERROR @BUILD_FILTER()| code = 'CE' ).   "#EC NOTEXT
    endtry.

    r_filter = lt_filter.

  endmethod.


  method class_constructor.
    data l_dummy_where  type zif_mockup_loader=>ty_where.
    data l_dummy_filter type zif_mockup_loader=>ty_filter.
    data dy_tab  type ref to cl_abap_tabledescr.
    data dy_type type ref to cl_abap_typedescr.
    data l_dummy_range type range of zif_mockup_loader=>ty_filter_type.

    dy_type = cl_abap_typedescr=>describe_by_data( l_dummy_where ).
    g_ty_where_abs_name = dy_type->absolute_name.

    dy_type = cl_abap_typedescr=>describe_by_data( l_dummy_filter ).
    g_ty_filter_abs_name = dy_type->absolute_name.

    dy_tab ?= cl_abap_typedescr=>describe_by_data( l_dummy_range ).
    g_range_key = dy_tab->key.

  endmethod.


  method conv_nc_struc_to_filter.

    data dy_struc      type ref to cl_abap_structdescr.
    data dy_table      type ref to cl_abap_tabledescr.
    data l_filter      type zif_mockup_loader=>ty_filter.
    data lt_components type cl_abap_structdescr=>component_table.
    data l_component   like line of lt_components.

    field-symbols <tab> type any table.
    field-symbols <val> type any.

    dy_struc = id_struc.
    if dy_struc is initial.
      dy_struc ?= cl_abap_typedescr=>describe_by_data( i_where ).
    endif.

    lt_components  = dy_struc->get_components( ).
    loop at lt_components into l_component.
      if l_component-type->kind = cl_abap_typedescr=>kind_table.

        if is_range( l_component-type ) = abap_false. " Not range-like structure ?
          zcx_mockup_loader_error=>raise(
            msg  = |I_WHERE must be a structure of ranges or values|
            code = 'WS' ).   "#EC NOTEXT
        endif.

        l_filter-name = l_component-name.
        l_filter-type = zif_mockup_loader=>c_filter_type-range.
        assign component l_component-name of structure i_where to <tab>.
        get reference of <tab> into l_filter-valref.
        append l_filter to rt_filter.
        " TODO: potential bug, if ref value is changed between build_filter and its usage
        " better: copy value/table to a newly created data ref

      elseif l_component-type->kind = cl_abap_typedescr=>kind_elem.

        l_filter-name = l_component-name.
        l_filter-type = zif_mockup_loader=>c_filter_type-value.
        assign component l_component-name of structure i_where to <val>.
        get reference of <val> into l_filter-valref.
        append l_filter to rt_filter.

      else.
        zcx_mockup_loader_error=>raise(
          msg  = |I_WHERE must be a structure of ranges or values|
          code = 'WS' ).   "#EC NOTEXT
      endif.
    endloop.
  endmethod.


  method conv_range_to_filter.
    data dy_data type ref to cl_abap_datadescr.
    dy_data ?= cl_abap_typedescr=>describe_by_data( i_range ).

    if abap_false = is_range( dy_data ).
      zcx_mockup_loader_error=>raise( msg  = 'I_RANGE must be a range' code = 'ER' ). "#EC NOTEXT
    endif.

    field-symbols <value> type any.
    create data r_filter-valref type handle dy_data.
    assign r_filter-valref->* to <value>.

    r_filter-type = zif_mockup_loader=>c_filter_type-range.
    r_filter-name = to_upper( i_where ).
    <value>       = i_range.

  endmethod.


  method conv_single_val_to_filter.
    data dy_data type ref to cl_abap_datadescr.
    dy_data ?= cl_abap_typedescr=>describe_by_data( i_value ).

    if dy_data->kind <> cl_abap_typedescr=>kind_elem.
      zcx_mockup_loader_error=>raise( msg  = 'I_VALUE must be of elementary type' code = 'ET' ). "#EC NOTEXT
    endif.

    field-symbols <value> type any.
    create data r_filter-valref type handle dy_data.
    assign r_filter-valref->* to <value>.

    r_filter-type = zif_mockup_loader=>c_filter_type-value.
    r_filter-name = to_upper( i_where ).
    <value>       = i_value.

  endmethod.


  method conv_string_to_filter.
    field-symbols <value> type string.

    r_filter-type = zif_mockup_loader=>c_filter_type-value.
    create data r_filter-valref type string.
    assign r_filter-valref->* to <value>.

    split i_where at '=' into r_filter-name <value>.
    shift r_filter-name right deleting trailing space.
    shift r_filter-name left deleting leading space.
    shift <value>       right deleting trailing space.
    shift <value>       left deleting leading space.
    translate r_filter-name to upper case.

    if r_filter-name is initial or <value> is initial.
      zcx_mockup_loader_error=>raise( msg = |Incorrect I_WHERE string pattern| code = 'SP' ).   "#EC NOTEXT
    endif.

  endmethod.


  method conv_tt_where_to_filter.
    data l_filter         like line of r_filter.
    field-symbols <tab>   type any table.
    field-symbols <where> type zif_mockup_loader=>ty_where.

    assign i_where to <tab>.
    loop at <tab> assigning <where>.
      l_filter = conv_where_to_filter( <where> ).
      append l_filter to r_filter.
    endloop.

  endmethod.


  method conv_where_to_filter.

    r_filter-name   = to_upper( i_where-name ).
    r_filter-valref = i_where-range.
    r_filter-type   = zif_mockup_loader=>c_filter_type-range.
    if abap_false = is_range( cl_abap_typedescr=>describe_by_data_ref( r_filter-valref ) ).
      zcx_mockup_loader_error=>raise( msg = |I_WHERE-RANGE must be a range table| code = 'RT' ).   "#EC NOTEXT
    endif.
  endmethod.


  method does_line_fit_filter.
    data l_filter         like line of i_filter.
    field-symbols <field> type any.
    field-symbols <range> type any table.
    field-symbols <value> type any.

    if lines( i_filter ) = 0.
      r_yesno = abap_true.
      return.
    endif.

    loop at i_filter into l_filter.
      unassign: <field>, <range>, <value>.
      assign component l_filter-name of structure i_line to <field>.
      check <field> is assigned. " Just skip irrelevant ranges

      if l_filter-type = zif_mockup_loader=>c_filter_type-range.
        assign l_filter-valref->* to <range>.
        r_yesno = boolc( <field> in <range> ).
      elseif l_filter-type = zif_mockup_loader=>c_filter_type-value.
        assign l_filter-valref->* to <value>.
        r_yesno = boolc( <field> = <value> ).
        " cx_sy_conversion_error does not catch that :(
      else.
        assert 1 = 0.
      endif.

      if l_filter-operation is initial.
        l_filter-operation = zif_mockup_loader=>c_filter_op-and.
      endif.

      if ( r_yesno = abap_false and l_filter-operation = zif_mockup_loader=>c_filter_op-and )
        or ( r_yesno = abap_true and l_filter-operation = zif_mockup_loader=>c_filter_op-or ).
        return.
      endif.

    endloop.

  endmethod.


  method filter_table.

    data ld_dst_type type ref to cl_abap_typedescr.
    data ld_dst_struc type ref to cl_abap_structdescr.

    if boolc( i_filter is supplied ) = boolc( i_where is supplied ). " XOR
      zcx_mockup_loader_error=>raise( msg = 'i_where or i_filter must be supplied' code = 'OO' ). "#EC NOTEXT
    endif.

    data lt_filter like i_filter.
    if i_where is supplied.
      lt_filter = build_filter( i_where = i_where ).
    else. " i_filter is supplied
      lt_filter = i_filter.
    endif.

    clear e_container.

    ld_dst_type  = cl_abap_typedescr=>describe_by_data( e_container ).
    ld_dst_struc = validate_destination_type(
      i_corresponding = i_corresponding
      id_src_type     = cl_abap_typedescr=>describe_by_data( i_tab )
      id_dst_type     = ld_dst_type ).

    " Copy data
    data lv_fit type abap_bool value abap_true. " true for corresponding
    data lv_filter_empty type abap_bool.
    data lr_buffer type ref to data.
    field-symbols <container_tab> type any table.
    field-symbols <record> type any.
    field-symbols <buf> type any.

    if i_corresponding = abap_true.
      create data lr_buffer type handle ld_dst_struc.
      assign lr_buffer->* to <buf>.
    endif.

    if ld_dst_type->kind = cl_abap_typedescr=>kind_table.
      assign e_container to <container_tab>.
    endif.

    lv_filter_empty = boolc( lines( lt_filter ) = 0 ).

    loop at i_tab assigning <record>.
      if lv_filter_empty = abap_false.
        lv_fit = does_line_fit_filter(
          i_line   = <record>
          i_filter = lt_filter ).
      endif.
      if lv_fit = abap_true.
        if i_corresponding = abap_true.
          if ld_dst_type->kind = cl_abap_typedescr=>kind_struct. " Structure requested
            move-corresponding <record> to e_container.
            exit. " Only first line goes to structure and then exits
          else. " Table
            move-corresponding <record> to <buf>.
            insert <buf> into table <container_tab>.
          endif.
        else.
          if ld_dst_type->kind = cl_abap_typedescr=>kind_struct. " Structure requested
            e_container = <record>.
            exit. " Only first line goes to structure and then exits
          else. " Table
            insert <record> into table <container_tab>.
          endif.
        endif.
      endif.
    endloop.

  endmethod.


  method is_range.

    if id_type->kind <> id_type->kind_table.
      return. " false
    endif.

    data ld_table type ref to cl_abap_tabledescr.
    ld_table ?= id_type.
    if ld_table->key = g_range_key. " mmm ?
      rv_yes = abap_true.
    endif.

  endmethod.


  method validate_destination_type.

    data ld_src_struc type ref to cl_abap_structdescr.
    data ld_dst_struc type ref to cl_abap_structdescr.
    data ld_src_table type ref to cl_abap_tabledescr.
    data ld_dst_table type ref to cl_abap_tabledescr.

    " Check proper type
    case id_dst_type->kind.
      when cl_abap_typedescr=>kind_table. " Table
        ld_dst_table ?= id_dst_type.
        ld_dst_struc ?= ld_dst_table->get_table_line_type( ).
      when cl_abap_typedescr=>kind_struct. " Structure
        ld_dst_struc ?= id_dst_type.
      when others. " Not a table or structure ?
        zcx_mockup_loader_error=>raise( msg = 'Table or structure containers only' code = 'WT' ). "#EC NOTEXT
    endcase.

    if i_corresponding = abap_false.
      " Check tables have same line time
      ld_src_table ?= id_src_type.
      ld_src_struc ?= ld_src_table->get_table_line_type( ).
      if ld_src_struc->absolute_name <> ld_dst_struc->absolute_name.
        zcx_mockup_loader_error=>raise( msg = 'Src and dst line types are not similar' code = 'LT' ). "#EC NOTEXT
      endif.
    endif.

    rd_dst_struc = ld_dst_struc.

  endmethod.
ENDCLASS.
