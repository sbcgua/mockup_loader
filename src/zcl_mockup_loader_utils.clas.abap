class ZCL_MOCKUP_LOADER_UTILS definition
  public
  final
  create public .

  public section.

    constants:
      begin of c_filter_type,
        value type c length 1 value 'V',
        range type c length 1 value 'R',
      end of c_filter_type.

    types:
      begin of ty_filter,
        name   type string,
        valref type ref to data,
        type   type char1,
      end of ty_filter .
    types:
      tt_filter type standard table of ty_filter with key name .
    types:
      begin of ty_where,
        name  type string,
        range type ref to data,
      end of ty_where .
    types:
      tt_where type standard table of ty_where with key name .

    type-pools abap .
    class-data g_ty_where_abs_name type abap_abstypename read-only .
    class-data g_range_key type abap_keydescr_tab read-only .
    class-data g_ty_filter_abs_name type abap_abstypename read-only .

    class-methods filter_table
      importing
        !i_filter type tt_filter optional
        !i_tab type any table
        !i_where type any optional
      exporting
        !e_container type any
      raising
        zcx_mockup_loader_error .
    class-methods build_filter
      importing
        !i_where type any
        !i_single_value type any optional
      returning
        value(r_filter) type tt_filter
      raising
        zcx_mockup_loader_error .
    class-methods does_line_fit_filter
      importing
        !i_line type any
        !i_filter type tt_filter
      returning
        value(r_yesno) type abap_bool .
    class-methods class_constructor .

    class-methods conv_tt_where_to_filter
      importing
        !i_where type any
      returning
        value(r_filter) type tt_filter
      raising
        zcx_mockup_loader_error .
    class-methods conv_nc_struc_to_filter
      importing
        !i_where  type any
        !id_struc type ref to cl_abap_structdescr optional
      returning
        value(rt_filter) type tt_filter
      raising
        zcx_mockup_loader_error .
    class-methods conv_single_val_to_filter
      importing
        !i_where type csequence
        !i_value type any
      returning
        value(r_filter) type ty_filter
      raising
        zcx_mockup_loader_error .
    class-methods conv_string_to_filter
      importing
        !i_where type clike
      returning
        value(r_filter) type ty_filter
      raising
        zcx_mockup_loader_error .
    class-methods conv_where_to_filter
      importing
        !i_where type ty_where
      returning
        value(r_filter) type ty_filter
      raising
        zcx_mockup_loader_error
        cx_sy_move_cast_error .

  protected section.
  private section.

ENDCLASS.



CLASS ZCL_MOCKUP_LOADER_UTILS IMPLEMENTATION.


  method BUILD_FILTER.
    data dy_type       type ref to cl_abap_typedescr.
    data dy_struc      type ref to cl_abap_structdescr.
    data dy_table      type ref to cl_abap_tabledescr.
    data l_filter      type ty_filter.
    data lt_filter     type tt_filter.

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


        when cl_abap_typedescr=>typekind_struct2. " structure can be ty_where or "named components"
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
    data l_dummy_where  type ty_where.
    data l_dummy_filter type ty_filter.
    data dy_tab  type ref to cl_abap_tabledescr.
    data dy_type type ref to cl_abap_typedescr.
    data l_dummy_range type range of char1.

    dy_type = cl_abap_typedescr=>describe_by_data( l_dummy_where ).
    g_ty_where_abs_name = dy_type->absolute_name.

    dy_type = cl_abap_typedescr=>describe_by_data( l_dummy_filter ).
    g_ty_filter_abs_name = dy_type->absolute_name.

    dy_tab ?= cl_abap_typedescr=>describe_by_data( l_dummy_range ).
    g_range_key = dy_tab->key.

  endmethod.


  method CONV_NC_STRUC_TO_FILTER.

    data dy_struc      type ref to cl_abap_structdescr.
    data dy_table      type ref to cl_abap_tabledescr.
    data l_filter      type ty_filter.
    data lt_components type cl_abap_structdescr=>component_table.
    data l_component   like line of lt_components.

    field-symbols <tab>  type any table.

    dy_struc = id_struc.
    if dy_struc is initial.
      dy_struc ?= cl_abap_typedescr=>describe_by_data( i_where ).
    endif.

    lt_components  = dy_struc->get_components( ).
    loop at lt_components into l_component.
      if l_component-type->kind <> cl_abap_typedescr=>kind_table.
        zcx_mockup_loader_error=>raise(
          msg  = |I_WHERE must be a structure of ranges|
          code = 'WS' ).   "#EC NOTEXT
      endif.

      dy_table ?= l_component-type.
      if dy_table->key ne g_range_key. " Not range-like structure ?
        zcx_mockup_loader_error=>raise(
          msg  = |I_WHERE must be a structure of ranges|
          code = 'WS' ).   "#EC NOTEXT
      endif.

      l_filter-name = l_component-name.
      l_filter-type = c_filter_type-range.
      assign component l_component-name of structure i_where to <tab>.
      get reference of <tab> into l_filter-valref.
      append l_filter to rt_filter.
    endloop.
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

    r_filter-type = c_filter_type-value.
    r_filter-name = to_upper( i_where ).
    <value>       = i_value.

  endmethod.


  method CONV_STRING_TO_FILTER.
    field-symbols <value> type string.

    r_filter-type = c_filter_type-value.
    create data r_filter-valref type string.
    assign r_filter-valref->* to <value>.

    split i_where at '=' into r_filter-name <value>.
    shift r_filter-name right deleting trailing space.
    shift r_filter-name left  deleting leading space.
    shift <value>       right deleting trailing space.
    shift <value>       left  deleting leading space.
    translate r_filter-name to upper case.

    if r_filter-name is initial or <value> is initial.
      zcx_mockup_loader_error=>raise( msg = |Incorrect I_WHERE string pattern| code = 'SP' ).   "#EC NOTEXT
    endif.

  endmethod.


  method CONV_TT_WHERE_TO_FILTER.
    data l_filter         type ty_filter.
    field-symbols <tab>   type any table.
    field-symbols <where> type ty_where.

    assign i_where to <tab>.
    loop at <tab> assigning <where>.
      l_filter = conv_where_to_filter( <where> ).
      append l_filter to r_filter.
    endloop.

  endmethod.


  method conv_where_to_filter.
    data dy_table      type ref to cl_abap_tabledescr.

    r_filter-name   = to_upper( i_where-name ).
    r_filter-valref = i_where-range.
    r_filter-type   = c_filter_type-range.
    dy_table ?= cl_abap_typedescr=>describe_by_data_ref( r_filter-valref ). " Assume table, cast_error otherwise
    if dy_table->key ne g_range_key. " Not range ?
      zcx_mockup_loader_error=>raise( msg = |I_WHERE-RANGE must be a range table| code = 'RT' ).   "#EC NOTEXT
    endif.
  endmethod.


  method DOES_LINE_FIT_FILTER.
    data l_filter         type ty_filter.
    field-symbols <field> type any.
    field-symbols <range> type any table.
    field-symbols <value> type any.

    r_yesno = abap_true.

    loop at i_filter into l_filter.
      assign component l_filter-name of structure i_line to <field>.
      check <field> is assigned. " Just skip irrelevant ranges

      if l_filter-type = c_filter_type-range.
        assign l_filter-valref->* to <range>.
        if not <field> in <range>.
          r_yesno = abap_false.
        endif.
      elseif l_filter-type = c_filter_type-value.
        assign l_filter-valref->* to <value>.
        if not <field> = <value>. " cx_sy_conversion_error does not catch that :(
          r_yesno = abap_false.
        endif.
      else.
        assert 1 = 0.
      endif.

      if r_yesno = abap_false.
        return.
      endif.

      unassign: <field>, <range>, <value>.
    endloop.

  endmethod.


  method FILTER_TABLE.
    data dy_type2      type ref to cl_abap_typedescr.
    data dy_struc      type ref to cl_abap_structdescr.
    data dy_stru2      type ref to cl_abap_structdescr.
    data dy_table      type ref to cl_abap_tabledescr.
    data dy_tabl2      type ref to cl_abap_tabledescr.

    field-symbols <container_tab> type any table.

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

    " Check proper type
    dy_type2 = cl_abap_typedescr=>describe_by_data( e_container ).
    case dy_type2->kind.
      when cl_abap_typedescr=>kind_table. " Table
        dy_tabl2 ?= dy_type2.
        dy_stru2 ?= dy_tabl2->get_table_line_type( ).
        assign e_container to <container_tab>.
      when cl_abap_typedescr=>kind_struct. " Structure
        dy_stru2 ?= dy_type2.
      when others. " Not a table or structure ?
        zcx_mockup_loader_error=>raise( msg = 'Table or structure containers only' code = 'WT' ). "#EC NOTEXT
    endcase.

    " Check tables have same line time
    dy_table ?= cl_abap_typedescr=>describe_by_data( i_tab ).
    dy_struc ?= dy_table->get_table_line_type( ).
    if dy_struc->absolute_name <> dy_stru2->absolute_name.
      zcx_mockup_loader_error=>raise( msg = 'Src and dst line types are not similar' code = 'LT' ). "#EC NOTEXT
    endif.

    " create line container
    data ld_record type ref to data.
    field-symbols <record> type any.
    create data ld_record type handle dy_struc.
    assign ld_record->* to <record>.

    data lv_fit type abap_bool.
    loop at i_tab assigning <record>.
      lv_fit = does_line_fit_filter(
        i_line   = <record>
        i_filter = lt_filter ).
      if lv_fit = abap_true.
        if dy_type2->kind = cl_abap_typedescr=>kind_struct. " Structure requested
          e_container = <record>.
          exit. " Only first line goes to structure and then exits
        else. " Table
          insert <record> into table <container_tab>.
        endif.
      endif.
    endloop.

  endmethod.
ENDCLASS.
