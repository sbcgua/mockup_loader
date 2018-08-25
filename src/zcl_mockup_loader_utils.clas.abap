class ZCL_MOCKUP_LOADER_UTILS definition
  public
  final
  create public .

public section.

  types:
    begin of ty_filter,
      name  type string,
      range type ref to data,
      type  type char1,
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

  type-pools ABAP .
  class-data G_TY_WHERE_ABS_NAME type ABAP_ABSTYPENAME read-only .
  class-data G_RANGE_KEY type ABAP_KEYDESCR_TAB read-only .

  class-methods FILTER_TABLE
    importing
      !I_FILTER type TT_FILTER optional
      !I_TAB type ANY TABLE
      !I_WHERE type ANY optional
    exporting
      !E_CONTAINER type ANY
    raising
      ZCX_MOCKUP_LOADER_ERROR .
  class-methods BUILD_FILTER
    importing
      !I_WHERE type ANY
    returning
      value(R_FILTER) type TT_FILTER
    raising
      ZCX_MOCKUP_LOADER_ERROR .
  class-methods DOES_LINE_FIT_FILTER
    importing
      !I_LINE type ANY
      !I_FILTER type TT_FILTER
    returning
      value(R_YESNO) type ABAP_BOOL .
  class-methods CLASS_CONSTRUCTOR .
protected section.
private section.

  class-methods CONV_STRING_TO_FILTER
    importing
      !I_WHERE type CLIKE
    returning
      value(R_FILTER) type TY_FILTER
    raising
      ZCX_MOCKUP_LOADER_ERROR .
  class-methods CONV_WHERE_TO_FILTER
    importing
      !I_WHERE type TY_WHERE
    returning
      value(R_FILTER) type TY_FILTER
    raising
      ZCX_MOCKUP_LOADER_ERROR
      CX_SY_MOVE_CAST_ERROR .
ENDCLASS.



CLASS ZCL_MOCKUP_LOADER_UTILS IMPLEMENTATION.


method BUILD_FILTER.
  data dy_type       type ref to cl_abap_typedescr.
  data dy_struc      type ref to cl_abap_structdescr.
  data dy_table      type ref to cl_abap_tabledescr.

  data l_filter      type ty_filter.
  data l_where       type ty_where.
  data lt_filter     type tt_filter.
  data lt_components type cl_abap_structdescr=>component_table.
  data l_component   like line of lt_components.

  field-symbols <tab>  type any table.

  dy_type = cl_abap_typedescr=>describe_by_data( i_where ).

  try.
    case dy_type->type_kind.
    when cl_abap_typedescr=>typekind_table. " Table -> expect tt_where
      dy_table ?= dy_type.
      dy_struc ?= dy_table->get_table_line_type( ).
      if not dy_struc->absolute_name cs g_ty_where_abs_name.
        zcx_mockup_loader_error=>raise( msg = |I_WHERE table must be of TT_WHERE type| code = 'WT' ).   "#EC NOTEXT
      endif.

      assign i_where to <tab>.
      loop at <tab> into l_where.
        l_filter = conv_where_to_filter( l_where ).
        append l_filter to lt_filter.
      endloop.

    when cl_abap_typedescr=>typekind_struct2. " structure can be ty_where or "named components"
      dy_struc      ?= dy_type.

      if dy_struc->absolute_name = g_ty_where_abs_name. " ty_where
        l_filter = conv_where_to_filter( i_where ).
        append l_filter to lt_filter.

      else.                      " structure with named components per range
        lt_components  = dy_struc->get_components( ).
        loop at lt_components into l_component.
          if l_component-type->kind <> cl_abap_typedescr=>kind_table.
            zcx_mockup_loader_error=>raise( msg = |I_WHERE must be a structure of ranges or TY_WHERE| code = 'WS' ).   "#EC NOTEXT
          endif.

          dy_table ?= l_component-type.
          if dy_table->key ne g_range_key. " Not range-like structure ?
            zcx_mockup_loader_error=>raise( msg = |I_WHERE must be a structure of ranges or TY_WHERE| code = 'WS' ).   "#EC NOTEXT
          endif.

          l_filter-name = l_component-name.
          l_filter-type = 'R'. " Range
          assign component l_component-name of structure i_where to <tab>.
          get reference of <tab> into l_filter-range.
          append l_filter to lt_filter.
        endloop.
      endif.

    when cl_abap_typedescr=>typekind_char or cl_abap_typedescr=>typekind_string.
      l_filter = conv_string_to_filter( i_where ).
      append l_filter to lt_filter.

    when others.
      zcx_mockup_loader_error=>raise( msg = |Unsupported type { dy_type->absolute_name } of I_WHERE| code = 'UT' ).   "#EC NOTEXT
    endcase.

  catch cx_sy_move_cast_error.
    zcx_mockup_loader_error=>raise( msg = |CX_SY_MOVE_CAST_ERROR @BUILD_FILTER()| code = 'CE' ).   "#EC NOTEXT
  endtry.

  r_filter = lt_filter.

endmethod.


method class_constructor.
  data l_dummy type ty_where.
  data dy_tab  type ref to cl_abap_tabledescr.
  data dy_type type ref to cl_abap_typedescr.

  dy_type = cl_abap_typedescr=>describe_by_data( l_dummy ).
  g_ty_where_abs_name = dy_type->absolute_name.

  dy_tab ?= cl_abap_typedescr=>describe_by_name( 'SVER_TABLE_TYPE_VERI_RANGE' ).
  g_range_key = dy_tab->key.

endmethod.


method CONV_STRING_TO_FILTER.
  field-symbols <cond> type string.

  r_filter-type = 'S'. " String
  create data r_filter-range type string.
  assign r_filter-range->* to <cond>.

  split i_where at '=' into r_filter-name <cond>.
  shift r_filter-name right deleting trailing space.
  shift r_filter-name left  deleting leading space.
  shift <cond>        right deleting trailing space.
  shift <cond>        left  deleting leading space.
  translate r_filter-name to upper case.

  if r_filter-name is initial or <cond> is initial.
    zcx_mockup_loader_error=>raise( msg = |Incorrect I_WHERE string pattern| code = 'SP' ).   "#EC NOTEXT
  endif.

endmethod.


method conv_where_to_filter.
  data dy_table      type ref to cl_abap_tabledescr.

  r_filter-name  = i_where-name.
  r_filter-range = i_where-range.
  r_filter-type  = 'R'. " Range
  dy_table ?= cl_abap_typedescr=>describe_by_data_ref( r_filter-range ). " Assume table, cast_error otherwise
  if dy_table->key ne g_range_key. " Not range ?
    zcx_mockup_loader_error=>raise( msg = |I_WHERE-RANGE must be a range table| code = 'RT' ).   "#EC NOTEXT
  endif.
endmethod.


method DOES_LINE_FIT_FILTER.
  data l_filter         type ty_filter.
  field-symbols <field> type any.
  field-symbols <range> type any table.
  field-symbols <cond>  type string.

  r_yesno = abap_true.

  loop at i_filter into l_filter.
    assign component l_filter-name of structure i_line to <field>.
    check <field> is assigned. " Just skip irrelevant ranges

    if l_filter-type = 'R'.               " Range
      assign l_filter-range->* to <range>.
    else.                                 " String
      assign l_filter-range->* to <cond>.
    endif.

    if <range> is assigned and not <field> in <range>
    or <cond>  is assigned and not <field> = <cond>. " cx_sy_conversion_error does not catch that :(
      r_yesno = abap_false.
      exit.
    endif.

    unassign: <field>, <range>, <cond>.
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
    lt_filter = build_filter( i_where ).
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
