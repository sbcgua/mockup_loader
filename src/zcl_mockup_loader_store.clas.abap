class ZCL_MOCKUP_LOADER_STORE definition
  public
  final
  create private .

public section.

  types:
    begin of ty_store,
      name    type char40,
      tabkey  type abap_compname,
      data    type ref to data,
    end of ty_store .
  types:
    tt_store type standard table of ty_store with key name .

  class-methods GET_INSTANCE
    returning
      value(RO_INSTANCE) type ref to ZCL_MOCKUP_LOADER_STORE .
  class-methods FREE_INSTANCE .
  methods PURGE
    importing
      !I_NAME type CHAR40 .
  class-methods RETRIEVE
    importing
      !I_NAME type CHAR40
      !I_SIFT type CLIKE optional
      !I_WHERE type ANY optional
    exporting
      !E_DATA type ANY
    exceptions
      RETRIEVE_ERROR .
  type-pools ABAP .
  methods STORE
    importing
      !I_NAME type CHAR40
      !I_DATA type ANY
      !I_TABKEY type ABAP_COMPNAME optional
    raising
      ZCX_MOCKUP_LOADER_ERROR .
protected section.
private section.

  class-data GO_INSTANCE type ref to ZCL_MOCKUP_LOADER_STORE .
  data MT_STORE type TT_STORE .

  type-pools ABAP .
  methods _STORE
    importing
      !I_NAME type CHAR40
      !I_DATA_REF type ref to DATA
      !I_TABKEY type ABAP_COMPNAME optional
    raising
      ZCX_MOCKUP_LOADER_ERROR .
  methods _RETRIEVE
    importing
      !I_NAME type CHAR40
      !I_SIFT type CLIKE optional
      !I_WHERE type ANY optional
    exporting
      !E_DATA type ANY
    raising
      ZCX_MOCKUP_LOADER_ERROR .
ENDCLASS.



CLASS ZCL_MOCKUP_LOADER_STORE IMPLEMENTATION.


method free_instance.
  if go_instance is not initial.
    free go_instance.
  endif.
endmethod.


method get_instance.
  if go_instance is initial.
    create object go_instance.
  endif.
  ro_instance = go_instance.
endmethod.


method purge.
  data l_store type ty_store.

  if i_name = '*'. " Delete all
    loop at mt_store into l_store.
      free l_store-data.
    endloop.
    clear mt_store.

  else.            " Delete specific record
    read table mt_store with key name = i_name into l_store.
    if sy-subrc is initial.
      delete mt_store index sy-tabix.
      free l_store-data.
    endif.
  endif.

endmethod.


method retrieve.
  data lx_error type ref to zcx_mockup_loader_error.
  data lx_unexp type ref to cx_static_check.

  try .
    get_instance( )->_retrieve(
      exporting
        i_name  = i_name
        i_sift  = i_sift
        i_where = i_where
      importing
        e_data  = e_data ).

  catch zcx_mockup_loader_error into lx_error.

    " Switch to non-class exceptions to ensure better code readability
    " and compatibility with substituted select results
    " e.g. zcl_mockup_loader=>retrieve( ... ). if sy_subrc is not initial ...
    cl_message_helper=>set_msg_vars_for_if_t100_msg( text = lx_error ).
    message
      id sy-msgid
      type sy-msgty
      number sy-msgno
      raising retrieve_error
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  catch cx_static_check into lx_unexp.

    sy-msgid = 'SY'.
    sy-msgty = 'E'.
    sy-msgno = '499'. " & & & &
    sy-msgv1 = 'ZCL_MOCKUP_LOADER'.
    sy-msgv2 = 'RETRIEVE()'.
    sy-msgv3 = lx_unexp->get_text( ).

    message
      id sy-msgid
      type sy-msgty
      number sy-msgno
      raising retrieve_error
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  endtry.

endmethod.


method store.
  data          lr_data type ref to data.
  field-symbols <data>  type any.

  " Create clone container and copy data there
  create data lr_data like i_data.
  assign lr_data->* to <data>.
  <data> = i_data.

  me->_store( i_name     = i_name
              i_data_ref = lr_data
              i_tabkey   = i_tabkey ).
endmethod.


method _retrieve.
  data:
        l_store     type ty_store,
        lt_filter   type zcl_mockup_loader=>tt_filter,
        r_data_tab  type ref to data,
        ld_src      type ref to cl_abap_typedescr,
        ld_dst      type ref to cl_abap_typedescr,
        ld_tab      type ref to cl_abap_tabledescr,
        ld_src_line type ref to cl_abap_structdescr,
        ld_dst_line type ref to cl_abap_structdescr.

  field-symbols:
        <line>    type any,
        <tabkey>  type any,
        <src_tab> type any table,
        <tmp_tab> type standard table,
        <data>    type any.

  clear e_data.

  " Validate parameters
  if i_sift is not initial and i_where is not initial.
    zcx_mockup_loader_error=>raise( msg = |Pass just one filter param| code = 'WP' ). "#EC NOTEXT
  endif.

  " Find store
  read table mt_store with key name = i_name into l_store.
  if sy-subrc is not initial.
    zcx_mockup_loader_error=>raise( msg = |Cannot find store { i_name }| code = 'NF' ). "#EC NOTEXT
  endif.

  assign l_store-data->* to <data>.

  " Build filter
  if i_sift is not initial.
    if l_store-tabkey is initial.
      zcx_mockup_loader_error=>raise( msg = 'Tabkey field not found' code = 'FM' ). "#EC NOTEXT
    endif.
    zcl_mockup_loader=>build_filter(
      exporting i_where  = l_store-tabkey && '=' && i_sift
      importing e_filter = lt_filter ).
  elseif i_where is not initial.
    zcl_mockup_loader=>build_filter(
      exporting i_where  = i_where
      importing e_filter = lt_filter ).
  endif.

  " Ensure types are the same
  ld_src = cl_abap_typedescr=>describe_by_data( <data> ).
  ld_dst = cl_abap_typedescr=>describe_by_data( e_data ).

  if ld_src->kind = 'T'.
    ld_tab      ?= ld_src.
    ld_src_line ?= ld_tab->get_table_line_type( ).
  endif.

  " Ensure filter is applied to a table
  if lt_filter is not initial and ld_src->kind <> 'T'.
    zcx_mockup_loader_error=>raise( msg = 'Filtering is relevant for tables only' code = 'TO' ). "#EC NOTEXT
  endif.

  " If types are not equal try going deeper to table line structure
  if ld_src->absolute_name ne ld_dst->absolute_name.
    if ld_src->kind = 'T' and ld_dst->kind = 'T'. " Table => Table
      ld_tab      ?= ld_dst.
      ld_dst_line ?= ld_tab->get_table_line_type( ).
    elseif lt_filter is not initial and ld_src->kind = 'T' and ld_dst->kind = 'S'. " Table + filter => Structure
      ld_dst_line ?= ld_dst.
    else.
      zcx_mockup_loader_error=>raise( msg = |Types differ for store { i_name }| code = 'TT' ). "#EC NOTEXT
    endif.

    if ld_src_line->absolute_name ne ld_dst_line->absolute_name.
      zcx_mockup_loader_error=>raise( msg = |Types differ for store { i_name }| code = 'TS' ). "#EC NOTEXT
    endif.
  endif.

  " Copy or sift (filter with tabkey) values
  if lt_filter is initial.
    e_data = <data>.

  else. " Assuming ld_src->kind = 'T' -> see STORE
    assert ld_dst->kind ca 'ST'.
    assign l_store-data->* to <src_tab>.

    if ld_dst->kind = 'T'.
      ld_tab = cl_abap_tabledescr=>create(
        p_line_type  = ld_src_line
        p_table_kind = cl_abap_tabledescr=>tablekind_std
        p_unique     = abap_false ).

      create data r_data_tab type handle ld_tab.
      assign r_data_tab->* to <tmp_tab>.
    endif.

    loop at <src_tab> assigning <line>.
      if zcl_mockup_loader=>does_line_fit_filter( i_line = <line> i_filter = lt_filter ) = abap_true.
        if ld_dst->kind = 'S'. " Structure
          e_data = <line>.
          exit. " Only first line goes to structure and then exits
        else. " Table
          append <line> to <tmp_tab>.
        endif.
      endif.
    endloop.

    if ld_dst->kind = 'T'.
      e_data = <tmp_tab>.
      free r_data_tab.
    endif.

  endif.

  if e_data is initial.
    zcx_mockup_loader_error=>raise( msg = 'No data returned' code = '04' ). "#EC NOTEXT
  endif.

endmethod.


method _store.
  data:
        l_store       type ty_store,
        lo_type       type ref to cl_abap_typedescr,
        lo_tab_type   type ref to cl_abap_tabledescr,
        lo_str_type   type ref to cl_abap_structdescr.

  " Check if tabkey exists
  if i_tabkey is not initial.
    lo_type = cl_abap_typedescr=>describe_by_data_ref( i_data_ref ).
    if lo_type->kind <> 'T'. " Not table ?
      zcx_mockup_loader_error=>raise( msg = 'Tabkey is relevant for tables only' code = 'TO' ). "#EC NOTEXT
    endif.

    lo_tab_type ?= lo_type.
    lo_str_type ?= lo_tab_type->get_table_line_type( ).

    read table lo_str_type->components with key name = i_tabkey transporting no fields.
    if sy-subrc <> 0.
      zcx_mockup_loader_error=>raise( msg = 'Tabkey field not found' code = 'FM' ). "#EC NOTEXT
    endif.
  endif.

  " Store data
  l_store-name   = i_name.
  l_store-tabkey = i_tabkey.
  l_store-data   = i_data_ref.

  me->purge( i_name ).
  append l_store to me->mt_store.

endmethod.
ENDCLASS.
