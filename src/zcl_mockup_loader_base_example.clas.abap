class ZCL_MOCKUP_LOADER_BASE_EXAMPLE definition
  public
  final
  create public
  for testing
  duration short
  risk level harmless .

  public section.
    " THIS CLASS IN AN EXAMPLE
    " It was marked as final intentionally (to avoid occasional subclassing), real class would be not final

    constants gc_required_mockup_loader_ver type string value 'v2.2.2'.

    methods constructor.
    class-methods class_constructor.

  protected section.

    class-data:
      gi_ml           type ref to zif_mockup_loader,
      gi_proxy_target type ref to zif_mockup_loader_stub_dummy,
      gi_da_stub      type ref to zif_mockup_loader_stub_dummy.

    " This is shortcut to set up custom mock to pass calls to
    class-methods _set_proxy_target
      importing
        ii_proxy_target type ref to zif_mockup_loader_stub_dummy.

    " Connects a method to a mock
    " Results in stub_factory->connect call, but needed since the stub is autocreated later
    class-methods _connect
      importing
        iv_str type string.

    " 2 methods to test any structure for ONLY and SKIP markers and report on them if found
    " _has_only also returns abap_true if the marker is found -> the value can be then used in test case loop, see examples
    class-methods _has_only_and_warn
      importing
        it_index type standard table
      returning
        value(rv_yes) type abap_bool.
    class-methods _has_skip_and_warn
      importing
        it_index type standard table
      returning
        value(rv_yes) type abap_bool.

    " A shortcut to fire a warning message
    class-methods _warn
      importing
        i_msg type string.

  private section.

    class-data go_stub_factory type ref to zcl_mockup_loader_stub_factory.
    class-data gt_connections type string_table.

    methods _inject_db_mock.
    class-methods _setup_loader.
    class-methods _gen_stub.
    class-methods _connect_defaults.
    class-methods _count_markers
      importing
        it_index type standard table
        iv_field type abap_compname
      returning
        value(rv_count) type i.
    class-methods _has_marker_and_warn
      importing
        it_index type standard table
        iv_field type abap_compname
      returning
        value(rv_yes) type abap_bool.

ENDCLASS.



CLASS ZCL_MOCKUP_LOADER_BASE_EXAMPLE IMPLEMENTATION.


  method class_constructor.
    _setup_loader( ).
    _connect_defaults( ).
  endmethod.


  method constructor.
    if gi_da_stub is not bound. "skip if generated
      _gen_stub( ).
    endif.
    _inject_db_mock( ).
  endmethod.


  method _connect.
    append iv_str to gt_connections.
  endmethod.


  method _connect_defaults.

    " Here should be connections which are common for all tested classes
    " Such as settings, typical master data, etc.
    " For exceptional cases these connections may be overriden in a specific test class if required

*    _connect( 'get_company_code -> ./cc [bukrs = i_bukrs]' ).
*    _connect( 'get_document_type -> ./doct [blart = i_doctype]' ).
*    _connect( 'get_partner_vbund -> ./tradep(vbund) [partner_no = i_partner]' ).
*    _connect( 'get_tax_code_conditions -> ./~taxes [tax_code = i_mwskz]' ).
*    _connect( 'get_tax_type -> ./taxes(mwart) [tax_code = i_mwskz]' ).

  endmethod.


  method _count_markers.
    field-symbols <i> type any.
    field-symbols <fld> type abap_bool.

    loop at it_index assigning <i>.
      assign component iv_field of structure <i> to <fld>.
      if sy-subrc <> 0.
        return. " No field in the structure
      endif.
      if <fld> = abap_true.
        rv_count = rv_count + 1.
      endif.
    endloop.
  endmethod.


  method _gen_stub.

    data lx type ref to cx_static_check.

    try.

      data lo_stub_factory type ref to zcl_mockup_loader_stub_factory.
      data ld_stub_intf type ref to cl_abap_refdescr.
      field-symbols <c> like line of gt_connections.

      ld_stub_intf ?= cl_abap_typedescr=>describe_by_data( gi_da_stub ). " Avoid hardcoding names when possible

      create object lo_stub_factory
        exporting
          ii_ml_instance    = gi_ml
          i_allow_overrides = abap_true
          io_proxy_target   = gi_proxy_target
          i_interface_name  = |{ ld_stub_intf->get_referenced_type( )->get_relative_name( ) }|.

      loop at gt_connections assigning <c>.
        lo_stub_factory->connect( <c> ).
      endloop.

      gi_da_stub ?= lo_stub_factory->generate_stub( ).

    catch zcx_mockup_loader_error into lx.
      cl_abap_unit_assert=>fail( lx->get_text( ) ).
    endtry.

  endmethod.


  method _has_marker_and_warn.

    data lv_count type i.
    lv_count = _count_markers(
      it_index = it_index
      iv_field = iv_field ).
    if lv_count > 0.
      _warn( |test cases has { lv_count } { iv_field } markers, please check| ).
      rv_yes = abap_true.
    endif.

  endmethod.


  method _has_only_and_warn.
    rv_yes = _has_marker_and_warn(
      it_index = it_index
      iv_field = 'ONLY' ).
  endmethod.


  method _has_skip_and_warn.
    rv_yes = _has_marker_and_warn(
      it_index = it_index
      iv_field = 'SKIP' ).
  endmethod.


  method _inject_db_mock.
    " In real life this shuold be a global class which is a real commonly used DAO or some kind of DAO factory
    " DAO = data accessor object
    lcl_dao=>inject_instance( gi_da_stub ).
  endmethod.


  method _setup_loader.

    data lx type ref to cx_static_check.

    try.

      gi_ml = zcl_mockup_loader=>create(
        i_cache_timeout = 5       " may help in case of many test classes
        i_encoding      = zif_mockup_loader=>encoding_utf8
        i_amt_format    = ' .'
        i_path          = 'ZMOCKUP_LOADER_EXAMPLE' ).

      if gi_ml->is_redirected( ) = abap_true.
        _warn( 'Redirect is ON' ).
      endif.

    catch zcx_mockup_loader_error into lx.
      cl_abap_unit_assert=>fail( lx->get_text( ) ).
    endtry.

  endmethod.


  method _set_proxy_target.
    gi_proxy_target = ii_proxy_target.
  endmethod.


  method _warn.
    cl_abap_unit_assert=>fail(
      msg   = i_msg
      level = if_aunit_constants=>tolerable
      quit  = if_aunit_constants=>no ).
  endmethod.
ENDCLASS.
