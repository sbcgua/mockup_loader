class ZCL_MOCKUP_LOADER_DEEP_PROVIDR definition
  public
  final
  create public .

  public section.

    interfaces zif_text2tab_deep_provider .

    methods constructor
      importing
        !ii_ml_instance type ref to zif_mockup_loader .

  protected section.
  private section.
    types:
      begin of ty_cache,
        location type string,
        dref type ref to data,
      end of ty_cache,
      tt_cache type standard table of ty_cache with key location.

    data mi_ml_instance type ref to zif_mockup_loader.
    data mt_cache type tt_cache.
ENDCLASS.



CLASS ZCL_MOCKUP_LOADER_DEEP_PROVIDR IMPLEMENTATION.


  method constructor.
    mi_ml_instance = ii_ml_instance.
  endmethod.


  method zif_text2tab_deep_provider~select.

    data ls_address type zif_text2tab=>ty_deep_address.
    data ls_filter type zif_mockup_loader=>ty_filter.

    clear e_container.

    ls_address = zcl_text2tab_utils=>parse_deep_address( i_address ).

    if ls_address-key_value is initial and ls_address-ref_field is initial.
      return. " empty dataset
    elseif ls_address-key_value is not initial.
      ls_filter-name = ls_address-key_field.
      ls_filter-type = zif_mockup_loader=>c_filter_type-value.
      get reference of ls_address-key_value into ls_filter-valref.
    else. " ref field is not initial
      field-symbols <valref> type any.

      ls_filter-name = ls_address-key_field.
      ls_filter-type = zif_mockup_loader=>c_filter_type-value.
      assign component ls_address-ref_field of structure i_cursor to <valref>.
      if sy-subrc <> 0.
        raise exception type zcx_text2tab_error
          exporting
            msg  = |Cannot find { ls_address-ref_field } in {
              cl_abap_typedescr=>describe_by_data( i_cursor )->absolute_name }|
            code = 'ZZ'                 " TODO improve error visibility
            methname = 'deep->select'.  "#EC NOTEXT

      endif.
      get reference of <valref> into ls_filter-valref.
    endif.

    data lx type ref to zcx_mockup_loader_error.
    field-symbols <cache> like line of mt_cache.
    field-symbols <tab> type standard table.

    try.

      read table mt_cache assigning <cache> with key location = ls_address-location.

      if sy-subrc <> 0. " Not found in cache
        append initial line to mt_cache assigning <cache>.
        <cache>-location = ls_address-location.
        <cache>-dref     = zcl_text2tab_utils=>create_standard_table_of( e_container ).
        assign <cache>-dref->* to <tab>.
        assert sy-subrc = 0.

        mi_ml_instance->load_data(
          exporting
            i_obj    = ls_address-location
            i_strict = abap_false " ????
          importing
            e_container = <tab> ).
      endif.

      assign <cache>-dref->* to <tab>.
      assert sy-subrc = 0.
      zcl_mockup_loader_utils=>filter_table(
        exporting
          i_where = ls_filter
          i_tab   = <tab>
        importing
          e_container = e_container ).

    catch zcx_mockup_loader_error into lx.
      raise exception type zcx_text2tab_error
        exporting
          msg  = |@{ ls_address-location }: { lx->get_text( ) }|
          code = 'ZZ'                 " TODO improve error visibility
          methname = 'DEEP->select'.  "#EC NOTEXT
    endtry.

  endmethod.
ENDCLASS.
