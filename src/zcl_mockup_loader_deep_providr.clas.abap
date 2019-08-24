class ZCL_MOCKUP_LOADER_DEEP_PROVIDR definition
  public
  final
  create public .

public section.

  interfaces ZIF_TEXT2TAB_DEEP_PROVIDER .

  methods CONSTRUCTOR
    importing
      !II_ML_INSTANCE type ref to ZIF_MOCKUP_LOADER .
  protected section.
  private section.
    data mi_ml_instance type ref to zif_mockup_loader.

ENDCLASS.



CLASS ZCL_MOCKUP_LOADER_DEEP_PROVIDR IMPLEMENTATION.


  method constructor.
    mi_ml_instance = ii_ml_instance.
  endmethod.


  method zif_text2tab_deep_provider~select.

    data ls_address type zcl_text2tab_utils=>ty_deep_address.
    data ls_filter type zcl_mockup_loader_utils=>ty_filter.

    clear e_container.

    ls_address = zcl_text2tab_utils=>parse_deep_address( i_address ).

    if ls_address-key_value is initial and ls_address-ref_field is initial.
      return. " empty dataset
    elseif ls_address-key_value is not initial.
      ls_filter-name = ls_address-key_field.
      ls_filter-type = zcl_mockup_loader_utils=>c_filter_type-value.
      get reference of ls_address-key_value into ls_filter-valref.
    else. " ref field is not initial
      field-symbols <valref> type any.

      ls_filter-name = ls_address-key_field.
      ls_filter-type = zcl_mockup_loader_utils=>c_filter_type-value.
      assign component ls_address-ref_field of structure i_cursor to <valref>.
      if sy-subrc <> 0.
        raise exception type zcx_text2tab_error
          exporting
            msg  = |Cannot find { ls_address-ref_field } in {
              cl_abap_typedescr=>describe_by_data( i_cursor )->absolute_name }|
            code = 'ZZ' " ???
            methname = 'DEEP->select'. " ???
*            line = line
*            field = field
*            structure = structure
*            location = location.
      endif.
      get reference of <valref> into ls_filter-valref.
    endif.

    data lx type ref to zcx_mockup_loader_error.
    try.
      mi_ml_instance->load_data(
        exporting
          i_obj    = ls_address-location
          i_strict = abap_false " ????
          i_where  = ls_filter
        importing
          e_container = e_container ).
    catch zcx_mockup_loader_error into lx.
      raise exception type zcx_text2tab_error
        exporting
          msg  = |@{ ls_address-location }: { lx->get_text( ) }|
          code = 'ZZ' " ???
          methname = 'DEEP->select'. " ???
    endtry.

  endmethod.
ENDCLASS.
