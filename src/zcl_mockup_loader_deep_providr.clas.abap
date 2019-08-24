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
      ls_filter = zcl_mockup_loader_utils=>conv_single_val_to_filter(
        i_where = ls_address-key_field
        i_value = ls_address-key_value ).
    else. " ref field is not initial
      field-symbols <valref> type any.

      ls_filter-name = ls_address-key_field.
      ls_filter-type = 'V'. " Value " TODO refactor to a constant
      assign component ls_address-ref_field of structure i_cursor to <valref>.
      if sy-subrc <> 0.
        "throw
      endif.
      get reference of <valref> into ls_filter-valref.

    endif.

    mi_ml_instance->load_data(
      exporting
        i_obj    = ls_address-location
        i_strict = abap_false " ????
        i_where  = ls_filter
      importing
        e_container = e_container ).

  endmethod.
ENDCLASS.
