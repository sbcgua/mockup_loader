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

    clear e_container.

  endmethod.
ENDCLASS.
