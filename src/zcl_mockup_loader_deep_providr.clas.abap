class ZCL_MOCKUP_LOADER_DEEP_PROVIDR definition
  public
  final
  create public .

  public section.
    interfaces zif_text2tab_deep_provider.

    methods constructor
      importing
        io_ml_instance type ref to zcl_mockup_loader.

  protected section.
  private section.
    data mo_ml_instance type ref to zcl_mockup_loader.

ENDCLASS.



CLASS ZCL_MOCKUP_LOADER_DEEP_PROVIDR IMPLEMENTATION.


  method constructor.
    mo_ml_instance = io_ml_instance.
  endmethod.


  method zif_text2tab_deep_provider~select.

    clear e_container.

  endmethod.
ENDCLASS.
