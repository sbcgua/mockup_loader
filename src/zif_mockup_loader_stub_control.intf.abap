interface ZIF_MOCKUP_LOADER_STUB_CONTROL
  public .

  methods enable
    importing
      i_method type abap_methname optional.

  methods disable
    importing
      i_method type abap_methname optional.

  methods get_call_count
    importing
      i_method type abap_methname
    returning
      value(rv_call_count) type i.

endinterface.
