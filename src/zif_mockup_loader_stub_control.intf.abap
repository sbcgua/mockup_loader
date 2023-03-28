interface zif_mockup_loader_stub_control
  public.

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

  methods set_proxy_target
    importing
      io_proxy_target type ref to object optional.

endinterface.
