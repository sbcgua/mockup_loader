interface zif_mockup_loader
  public .

  methods load_raw_x
    importing
      !i_obj_path type string
    returning
      value(r_content) type xstring
    raising
      zcx_mockup_loader_error .

  methods load_data
    importing
      !i_obj type string
      !i_strict type abap_bool default abap_true
      !i_where type any optional
    exporting
      !e_container type any
    raising
      zcx_mockup_loader_error .

endinterface.
