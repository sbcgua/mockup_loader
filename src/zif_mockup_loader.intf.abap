interface zif_mockup_loader
  public .

  methods load_blob
    importing
      !i_obj_path type string
    returning
      value(r_content) type xstring
    raising
      zcx_mockup_loader_error .

  methods load_data
    importing
      !i_obj    type string
      !i_strict type abap_bool default abap_false
      !i_corresponding type abap_bool default abap_false
      !i_deep   type abap_bool default abap_false
      !i_where  type any optional
    exporting
      !e_container type any
    raising
      zcx_mockup_loader_error .

  methods set_params
    importing
      !i_amt_format type char2 optional
      !i_encoding type abap_encoding optional
      !i_date_format type char4 optional
      !i_begin_comment type char1 optional .

endinterface.
