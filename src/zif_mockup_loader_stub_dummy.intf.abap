interface zif_mockup_loader_stub_dummy
  public.

  types:
    begin of ty_params,
      param1 type string,
      connid type s_conn_id,
    end of ty_params.

  types:
    begin of ty_sflight_extract,
      connid type sflight-connid,
      fldate type sflight-fldate,
      price type sflight-price,
    end of ty_sflight_extract,
    tt_sflight_extract type standard table of ty_sflight_extract with default key.

  types:
    begin of ty_deep,
      carrid type sflight-carrid,
      connid type sflight-connid,
      prices type tt_sflight_extract,
    end of ty_deep,
    tt_deep type standard table of ty_deep with key carrid connid.

  types:
    ty_connid_range type range of sflight-connid.

  methods tab_return
    importing
      !i_connid type s_conn_id
    returning
      value(r_tab) type flighttab .

  methods tab_return_w_date
    importing
      !i_connid type s_conn_id
      !i_fldate type sflight-fldate
    returning
      value(r_tab) type flighttab .

  methods tab_return_by_range
    importing
      !ir_connid type ty_connid_range
    returning
      value(r_tab) type flighttab .

  methods tab_return_w_struc_param
    importing
      !i_params type ty_params
    returning
      value(r_tab) type flighttab .

  methods tab_return_extract_by_date
    importing
      !i_fldate type sflight-fldate
    returning
      value(r_tab) type tt_sflight_extract .

  methods tab_export
    importing
      !i_connid type s_conn_id
    exporting
      !e_tab type flighttab .
  methods tab_change
    importing
      !i_connid type s_conn_id
    changing
      !c_tab type flighttab .

  methods wrong_return
    importing
      !i_connid type s_conn_id
    returning
      value(r_tab) type sflight-carrid .

  methods wrong_sift
    importing
      !i_connid type sflight
    returning
      value(r_tab) type flighttab .

  methods gen_param_target
    importing
      !i_p1 type i
      !i_p2 type clike
      !i_p3 type data
      !i_p4 type data optional
    changing
      c_tab type flighttab.

  methods proxy_test
    importing
      !i_p1 type string
      !i_p2 type i optional
    returning value(r_val) type string .

  methods return_value
    importing
      !i_connid type s_conn_id
    returning
      value(r_val) type sflight-price .

  methods exists
    importing
      !i_connid type s_conn_id
    returning
      value(r_yes) type abap_bool .

  methods return_value_w_date
    importing
      !i_connid type s_conn_id
      !i_fldate type sflight-fldate
    returning
      value(r_val) type sflight-price .

  methods return_deep
    importing
      !i_connid type s_conn_id
    returning
      value(r_deep) type tt_deep .

endinterface.
