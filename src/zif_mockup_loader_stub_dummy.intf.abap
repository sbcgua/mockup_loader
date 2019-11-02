interface zif_mockup_loader_stub_dummy
  public .

  types:
    begin of ty_params,
      param1 type string,
      connid type s_conn_id,
    end of ty_params.

  methods tab_return
    importing
      !i_connid type s_conn_id
    returning
      value(r_tab) type flighttab .

  methods tab_return_w_struc_param
    importing
      !i_params type ty_params
    returning
      value(r_tab) type flighttab .

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
      value(r_tab) type char1 .

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

endinterface.
