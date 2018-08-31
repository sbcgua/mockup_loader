interface zif_mockup_loader_stub_dummy
  public .


  methods tab_return
    importing
      !i_connid type s_conn_id
    returning
      value(rtab) type flighttab .
  methods tab_export
    importing
      !i_connid type s_conn_id
    exporting
      !etab type flighttab .
  methods tab_change
    importing
      !i_connid type s_conn_id
    changing
      !ctab type flighttab .

  methods wrong_return
    importing
      !i_connid type s_conn_id
    returning
      value(rtab) type char1 .

  methods wrong_sift
    importing
      !i_connid type sflight
    returning
      value(rtab) type flighttab .

  methods gen_param_target
    importing
      !p1 type i
      !p2 type clike
      !p3 type data
      !p4 type data optional
    changing
      ctab type flighttab.

endinterface.
