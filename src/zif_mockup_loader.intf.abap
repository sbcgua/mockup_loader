interface zif_mockup_loader
  public .

  constants version type string value 'v2.1.10'. "#EC NOTEXT
  constants origin type string value 'https://github.com/sbcgua/mockup_loader'. "#EC NOTEXT
  constants license type string value 'MIT'. "#EC NOTEXT

  types:
    ty_src_type     type c length 4,
    ty_date_format  type c length 4,
    ty_amt_format   type c length 2,
    ty_comment_char type c length 1.

  types:
    ty_filter_type type c length 1.

  constants:
    begin of c_filter_type,
      value type ty_filter_type value 'V',
      range type ty_filter_type value 'R',
    end of c_filter_type.

  types:
    begin of ty_filter,
      name   type string,
      valref type ref to data,
      type   type ty_filter_type,
    end of ty_filter .
  types:
    tt_filter type standard table of ty_filter with key name .
  types:
    begin of ty_where,
      name  type string,
      range type ref to data,
    end of ty_where .
  types:
    tt_where type standard table of ty_where with key name .

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
      !i_rename_fields type any optional
    exporting
      !e_container type any
    raising
      zcx_mockup_loader_error .

  methods set_params
    importing
      !i_amt_format type ty_amt_format optional
      !i_encoding type abap_encoding optional
      !i_date_format type ty_date_format optional
      !i_begin_comment type ty_comment_char optional .

  methods is_redirected
    returning
      value(r_yes) type abap_bool.

endinterface.
