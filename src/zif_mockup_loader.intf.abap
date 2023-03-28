interface zif_mockup_loader
  public.

  constants version type string value 'v2.2.0'. "#EC NOTEXT
  constants origin type string value 'https://github.com/sbcgua/mockup_loader'. "#EC NOTEXT
  constants license type string value 'MIT'. "#EC NOTEXT

  " Dependencies
  constants c_required_text2tab_ver type string value 'v2.3.2'.

**********************************************************************
* COMMON TYPES
**********************************************************************

  types:
    ty_src_type     type c length 4,
    ty_date_format  type c length 4,
    ty_amt_format   type c length 2,
    ty_comment_char type c length 1.

**********************************************************************
* FILTER RELATED TYPES
**********************************************************************

  types:
    ty_filter_operation type c length 1.

  constants:
    begin of c_filter_op,
      or type ty_filter_operation value '|',
      and type ty_filter_operation value '&',
    end of c_filter_op.

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
      operation type ty_filter_operation, " Internal usage for the moment
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

  types:
    tty_conv_exits type standard table of abap_editmask with default key.

**********************************************************************
* STUB RELATED TYPES
**********************************************************************

  types:
    begin of ty_stub_filter_param,
      mock_tab_key    type abap_compname,
      sift_param      type string,
      sift_const      type string,
      operation       type ty_filter_operation, " Internal usage for the moment
    end of ty_stub_filter_param.
  types:
    tty_stub_filter_params type standard table of ty_stub_filter_param with key mock_tab_key.
  types:
    tty_stub_sift_values type standard table of ref to data with default key.

  types:
    begin of ty_mock_config,
      method_name     type abap_methname,
      mock_name       type string,
      load_strict     type abap_bool,
      corresponding   type abap_bool,
      sift_param      type string,
      sift_const      type string,
      mock_tab_key    type abap_compname,
      output_param    type abap_parmname,
      output_pkind    type abap_parmkind,
      output_type     type ref to cl_abap_datadescr,
      as_proxy        type abap_bool,
      field_only      type abap_parmname,
      const_value     type string,
      deep            type abap_bool,
      filter          type tty_stub_filter_params,
    end of ty_mock_config .
  types:
    tt_mock_config type standard table of ty_mock_config with key method_name .
  types:
    tty_mock_config_by_methname type sorted table of ty_mock_config with unique key method_name .

**********************************************************************
* METHODS
**********************************************************************

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
      !i_begin_comment type ty_comment_char optional
      !it_ignore_conv_exits type tty_conv_exits optional .

  methods is_redirected
    returning
      value(r_yes) type abap_bool.
  methods cd
    importing
      !i_path type string.

endinterface.
