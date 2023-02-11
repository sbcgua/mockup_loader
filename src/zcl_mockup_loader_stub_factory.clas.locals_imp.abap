class lcl_connect_string_parser definition final.
  public section.
    class-methods parse
      importing
        i_connect_string type string
      returning
        value(rs_parsed) type zif_mockup_loader=>ty_mock_config
      raising
        zcx_mockup_loader_error.
    methods constructor
      importing
        i_connect_string type string
      raising
        zcx_mockup_loader_error.

    data ms_parsed type zif_mockup_loader=>ty_mock_config read-only.

  private section.

    methods parse_connect_string
      importing
        i_connect_string type string
      raising
        zcx_mockup_loader_error .

    methods parse_filter_item
      importing
        i_filter type string
      returning
        value(rs_filter_param) type zif_mockup_loader=>ty_stub_filter_param
      raising
        zcx_mockup_loader_error .

    methods parse_method_and_mock
      importing
        i_pair type string
      raising
        zcx_mockup_loader_error.
    methods parse_filter
      importing
        i_filter type string
      raising
        zcx_mockup_loader_error.
    methods detect_corresp_and_const.
    methods detect_deep.

endclass.

class lcl_connect_string_parser implementation.

  method parse.
    data lo type ref to lcl_connect_string_parser.
    create object lo exporting i_connect_string = i_connect_string.
    rs_parsed = lo->ms_parsed.
  endmethod.

  method constructor.
    parse_connect_string( i_connect_string ).
  endmethod.

  method parse_connect_string.
    " for mock          `METHOD -> file`
    " for mock w/params `METHOD -> file [param = key]`
    " for mock w/params `METHOD -> file [param = key, param2 = key2]`
    " for mock w/params `METHOD -> file [param = "const"]`
    " for proxy         `METHOD -> *`
    " for corresponding `METHOD -> ~file`
    " for single field  `METHOD -> file(field_name)`
    " for fixed value   `METHOD -> =value`
    " for deep          `METHOD -> :deep:value`
    " TODO 'directives' - :xxx:

    data lv_pair type string.
    data lv_filter type string.
    data lv_len type i.

    " Pair and filter
    split i_connect_string at '[' into lv_pair lv_filter.
    if lv_filter is not initial.
      lv_len = strlen( lv_filter ).
      if substring( val = lv_filter off = lv_len - 1 ) <> ']'.
        zcx_mockup_loader_error=>raise( msg = 'incorrect connect string format' code = 'SF' ).
      endif.
      lv_filter = substring(
        val = lv_filter
        len = lv_len - 1 ).
    endif.

    parse_method_and_mock( lv_pair ).
    detect_corresp_and_const( ).
    detect_deep( ).
    parse_filter( lv_filter ).

  endmethod.

  method parse_method_and_mock.

    data lv_len type i.

    split i_pair at '->' into ms_parsed-method_name ms_parsed-mock_name.

    " Field only
    split ms_parsed-mock_name at '(' into ms_parsed-mock_name ms_parsed-field_only.
    if ms_parsed-field_only is not initial.
      lv_len = strlen( ms_parsed-field_only ).
      if substring( val = ms_parsed-field_only off = lv_len - 1 ) <> ')'.
        zcx_mockup_loader_error=>raise( msg = 'incorrect connect string format' code = 'SF' ).
      endif.
      ms_parsed-field_only = substring(
        val = ms_parsed-field_only
        len = lv_len - 1 ).
    endif.

    condense ms_parsed-method_name.
    condense ms_parsed-mock_name.

  endmethod.

  method detect_corresp_and_const.

    if ms_parsed-mock_name cp '*/~*'.
      ms_parsed-mock_name = replace(
        val = ms_parsed-mock_name
        sub = '/~'
        with = '/' ).
      ms_parsed-corresponding = abap_true.
    elseif ms_parsed-mock_name cp '~*'.
      ms_parsed-mock_name = replace(
        val = ms_parsed-mock_name
        sub = '~'
        with = '' ).
      ms_parsed-corresponding = abap_true.
    elseif ms_parsed-mock_name+0(1) = '='.
      ms_parsed-const_value = ms_parsed-mock_name.
      clear ms_parsed-mock_name.
      shift ms_parsed-const_value left by 1 places.
      condense ms_parsed-const_value.
    endif.

  endmethod.

  method detect_deep.

    if ms_parsed-mock_name cp ':deep:*'.
      ms_parsed-mock_name = replace(
        val = ms_parsed-mock_name
        sub = ':deep:'
        with = '' ).
      ms_parsed-deep = abap_true.
      condense ms_parsed-mock_name.
    endif.

  endmethod.

  method parse_filter.

    data lv_offs type i.
    data lt_filter_items type string_table.
    data lv_sep type c.
    field-symbols <str> like line of lt_filter_items.
    field-symbols <f> like line of ms_parsed-filter.

    " detect multi filter
    find first occurrence of regex '[,&]' in i_filter match offset lv_offs.

    if sy-subrc = 0.
      lv_sep = i_filter+lv_offs(1).
      split i_filter at lv_sep into table lt_filter_items.

      loop at lt_filter_items assigning <str>.
        append initial line to ms_parsed-filter assigning <f>.
        <f> = parse_filter_item( <str> ).
      endloop.
    else.
      move-corresponding parse_filter_item( i_filter ) to ms_parsed.
    endif.

  endmethod.

  method parse_filter_item.

    data lv_len type i.

    split i_filter at '=' into rs_filter_param-mock_tab_key rs_filter_param-sift_param.
    condense rs_filter_param-sift_param.

    if rs_filter_param-sift_param is not initial and rs_filter_param-sift_param+0(1) = '"'.
      lv_len = strlen( rs_filter_param-sift_param ).
      if substring( val = rs_filter_param-sift_param off = lv_len - 1 ) <> '"'.
        zcx_mockup_loader_error=>raise( msg = 'incorrect connect string format' code = 'SF' ).
      endif.
      rs_filter_param-sift_const = substring(
        val = rs_filter_param-sift_param
        len = lv_len - 2
        off = 1 ).
      clear rs_filter_param-sift_param.
    endif.

    condense rs_filter_param-sift_const.
    condense rs_filter_param-mock_tab_key.

  endmethod.

endclass.
