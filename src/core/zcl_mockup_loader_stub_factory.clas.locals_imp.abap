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
        zcx_mockup_loader_error.

    methods parse_filter_item
      importing
        i_filter type string
      returning
        value(rs_filter_param) type zif_mockup_loader=>ty_stub_filter_param
      raising
        zcx_mockup_loader_error.

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
    " for mock           `METHOD -> file`
    " for mock w/params  `METHOD -> file [param = key]`
    " for mock w/params  `METHOD -> file [param = key, param2 = key2]`
    " for mock w/params  `METHOD -> file [param = "const"]`
    " for proxy          `METHOD -> *`
    " for corresponding  `METHOD -> ~file`
    " for single field   `METHOD -> file(field_name)`
    " to check exists    `METHOD -> file(?)`
    " for fixed value    `METHOD -> =value`
    " for deep           `METHOD -> :deep:value`
    " to spec. out param `METHOD(out_param) -> file`
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
      parse_filter( lv_filter ).
    endif.

    parse_method_and_mock( lv_pair ).
    detect_corresp_and_const( ).
    detect_deep( ).

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
      condense ms_parsed-field_only.
      if ms_parsed-field_only is initial.
        zcx_mockup_loader_error=>raise( msg = 'missing field only' code = 'MFO' ).
      endif.
    endif.

    " Output param
    split ms_parsed-method_name at '(' into ms_parsed-method_name ms_parsed-output_param.
    if ms_parsed-output_param is not initial.
      lv_len = strlen( ms_parsed-output_param ).
      if substring( val = ms_parsed-output_param off = lv_len - 1 ) <> ')'.
        zcx_mockup_loader_error=>raise( msg = 'missing output param' code = 'WOP' ).
      endif.
      ms_parsed-output_param = substring(
        val = ms_parsed-output_param
        len = lv_len - 1 ).
      condense ms_parsed-output_param.
      if ms_parsed-output_param is initial.
        zcx_mockup_loader_error=>raise( msg = 'missing output param' code = 'MOP' ).
      endif.
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

    data lv_pre type i.
    data lv_break type i.
    data lv_len type i.
    data lt_filter_items type string_table.
    data lv_op type zif_mockup_loader=>ty_filter_operation.
    data lv_op_next type zif_mockup_loader=>ty_filter_operation.
    field-symbols <str> like line of lt_filter_items.
    field-symbols <f> like line of ms_parsed-filter.

    " detect multi filter
    lv_len = strlen( i_filter ).
    do.
      lv_break = find(
        val   = i_filter
        regex = `[,&|]`
        off   = lv_pre ).
      if lv_break < 0.
        append initial line to lt_filter_items assigning <str>.
        <str> = substring(
          val = i_filter
          off = lv_pre
          len = lv_len - lv_pre ).
        exit.
      endif.

      lv_op_next = i_filter+lv_break(1).
      if lv_op_next = ','.
        lv_op_next = '&'.
      endif.
      if lv_op is not initial and lv_op <> lv_op_next.
        zcx_mockup_loader_error=>raise( msg = '& and | at the same time is currently not supported' code = 'OA' ).
      elseif lv_op is initial.
        lv_op = lv_op_next.
      endif.

      append initial line to lt_filter_items assigning <str>.
      <str> = substring(
        val = i_filter
        off = lv_pre
        len = lv_break - lv_pre ).
      lv_pre = lv_break + 1.
    enddo.

    if lv_op is initial.
      move-corresponding parse_filter_item( i_filter ) to ms_parsed.
    else.
      loop at lt_filter_items assigning <str>.
        append initial line to ms_parsed-filter assigning <f>.
        <f> = parse_filter_item( <str> ).
        <f>-operation = lv_op.
      endloop.
    endif.

  endmethod.

  method parse_filter_item.

    data lv_len type i.

    split i_filter at '=' into rs_filter_param-mock_tab_key rs_filter_param-sift_param.
    condense rs_filter_param-mock_tab_key.
    condense rs_filter_param-sift_param.

    if rs_filter_param-mock_tab_key is initial or rs_filter_param-sift_param is initial.
      zcx_mockup_loader_error=>raise( msg = 'incorrect connect string format' code = 'SF' ).
    endif.

    if rs_filter_param-sift_param+0(1) = '"'.
      lv_len = strlen( rs_filter_param-sift_param ).
      if lv_len < 2 or substring( val = rs_filter_param-sift_param off = lv_len - 1 ) <> '"'.
        zcx_mockup_loader_error=>raise( msg = 'incorrect connect string format' code = 'SF' ).
      endif.
      rs_filter_param-sift_const = substring(
        val = rs_filter_param-sift_param
        len = lv_len - 2
        off = 1 ).
      condense rs_filter_param-sift_const.
      rs_filter_param-sift_initial = boolc( rs_filter_param-sift_const is initial ).
      clear rs_filter_param-sift_param.
    endif.

  endmethod.

endclass.
