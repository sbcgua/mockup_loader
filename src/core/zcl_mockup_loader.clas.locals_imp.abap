class lcl_zip_archive definition final.
  public section.
    interfaces lif_archive.

    class-methods new
      importing
        i_zip_blob type xstring
      returning
        value(ro_instance) type ref to lcl_zip_archive
      raising
        zcx_mockup_loader_error.
    methods constructor
      importing
        i_zip_blob type xstring
      raising
        zcx_mockup_loader_error.

  private section.
    data mo_zip type ref to cl_abap_zip.
endclass.

class lcl_zip_archive implementation.

  method new.
    create object ro_instance exporting i_zip_blob = i_zip_blob.
  endmethod.

  method constructor.

    create object mo_zip.

    mo_zip->load(
      exporting
        zip    = i_zip_blob
      exceptions
        others = 4 ).

    if sy-subrc <> 0 or lines( mo_zip->files ) = 0.
      zcx_mockup_loader_error=>raise( msg = 'ZIP load failed' code = 'ZE' ).  "#EC NOTEXT
    endif.

    field-symbols <f> like line of mo_zip->files.
    loop at mo_zip->files assigning <f>.
      append <f>-name to lif_archive~files.
    endloop.

  endmethod.

  method lif_archive~get.

    mo_zip->get(
      exporting
        name    = name
      importing
        content = r_content
      exceptions
        zip_index_error = 1
        others = 4 ).
    if sy-subrc <> 0.
      raise exception type zcx_mockup_loader_error.
    endif.

  endmethod.

endclass.

**********************************************************************

class lcl_text_archive definition final.
  public section.
    interfaces lif_archive.

    class-methods new
      importing
        i_blob type xstring
      returning
        value(ro_instance) type ref to lcl_text_archive
      raising
        zcx_mockup_loader_error.
    methods constructor
      importing
        i_blob type xstring
      raising
        zcx_mockup_loader_error.

    class-methods xstring_to_string_utf8
      importing
        i_data           type xstring
      returning
        value(rv_string) type string
      raising
        zcx_mockup_loader_error.

    class-methods string_to_xstring_utf8
      importing
        i_str           type string
      returning
        value(rv_xdata) type xstring
      raising
        zcx_mockup_loader_error.

  private section.

    types ty_data_type type c length 1.
    constants:
      begin of c_data_type,
        text type ty_data_type value 'T',
      end of c_data_type.

    types:
      begin of ty_index,
        name  type string,
        start type i,
        size  type i,
        dtype type ty_data_type,
      end of ty_index.

    data mt_data  type string_table.
    data mt_index type sorted table of ty_index with unique key name.

    class-methods parse_structure
      importing
        it_data type string_table
      returning
        value(rt_index) like mt_index
      raising
        zcx_mockup_loader_error.

    class-methods parse_file_tag
      importing
        iv_file_params type string
      returning
        value(rs_file) like line of mt_index
      raising
        zcx_mockup_loader_error.

endclass.

class lcl_text_archive implementation.

  method new.
    create object ro_instance exporting i_blob = i_blob.
  endmethod.

  method constructor.

    split xstring_to_string_utf8( i_blob ) at |\n| into table mt_data.

    mt_index = parse_structure( mt_data ).

    field-symbols <i> like line of mt_index.
    loop at mt_index assigning <i>.
      append <i>-name to lif_archive~files.
    endloop.

  endmethod.

  method string_to_xstring_utf8.

    call function 'SCMS_STRING_TO_XSTRING'
      exporting
        text     = i_str
        encoding = '4110'
      importing
        buffer = rv_xdata
      exceptions
        others = 4.

    if sy-subrc <> 0.
      zcx_mockup_loader_error=>raise( 'Error in xstring_to_string_utf8' ).
    endif.

  endmethod.

  method xstring_to_string_utf8.

    data lo_convert_in type ref to cl_abap_conv_in_ce.

    try.
      lo_convert_in = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).
      lo_convert_in->convert(
        exporting
          input = i_data
          n     = xstrlen( i_data )
        importing
          data  = rv_string ).
    catch
      cx_parameter_invalid_range
      cx_sy_codepage_converter_init
      cx_sy_conversion_codepage
      cx_parameter_invalid_type.
      zcx_mockup_loader_error=>raise( 'Error in xstring_to_string_utf8' ).
    endtry.

  endmethod.


  method parse_structure.

    constants:
      begin of c_state,
        start type i value 0,
        head type i value 1,
        file type i value 2,
        data type i value 4,
        end_of_data type i value 5,
      end of c_state.

    data lv_state type i value c_state-start.
    data lv_is_markup type abap_bool.
    data lv_tag type string.
    data lv_params type string.
    data ls_file like line of rt_index.
    data lv_no type i.
    data lx type ref to zcx_mockup_loader_error.
    data lv_expected_file_count type i.

    field-symbols <i> like line of it_data.

    loop at it_data assigning <i>.
      lv_no = sy-tabix.

      if lv_state <> c_state-data.
        lv_is_markup = boolc( strlen( <i> ) > 2 and find( val = <i> sub = '!!' len = 2 ) = 0 ).
        if lv_is_markup = abap_true.
          split <i> at ` ` into lv_tag lv_params.
          lv_tag = substring( val = lv_tag off = 2 ).
        endif.
      endif.

      case lv_state.
        when c_state-start.
          if not ( lv_is_markup = abap_true and lv_tag = 'MOCKUP-LOADER-FORMAT' ).
            zcx_mockup_loader_error=>raise(
              msg = |Text format parser @{ lv_no }: MOCKUP-LOADER-FORMAT tag expected|
              code = 'TPE' ).
          endif.

          data lv_version type string.
          lv_version = lv_params.

          if lv_version <> '1.0'.
            zcx_mockup_loader_error=>raise(
              msg = |Text format parser @{ lv_no }: format version is not supported|
              code = 'TPE' ).
          endif.

          lv_state = c_state-head.

        when c_state-head.
          if lv_is_markup = abap_true and lv_tag = 'FILE'.
            lv_state = c_state-file.
          elseif lv_is_markup = abap_true and lv_tag = 'FILE-COUNT'.
            if not ( lv_params co '0123456789' ).
              zcx_mockup_loader_error=>raise( msg = |Text format parser @{ lv_no }: incorrect file count value| code = 'TPE' ).
            endif.
            lv_expected_file_count = lv_params.
          endif.

        when c_state-data.
          if lv_no - ls_file-start + 1 = ls_file-size.
            lv_state = c_state-end_of_data.
          else.
            continue.
          endif.

        when c_state-end_of_data.
          if <i> is initial.
            continue.
          elseif lv_is_markup = abap_true and lv_tag = 'FILE'.
            lv_state = c_state-file.
          elseif lv_is_markup = abap_true.
            zcx_mockup_loader_error=>raise( msg = |Text format parser @{ lv_no }: unexpected tag| code = 'TPE' ).
          else.
            zcx_mockup_loader_error=>raise( msg = |Text format parser @{ lv_no }: unexpected data| code = 'TPE' ).
          endif.

        when others.
          zcx_mockup_loader_error=>raise( msg = |Text format parser @{ lv_no }: unexpected state { lv_state }| code = 'TPE' ).
      endcase.

      " Process start-of-file
      if lv_state = c_state-file.
        try.
          ls_file = parse_file_tag( lv_params ).
          ls_file-start = lv_no + 1.
        catch zcx_mockup_loader_error into lx.
          lx->remsg( replace( val = lx->msg sub = '@' with = |@{ lv_no }| ) ).
          raise exception lx.
        endtry.

        insert ls_file into table rt_index.
        if sy-subrc <> 0.
          zcx_mockup_loader_error=>raise( msg = |Text format parser @{ lv_no }: file duplicate { ls_file-name }| code = 'TPE' ).
        endif.

        if ls_file-size > 0.
          lv_state = c_state-data. " start of data
        else.
          lv_state = c_state-end_of_data.
        endif.

      endif.

    endloop.

    if lv_state = c_state-data.
      zcx_mockup_loader_error=>raise( msg = |Text format parser: unexpected end of data in { ls_file-name }| code = 'TPE' ).
    endif.

    if lines( rt_index ) <> lv_expected_file_count.
      zcx_mockup_loader_error=>raise(
        msg = |Text format parser: file count ({ lines( rt_index ) }) <> expected ({ lv_expected_file_count })|
        code = 'TPE' ).
    endif.

  endmethod.

  method parse_file_tag.

    data lv_type type string.
    data lv_size type string.

    split iv_file_params at ` ` into rs_file-name lv_type lv_size.

    if rs_file-name is initial.
      zcx_mockup_loader_error=>raise( msg = |Text format parser @: file name is not defined| code = 'TPE' ).
    elseif lv_type is initial.
      zcx_mockup_loader_error=>raise( msg = |Text format parser @: file type is not defined| code = 'TPE' ).
    elseif lv_size is initial.
      zcx_mockup_loader_error=>raise( msg = |Text format parser @: file size is not defined| code = 'TPE' ).
    endif.

    if lv_type = 'text'.
      rs_file-dtype = c_data_type-text.
    else.
      zcx_mockup_loader_error=>raise( msg = |Text format parser @: file type { lv_type } is not supported| code = 'TPE' ).
    endif.

    if lv_size co '0123456789'.
      rs_file-size = lv_size.
    else.
      zcx_mockup_loader_error=>raise( msg = |Text format parser @: file size is incorrect| code = 'TPE' ).
    endif.

  endmethod.


  method lif_archive~get.

    field-symbols <index> like line of mt_index.

    read table mt_index assigning <index> with key name = name.
    if sy-subrc <> 0.
      raise exception type zcx_mockup_loader_error.
    endif.

    if <index>-dtype <> c_data_type-text.
      zcx_mockup_loader_error=>raise( msg = |Unsupported data type in file { <index>-name }| code = 'UNST' ).
    endif.

    if <index>-size = 0.
      return. " Empty file
    endif.

    data lt_file_data like mt_data.
    field-symbols <i> like line of mt_data.

    loop at mt_data assigning <i> from <index>-start to <index>-start + <index>-size - 1.
      append <i> to lt_file_data.
    endloop.

    data lv_str type string.
    lv_str = concat_lines_of( table = lt_file_data sep = |\n| ).

    call function 'SCMS_STRING_TO_XSTRING'
      exporting
        text     = lv_str
        encoding = '4110'
      importing
        buffer = r_content
      exceptions
        others = 4.

    if sy-subrc <> 0.
      zcx_mockup_loader_error=>raise( msg = |Error in convertion of file { <index>-name }| code = 'SXCO' ).
    endif.

  endmethod.

endclass.
