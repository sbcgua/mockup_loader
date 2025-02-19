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

    methods parse
      importing
        i_data type string
      raising
        zcx_mockup_loader_error.

    class-methods xstring_to_string_utf8
      importing
        i_data           type xstring
      returning
        value(rv_string) type string .

endclass.

class lcl_text_archive implementation.

  method new.
    create object ro_instance exporting i_blob = i_blob.
  endmethod.

  method constructor.
    parse( xstring_to_string_utf8( i_blob ) ).
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
    endtry.

  endmethod.

  method parse.

*start - expect head
*head_trail - expect FILE or non !!
*file - expect !! or non !! to start data
*data - expect existing line
*gap - expect empty line or FILE

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

    data lt_file_data like mt_data.
    field-symbols <i> like line of mt_data.

    loop at mt_data assigning <i> from <index>-start to <index>-start + <index>-size.
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
