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
        content = content
      exceptions
        zip_index_error = 1
        others = 4 ).
    if sy-subrc <> 0.
      raise read_error.
    endif.

  endmethod.

endclass.

**********************************************************************

class lcl_text_archive definition final.
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
        i_zip_blob type xstring
      raising
        zcx_mockup_loader_error.

endclass.

class lcl_text_archive implementation.

  method new.
    create object ro_instance exporting i_zip_blob = i_zip_blob.
  endmethod.

  method constructor.
  endmethod.

  method parse.
  endmethod.

  method lif_archive~get.
  endmethod.

endclass.
