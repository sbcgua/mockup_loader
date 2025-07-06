interface lif_archive.
  data files type string_table.
  methods get
    importing
      name type string
    returning
      value(r_content) type xstring
    raising
      zcx_mockup_loader_error.
endinterface.
