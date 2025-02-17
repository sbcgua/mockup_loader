interface lif_archive.
  data files type string_table.
  methods get
    importing
      name type string
    exporting
      content type xstring
    exceptions
      read_error.
endinterface.
