interface zif_mockup_loader_cache_provdr
  public .

  data mv_blob_cache_reuse_count type i read-only.

  methods blob_get
    importing
      i_key type string
    returning
      value(r_blob) type xstring.

  methods blob_set
    importing
      i_key type string
      i_blob type xstring.

endinterface.
