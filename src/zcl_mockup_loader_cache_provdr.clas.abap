class ZCL_MOCKUP_LOADER_CACHE_PROVDR definition
  public
  final
  create public .

  public section.

    interfaces zif_mockup_loader_cache_provdr .

    class-methods create
      importing
        !i_blob_cache_timeout type i
      returning
        value(ro_instance) type ref to zcl_mockup_loader_cache_provdr.

    methods reset.

  protected section.
  private section.
    constants c_blob_cache_meta_mem_id type c length 40 value 'zcl_mockup_loader:blob_cache:meta'.
    constants c_blob_cache_mem_id type c length 40 value 'zcl_mockup_loader:blob_cache'.

    data mv_blob_cache_timeout type i.

    types:
      begin of ty_blob_cache,
        key type string,
        blob type xstring,
      end of ty_blob_cache.

    class-data gt_blob_cache type standard table of ty_blob_cache.

ENDCLASS.



CLASS ZCL_MOCKUP_LOADER_CACHE_PROVDR IMPLEMENTATION.


  method create.
    create object ro_instance.
    ro_instance->mv_blob_cache_timeout = i_blob_cache_timeout.
  endmethod.


  method reset.
    clear gt_blob_cache.
    clear zif_mockup_loader_cache_provdr~mv_blob_cache_reuse_count.
  endmethod.


  method zif_mockup_loader_cache_provdr~blob_get.

    field-symbols <cache> like line of gt_blob_cache.

    read table gt_blob_cache with key key = i_key assigning <cache>.
    if sy-subrc = 0.
      r_blob = <cache>-blob.
      zif_mockup_loader_cache_provdr~mv_blob_cache_reuse_count =
        zif_mockup_loader_cache_provdr~mv_blob_cache_reuse_count + 1.
    endif.

  endmethod.


  method zif_mockup_loader_cache_provdr~blob_set.

    field-symbols <cache> like line of gt_blob_cache.

    read table gt_blob_cache with key key = i_key assigning <cache>.
    if sy-subrc <> 0.
      append initial line to gt_blob_cache assigning <cache>.
      <cache>-key = i_key.
    endif.
    <cache>-blob = i_blob.

  endmethod.
ENDCLASS.
