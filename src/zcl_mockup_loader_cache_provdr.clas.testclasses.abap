class ltcl_cache_test definition
  for testing
  risk level harmless
  duration short.

  private section.

    methods blob_cache for testing.
endclass.

class ltcl_cache_test implementation.

  method blob_cache.

    data lo_cache type ref to zcl_mockup_loader_cache_provdr.
    data li_cache type ref to zif_mockup_loader_cache_provdr.

    lo_cache = zcl_mockup_loader_cache_provdr=>create(
      i_blob_cache_timeout = 5 ).
    lo_cache->reset( ).
    li_cache = lo_cache.

    li_cache->blob_set(
      i_key  = '123'
      i_blob = 'FFFFFF' ).
    cl_abap_unit_assert=>assert_equals(
      act = li_cache->blob_get( i_key  = '123' )
      exp = 'FFFFFF' ).
    cl_abap_unit_assert=>assert_equals(
      act = li_cache->mv_blob_cache_reuse_count
      exp = 1 ).

    li_cache = zcl_mockup_loader_cache_provdr=>create(
      i_blob_cache_timeout = 5 ).
    cl_abap_unit_assert=>assert_equals(
      act = li_cache->blob_get( i_key  = '123' )
      exp = 'FFFFFF' ).
    cl_abap_unit_assert=>assert_equals(
      act = li_cache->mv_blob_cache_reuse_count
      exp = 2 ).

  endmethod.

endclass.
