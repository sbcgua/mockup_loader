*/--------------------------------------------------------------------------------\
*| This file is part of Mockup loader                                             |
*|                                                                                |
*| The MIT License (MIT)                                                          |
*|                                                                                |
*| Copyright (c) 2015 Alexander Tsybulsky, SBCG Team (www.sbcg.com.ua)            |
*|                                                                                |
*| Permission is hereby granted, free of charge, to any person obtaining a copy   |
*| of this software and associated documentation files (the "Software"), to deal  |
*| in the Software without restriction, including without limitation the rights   |
*| to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      |
*| copies of the Software, and to permit persons to whom the Software is          |
*| furnished to do so, subject to the following conditions:                       |
*|                                                                                |
*| The above copyright notice and this permission notice shall be included in all |
*| copies or substantial portions of the Software.                                |
*|                                                                                |
*| THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     |
*| IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       |
*| FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    |
*| AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         |
*| LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  |
*| OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  |
*| SOFTWARE.                                                                      |
*|--------------------------------------------------------------------------------|
*| project homepage: https://github.com/sbcgua/mockup_loader                      |
*\--------------------------------------------------------------------------------/

class ZCL_MOCKUP_LOADER definition
  public
  final
  create private.

  public section.

    interfaces zif_mockup_loader.
    aliases:
      set_params for zif_mockup_loader~set_params,
      load_blob for zif_mockup_loader~load_blob,
      load_data for zif_mockup_loader~load_data.

    class-methods create
      importing
        !i_path type string
        !i_type type zif_mockup_loader=>ty_src_type default 'MIME'
        !i_amt_format type zif_mockup_loader=>ty_amt_format optional
        !i_encoding type abap_encoding optional
        !i_date_format type zif_mockup_loader=>ty_date_format optional
        !i_begin_comment type zif_mockup_loader=>ty_comment_char optional
        !i_skip_lines_starting_with type zif_mockup_loader=>ty_comment_char optional
        !it_ignore_conv_exits type zif_mockup_loader=>tty_conv_exits optional
        !i_cache_timeout type i optional " Experimental feature, do not use yet
      returning
        value(ro_instance) type ref to zcl_mockup_loader
      raising
        zcx_mockup_loader_error.
    class-methods assert_version
      importing
        !i_required_version type string
      raising
        zcx_mockup_loader_error.
    class-methods check_version_fits
      importing
        !i_required_version type string
      returning
        value(r_fits) type abap_bool.
    class-methods flush_cache.
    methods constructor
      raising
        zcx_mockup_loader_error.

    class-data:
      begin of gs_cache_stats read-only,
        stash_count type i,
        reuse_count type i,
        import_count type i,
      end of gs_cache_stats.

  protected section.
  private section.

    types:
      begin of ty_load_params,
        obj_path         type string,
        is_strict        type abap_bool,
        is_corresponding type abap_bool,
        is_deep          type abap_bool,
        filter           type zif_mockup_loader=>tt_filter,
        rename_map       type zif_text2tab=>tt_field_name_map,
      end of ty_load_params.

    types:
      begin of ty_zip_cache,
        key type string,
        is_txt type abap_bool,
        zip_blob type xstring,
      end of ty_zip_cache.

    constants gc_zip_cache_ts_mem_id type c length 40 value 'zcl_mockup_loader:zip_cache:ts'.
    constants gc_zip_cache_mem_id type c length 40 value 'zcl_mockup_loader:zip_cache'.

    class-data gt_zip_cache type standard table of ty_zip_cache.

    data mi_archive type ref to lif_archive.
    data mv_amt_format type zif_mockup_loader=>ty_amt_format.
    data mv_encoding type abap_encoding.
    data mv_date_format type zif_mockup_loader=>ty_date_format.
    data mv_begin_comment type zif_mockup_loader=>ty_comment_char.
    data mv_skip_lines_starting_with type zif_mockup_loader=>ty_comment_char.
    data mv_is_redirected type abap_bool.
    data mt_ignore_conv_exits type zif_mockup_loader=>tty_conv_exits.
    data mv_dir type string.
    data mr_ref_to_container type ref to data.
    data ms_next_load_params type ty_load_params.

    class-methods read_zip_blob
      importing
        !i_path type string
        !i_type type zif_mockup_loader=>ty_src_type
      exporting
        e_is_txt type abap_bool
        e_xdata  type xstring
      raising
        zcx_mockup_loader_error.

    methods parse_data
      importing
        !i_rawdata type string
        !i_strict type abap_bool
        !i_corresponding type abap_bool default abap_false
        !i_deep type abap_bool default abap_false
        !i_where type any optional
        !i_rename_fields type any optional
      exporting
        !e_container type any
      raising
        zcx_mockup_loader_error.
    methods read_zip
      importing
        !i_name type string
      returning
        value(r_rawdata) type string
      raising
        zcx_mockup_loader_error.
    methods find_file_case_insensitive
      importing
        i_path type string
      returning
        value(rv_file) type string.
    class-methods redirect_source
      changing
        c_src_type      type zif_mockup_loader=>ty_src_type
        c_src_path      type string
        c_is_redirected type abap_bool.
    class-methods build_table_type
      importing
        io_type_descr   type ref to cl_abap_typedescr
        it_filter       type zif_mockup_loader=>tt_filter
        i_corresponding type abap_bool default abap_false
      returning
        value(ro_table_descr) type ref to cl_abap_tabledescr
      raising
        zcx_mockup_loader_error.
    methods normalize_path
      importing
        i_path type string
      returning
        value(rv_path) type string.
ENDCLASS.



CLASS ZCL_MOCKUP_LOADER IMPLEMENTATION.


  method assert_version.

    data lv_version_ok type abap_bool.

    lv_version_ok = zcl_text2tab_utils=>check_version_fits(
      i_current_version  = zif_mockup_loader=>version
      i_required_version = i_required_version ).

    if lv_version_ok = abap_false.
      zcx_mockup_loader_error=>raise(
        |mockup loader version ({ zif_mockup_loader=>version
        }) < required ({ i_required_version })| ). "#EC NOTEXT
    endif.

  endmethod.


  method build_table_type.

    data lo_table_descr type ref to cl_abap_tabledescr.
    data lo_struc_descr type ref to cl_abap_structdescr.

    case io_type_descr->kind.
      when 'T'. " Table
        lo_table_descr ?= io_type_descr.
        lo_struc_descr ?= lo_table_descr->get_table_line_type( ).
      when 'S'. " Structure
        lo_struc_descr ?= io_type_descr.
      when others. " Not a table or structure ?
        zcx_mockup_loader_error=>raise( msg = 'Table or structure containers only' code = 'DT' ). "#EC NOTEXT
    endcase.

    if i_corresponding = abap_true and lines( it_filter ) > 0.

      data lt_components type cl_abap_structdescr=>component_table.
      data lt_components_to_add type cl_abap_structdescr=>component_table.
      data ld_value type ref to cl_abap_typedescr.
      data ld_range_tab type ref to cl_abap_tabledescr.
      data ld_range_line type ref to cl_abap_structdescr.

      lt_components = lo_struc_descr->get_components( ).
      sort lt_components by name.

      field-symbols <f> like line of it_filter.
      field-symbols <addcomp> like line of lt_components_to_add.

      loop at it_filter assigning <f>.
        read table lt_components transporting no fields
          binary search
          with key name = <f>-name.
        if sy-subrc is initial.
          continue. " found, no need to add
        endif.
        ld_value = cl_abap_typedescr=>describe_by_data_ref( <f>-valref ).
        if <f>-type = zif_mockup_loader=>c_filter_type-range.
          ld_range_tab ?= ld_value.
          ld_range_line ?= ld_range_tab->get_table_line_type( ).
          ld_value = ld_range_line->get_component_type( 'LOW' ).
        endif.
        append initial line to lt_components_to_add assigning <addcomp>.
        <addcomp>-name = to_upper( <f>-name ).
        <addcomp>-type ?= ld_value.
      endloop.

      append lines of lt_components_to_add to lt_components.
      lo_struc_descr = cl_abap_structdescr=>get( lt_components ).

    endif.

    ro_table_descr = cl_abap_tabledescr=>create( lo_struc_descr ).

  endmethod.


  method check_version_fits.

    r_fits = zcl_text2tab_utils=>check_version_fits(
      i_current_version  = zif_mockup_loader=>version
      i_required_version = i_required_version ).

  endmethod.


  method constructor.

    constants lc_required_ver type string value zif_mockup_loader=>c_required_text2tab_ver.

    if zcl_text2tab_parser=>check_version_fits( lc_required_ver ) = abap_false.
      zcx_mockup_loader_error=>raise(
        msg  = |text2tab version ({ zif_text2tab=>version
          }) is lower than required ({ lc_required_ver })|
        code = 'VL' ). "#EC NOTEXT
    endif.

  endmethod.


  method create.

    create object ro_instance.

    ro_instance->set_params(
      i_amt_format         = i_amt_format
      i_encoding           = i_encoding
      i_date_format        = i_date_format
      i_begin_comment      = i_begin_comment
      i_skip_lines_starting_with = i_skip_lines_starting_with
      it_ignore_conv_exits = it_ignore_conv_exits ).

    data l_src_type type zif_mockup_loader=>ty_src_type.
    data l_src_path type string.

    l_src_type = i_type.
    l_src_path = i_path.

    redirect_source(
      changing
        c_is_redirected = ro_instance->mv_is_redirected
        c_src_type = l_src_type
        c_src_path = l_src_path ).

    data lv_xdata type xstring.
    data lv_zip_cache_key type string.
    data lv_cache_timestamp type timestamp.
    data lv_now_timestamp type timestamp.
    data lv_diff type i.
    data lv_is_txt_format type abap_bool.
    field-symbols <zip_cache> like line of gt_zip_cache.

    if i_cache_timeout > 0. " Caching active
      lv_zip_cache_key = l_src_type && ':' && l_src_path.

      import
        ts = lv_cache_timestamp
        cs = gs_cache_stats
        from memory id gc_zip_cache_ts_mem_id.

      if lines( gt_zip_cache ) = 0.
        if lv_cache_timestamp is not initial. " sy-subrc = 0.
          get time stamp field lv_now_timestamp.
          lv_diff = cl_abap_tstmp=>subtract(
            tstmp1 = lv_now_timestamp
            tstmp2 = lv_cache_timestamp ).
          if lv_diff < i_cache_timeout.
            import cache = gt_zip_cache from memory id gc_zip_cache_mem_id.
            if sy-subrc = 0.
              " confirm actuality
              gs_cache_stats-import_count = gs_cache_stats-import_count + 1.
              get time stamp field lv_now_timestamp.
              export
                ts = lv_now_timestamp
                cs = gs_cache_stats
                to memory id gc_zip_cache_ts_mem_id.
            endif.
*          else.
*            clear gv_cache_reuse_count.
          endif.
        endif.
      endif.

      read table gt_zip_cache assigning <zip_cache> with key key = lv_zip_cache_key.
      if sy-subrc = 0.
        lv_xdata = <zip_cache>-zip_blob.
        lv_is_txt_format = <zip_cache>-is_txt.
        gs_cache_stats-reuse_count = gs_cache_stats-reuse_count + 1.
        get time stamp field lv_now_timestamp.
        export
          ts = lv_now_timestamp
          cs = gs_cache_stats
          to memory id gc_zip_cache_ts_mem_id.
      endif.
    endif.

    if <zip_cache> is not assigned. " Cache not found
      read_zip_blob(
        exporting
          i_type = l_src_type
          i_path = l_src_path
        importing
          e_xdata  = lv_xdata
          e_is_txt = lv_is_txt_format ).

      if i_cache_timeout > 0. " Caching active
        append initial line to gt_zip_cache assigning <zip_cache>.
        <zip_cache>-key      = lv_zip_cache_key.
        <zip_cache>-is_txt   = lv_is_txt_format.
        <zip_cache>-zip_blob = lv_xdata.

        gs_cache_stats-stash_count = gs_cache_stats-stash_count + 1.
        get time stamp field lv_now_timestamp.
        export
          ts = lv_now_timestamp
          cs = gs_cache_stats
          to memory id gc_zip_cache_ts_mem_id.
        export
          cache = gt_zip_cache
          to memory id gc_zip_cache_mem_id.
      endif.
    endif.

    if lv_is_txt_format = abap_true.
      ro_instance->mi_archive = lcl_text_archive=>new( lv_xdata ).
    else.
      ro_instance->mi_archive = lcl_zip_archive=>new( lv_xdata ).

      " check if archive is in text+zip format
      if lines( ro_instance->mi_archive->files ) = 1.
        data first_file type string.
        read table ro_instance->mi_archive->files into first_file index 1.
        if sy-subrc = 0 and first_file = zif_mockup_loader=>c_txt_bundle_filename.
          " This is a text+zip archive, so we need to read it as text
          ro_instance->mi_archive = lcl_text_archive=>new(
            ro_instance->mi_archive->get( zif_mockup_loader=>c_txt_bundle_filename ) ).
        endif.
      endif.

    endif.

  endmethod.


  method find_file_case_insensitive.

    data lv_path like i_path.
    lv_path = to_lower( i_path ).

    loop at mi_archive->files into rv_file.
      if to_lower( rv_file ) = lv_path.
        exit.
      endif.
      clear rv_file.
    endloop.

  endmethod.


  method flush_cache.

    delete from memory id gc_zip_cache_ts_mem_id.
    delete from memory id gc_zip_cache_mem_id.

  endmethod.


  method normalize_path.
    rv_path = i_path.
    if strlen( rv_path ) >= 2 and rv_path+0(2) = './'.
      rv_path = mv_dir && rv_path+1.
    endif.
  endmethod.


  method parse_data.
    data:
      lx_dp          type ref to zcx_text2tab_error,
      lo_type_descr  type ref to cl_abap_typedescr,
      lt_filter      type zif_mockup_loader=>tt_filter,
      lo_table_descr type ref to cl_abap_tabledescr,
      ld_temp_tab    type ref to data.
    field-symbols:
      <container>     type any,
      <temp_tab>      type standard table.

    " Handle data reference container (use exporting value ???)
    lo_type_descr = cl_abap_typedescr=>describe_by_data( e_container ).
    if lo_type_descr->type_kind = cl_abap_typedescr=>typekind_dref.
      lo_type_descr = cl_abap_typedescr=>describe_by_data_ref( e_container ).
      assign e_container->* to <container>.
    else.
      assign e_container to <container>.
    endif.
    clear <container>.

    " Build filter
    if i_where is not initial.
      lt_filter = zcl_mockup_loader_utils=>build_filter( i_where ).
    endif.

    " Identify container type and create temp container
    lo_table_descr = build_table_type(
      i_corresponding = i_corresponding
      it_filter       = lt_filter
      io_type_descr   = lo_type_descr ).
    create data ld_temp_tab type handle lo_table_descr.
    assign ld_temp_tab->* to <temp_tab>.

    try.
      data lo_parser type ref to zcl_text2tab_parser.
      data lo_deep_provider type ref to zcl_mockup_loader_deep_providr.

      if i_deep = abap_true.
        create object lo_deep_provider exporting ii_ml_instance = me.
      endif.

      lo_parser = zcl_text2tab_parser=>create(
        i_pattern       = <temp_tab>
        i_amount_format = mv_amt_format
        i_date_format   = mv_date_format
        i_deep_provider = lo_deep_provider
        i_skip_lines_starting_with = mv_skip_lines_starting_with
        i_begin_comment = mv_begin_comment ).

      field-symbols <convexit> like line of mt_ignore_conv_exits.
      loop at mt_ignore_conv_exits assigning <convexit>.
        lo_parser->ignore_conv_exit( <convexit> ).
      endloop.

      lo_parser->parse(
        exporting
          i_data     = i_rawdata
          i_strict   = i_strict
          i_corresponding = i_corresponding
          i_rename_fields = i_rename_fields
          i_has_head = abap_true " assume head always, maybe change later
        importing
          e_container = <temp_tab> ).
    catch zcx_text2tab_error into lx_dp.
      zcx_mockup_loader_error=>raise( msg = lx_dp->get_text( ) code = 'XE' ).
    endtry.

    zcl_mockup_loader_utils=>filter_table(
      exporting
        i_filter    = lt_filter
        i_tab       = <temp_tab>
        i_corresponding = i_corresponding
      importing
        e_container = <container> ).

  endmethod.


  method read_zip.
    data:
      l_xstring type xstring,
      lo_conv   type ref to cl_abap_conv_in_ce,
      l_ex      type ref to cx_root.

    l_xstring = zif_mockup_loader~load_blob( i_name ).

    " Remove unicode signatures
    case mv_encoding.
      when zif_mockup_loader=>encoding_utf8.
        shift l_xstring left deleting leading cl_abap_char_utilities=>byte_order_mark_utf8 in byte mode.
      when zif_mockup_loader=>encoding_utf16.
        shift l_xstring left deleting leading cl_abap_char_utilities=>byte_order_mark_little in byte mode.
    endcase.

    try.
      lo_conv = cl_abap_conv_in_ce=>create( encoding = mv_encoding ).
      lo_conv->convert(
        exporting
          input = l_xstring
        importing
          data  = r_rawdata ).
    catch cx_root into l_ex.
      zcx_mockup_loader_error=>raise( msg = 'Codepage conversion error' code = 'CP' ). "#EC NOTEXT
    endtry.

  endmethod.


  method read_zip_blob.

    data l_key       type wwwdatatab.
    data l_size      type int4.
    data lt_w3mime   type table of w3mime.
    data ls_w3mime   type w3mime.
    data lt_params   type table of wwwparams.
    data lv_file_ext type string.
    field-symbols <param> like line of lt_params.

    " Load data
    case i_type.
      when 'MIME'. " Load from SMW0
        l_key-relid = 'MI'.
        l_key-objid = i_path.

        call function 'WWWPARAMS_READ_ALL'
          exporting
            type   = l_key-relid
            objid  = l_key-objid
          tables
            params = lt_params
          exceptions
            others = 1.

        if sy-subrc <> 0.
          zcx_mockup_loader_error=>raise( msg = 'SMW0 data import error' code = 'RE' ).  "#EC NOTEXT
        endif.

        " 'filesize', 'filename', 'fileextension', 'mimetype', 'version'
        read table lt_params assigning <param> with key name = 'fileextension'.
        if sy-subrc <> 0.
          zcx_mockup_loader_error=>raise( msg = 'SMW0 data import error' code = 'RE' ).  "#EC NOTEXT
        endif.
        lv_file_ext = <param>-value. " Maybe check mimetype also? default is ZIP!

        read table lt_params assigning <param> with key name = 'filesize'.
        if sy-subrc <> 0.
          zcx_mockup_loader_error=>raise( msg = 'SMW0 data import error' code = 'RE' ).  "#EC NOTEXT
        endif.
        l_size = <param>-value.

        call function 'WWWDATA_IMPORT'
          exporting
            key    = l_key
          tables
            mime   = lt_w3mime
          exceptions
            others = 1.

        if sy-subrc <> 0.
          zcx_mockup_loader_error=>raise( msg = 'SMW0 data import error' code = 'RE' ).  "#EC NOTEXT
        endif.

*        describe field ls_w3mime length l_size in byte mode.
*        l_size = l_size * lines( lt_w3mime ).

      when 'FILE'. " Load from frontend
        call function 'GUI_UPLOAD'
          exporting
            filename   = i_path
            filetype   = 'BIN'
          importing
            filelength = l_size
          tables
            data_tab   = lt_w3mime
          exceptions
            others = 1.

        data lv_dot_offs type i.
        lv_dot_offs = find_end( val = i_path sub = '.' ).
        if lv_dot_offs > 0.
          lv_file_ext = substring( val = i_path off = lv_dot_offs - 1 ).
        endif.

        if sy-subrc is not initial.
          zcx_mockup_loader_error=>raise( msg = |Cannot upload file: { i_path }| code = 'RE' ). "#EC NOTEXT
        endif.

      when others.
        if sy-subrc is not initial.
          zcx_mockup_loader_error=>raise( msg = 'Wrong source type' code = 'WS' ). "#EC NOTEXT
        endif.

    endcase.

    " Convert to xstring
    call function 'SCMS_BINARY_TO_XSTRING'
      exporting
        input_length = l_size
      importing
        buffer       = e_xdata " FM clears this internally
      tables
        binary_tab   = lt_w3mime
      exceptions
        failed       = 1.

    if sy-subrc is not initial.
      zcx_mockup_loader_error=>raise( 'Binary to string error' ). "#EC NOTEXT
    endif.

    e_is_txt = boolc( to_lower( lv_file_ext ) = '.txt' ).

  endmethod.


  method redirect_source.

    data:
      l_redirect_type type zif_mockup_loader=>ty_src_type,
      l_redirect_mime type c length 128,
      l_redirect_file type c length 128.

    " Get re-direction settings from session memory
    get parameter id zif_mockup_loader=>c_env_param-type field l_redirect_type.
    if l_redirect_type is initial.
      return.
    endif.

    get parameter id zif_mockup_loader=>c_env_param-mime field l_redirect_mime.
    get parameter id zif_mockup_loader=>c_env_param-path field l_redirect_file.

    if l_redirect_type = 'MIME' and l_redirect_mime is not initial.
      " Just redirect always - maybe not good, maybe refactor
      " or maybe remove feature at all ? not used, at least by me :)
      c_src_type = l_redirect_type.
      c_src_path = l_redirect_mime.
      c_is_redirected = abap_true.
    elseif l_redirect_type = 'FILE' and l_redirect_file is not initial and l_redirect_mime = c_src_path.
      " Redirect only if redirect mime = original mime
      c_src_type = l_redirect_type.
      c_src_path = l_redirect_file.
      c_is_redirected = abap_true.
    endif.

  endmethod.


  method zif_mockup_loader~cd.
    mv_dir = i_path.
    ri_ml  = me.
  endmethod.


  method zif_mockup_loader~into.

    load_data(
      exporting
        i_obj           = ms_next_load_params-obj_path
        i_strict        = ms_next_load_params-is_strict
        i_corresponding = ms_next_load_params-is_corresponding
        i_deep          = ms_next_load_params-is_deep
        i_where         = ms_next_load_params-filter
        i_rename_fields = ms_next_load_params-rename_map
      importing
        e_container = data ).

    clear ms_next_load_params.

  endmethod.


  method zif_mockup_loader~is_redirected.
    r_yes = mv_is_redirected.
  endmethod.


  method zif_mockup_loader~load.

    data lx_parser type ref to zcx_text2tab_error.

    clear ms_next_load_params.

    ms_next_load_params-obj_path         = i_obj.
    ms_next_load_params-is_strict        = i_strict.
    ms_next_load_params-is_corresponding = i_corresponding.
    ms_next_load_params-is_deep          = i_deep.
    ms_next_load_params-filter           = zcl_mockup_loader_utils=>build_filter( i_where ).

    try.
      ms_next_load_params-rename_map = zcl_text2tab_utils=>build_rename_map( i_rename_fields ).
    catch zcx_text2tab_error into lx_parser.
      zcx_mockup_loader_error=>raise( msg = lx_parser->get_text( ) code = 'XE' ).
    endtry.

    ri_self = me.

  endmethod.


  method zif_mockup_loader~load_blob.

    data lv_file type string.
    data lv_path like i_obj_path.

    lv_path = normalize_path( i_obj_path ).

    try.
      r_content = mi_archive->get( lv_path ).
    catch zcx_mockup_loader_error.
      sy-subrc = 4.
    endtry.

    if sy-subrc <> 0.
      lv_file = find_file_case_insensitive( lv_path ).

      if lv_file is initial.
        zcx_mockup_loader_error=>raise( msg = |Cannot read { i_obj_path }| code = 'ZF' ). "#EC NOTEXT
      else.
        try.
          r_content = mi_archive->get( lv_file ).
        catch zcx_mockup_loader_error.
          sy-subrc = 4.
        endtry.

        if sy-subrc <> 0.
          zcx_mockup_loader_error=>raise( msg = |Cannot read { i_obj_path }| code = 'ZF' ). "#EC NOTEXT
        endif.
      endif.
    endif.

  endmethod.


  method zif_mockup_loader~load_data.

    data l_rawdata  type string.
    field-symbols <container> type any.

    if i_obj is initial.
      zcx_mockup_loader_error=>raise( msg = 'No object path supplied' code = 'OP' ). "#EC NOTEXT
    endif.

    if e_container is not supplied and mr_ref_to_container is initial.
      zcx_mockup_loader_error=>raise( msg = 'No container supplied' code = 'NC' ). "#EC NOTEXT
    endif.

    if e_container is supplied.
      assign e_container to <container>.
    else.
      assign mr_ref_to_container->* to <container>.
    endif.

    l_rawdata = read_zip( i_obj && '.txt' ).

    parse_data(
      exporting
        i_rawdata       = l_rawdata
        i_strict        = i_strict
        i_corresponding = i_corresponding
        i_deep          = i_deep
        i_where         = i_where
        i_rename_fields = i_rename_fields
      importing
        e_container = <container> ).

    clear ms_next_load_params.

  endmethod.


  method zif_mockup_loader~set_params.

    if i_amt_format is initial or i_amt_format+1(1) is initial. " Empty param or decimal separator
      me->mv_amt_format = ' ,'. " Defaults
    else.
      me->mv_amt_format = i_amt_format.
    endif.

    if i_encoding is initial.
      me->mv_encoding = zif_mockup_loader=>encoding_utf8.
    else.
      me->mv_encoding = i_encoding.
    endif.

    if i_date_format is initial
      or not i_date_format+3(1) co ' ./-'
      or not ( i_date_format+0(3) = 'DMY'
        or i_date_format+0(3) = 'MDY'
        or i_date_format+0(3) = 'YMD' ).
      me->mv_date_format = 'DMY.'. " DD.MM.YYYY
    else.
      me->mv_date_format = i_date_format.
    endif.

    me->mv_begin_comment = i_begin_comment.
    me->mt_ignore_conv_exits = it_ignore_conv_exits.
    me->mv_skip_lines_starting_with = i_skip_lines_starting_with.

    ri_ml = me.

  endmethod.


  method zif_mockup_loader~to.
    mr_ref_to_container = i_ref.
    ri_ml = me.
  endmethod.
ENDCLASS.
