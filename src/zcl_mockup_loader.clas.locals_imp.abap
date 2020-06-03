class lcl_map definition final.
  public section.

    types:
      begin of ty_map_entry,
        key type string,
        val type ref to data,
      end of ty_map_entry.
    types:
      tt_map type standard table of ty_map_entry with key key,
      tts_map type sorted table of ty_map_entry with unique key key.

    methods has
      importing
        i_key type string
      returning
        value(r_yes) type abap_bool.
    methods get_ref
      importing
        i_key type string
      returning
        value(r_ref) type ref to data.
    " get (exporting)
    " get_ref_force ?
    " set
  private section.
    data mt_map type tts_map.
endclass.

class lcl_map implementation.
  method has.
    read table mt_map transporting no fields with key key = i_key.
    r_yes = boolc( sy-subrc = 0 ).
  endmethod.

  method get_ref.
    data l_entry like line of mt_map.
    read table mt_map into l_entry with key key = i_key.
    r_ref = l_entry-val. " sy-subrc <> 0 ? it is OK
  endmethod.
endclass.
