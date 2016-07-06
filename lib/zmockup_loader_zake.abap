*&---------------------------------------------------------------------*
*& ZMOCKUP_LOADER_ZAKE - Mockup loader SAP link builder
*&   Creates separate slinkees and the whole package in the build dir
*&   Sorry for hardcoding - the program is in fact for internal usage
*&---------------------------------------------------------------------*

* Thanks to Uweku for the idea to use ZAKE :)

report zmockup_loader_zake.

define _addobj.
  append initial line to lt_objects assigning <object>.
  <object>-object   = &1.
  <object>-obj_name = &2.
end-of-definition.

define _setattr.
  assign o_zake->(&1) to <value>.
  <value> = &2.
end-of-definition.

start-of-selection.

  data:
        lx_error   type ref to cx_root,
        o_zake     type ref to object,
        lt_objects type scts_tadir.

  field-symbols <object> like line of lt_objects.
  field-symbols <value>  type any.

  " Build Object list for Export
  _addobj 'DEVC' 'ZMOCKUP_LOADER'.
  _addobj 'CLAS' 'ZCL_MOCKUP_LOADER'.
  _addobj 'PROG' 'ZMOCKUP_LOADER_EXAMPLE'.
  _addobj 'PROG' 'ZMOCKUP_LOADER_SWITCH_SOURCE'.
  _addobj 'TRAN' 'ZMOCKUP_LOADER_SWSRC'.
  _addobj 'PARA' 'ZMOCKUP_LOADER_SPATH'.
  _addobj 'PARA' 'ZMOCKUP_LOADER_STYPE'.


  try.
    create object o_zake type ('ZCL_ZAKE_SVN')
      exporting i_svnpath   = 'c:\sap\'
                i_localpath = 'c:\sap\'.

*    o_zake->set_testrun( 'X' ).

    call method o_zake->('SET_PACKAGE')
      exporting i_package = 'ZMOCKUP_LOADER'.


    call method o_zake->('SET_CHECKIN_OBJECTS')
      exporting i_objects = lt_objects.

    " Build separate slinkees


    _setattr 'DOWNLOAD_SLINKEES_TO_LM' abap_true.
    _setattr 'DOWNLOAD_NUGGET_TO_LM'   space.
    _setattr 'DOWNLOAD_ZIP_TO_LM_FLAG' space.
    call method o_zake->('CREATE_SLINKEES').

    " Build a complete package too

    _setattr 'DOWNLOAD_SLINKEES_TO_LM' space.
    _setattr 'DOWNLOAD_NUGGET_TO_LM'   abap_true.
    call method o_zake->('CREATE_SLINKEES')
      exporting i_nugget_name = 'ZMOCKUP_LOADER'.

  catch cx_root into lx_error.
    data l_msg type string.
    l_msg = lx_error->get_text( ).
    write: / 'Error occured:', l_msg. "#EC NOTEXT
  endtry.

  write: / 'Build successful'. "#EC NOTEXT
