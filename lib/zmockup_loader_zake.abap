*&---------------------------------------------------------------------*
*& ZMOCKUP_LOADER_ZAKE - Mockup loader SAP link builder
*&   Creates separate slinkees and the whole package in the build dir
*&   Sorry for hardcoding - the program is in fact for internal usage
*&---------------------------------------------------------------------*

* Thanks to Uweku for the idea to use ZAKE :)

report zmockup_loader_zake.

define addobj.
  object-object   = &1.
  object-obj_name = &2.
  append object to objects.
end-of-definition.

data:
      ex      type ref to zcx_saplink,
      o_zake  type ref to zcl_zake_svn,
      objects type scts_tadir,
      object  like line of objects.

try.
  create object o_zake exporting i_svnpath = 'c:\sap\' i_localpath = 'c:\sap\'.

*  o_zake->set_testrun( 'X' ).
  o_zake->set_package( 'ZMOCKUP_LOADER' ).

  " Build Object list for Export
  addobj 'DEVC' 'ZMOCKUP_LOADER'.
  addobj 'CLAS' 'ZCL_MOCKUP_LOADER'.
  addobj 'CLAS' 'ZCX_MOCKUP_LOADER_ERROR'.
  addobj 'PROG' 'ZMOCKUP_LOADER_EXAMPLE'.
  addobj 'PROG' 'ZMOCKUP_LOADER_SWITCH_SOURCE'.
  addobj 'TRAN' 'ZMOCKUP_LOADER_SWSRC'.
  addobj 'PARA' 'ZMOCKUP_LOADER_SPATH'.
  addobj 'PARA' 'ZMOCKUP_LOADER_STYPE'.

  o_zake->set_checkin_objects( objects ).

  " Build separate slinkees

  o_zake->download_slinkees_to_lm = abap_true.
  o_zake->download_nugget_to_lm   = space.
  o_zake->download_zip_to_lm_flag = space.
  o_zake->create_slinkees( ).

  " Build a complete package too

  o_zake->download_slinkees_to_lm = space.
  o_zake->download_nugget_to_lm   = abap_true.
  o_zake->create_slinkees( 'ZMOCKUP_LOADER' ).

  write: / 'Build successful'. "#EC NOTEXT

catch zcx_saplink into ex.
  write: / 'An error occured: ', ex->msg. "#EC NOTEXT
endtry.
