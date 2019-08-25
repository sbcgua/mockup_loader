class zcl_mockup_loader_apack_mnfst definition
  public
  final
  create public .

  public section.
    interfaces zif_apack_manifest.
    methods constructor.
  protected section.
  private section.
ENDCLASS.



CLASS ZCL_MOCKUP_LOADER_APACK_MNFST IMPLEMENTATION.


  method constructor.
    zif_apack_manifest~descriptor-group_id    = 'sbcgua'.
    zif_apack_manifest~descriptor-artifact_id = 'mockup_loader'.
    zif_apack_manifest~descriptor-version     = zif_mockup_loader_constants=>version.
    zif_apack_manifest~descriptor-git_url     = 'https://github.com/sbcgua/mockup_loader.git'.

    field-symbols <d> like line of zif_apack_manifest~descriptor-dependencies.

    append initial line to zif_apack_manifest~descriptor-dependencies assigning <d>.
    <d>-group_id    = 'sbcgua'.
    <d>-artifact_id = 'text2tab'.
    <d>-git_url     = 'https://github.com/sbcgua/abap_data_parser.git'.
  endmethod.
ENDCLASS.
