class ZCX_MOCKUP_LOADER_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  types:
    ty_rc type c length 2.

  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_MOCKUP_LOADER_ERROR,
      msgid type symsgid value 'SY',
      msgno type symsgno value '499',
      attr1 type scx_attrname value 'METHNAME',
      attr2 type scx_attrname value 'MSG',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_MOCKUP_LOADER_ERROR .
  data METHNAME type STRING read-only .
  data MSG type STRING read-only .
  data CODE type ty_rc read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !METHNAME type STRING optional
      !MSG type STRING optional
      !CODE type ty_rc optional .
  class-methods RAISE
    importing
      !MSG type STRING
      !CODE type ty_rc optional
    raising
      ZCX_MOCKUP_LOADER_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCX_MOCKUP_LOADER_ERROR IMPLEMENTATION.


method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->METHNAME = METHNAME .
me->MSG = MSG .
me->CODE = CODE .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_MOCKUP_LOADER_ERROR .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
endmethod.


method raise.
  data:
        l_methname  type string,
        l_sys_call  type sys_calls,
        l_sys_stack type sys_callst.

  call function 'SYSTEM_CALLSTACK' " Get stack information
    exporting max_level    = 2
    importing et_callstack = l_sys_stack.

  read table l_sys_stack into l_sys_call index 2.
  l_methname = |[{ l_sys_call-eventname }]|.

  raise exception type zcx_mockup_loader_error
    exporting
      methname = l_methname
      msg      = msg
      code     = code.
endmethod.
ENDCLASS.
