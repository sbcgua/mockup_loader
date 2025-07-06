class ZCX_MOCKUP_LOADER_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  types:
    ty_rc type c length 4 .

  constants:
    begin of ZCX_MOCKUP_LOADER_ERROR,
      msgid type symsgid value '00',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'A1',
      attr2 type scx_attrname value 'A2',
      attr3 type scx_attrname value 'A3',
      attr4 type scx_attrname value 'A4',
    end of ZCX_MOCKUP_LOADER_ERROR .
  data METHNAME type STRING read-only .
  data MSG type STRING read-only .
  data CODE type TY_RC read-only .
  data A1 type SYMSGV read-only .
  data A2 type SYMSGV read-only .
  data A3 type SYMSGV read-only .
  data A4 type SYMSGV read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !METHNAME type STRING optional
      !MSG type STRING optional
      !CODE type TY_RC optional
      !A1 type SYMSGV optional
      !A2 type SYMSGV optional
      !A3 type SYMSGV optional
      !A4 type SYMSGV optional .
  class-methods RAISE
    importing
      !MSG type STRING
      !CODE type TY_RC optional
    raising
      ZCX_MOCKUP_LOADER_ERROR .
  methods REMSG
    importing
      !MSG type STRING .
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
me->A1 = A1 .
me->A2 = A2 .
me->A3 = A3 .
me->A4 = A4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_MOCKUP_LOADER_ERROR .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
endmethod.


method raise.

  data lt_callstack type abap_callstack.
  data ls_callpoint like line of lt_callstack.

  data:
    begin of ls_split,
      a1 like a1,
      a2 like a1,
      a3 like a1,
      a4 like a1,
    end of ls_split.
  data lv_msg type string.

  call function 'SYSTEM_CALLSTACK' " Get stack information
    exporting max_level = 2
    importing callstack = lt_callstack.

  read table lt_callstack into ls_callpoint index 2.

  if ls_callpoint-blockname is not initial.
    lv_msg = |[{ ls_callpoint-blockname }] { msg }|.
  else.
    lv_msg = msg.
  endif.
  ls_split = lv_msg.

  raise exception type zcx_mockup_loader_error
    exporting
      methname = ls_callpoint-blockname
      msg      = msg
      code     = code
      a1       = ls_split-a1
      a2       = ls_split-a2
      a3       = ls_split-a3
      a4       = ls_split-a4.

endmethod.


method remsg.
  data:
    begin of ls_split,
      a1 like a1,
      a2 like a1,
      a3 like a1,
      a4 like a1,
    end of ls_split.

  me->msg  = msg.
  ls_split = msg.
  me->a1   = ls_split-a1.
  me->a2   = ls_split-a2.
  me->a3   = ls_split-a3.
  me->a4   = ls_split-a4.
endmethod.
ENDCLASS.
