* Local definitions and classes

class lcx_error definition inheriting from cx_static_check.
  public section.
    interfaces if_t100_message.

    data methname type string read-only.
    data msg      type string read-only.
    data code     type char2  read-only.

    methods constructor
      importing methname type string
                msg      type string
                code     type char2.

    class-methods raise
      importing msg  type string
                code type char2 optional
      raising   cx_static_check.

endclass.

class lcx_error implementation.

  method constructor.
    super->constructor( ).

    me->methname = methname.
    me->msg      = msg.
    me->code     = code.

    me->if_t100_message~t100key-msgid = 'SY'. " & & & &
    me->if_t100_message~t100key-msgno = '499'.
    me->if_t100_message~t100key-attr1 = 'METHNAME'.
    me->if_t100_message~t100key-attr2 = 'MSG'.
  endmethod.

  method raise.
    data:
          l_methname  type string,
          sys_call    type sys_calls,
          sys_stack   type sys_callst.

    call function 'SYSTEM_CALLSTACK' " Get stack information
      exporting max_level    = 2
      importing et_callstack = sys_stack.

    read table sys_stack into sys_call index 2.
    l_methname = |[{ sys_call-eventname }]|.

    raise exception type lcx_error
      exporting
        methname = l_methname
        msg      = msg
        code     = code.

  endmethod.

endclass.
