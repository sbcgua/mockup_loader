method RAISE.
  data:
        l_methname  type string,
        sys_call    type sys_calls,
        sys_stack   type sys_callst.

  " Get stack information
  call function 'SYSTEM_CALLSTACK'
    exporting max_level    = 2
    importing et_callstack = sys_stack.

  read table sys_stack into sys_call index 2.
  l_methname = sys_call-eventname && '():'.

  raise exception type zcx_mockup_loader_error
    exporting
      methname = l_methname
      msg      = msg
      code     = code.

endmethod.
