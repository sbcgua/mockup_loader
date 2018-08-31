class zcl_mockup_loader_stub_double definition
  public
  final
  inheriting from zcl_mockup_loader_stub_base
  create public
  for testing .

public section.
  interfaces if_abap_testdouble_answer.

protected section.
private section.
endclass.



class zcl_mockup_loader_stub_double implementation.


  method if_abap_testdouble_answer~answer.

  " find config
  field-symbols <conf> like line of mt_config.
  read table mt_config with key method_name = method_name assigning <conf>.
  if <conf> is not assigned.
    return.
  endif.

  data lr_param type ref to data.
  field-symbols <siftval> type any.
  if <conf>-sift_param is not initial.
    lr_param = arguments->get_param_importing( <conf>-sift_param ).
  else.
    create data lr_param type c. " just to pass it, will not be handled further
  endif.
  assign lr_param->* to <siftval>.

  data lr_data type ref to data.
  data lx      type ref to zcx_mockup_loader_error.
  try.
    lr_data = get_mock_data(
      i_method_name = method_name
      i_sift_value  = <siftval> ).
  catch zcx_mockup_loader_error into lx.
    raise exception type cx_atd_exception_core exporting object_name = 'ZCX_MOCKUP_LOADER_ERROR'.
  endtry.

  field-symbols <container> type any.
  assign lr_data->* to <container>.
  case <conf>-output_pkind.
    when 'R'.
      result->set_param_returning( <container> ).
    when 'E'.
      result->set_param_exporting( name = <conf>-output_param  value = <container> ).
    when 'C'.
      result->set_param_changing( name = <conf>-output_param  value = <container> ).
    when others.
      " Error unexpected
  endcase.

endmethod.

endclass.
