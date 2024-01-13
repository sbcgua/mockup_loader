class lcl_dao definition final for testing.
  public section.
    class-methods get_instance
      returning
        value(ri_instance) type ref to zif_mockup_loader_stub_dummy.
    class-methods inject_instance
      importing
        ii_instance type ref to zif_mockup_loader_stub_dummy.
    interfaces zif_mockup_loader_stub_dummy.

  private section.
    class-data mi_instance type ref to zif_mockup_loader_stub_dummy.
endclass.

class lcl_dao implementation.

  method get_instance.
    if mi_instance is not bound.
      create object mi_instance type lcl_dao.
    endif.
    ri_instance = mi_instance.
  endmethod.

  method inject_instance.
    mi_instance = ii_instance.
  endmethod.

endclass.

**********************************************************************
* SINGLETON SETTINGS CLASS (a sample way to get test env indicator)
**********************************************************************
class lcl_context definition final create private.

  public section.
    data a_carrid  type sflight-carrid read-only.       " Indicates execution in test environment
    data a_testenv type abap_bool read-only.

    class-methods get_instance
      returning value(ro_instance) type ref to lcl_context.

    methods set_carrid
      importing i_carrid type sflight-carrid.

  private section.
    class-data go_instance type ref to lcl_context.     " Some settings for the production code

endclass.

class lcl_context implementation.
  method get_instance. " Get sinleton instance
    if go_instance is not bound.
      create object go_instance.
    endif.
    ro_instance = go_instance.
  endmethod.

  method set_carrid. " Setup context for production environment
    clear: me->a_carrid, me->a_testenv.
    me->a_carrid = i_carrid.
    if i_carrid = 'ZZZ'. " Special test env airline - non existing !
      me->a_testenv = abap_true.
    endif.
  endmethod.

endclass.

**********************************************************************
* SOME BUSINESS LOGIC CLASS - the object to test
**********************************************************************
class lcl_main_logic definition final create public.

  public section.
    methods constructor.
    methods get_price
      importing
        i_connid type sflight-connid
        i_date   type sflight-fldate
      returning value(r_price) type sflight-price
      exceptions not_found.

  private section.
    data o_context type ref to lcl_context.

endclass.

class lcl_main_logic implementation.
  method constructor.
    o_context = lcl_context=>get_instance( ). " Get context
  endmethod.

  method get_price. " Get price of the connection in the context airline
    data ls_flight type sflight.

    if o_context->a_testenv = abap_false. " Production env
      select single price into corresponding fields of ls_flight
        from sflight
        where carrid = o_context->a_carrid
        and connid = i_connid
        and fldate = i_date.
    else.                                  " Test env
      zcl_mockup_loader_store=>retrieve(
        exporting
          i_name = 'SFLIGHT'
          i_sift = i_connid
        importing
          e_data = ls_flight
        exceptions others = 4 ).
    endif.

    if sy-subrc is not initial. " Selection error ?
      raise not_found.
    endif.

    r_price = ls_flight-price.

  endmethod.
endclass.
