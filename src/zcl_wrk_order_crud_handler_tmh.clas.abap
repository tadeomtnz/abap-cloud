CLASS zcl_wrk_order_crud_handler_tmh DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_wrk_order_crud_handler_tmh IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    DELETE FROM ztbl_work_order.
    DELETE FROM ztbl_customers.
    DELETE FROM ztbl_technician.
    DELETE FROM ztbl_wrkordr_his.
    DELETE FROM ztbl_status_tmh.
    DELETE FROM ztbl_priority_tm.

    DATA: lt_insert1 TYPE TABLE OF ztbl_work_order,
          lt_insert2 TYPE TABLE OF ztbl_customers,
          lt_insert3 TYPE TABLE OF ztbl_technician,
          lt_insert4 TYPE TABLE OF ztbl_wrkordr_his,
          lt_insert5 TYPE TABLE OF ztbl_status_tmh,
          lt_insert6 TYPE TABLE OF ztbl_priority_tm.

    lt_insert1 = VALUE #( ( work_order_id = 1
                            customer_id   = 1
                            technician_id = 1
                            creation_date = '20250101'
                            status        = 'CO'
                            priority      = 'B'
                            description   = 'Testing Row' ) ).

    lt_insert2 = VALUE #( ( customer_id = 1
                            name        = 'John Doe'
                            address     = '123 Fake Street'
                            phone       = '555 555 5555' ) ).

    lt_insert3 = VALUE #( ( technician_id = 1
                            name          = 'Jane Doe'
                            specialty     = 'Sales' ) ).

    lt_insert4 = VALUE #( ( history_id         = 1
                            modification_date  = '20250206'
                            change_description = 'Placeholder Change' ) ).

    lt_insert5 = VALUE #( ( status_description = 'Completed' ) ).

    lt_insert6 = VALUE #( ( priority_description = 'Medium priority...' ) ).

    INSERT ztbl_work_order FROM TABLE @lt_insert1.
    INSERT ztbl_customers FROM TABLE @lt_insert2.
    INSERT ztbl_technician FROM TABLE @lt_insert3.
    INSERT ztbl_wrkordr_his FROM TABLE @lt_insert4.
    INSERT ztbl_status_tmh FROM TABLE @lt_insert5.
    INSERT ztbl_priority_tm FROM TABLE @lt_insert6.

    IF sy-subrc EQ 0.
      out->write( 'Placeholders Added' ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
