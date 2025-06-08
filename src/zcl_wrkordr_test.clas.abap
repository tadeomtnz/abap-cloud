CLASS zcl_wrkordr_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    METHODS:
      test_create_work_order IMPORTING iv_customer_id          TYPE z_custmr
                                       iv_technician_id        TYPE z_technician
                                       iv_priority             TYPE z_priority_tmh
                                       iv_description          TYPE z_description_tmh
                             RETURNING VALUE(rv_create_result) TYPE string,

      test_read_work_order IMPORTING iv_work_order_id      TYPE z_wrkordr
                           RETURNING VALUE(rv_read_result) TYPE ztbl_work_order,

      test_update_work_order IMPORTING iv_work_order_id        TYPE z_wrkordr
                                       iv_status               TYPE z_status_tmh
                                       iv_description          TYPE z_description_tmh
                             RETURNING VALUE(rv_update_result) TYPE string,

      test_delete_work_order IMPORTING iv_work_order_id        TYPE z_wrkordr
                                       iv_status               TYPE z_status_tmh
                             RETURNING VALUE(rv_delete_result) TYPE string.

    CONSTANTS: BEGIN OF mc_valid_status,
                 Pending   TYPE z_status_tmh VALUE 'PE',
                 Completed TYPE z_status_tmh VALUE 'CO',
                 Cancelled TYPE z_status_tmh VALUE 'CA',
               END OF mc_valid_status,

               BEGIN OF mc_valid_priority,
                 High   TYPE z_priority_tmh VALUE 'A',
                 Medium TYPE z_priority_tmh VALUE 'B',
                 Low    TYPE z_priority_tmh VALUE 'C',
               END OF mc_valid_priority.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_wrkordr_test IMPLEMENTATION.

  METHOD test_create_work_order.

    DATA(lr_create_order_test) = NEW zcl_wkrordr_crud_handler( ).

    lr_create_order_test->create_work_order(
        EXPORTING
                iv_customer_id = iv_customer_id
                iv_technician_id = iv_technician_id
                iv_priority = iv_priority
                iv_description = iv_description
        RECEIVING
                rv_create_result = rv_create_result ).

  ENDMETHOD.

  METHOD test_read_work_order.

    DATA(lr_read_order_test) = NEW zcl_wkrordr_crud_handler( ).

    lr_read_order_test->read_work_order(
        EXPORTING
               iv_work_order_id = iv_work_order_id
        RECEIVING
                rv_read_result = rv_read_result ).

  ENDMETHOD.

  METHOD test_update_work_order.

    DATA(lr_update_order_test) = NEW zcl_wkrordr_crud_handler( ).

    lr_update_order_test->update_work_order(
        EXPORTING
            iv_work_order_id = iv_work_order_id
            iv_status = iv_status
            iv_description = iv_description
        RECEIVING
            rv_update_result = rv_update_result ).

  ENDMETHOD.

  METHOD test_delete_work_order.

    DATA(lr_delete_order_test) = NEW zcl_wkrordr_crud_handler( ).

    lr_delete_order_test->delete_work_order(
        EXPORTING
            iv_work_order_id = iv_work_order_id
            iv_status = iv_status
        RECEIVING
            rv_delete_result = rv_delete_result ).

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

******************TESTING ROWS******************

    DELETE FROM ztbl_customers WHERE customer_id EQ '10000003'.
    DELETE FROM ztbl_technician WHERE technician_id EQ '20000003'.

    INSERT ztbl_customers FROM @( VALUE #( customer_id = '10000003'
                                           name = 'John Doe'
                                           address = '123 Fake Street'
                                           phone = '555 555 5555' ) ).

    INSERT ztbl_technician FROM @( VALUE #( technician_id = '20000003'
                                            name = 'Jane Foe'
                                            specialty = 'Testing' ) ).

    DATA: lv_work_order_id TYPE z_wrkordr VALUE '1',
          lv_customer_id   TYPE z_custmr VALUE '10000003',
          lv_technician_id TYPE z_technician VALUE '20000003',
          lv_status        TYPE z_status_tmh VALUE 'PE',
          lv_priority      TYPE z_priority_tmh VALUE 'A',
          lv_description   TYPE z_description_tmh VALUE 'Testing Row'.

    CONSTANTS: lc_no_auth TYPE string VALUE 'No authorization to execute this operation'.

***************************SELECTED OPERATION*******************************
    DATA(lv_operation) = 'CREATE'.
****************************************************************************

******************CREATE******************

    IF lv_operation = 'CREATE'.

      AUTHORITY-CHECK OBJECT 'zao_tmh'
          ID 'zaf_custmr' FIELD lv_customer_id
          ID 'actvt' FIELD '01'.

      IF sy-subrc = 0.

        test_create_work_order(
            EXPORTING
                    iv_customer_id = lv_customer_id
                    iv_technician_id = lv_technician_id
                    iv_priority = lv_priority
                    iv_description = lv_description
            RECEIVING
                    rv_create_result = DATA(lv_create_result) ).

        out->write( lv_create_result ).

      ELSE.

        out->write( lc_no_auth ).
        RETURN.

      ENDIF.

******************READ******************

    ELSEIF lv_operation = 'READ'.

      AUTHORITY-CHECK OBJECT 'zao_tmh'
          ID 'zaf_wrkord' FIELD lv_work_order_id
          ID 'actvt' FIELD '03'.

      IF sy-subrc = 0.

        test_read_work_order(
            EXPORTING
                    iv_work_order_id = lv_work_order_id
            RECEIVING
                    rv_read_result = DATA(lv_read_result) ).

        out->write( lv_read_result ).

      ELSE.

        out->write( lc_no_auth ).

      ENDIF.

******************UPDATE******************

    ELSEIF lv_operation = 'UPDATE'.

      AUTHORITY-CHECK OBJECT 'zao_tmh'
          ID 'zaf_custmr' FIELD lv_work_order_id
          ID 'actvt' FIELD '02'.

      IF sy-subrc = 0.

        test_update_work_order(
            EXPORTING
                iv_work_order_id = lv_work_order_id
                iv_status = lv_status
                iv_description = lv_description
            RECEIVING
                rv_update_result = DATA(lv_update_result) ).

        out->write( lv_update_result ).

      ELSE.

        out->write( lc_no_auth ).

      ENDIF.

******************DELETE******************

    ELSEIF lv_operation = 'DELETE'.

      AUTHORITY-CHECK OBJECT 'zao_tmh'
          ID 'zaf_custmr' FIELD lv_work_order_id
          ID 'actvt' FIELD '06'.

      IF sy-subrc = 0.

        test_delete_work_order(
            EXPORTING
                iv_work_order_id = lv_work_order_id
                iv_status = lv_status
            RECEIVING
                rv_delete_result = DATA(lv_delete_result) ).

        out->write( lv_delete_result ).

      ELSE.

        out->write( lc_no_auth ).

      ENDIF.

*****************************************

    ELSE.

      out->write( |Invalid Operation.| ).

    ENDIF.

  ENDMETHOD.


ENDCLASS.
