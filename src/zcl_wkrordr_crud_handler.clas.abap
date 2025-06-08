CLASS zcl_wkrordr_crud_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      create_work_order IMPORTING iv_customer_id          TYPE z_custmr
                                  iv_technician_id        TYPE z_technician
                                  iv_priority             TYPE z_priority_tmh
                                  iv_description          TYPE z_description_tmh
                        RETURNING VALUE(rv_create_result) TYPE string,

      read_work_order IMPORTING iv_work_order_id      TYPE z_wrkordr
                      RETURNING VALUE(rv_read_result) TYPE string,

      update_work_order IMPORTING iv_work_order_id        TYPE z_wrkordr
                                  iv_status               TYPE z_status_tmh
                                  iv_description          TYPE z_description_tmh
                        RETURNING VALUE(rv_update_result) TYPE string,

      delete_work_order IMPORTING iv_work_order_id        TYPE z_wrkordr
                                  iv_status               TYPE z_status_tmh
                        RETURNING VALUE(rv_delete_result) TYPE string.



  PROTECTED SECTION.
  PRIVATE SECTION.
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

ENDCLASS.


CLASS zcl_wkrordr_crud_handler IMPLEMENTATION.

  METHOD create_work_order.

    DATA(lr_create_order_validate) = NEW zcl_wrkordr_validator( ). "Calls method to validate operation

    lr_create_order_validate->validate_create_order(
        EXPORTING
            iv_customer_id = iv_customer_id
            iv_technician_id = iv_technician_id
            iv_priority = iv_priority
        IMPORTING
            ev_error = rv_create_result
        RECEIVING
            rv_valid_cr = DATA(lv_valid) ).

    IF lv_valid = abap_true.               "If the specified values are valid, they are inserted into a new row in the DDBB table
      SELECT SINGLE FROM ztbl_work_order
         FIELDS MAX( work_order_id )
         WHERE work_order_id IS NOT INITIAL
         INTO @DATA(lv_work_order_id).

      INSERT ztbl_work_order FROM @( VALUE #( work_order_id = lv_work_order_id + 1
                                              customer_id = iv_customer_id
                                              technician_id = iv_technician_id
                                              creation_date = cl_abap_context_info=>get_system_date( )
                                              status = mc_valid_status-Pending
                                              priority = iv_priority
                                              description = iv_description ) ).

      IF sy-subrc = 0.
        rv_create_result = |Worker order { lv_work_order_id + 1 } has been succesfully created.|.
      ELSE.
        rv_create_result = |Work order was not created.|.
      ENDIF.
    ELSE.
      rv_create_result = |Specified values are not valid. Input different values.|.
    ENDIF.

  ENDMETHOD.

  METHOD read_work_order.

    SELECT FROM ztbl_work_order          "Reads the row from the DDBB table with specified work order ID
    FIELDS *
    WHERE work_order_id EQ @iv_work_order_id
    INTO @DATA(wawa).

      IF sy-subrc NE 0.
        rv_read_result = |no se pudo pq el sysubrc es { sy-subrc }|.
      ELSE.
        rv_read_Result = wawa.
      ENDIF.

    ENDSELECT.
  ENDMETHOD.

  METHOD update_work_order.

    DATA(lr_update_order_validate) = NEW zcl_wrkordr_validator( ). "Calls method to validate operation

    lr_update_order_validate->validate_update_order(
        EXPORTING
            iv_work_order_id = iv_work_order_id
            iv_status = iv_status
         IMPORTING
            ev_error = rv_update_result
         RECEIVING
            rv_valid_up = DATA(lv_valid) ).

    IF lv_valid = abap_true.                     "If the specified values are valid, updates the row with the specified work order ID into a specified status
      UPDATE ztbl_work_order SET status = @iv_status
                             WHERE work_order_id = @iv_work_order_id.
      IF sy-subrc EQ 0.

        SELECT SINGLE FROM ztbl_wrkord_his     "Reads last history ID
        FIELDS MAX( history_id )
        WHERE history_id GE 0
        INTO @DATA(lv_history_id).

        INSERT ztbl_wrkord_his FROM @( VALUE #(  history_id = lv_history_id + 1    "Updates and adds row to changes history DDBB table
                                                 work_order_id = iv_work_order_id
                                                 modification_date = cl_abap_context_info=>get_system_date( )
                                                 change_description = iv_description ) ).

        IF sy-subrc EQ 0.
          rv_update_result = |Order { iv_work_order_id } succesfully updated and has been registered to history table where history ID is { lv_history_id + 1 }.|.
        ELSE.
          rv_update_result = |Order { iv_work_order_id } succesfully updated, but the change record was not added to history table. sy-subrc is { sy-subrc }|.
        ENDIF.
      ELSE.
        rv_update_result = |Order { iv_work_order_id } was not updated.|.
      ENDIF.
    ELSE.
      rv_update_result = |Order { iv_work_order_id } is not valid to update.|.
    ENDIF.

  ENDMETHOD.

  METHOD delete_work_order.

    DATA(lr_create_order_validate) = NEW zcl_wrkordr_validator( ). "Calls method to validate operation

    lr_create_order_validate->validate_delete_order(
        EXPORTING
            iv_work_order_id = iv_work_order_id
            iv_status = iv_status
        IMPORTING
            ev_error = rv_delete_result
        RECEIVING
            rv_valid_de = DATA(lv_valid) ).

    IF lv_valid = abap_true.
      DELETE FROM ztbl_work_order WHERE work_order_id EQ @iv_work_order_id.
      IF sy-subrc EQ 0.
        rv_delete_result = |Order { iv_work_order_id } succesfully deleted.|.
      ELSE.
        rv_delete_result = |Order { iv_work_order_id } has not been deleted.|.
      ENDIF.
    ELSE.
      rv_delete_result = |Order { iv_work_order_id } is not valid to delete.|.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
