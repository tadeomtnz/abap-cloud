CLASS zcl_wrkordr_validator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      validate_create_order IMPORTING iv_customer_id     TYPE z_custmr
                                      iv_technician_id   TYPE z_technician
                                      iv_priority        TYPE z_priority_tmh
                            EXPORTING ev_error           TYPE string
                            RETURNING VALUE(rv_valid_cr) TYPE abap_bool,

      validate_update_order IMPORTING iv_work_order_id   TYPE z_wrkordr
                                      iv_status          TYPE z_status_tmh
                            EXPORTING ev_error           TYPE string
                            RETURNING VALUE(rv_valid_up) TYPE abap_bool,

      validate_delete_order IMPORTING iv_work_order_id   TYPE z_wrkordr
                                      iv_status          TYPE z_status_tmh
                            EXPORTING ev_error           TYPE string
                            RETURNING VALUE(rv_valid_de) TYPE abap_bool,

      validate_status_and_priority IMPORTING iv_status          TYPE z_status_tmh
                                             iv_priority        TYPE z_priority_tmh
                                   EXPORTING ev_error           TYPE string
                                   RETURNING VALUE(rv_valid_sp) TYPE abap_bool.



  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      check_customer_exists IMPORTING iv_customer_id       TYPE z_custmr
                            RETURNING VALUE(rv_exists_cus) TYPE abap_bool,

      check_technician_exists IMPORTING iv_technician_id     TYPE z_technician
                              RETURNING VALUE(rv_exists_tec) TYPE abap_bool,

      check_priority_valid IMPORTING iv_priority          TYPE z_priority_tmh
                           RETURNING VALUE(rv_exists_pri) TYPE abap_bool,

      check_order_exists IMPORTING iv_work_order_id     TYPE z_wrkordr
                         RETURNING VALUE(rv_exists_ord) TYPE abap_bool,

      check_order_history IMPORTING iv_work_order_id     TYPE z_wrkordr
                          RETURNING VALUE(rv_exists_his) TYPE abap_bool.



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



CLASS zcl_wrkordr_validator IMPLEMENTATION.

  METHOD check_customer_exists.

    SELECT SINGLE FROM ztbl_customers
    FIELDS customer_id
    WHERE customer_id EQ @iv_customer_id
    INTO @DATA(lv_customer_id).

    IF sy-subrc EQ 0.
      rv_exists_cus = abap_true.
    ELSE.
      rv_exists_cus = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD check_technician_exists.

    SELECT SINGLE FROM ztbl_technician
    FIELDS technician_id
    WHERE technician_id EQ @iv_technician_id
    INTO @DATA(lv_technician_id).

    IF sy-subrc EQ 0.
      rv_exists_tec = abap_true.
    ELSE.
      rv_exists_tec = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD check_priority_valid.

    IF iv_priority = mc_valid_priority-high.
      rv_exists_pri = abap_true.
    ELSEIF iv_priority = mc_valid_priority-medium.
      rv_exists_pri = abap_true.
    ELSEIF iv_priority = mc_valid_priority-low.
      rv_Exists_pri = abap_true.
    ELSE.
      rv_exists_pri = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD check_order_exists.

    SELECT SINGLE FROM ztbl_work_order
    FIELDS work_order_id
    WHERE work_order_id EQ @iv_work_order_id
    INTO @DATA(lv_work_order_id).

    IF sy-subrc EQ 0.
      rv_exists_ord = abap_true.
    ELSE.
      rv_exists_ord = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD check_order_history.

    SELECT SINGLE FROM ztbl_wrkord_his
    FIELDS work_order_id
    WHERE work_order_id EQ @iv_work_order_id
    INTO @DATA(lv_work_order_id).

    IF sy-subrc EQ 0.
      rv_exists_his = abap_true.
    ELSE.
      rv_exists_his = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD validate_create_order. "Validates the client, technician and priority are valid before allowing the creation of a work order

    " Check if customer exists
    DATA(lv_customer_exists) = check_customer_exists( iv_customer_id ).
    IF lv_customer_exists IS INITIAL.
      rv_valid_cr = abap_false.
      ev_error = |Customer ID { iv_customer_id } does NOT exist.|.
      RETURN.
    ENDIF.

    " Check if technician exists
    DATA(lv_technician_exists) = check_technician_exists( iv_technician_id ).
    IF lv_technician_exists IS INITIAL.
      rv_valid_cr = abap_false.
      ev_error = |Technician ID { iv_technician_id } does NOT exist.|.
      RETURN.
    ENDIF.

    " Check if priority is valid
    DATA(lv_priority_valid) = check_priority_valid( iv_priority ).
    IF lv_priority_valid IS INITIAL.
      rv_valid_cr = abap_false.
      ev_error = |Priority { iv_priority } is not valid.|.
      RETURN.
    ENDIF.

    rv_valid_cr = abap_true.

  ENDMETHOD.

  METHOD validate_update_order. "Validates if a work order can be updated if it exists and if its status allows it

    " Check if the work order exists
    DATA(lv_order_exists) = check_order_exists( iv_work_order_id ).
    IF lv_order_exists IS INITIAL.
      rv_valid_up = abap_false.
      ev_error = |Work Order ID { iv_work_order_id } does NOT exist.|.
      RETURN.
    ENDIF.

    " Check if the order status is editable (e.g., Pending)
    IF iv_status <> mc_valid_status-Pending.
      rv_valid_up = abap_false.
      ev_error = |Status is non editable|.
      RETURN.
    ENDIF.

    rv_valid_up = abap_true.

  ENDMETHOD.

  METHOD validate_delete_order. "Verifies only work orders with pending status and no previous history can be deleted

    " Check if the order exists
    DATA(lv_order_exists) = check_order_exists( iv_work_order_id ).
    IF lv_order_exists IS INITIAL.
      rv_valid_de = abap_false.
      ev_error = |Work Order ID { iv_work_order_id } does NOT exist.|.
      RETURN.
    ENDIF.

    " Check if the order status is "PE" (Pending)
    IF iv_status <> mc_valid_status-Pending.
      rv_valid_de = abap_false.
      ev_error = |Status is non editable|.
      RETURN.
    ENDIF.

    " Check if the order has a history (i.e., if it has been modified before)
    DATA(lv_has_history) = check_order_history( iv_work_order_id ).
    IF lv_has_history IS NOT INITIAL.
      rv_valid_de = abap_false.
      ev_error = |Order { iv_work_order_id } has been modified before.|.
      RETURN.
    ENDIF.

    rv_valid_de = abap_true.

  ENDMETHOD.

  METHOD validate_status_and_priority. "Validates both status and priority values are valid

    " Validate the status value
    IF iv_status <> mc_valid_status-pending.
      rv_valid_sp = abap_false.
      ev_error = |Status is not valid.|.
      RETURN.
    ENDIF.

    " Validate the priority value
    IF iv_priority <> mc_valid_priority-high.
      rv_valid_sp = abap_false.
      ev_error = |Priority { iv_priority } is not valid.|.
    ELSEIF iv_priority <> mc_valid_priority-medium.
      rv_valid_sp = abap_false.
      ev_error = |Priority { iv_priority } is not valid.|.
    ELSEIF iv_priority <> mc_valid_priority-low.
      rv_valid_sp = abap_false.
      ev_error = |Priority { iv_priority } is not valid.|.
      RETURN.
    ENDIF.

    rv_valid_sp = abap_true.

  ENDMETHOD.

ENDCLASS.
