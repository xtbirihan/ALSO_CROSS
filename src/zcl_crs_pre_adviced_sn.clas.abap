CLASS zcl_crs_pre_adviced_sn DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.
    METHODS process_serial_numbers IMPORTING iv_deliverynumber         TYPE char35
                                             iv_deliverynumber_created TYPE char10
                                             iv_customer_ordernumber   TYPE char10
                                             it_serialnumbers          TYPE ztt_pre_adviced_sn.
    METHODS get_messages RETURNING VALUE(rt_messages) TYPE bapiret1_t.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_delivery_document TYPE /scwm/sp_docno_int.
    DATA mv_delivery_date TYPE lfdat.
    DATA mv_purchase_document_number TYPE ebeln.
    DATA mv_warehousenumber TYPE /scwm/lgnum.

    DATA mt_serialnumbers TYPE ztt_pre_adviced_sn.

    DATA mo_messages_adapter TYPE REF TO zcl_core_messages_adapter.
    DATA mo_messages TYPE REF TO zcl_core_messages.

    METHODS data_is_valid RETURNING VALUE(rv_data_is_valid) TYPE abap_bool.
    METHODS insert_serialnumbers.
ENDCLASS.



CLASS ZCL_CRS_PRE_ADVICED_SN IMPLEMENTATION.


  METHOD constructor.
**********************************************************************
*& Key           : AD-230921
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Constructor
*&
**********************************************************************
    mo_messages_adapter = NEW zcl_core_messages_adapter(  ).
    mo_messages = NEW zcl_core_messages(  ).
  ENDMETHOD.


  METHOD data_is_valid.
**********************************************************************
*& Key           : AD-230921
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Check, if the provided data are valid
*&
**********************************************************************
    rv_data_is_valid = abap_true.

    IF mv_delivery_document IS INITIAL.
      rv_data_is_valid = abap_false.
      mo_messages->add_message( VALUE zstr_msg(  msgtyp = 'E'
                                                 msgnr = '015' " Parameter &1 missing
                                                 msgid = 'ZMC_CRS'
                                                 msgv1 = 'IV_DELIVERYNUMBER'
                                              )
                              ).
    ENDIF.

    IF mv_delivery_date IS INITIAL.
      rv_data_is_valid = abap_false.
      mo_messages->add_message( VALUE zstr_msg(  msgtyp = 'E'
                                                 msgnr = '015' " Parameter &1 missing
                                                 msgid = 'ZMC_CRS'
                                                 msgv1 = 'IV_DELIVERYNUMBER_CREATED'
                                          )
                              ).
    ENDIF.

    IF mv_purchase_document_number IS INITIAL.
      rv_data_is_valid = abap_false.
      mo_messages->add_message( VALUE zstr_msg(  msgtyp = 'E'
                                                 msgnr = '015' " Parameter &1 missing
                                                 msgid = 'ZMC_CRS'
                                                 msgv1 = 'IV_CUSTOMER_ORDERNUMBER'
                                          )
                              ).
    ENDIF.

    LOOP AT mt_serialnumbers ASSIGNING FIELD-SYMBOL(<ls_serialnumber>).
      SELECT SINGLE COUNT(*)
        INTO @DATA(lv_counter)
        FROM ztcross_preadvic
       WHERE lgnum = @mv_warehousenumber
         AND docno = @mv_delivery_document
         AND huident = @<ls_serialnumber>-palletnumber
         AND ebeln = @mv_purchase_document_number
         AND mfrpn = @<ls_serialnumber>-articlenumber
         AND serial = @<ls_serialnumber>-serialnumber.

      IF lv_counter <> 0.
        rv_data_is_valid = abap_false.
        mo_messages->add_message( VALUE zstr_msg(  msgtyp = 'E'
                                                   msgnr = '016' " Entry for pre advised SN already exists
                                                   msgid = 'ZMC_CRS'
                                                   msgv1 = |Deliverynumber:{ mv_delivery_document }|
                                                   msgv2 = |Palletnumber:{ <ls_serialnumber>-palletnumber }|
                                                   msgv3 = |Articlenumber:{ <ls_serialnumber>-articlenumber }|
                                                   msgv4 = |Serialnumber:{ <ls_serialnumber>-serialnumber }|
                                    )
                        ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_messages.
**********************************************************************
*& Key           : AD-230921
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Returns the messages collected during processing the serialnumbers
*&
**********************************************************************
    mo_messages_adapter->add_messages_from_object( mo_messages ).
    rt_messages = mo_messages_adapter->get_messages_bapiret1(  ).
  ENDMETHOD.


  METHOD insert_serialnumbers.
**********************************************************************
*& Key           : AD-230921
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Inserts the pre adviced serialnumbers into table ZTCROSS_PREADVIC
*&
**********************************************************************
    DATA lt_serialnumbers TYPE TABLE OF ztcross_preadvic.

    lt_serialnumbers = VALUE #( FOR <ls_serialno> IN mt_serialnumbers
                                    ( mandt  = sy-mandt
                                      lgnum = mv_warehousenumber
                                      docno = mv_delivery_document
                                      huident = <ls_serialno>-palletnumber
                                      ebeln = mv_purchase_document_number
                                      mfrpn = <ls_serialno>-articlenumber
                                      serial = <ls_serialno>-serialnumber
                                      lfdat = sy-datum ) ).

    INSERT ztcross_preadvic FROM TABLE lt_serialnumbers.

    IF sy-subrc <> 0.
      mo_messages->add_message( VALUE zstr_msg(  msgtyp = 'E'
                                                 msgnr = '017' " Error while inserting serial numbers. rc = &1
                                                 msgid = 'ZMC_CRS'
                                                 msgv1 = sy-subrc )
                              ).
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    IF sy-dbcnt = 0.
      mo_messages->add_message( VALUE zstr_msg(  msgtyp = 'E'
                                                 msgnr = '018' " No Serialnumbers inserted.
                                                 msgid = 'ZMC_CRS' )
                              ).
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    COMMIT WORK.
    mo_messages->add_message( VALUE zstr_msg(  msgtyp = 'E'
                                               msgnr = '019' " Insert successful. &1 Serialnumber(s) inserted.
                                               msgid = 'ZMC_CRS'
                                               msgv1 = |{ sy-dbcnt }| )
                          ).
  ENDMETHOD.


  METHOD process_serial_numbers.
**********************************************************************
*& Key           : AD-230921
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Validates the provided serial numbers and inserts them in the table
*& ZTCROSS_PREADVIC
**********************************************************************
    CLEAR mv_delivery_document.
    CLEAR mv_delivery_date.
    CLEAR mv_purchase_document_number.
    CLEAR mv_warehousenumber.
    REFRESH mt_serialnumbers.

    mv_delivery_document = iv_deliverynumber.
    mv_warehousenumber = 'DE50'. " <= How to determine the warehousenumber?

    CALL FUNCTION 'CONVERSION_EXIT_IDATE_INPUT'
      EXPORTING
        input  = iv_deliverynumber_created
      IMPORTING
        output = mv_delivery_date.


    mv_purchase_document_number = iv_customer_ordernumber.

    mt_serialnumbers = it_serialnumbers.

    IF data_is_valid(  ).
      insert_serialnumbers(  ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
