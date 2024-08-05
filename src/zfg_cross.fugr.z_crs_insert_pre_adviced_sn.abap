FUNCTION z_crs_insert_pre_adviced_sn.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_DELIVERYNUMBER) TYPE  CHAR35 OPTIONAL
*"     VALUE(IV_DELIVERYNUMBER_CREATED) TYPE  CHAR10 OPTIONAL
*"     VALUE(IV_CUSTOMER_ORDERNUMBER) TYPE  CHAR10 OPTIONAL
*"     VALUE(IT_SERIALNUMBERS) TYPE  ZTT_PRE_ADVICED_SN
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET1_T
*"----------------------------------------------------------------------
  DATA(lo_pre_adviced_sn) = NEW zcl_crs_pre_adviced_sn(  ).

  lo_pre_adviced_sn->process_serial_numbers( iv_deliverynumber = iv_deliverynumber
                                             iv_deliverynumber_created = iv_deliverynumber_created
                                             iv_customer_ordernumber = iv_customer_ordernumber
                                             it_serialnumbers = it_serialnumbers  ).

  et_return = lo_pre_adviced_sn->get_messages(  ).
ENDFUNCTION.
