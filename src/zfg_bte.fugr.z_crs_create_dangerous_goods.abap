FUNCTION Z_CRS_CREATE_DANGEROUS_GOODS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_MATNR) TYPE  MATNR
*"     VALUE(IV_HAZMATSIGN) TYPE  ZZ1_MHMSIGN01
*"----------------------------------------------------------------------
**********************************************************************
*& Key           : LH-260423
*& Request No.   : GAP-040 – Additional product master fields
**********************************************************************
*& Description (short)
*& Create DG master in asyncron RFC, this is needed because BAPI_DANGEROUSGOOD_SAVREPMUL
*& calls commit work and it is not allowd in update FM.
*& This FM must be called in V2 update task, because we want to create
*& DG master, when we create the product, but DG master needs existing product.
**********************************************************************
    zcl_crs_hazmat=>create_dangerous_product(
      iv_matnr      =  iv_matnr                " Material Number
      iv_hazmatsign =  iv_hazmatsign                " Haz. Mat 1
    ).


ENDFUNCTION.
