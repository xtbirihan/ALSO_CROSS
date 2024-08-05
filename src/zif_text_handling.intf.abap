interface ZIF_TEXT_HANDLING
  public .


  types:
    BEGIN OF ty_text,
      text_type	TYPE zde_text_type,
      text_id	  TYPE zde_text_id,
      text      TYPE zde_text,
    END OF ty_text .
  types:
    tt_text TYPE STANDARD TABLE OF ty_text with EMPTY KEY .
  types:
    tt_text_type TYPE STANDARD TABLE OF zde_text_type WITH EMPTY KEY .

  methods GET_TEXTS_FROM_BP
    importing
      !IV_BUSINESS_PARTNER type BU_PARTNER
      !IT_TEXT_TYPE type TT_TEXT_TYPE
      !IV_NO_DEL_NOTE type ABAP_BOOL
    returning
      value(RT_TEXT) type TT_TEXT .
  methods CHANGE_SHLP
    changing
      !CS_SHLP type SHLP_DESCR .
  methods CHECK_TEXT
    importing
      !IV_TEXT_TYPE type ZDE_TEXT_TYPE
      !IV_TEXT_ID type ZDE_TEXT_ID
    returning
      value(RV_EXISTS) type ABAP_BOOL .
  methods GET_TEXTS_FROM_PROD
    importing
      !IV_PRODUCT type MATNR
      !IT_TEXT_TYPE type TT_TEXT_TYPE
      !IV_ENTITLED type /SCWM/DE_ENTITLED
      !IV_LGNUM type /SCWM/LGNUM
    returning
      value(RT_TEXT) type TT_TEXT .
  methods CREATE_PACK_INSTR_STRING
    importing
      !IT_TEXT type TT_TEXT
    returning
      value(RV_PACK_INSTR) type STRING .
endinterface.
