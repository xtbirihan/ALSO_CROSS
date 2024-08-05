*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTCROSS_CART_TYP................................*
DATA:  BEGIN OF STATUS_ZTCROSS_CART_TYP              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTCROSS_CART_TYP              .
CONTROLS: TCTRL_ZTCROSS_CART_TYP
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTCROSS_CART_TYP              .
TABLES: ZTCROSS_CART_TYP               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
