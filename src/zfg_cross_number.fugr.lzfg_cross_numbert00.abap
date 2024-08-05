*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTCROSS_NUMBERS.................................*
DATA:  BEGIN OF STATUS_ZTCROSS_NUMBERS               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTCROSS_NUMBERS               .
CONTROLS: TCTRL_ZTCROSS_NUMBERS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTCROSS_NUMBERS               .
TABLES: ZTCROSS_NUMBERS                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
