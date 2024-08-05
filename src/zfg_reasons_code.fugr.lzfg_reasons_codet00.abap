*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTCROSS_REASONSC................................*
DATA:  BEGIN OF STATUS_ZTCROSS_REASONSC              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTCROSS_REASONSC              .
CONTROLS: TCTRL_ZTCROSS_REASONSC
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTCROSS_REASONSC              .
TABLES: ZTCROSS_REASONSC               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
