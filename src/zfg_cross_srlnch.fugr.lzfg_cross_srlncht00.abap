*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTCROSS_SERIALCH................................*
DATA:  BEGIN OF STATUS_ZTCROSS_SERIALCH              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTCROSS_SERIALCH              .
CONTROLS: TCTRL_ZTCROSS_SERIALCH
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTCROSS_SERIALCH              .
TABLES: ZTCROSS_SERIALCH               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
