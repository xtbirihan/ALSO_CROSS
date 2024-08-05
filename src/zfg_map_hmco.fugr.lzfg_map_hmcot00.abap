*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_MAP_HMCO....................................*
TABLES: ZMV_MAP_HMCO, *ZMV_MAP_HMCO. "view work areas
CONTROLS: TCTRL_ZMV_MAP_HMCO
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_MAP_HMCO. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_MAP_HMCO.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_MAP_HMCO_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_MAP_HMCO.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_MAP_HMCO_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_MAP_HMCO_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_MAP_HMCO.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_MAP_HMCO_TOTAL.

*.........table declarations:.................................*
TABLES: ZTCROSS_MAP_HMCO               .
