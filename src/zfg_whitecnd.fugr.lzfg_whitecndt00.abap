*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_WHITECND....................................*
TABLES: ZMV_WHITECND, *ZMV_WHITECND. "view work areas
CONTROLS: TCTRL_ZMV_WHITECND
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_WHITECND. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_WHITECND.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_WHITECND_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_WHITECND.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_WHITECND_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_WHITECND_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_WHITECND.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_WHITECND_TOTAL.

*.........table declarations:.................................*
TABLES: ZTCROSS_WHITECND               .
