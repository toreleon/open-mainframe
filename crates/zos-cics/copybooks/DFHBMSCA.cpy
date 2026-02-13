      ******************************************************************
      * DFHBMSCA - BMS Symbolic Attribute Characters
      * Standard copybook for BMS field attributes
      ******************************************************************
       01  DFHBMSCA.
      *    Field Attributes
           02  DFHBMPEM  PIC X VALUE X'00'.
           02  DFHBMPRF  PIC X VALUE X'F0'.
           02  DFHBMASF  PIC X VALUE X'C1'.
           02  DFHBMASK  PIC X VALUE X'C1'.
           02  DFHBMUNP  PIC X VALUE X'C4'.
           02  DFHBMUNN  PIC X VALUE X'D0'.
           02  DFHBMPRO  PIC X VALUE X'F0'.
           02  DFHBMBRY  PIC X VALUE X'F8'.
           02  DFHBMDAR  PIC X VALUE X'4C'.
           02  DFHBMFSE  PIC X VALUE X'C8'.
           02  DFHBMPSE  PIC X VALUE X'F4'.
      *    Extended Attributes
           02  DFHBMATR  PIC X VALUE X'C0'.
           02  DFHSA     PIC X VALUE X'00'.
           02  DFHCOLOR  PIC X VALUE X'00'.
           02  DFHPS     PIC X VALUE X'00'.
           02  DFHHLT    PIC X VALUE X'00'.
           02  DFHVAL    PIC X VALUE X'00'.
           02  DFHOUTLN  PIC X VALUE X'00'.
           02  DFHSOSI   PIC X VALUE X'00'.
      *    Color Values
           02  DFHDFCOL  PIC X VALUE X'00'.
           02  DFHBLUE   PIC X VALUE X'F1'.
           02  DFHRED    PIC X VALUE X'F2'.
           02  DFHPINK   PIC X VALUE X'F3'.
           02  DFHGREEN  PIC X VALUE X'F4'.
           02  DFHTURQ   PIC X VALUE X'F5'.
           02  DFHYELLO  PIC X VALUE X'F6'.
           02  DFHNEUT   PIC X VALUE X'F7'.
      *    Highlighting Values
           02  DFHDFHI   PIC X VALUE X'00'.
           02  DFHBLINK  PIC X VALUE X'F1'.
           02  DFHREVRS  PIC X VALUE X'F2'.
           02  DFHUNDLN  PIC X VALUE X'F4'.
      *    Cursor Position
           02  DFHIC     PIC S9(4) COMP VALUE +0.
