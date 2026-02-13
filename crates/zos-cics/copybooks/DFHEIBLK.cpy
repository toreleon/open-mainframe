      ******************************************************************
      * DFHEIBLK - Execute Interface Block
      * Standard copybook for CICS EIB structure
      ******************************************************************
       01  DFHEIBLK.
           02  EIBTIME   PIC S9(7) COMP-3.
           02  EIBDATE   PIC S9(7) COMP-3.
           02  EIBTRNID  PIC X(4).
           02  EIBTASKN  PIC S9(7) COMP-3.
           02  EIBTRMID  PIC X(4).
           02  DFHEIGDI  PIC S9(4) COMP.
           02  EIBCPOSN  PIC S9(4) COMP.
           02  EIBCALEN  PIC S9(4) COMP.
           02  EIBAID    PIC X(1).
           02  EIBFN     PIC X(2).
           02  EIBRCODE  PIC X(6).
           02  EIBDS     PIC X(8).
           02  EIBREQID  PIC X(8).
           02  EIBRSRCE  PIC X(8).
           02  EIBSYNC   PIC X(1).
           02  EIBFREE   PIC X(1).
           02  EIBRECV   PIC X(1).
           02  EIBSEND   PIC X(1).
           02  EIBATT    PIC X(1).
           02  EIBEOC    PIC X(1).
           02  EIBFMH    PIC X(1).
           02  EIBCOMPL  PIC X(1).
           02  EIBSIG    PIC X(1).
           02  EIBCONF   PIC X(1).
           02  EIBERR    PIC X(1).
           02  EIBERRCD  PIC X(4).
           02  EIBSYNRB  PIC X(1).
           02  EIBNODAT  PIC X(1).
           02  EIBRESP   PIC S9(8) COMP.
           02  EIBRESP2  PIC S9(8) COMP.
           02  EIBRLDBK  PIC X(1).
