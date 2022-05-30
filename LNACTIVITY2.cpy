       01 LN-MOD.
       02 LN-INPUT.
         03 LN-FNC                    PIC X(2).
            88 V-LN-FNC-SEE-ALL-CLIENTS VALUE "SE".
            88 V-LN-FNC-ADD-NEW-CLIENT  VALUE "AD".
            88 V-LN-FNC-SEE-ONE-CLIENT  VALUE "SO".
            88 V-LN-FNC-SEE-ALL-WAIT-OP VALUE "SW".
            88 V-LN-FNC-UPDATE_OP       VALUE "UP".
            88 V-LN-FNC-CONNECT          VALUE "CO".
            88 V-LN-FNC-DELETE          VALUE "DE".
            88 V-LN-FNC-PAGING-FIRST    VALUE "PF".
            88 V-LN-FNC-PAGING-NEXT     VALUE "PN".
            88 V-LN-FNC-PAGING-PREVIOUS VALUE "PP".
            88 V-LN-FNC-PAGING-LAST     VALUE "PL".
            88 V-LN-FNC-LIST-FIRST      VALUE "LF".
            88 V-LN-FNC-LIST-NEXT       VALUE "LN".
            88 V-LN-FNC-LIST-PREVIOUS   VALUE "LP".
            88 V-LN-FNC-LIST-LAST       VALUE "LL".
         03 LN-CONNECT.
           04 LN-DBALIAS                PIC X(9).
           04 LN-USERID                 PIC X(20).
           04 LN-PSWD                   PIC X(20).
         03 LN-INP-CLIENT.
           04 LN-INP-CLIENTID               PIC 9(3).
           04 LN-INP-NOM                    PIC X(50).
           04 LN-INP-PRENOM                 PIC X(50).
           04 LN-INP-RAISON-SOCIALE         PIC X(50).
           04 LN-INP-TYPECLIENT             PIC X(50).
           04 Ln-INP-EXTERNE                PIC 9(1).
         03 LN-INP-COMPTE.
           04 LN-INP-COMPTE-ID                     PIC 9(3).
           04 LN-INP-COMPTE-IBAN                   PIC X(50).
           04 LN-INP-COMPTE-DTOUV                  PIC X(10).
           04 LN-INP-COMPTE-SOLDE                  PIC S9(10)V99.
           04 LN-INP-CLIENT-ID2                    PIC 9(3).
         03 LN-INP-OPERATION.
           04 LN-INP-IDOPERATION            PIC 9(3).
           04 LN-INP-LIBELLE-OP             PIC X(50).
           04 LN-INP-MONTANT-OP             PIC 9(8)V99.
           04 LN-INP-COMPTEID2            PIC 9(3).
           04 LN-INP-IDCLIENT             PIC 9(3).
           04 LN-INP-DATE-OP                PIC X(10).
           04 LN-INP-STATUS-OP               PIC X(20).
       02 LN-OUTPUT.
         03 LN-MSG.
           04 LN-SQLCODE                PIC S9(10).
           04 LN-SQLSTATE               PIC X(5).
           04 LN-MSG-1                  PIC X(80).
           04 LN-MSG-2                  PIC X(80).
           04 LN-MSG-3                  PIC X(80).
           04 LN-MSG-4                  PIC X(80).
         03 LN-OUT-OPERATION.
           04 LN-OUT-IDOPERATION            PIC 9(3).
           04 LN-OUT-TYPE                   PIC X(50).
           04 LN-OUT-LIBELLE-OP             PIC X(50).
           04 LN-OUT-MONTANT-OP             PIC 9(8)V99.
           04 LN-OUT-COMPTEID2            PIC 9(3).
           04 LN-OUT-IDCLIENT             PIC 9(3).
           04 LN-OUT-DATE-OP                PIC X(10).
          03 LN-OUT-COMPTE.
            04 LN-OUT-COMPTE-ID          PIC 9(3).
            04 LN-OUT-COMPTE-IBAN          PIC X(50).
            04 LN-OUT-COMPTE-DTOUV          PIC X(10).
            04 LN-OUT-COMPTE-SOLDE          PIC S9(10)V99.
            04 LN-OUT-CLIENT-ID2             PIC 9(3).             
         03 LN-OUT-CLIENT.
           04 LN-OUT-CLIENTID                 PIC 9(3).
           04 LN-OUT-NOM                      PIC X(50).
           04 LN-OUT-PRENOM                   PIC X(50).
           04 LN-OUT-RAISON-SOCIALE           PIC X(50).
           04 LN-OUT-TYPECLIENT               PIC X(50).
           04 LN-OUT-EXTERNE                  PIC 9(1).
         03 LN-OUT-CLIENT-TABLE.
           04 LN-OUT-CLIENT-TAB-LINE-NR   PIC 9(2).
      *     see constant C-MAX-LINE-NR = 10 in the programs       
           04 LN-OUT-CLIENT-TAB OCCURS 10 TIMES. 
             05 LN-OUT-CLIENT-TAB-LINE.
               06 LN-OUT-CLIENT-TAB-IDCLIENT  PIC 9(3).
               06 LN-OUT-CLIENT-TAB-NOM       PIC X(50).
               06 LN-OUT-CLIENT-TAB-PRENOM    PIC X(50).
               06 LN-OUT-CLIENT-TAB-RSOCIALE   PIC X(50).
               06 LN-OUT-CLIENT-TAB-TYPECLIENT   PIC X(50).
               06 LN-OUT-CLIENT-TAB-EXTERNE   PIC 9(1).
               