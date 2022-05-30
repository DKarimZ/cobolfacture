       IDENTIFICATION DIVISION.
       PROGRAM-ID. entreeargent.
       AUTHOR. D.KISAMA.

      *> Ce programme permet d'effectuer une opération bancaire dans
      *> le but de cérditer un compte

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION. 
       
         01  WS-CURRENT-DATE-FIELDS.
           05  WS-DATE.
               10  WS-YEAR    PIC  9(4).
               10  FILLER     VALUE '-'.
               10  WS-MONTH   PIC  9(2).
               10  FILLER     VALUE '-'.
               10  WS-DAY     PIC  9(4).
           05  WS-TIME.
               10  WS-HOUR    PIC  9(2).
               10  WS-MINUTE  PIC  9(2).
               10  WS-SECOND  PIC  9(2).
               10  WS-MS      PIC  9(2).
           05  WS-DIFF-FROM-GMT       PIC S9(4).     

       LINKAGE SECTION. 
        77 L-SOLDE        PIC 9(5)V99.
        77 L-CREDIT       PIC 9(5)V99.
        77 L-NEWSOLDE     PIC 9(5)V99.
        77 L-LIBELLE      PIC X(12).
        77 L-DATE         PIC X(10).

        PROCEDURE DIVISION USING L-SOLDE, L-CREDIT, L-NEWSOLDE,L-DATE
        ,L-LIBELLE.

        0100-MAIN-PROCEDURE.
           ACCEPT WS-CURRENT-DATE-FIELDS  FROM DATE.
           If(L-DATE = WS-DATE)
               COMPUTE L-NEWSOLDE = L-SOLDE + L-CREDIT.
               MOVE 'PAIEMENT DE ' TO L-LIBELLE.  

       END PROGRAM entreeargent.
