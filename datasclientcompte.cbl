       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.
       AUTHOR. D.KISAMA.

      *> Ce programme permet d'imprimer dans un fichier .DAT les 
      *> élemnts de la base de données sélectionnées 

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. 
       SOURCE-COMPUTER. 
       OBJECT-COMPUTER. 

       INPUT-OUTPUT SECTION. 
       FILE-CONTROL.
           SELECT PRINT-FILE ASSIGN TO "PRINTFILE.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION. 
       FILE SECTION. 

       FD PRINT-FILE.
          01 DETAILS-LINE             PIC X(250).


       WORKING-STORAGE SECTION.

       01  SQLDA-ID pic 9(4) comp-5.
       01  SQLDSIZE pic 9(4) comp-5.
       01  SQL-STMT-ID pic 9(4) comp-5.
       01  SQLVAR-INDEX pic 9(4) comp-5.
       01  SQL-DATA-TYPE pic 9(4) comp-5.
       01  SQL-HOST-VAR-LENGTH pic 9(9) comp-5.
       01  SQL-S-HOST-VAR-LENGTH pic 9(9) comp-5.
       01  SQL-S-LITERAL pic X(258).
       01  SQL-LITERAL1 pic X(130).
       01  SQL-LITERAL2 pic X(130).
       01  SQL-LITERAL3 pic X(130).
       01  SQL-LITERAL4 pic X(130).
       01  SQL-LITERAL5 pic X(130).
       01  SQL-LITERAL6 pic X(130).
       01  SQL-LITERAL7 pic X(130).
       01  SQL-LITERAL8 pic X(130).
       01  SQL-LITERAL9 pic X(130).
       01  SQL-LITERAL10 pic X(130).
       01  SQL-IS-LITERAL pic 9(4) comp-5 value 1.
       01  SQL-IS-INPUT-HVAR pic 9(4) comp-5 value 2.
       01  SQL-CALL-TYPE pic 9(4) comp-5.
       01  SQL-SECTIONUMBER pic 9(4) comp-5.
       01  SQL-INPUT-SQLDA-ID pic 9(4) comp-5.
       01  SQL-OUTPUT-SQLDA-ID pic 9(4) comp-5.
       01  SQL-VERSION-NUMBER pic 9(4) comp-5.
       01  SQL-ARRAY-SIZE pic 9(4) comp-5.
       01  SQL-IS-STRUCT  pic 9(4) comp-5.
       01  SQL-IS-IND-STRUCT pic 9(4) comp-5.
       01  SQL-STRUCT-SIZE pic 9(4) comp-5.
       01  SQLA-PROGRAM-ID.
           05 SQL-PART1 pic 9(4) COMP-5 value 172.
           05 SQL-PART2 pic X(6) value "AEAVAI".
           05 SQL-PART3 pic X(24) value "KBTAVdFm01111 2         ".
           05 SQL-PART4 pic 9(4) COMP-5 value 8.
           05 SQL-PART5 pic X(8) value "DB2ADMIN".
           05 SQL-PART6 pic X(120) value LOW-VALUES.
           05 SQL-PART7 pic 9(4) COMP-5 value 8.
           05 SQL-PART8 pic X(8) value "DATASCLI".
           05 SQL-PART9 pic X(120) value LOW-VALUES.
                                

      *> SQL zone de communication (avec code erreurs etc)
       
           COPY "sqlca.cbl".

      *> Declaration des variables hotes utilisés lors des requêtes

           
      *EXEC SQL BEGIN DECLARE SECTION END-EXEC.

        01 HV-CLIENT.
           05 HV-CLIENT-ID            PIC S9(3) COMP-3.
           05 HV-CLIENT-NOM           PIC X(20).
           05 HV-CLIENT-PRENOM        PIC X(20).
           05 HV-CLIENT-RSOCIALE      PIC X(20).
           05 HV-CLIENT-TYPECLIENT    PIC X(20).
           05 HV-CLIENT-EXTERNE       PIC S9 COMP-3.
           
        01 HV-COMPTE.
           05 HV-COMPTE-ID            PIC S9(3) COMP-3 .
           05 HV-COMPTE-IBAN          PIC X(50).
           05 HV-COMPTE-DTOUV         PIC X(14).
           05 HV-COMPTE-SOLDE         PIC S9(6)V99 PACKED-DECIMAL.
           05 HV-CLIENT-ID2           PIC S9(3) COMP-3.

        01 HV-OPERATIONS.
           05 HV-LIBELLE-OP           PIC X(30).
           05 HV-MONTANT-OP           PIC S9(5)V99 PACKED-DECIMAL.
           05 HV-DATE-OP              PIC X(10).
           05 HV-STATUS-OP            PIC X(20).
      
           
      *EXEC SQL END DECLARE SECTION END-EXEC
                                                 

       *> Création d'un curseur pour afficher  clients et leur compte
      
           
      *EXEC SQL DECLARE CLICOMPTECUR CURSOR WITH HOLD FOR
      *
      *        SELECT DISTINCT(CLIENTS.IDCLIENT), NOM, PRENOM,
      *        RAISON_SOCIALE,TYPECLIENT,EXTERNE,COMPTES.COMPTEID, 
      *        IBAN,DATEOUVER,SOLDE,COMPTES.IDCLIENT,LIBELLE,MONTANT,
      *        DATEOP,STATUSOP FROM CLIENTS JOIN OPERATIONS 
      *        ON CLIENTS.IDCLIENT = OPERATIONS.IDCLIENT 
      *        JOIN COMPTES ON OPERATIONS.COMPTEID = COMPTES.COMPTEID
      *        WHERE CLIENTS.IDCLIENT = 001
      *        ORDER BY CLIENTS.IDCLIENT
      *         
      *     END-EXEC
                    
             
      *> Déclartion des variables de mon programme 

        01 WS-FIELDS.
      *     05 CLIENT-ID               PIC S9(3).
      *     05 FILLER                  PIC X(3) VALUE SPACES.
           05 CLIENT-NOM              PIC X(20).
           05 CLIENT-PRENOM           PIC X(20).
      *     05 CLIENT-TYPECLIENT       PIC X(20).
      *     05 CLIENT-EXTERNE          PIC 9.
      *     05 COMPTE-ID               PIC S9(3).
      *     05 FILLER                  PIC X(3) VALUE SPACES.
      *     05 COMPTE-IBAN             PIC X(50).
      *     05 COMPTE-DTOUV            PIC X(14).
           05 COMPTE-SOLDE            PIC 9(5).
           05 FILLER                  PIC X(3) VALUE SPACES.
           05 LIBELLE-OP              PIC X(30).
           05 FILLER                  PIC X(3) VALUE SPACES.
           05 MONTANT-OP              PIC 9(5)V99.
           05 FILLER                  PIC X(3) VALUE SPACES.
           05 DATE-OP                 PIC X(10).
           
       

       PROCEDURE DIVISION.

       0100-MAIN-PROCEDURE.

      *     MOVE 12 TO HV-CLIENT-ID.
           OPEN OUTPUT  PRINT-FILE.
           PERFORM 0200-CONNECT-BDD .
           PERFORM 0210-OBTAIN-DATA .
           PERFORM 0900-STOP-RUN.


       0200-CONNECT-BDD.
           
      *EXEC SQL CONNECT TO facture3 USER DB2ADMIN using hiroshima
      *     END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 1 TO SQL-STMT-ID 
           MOVE 3 TO SQLDSIZE 
           MOVE 2 TO SQLDA-ID 

           CALL "sqlgaloc" USING
               BY VALUE SQLDA-ID 
                        SQLDSIZE
                        SQL-STMT-ID
                        0

           MOVE "facture3"
            TO SQL-LITERAL1
           MOVE 8 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 0 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE SQL-LITERAL1
            BY VALUE 0
                     0

           MOVE "DB2ADMIN"
            TO SQL-LITERAL2
           MOVE 8 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 1 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE SQL-LITERAL2
            BY VALUE 0
                     0

           MOVE "hiroshima"
            TO SQL-LITERAL3
           MOVE 9 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 2 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE SQL-LITERAL3
            BY VALUE 0
                     0

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 2 TO SQL-INPUT-SQLDA-ID 
           MOVE 5 TO SQL-SECTIONUMBER 
           MOVE 29 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                   .

       0210-OBTAIN-DATA.

           
      *EXEC SQL OPEN CLICOMPTECUR
      *     END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 1 TO SQL-SECTIONUMBER 
           MOVE 26 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                   .

           PERFORM UNTIL SQLCODE = 100
              
      *EXEC SQL FETCH CLICOMPTECUR
      *           INTO :HV-CLIENT-ID,:HV-CLIENT-NOM, :HV-CLIENT-PRENOM,
      *           :HV-CLIENT-RSOCIALE,:HV-CLIENT-TYPECLIENT,
      *           :Hv-CLIENT-EXTERNE,:HV-COMPTE-ID,:HV-COMPTE-IBAN,
      *           :HV-COMPTE-DTOUV,:HV-COMPTE-SOLDE,:HV-CLIENT-ID2,
      *          :HV-LIBELLE-OP,:HV-MONTANT-OP,:HV-DATE-OP,:HV-STATUS-OP
      *        END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 2 TO SQL-STMT-ID 
           MOVE 15 TO SQLDSIZE 
           MOVE 3 TO SQLDA-ID 

           CALL "sqlgaloc" USING
               BY VALUE SQLDA-ID 
                        SQLDSIZE
                        SQL-STMT-ID
                        0

           MOVE 3 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 0 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE HV-CLIENT-ID
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 20 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 1 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE HV-CLIENT-NOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 20 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 2 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE HV-CLIENT-PRENOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 20 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 3 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE HV-CLIENT-RSOCIALE
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 20 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 4 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE HV-CLIENT-TYPECLIENT
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 1 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 5 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE HV-CLIENT-EXTERNE
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 3 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 6 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE HV-COMPTE-ID
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 7 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE HV-COMPTE-IBAN
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 14 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 8 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE HV-COMPTE-DTOUV
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 520 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 9 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE HV-COMPTE-SOLDE
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 3 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 10 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE HV-CLIENT-ID2
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 30 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 11 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE HV-LIBELLE-OP
            OF
            HV-OPERATIONS
            BY VALUE 0
                     0

           MOVE 519 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 12 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE HV-MONTANT-OP
            OF
            HV-OPERATIONS
            BY VALUE 0
                     0

           MOVE 10 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 13 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE HV-DATE-OP
            OF
            HV-OPERATIONS
            BY VALUE 0
                     0

           MOVE 20 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 14 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE HV-STATUS-OP
            OF
            HV-OPERATIONS
            BY VALUE 0
                     0

           MOVE 3 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 1 TO SQL-SECTIONUMBER 
           MOVE 25 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                      
           PERFORM  0300-PRINT-DATA 
           END-PERFORM.

           
      *EXEC SQL CLOSE CLICOMPTECUR
      *     END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 1 TO SQL-SECTIONUMBER 
           MOVE 20 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                   .


       0300-PRINT-DATA.
      *     MOVE HV-CLIENT-ID TO CLIENT-ID.
           MOVE HV-CLIENT-NOM TO CLIENT-NOM.
           MOVE HV-CLIENT-PRENOM TO CLIENT-PRENOM.
      *     MOVE HV-CLIENT-TYPECLIENT TO CLIENT-TYPECLIENT.
      *     MOVE HV-CLIENT-EXTERNE TO CLIENT-EXTERNE.
      *     MOVE HV-COMPTE-ID TO COMPTE-ID.
      *     MOVE HV-COMPTE-IBAN TO COMPTE-IBAN.
      *     MOVE HV-COMPTE-DTOUV TO COMPTE-DTOUV.
           MOVE HV-COMPTE-SOLDE TO COMPTE-SOLDE.
           MOVE HV-LIBELLE-OP TO LIBELLE-OP.
           MOVE HV-MONTANT-OP TO MONTANT-OP.
           MOVE HV-DATE-OP TO DATE-OP.
      *     MOVE HV-STATUS-OP TO STATUS-OP.
           MOVE Ws-FIELDS TO DETAILS-LINE.
           WRITE DETAILS-LINE AFTER ADVANCING 1 LINE.

       0900-STOP-RUN.
           CLOSE PRINT-FILE.
           STOP RUN.

          END PROGRAM MAIN.
