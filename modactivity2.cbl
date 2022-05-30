       IDENTIFICATION DIVISION.
       PROGRAM-ID. modactivity2.
      
       ENVIRONMENT DIVISION.
      
       DATA DIVISION.
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
           05 SQL-PART3 pic X(24) value "UAunScFm01111 2         ".
           05 SQL-PART4 pic 9(4) COMP-5 value 8.
           05 SQL-PART5 pic X(8) value "DB2ADMIN".
           05 SQL-PART6 pic X(120) value LOW-VALUES.
           05 SQL-PART7 pic 9(4) COMP-5 value 8.
           05 SQL-PART8 pic X(8) value "MODACTIV".
           05 SQL-PART9 pic X(120) value LOW-VALUES.
                               
      *> max number of lines for the list screen   
       78 C-MAX-LINE-NR                VALUE 10.  
      *> indices for cycles
       01 WS-IND-1                     PIC S9(4) COMP.
       
      *> linkage for DB2SQLMSG.cob   
       COPY "LNSQLMSG.cpy".

      *> SQL communication area
       COPY "sqlca.cbl".
       
      *> SQL status
       01 WS-SQL-STATUS                PIC S9(9) COMP-5.
          88 SQL-STATUS-OK             VALUE    0.
          88 SQL-STATUS-NOT-FOUND      VALUE  100.
          88 SQL-STATUS-DUP            VALUE -803.
       
      *> SQL declare variables
            
      *EXEC SQL BEGIN DECLARE SECTION END-EXEC.
      
        01 HV-CLIENT.
          05 CLIENT-ID            PIC S9(3) COMP-3.
          05 CLIENT-NOM           PIC X(50).
          05 CLIENT-PRENOM        PIC X(50).
          05 CLIENT-RSOCIALE      PIC X(50).
          05 CLIENT-TYPECLIENT    PIC X(50).
          05 CLIENT-EXTERNE       PIC S9(1) COMP-3.
          05 INDICATOR-TAB-CLIENT. 

           10 CLIENT-ID-I          PIC S9(4) COMP-5.
           10 CLIENT-NOM-I         PIC S9(4) COMP-5.
           10 CLIENT-PRENOM-I      PIC S9(4) COMP-5.
           10 CLIENT-RSOCIALE-I    PIC S9(4) COMP-5.
           10 CLIENT-TYPECLIENT-I  PIC S9(4) COMP-5.
           10 CLIENT-EXTERNE-I     PIC S9(4) COMP-5.
          
        01 HV-COMPTE.
          05 COMPTE-ID            PIC S9(3) COMP-3 .
          05 COMPTE-IBAN          PIC X(50).
          05 COMPTE-DTOUV         PIC X(10).
          05 COMPTE-SOLDE         PIC S9(10)V99 PACKED-DECIMAL.
          05 CLIENT-ID2           PIC S9(3) COMP-3.
          05 INDICATOR-TAB-COMPTE. 

           10 COMPTE-ID-I        PIC S9(4) COMP-5.
           10 COMPTE-IBAN-I      PIC S9(4) COMP-5.
           10 COMPTE-DTOUV-I     PIC S9(4) COMP-5.
           10 COMPTE-SOLDE-I     PIC S9(4) COMP-5.
           10 CLIENT-ID2-I       PIC S9(4) COMP-5.
          
        01 HV-OPERATIONS.
          05 IDOPERATION          PIC S9(3) COMP-3.
          05 LIBELLE-OP           PIC X(50).
          05 MONTANT-OP           PIC S9(8)V99 PACKED-DECIMAL.
          05 COMPTEID-OP          PIC S9(3) COMP-3.
          05 IDCLIENT-OP          PIC S9(3) COMP-3.
          05 TYPE-OP              PIC X(50).
          05 DATE-OP              PIC X(10).
          05 STATUS-OP             PIC X(20).
          05 INDICATOR-TAB-OPERATIONS. 
      
           10 IDOPERATION-I      PIC S9(4) COMP-5.  
           10 LIBELLE-OP-I       PIC S9(4) COMP-5.
           10 MONTANT-OP-I       PIC S9(4) COMP-5.
           10 DATE-OP-I          PIC S9(4) COMP-5.


      *> connect fields with variable length 
        01 HV-DBALIAS.
           49 HV-DBALIAS-LEN            PIC S9(4) COMP-5.
           49 HV-DBALIAS-BUF            PIC X(9).
        01 HV-USERID.                   
           49 HV-USERID-LEN             PIC S9(4) COMP-5.
           49 HV-USERID-BUF             PIC X(20).
        01 HV-PSWD.                     
           49 HV-PSWD-LEN               PIC S9(4) COMP-5.
           49 HV-PSWD-BUF               PIC X(20).
      
      
            
      *EXEC SQL END   DECLARE SECTION END-EXEC
                                                    
      

           
      *EXEC SQL DECLARE CURSOR_OP_PF CURSOR WITH HOLD FOR
      *      SELECT  CLIENTS.IDCLIENT 
      *             ,NOM
      *             ,PRENOM
      *             ,RAISON_SOCIALE
      *             ,TYPECLIENT
      *             ,COMPTES.COMPTEID
      *             ,SOLDE
      *             ,COMPTES.IDCLIENT
      *             ,IDOPERATION
      *             ,LIBELLE
      *             ,MONTANT
      *             ,DATEOP
      *       FROM   CLIENTS 
      *       JOIN   OPERATIONS
      *         ON   CLIENTS.IDCLIENT = OPERATIONS.IDCLIENT
      *       JOIN   COMPTES 
      *         ON   OPERATIONS.COMPTEID = COMPTES.COMPTEID
      *    ORDER BY  IDOPERATION         ASC
      *     END-EXEC
                    
                    

      *> cursor for paging next1

           
      *EXEC SQL DECLARE  CURSOR_OP_PN CURSOR WITH HOLD FOR
      *     SELECT  CLIENTS.IDCLIENT 
      *            ,NOM
      *            ,PRENOM
      *            ,RAISON_SOCIALE
      *            ,TYPECLIENT
      *            ,COMPTES.COMPTEID
      *            ,SOLDE
      *            ,COMPTES.IDCLIENT
      *            ,IDOPERATION
      *            ,LIBELLE
      *            ,MONTANT
      *            ,DATEOP
      *      FROM   CLIENTS 
      *      JOIN   OPERATIONS
      *        ON   CLIENTS.IDCLIENT = OPERATIONS.IDCLIENT
      *      JOIN   COMPTES 
      *        ON   OPERATIONS.COMPTEID = COMPTES.COMPTEID
      *    WHERE        ( IDOPERATION ) > : HV-OPERATIONS.IDOPERATION
      *   ORDER BY  IDOPERATION         ASC
      *     END-EXEC
                    



      *> cursor for paging previous


           
      *EXEC SQL DECLARE  CURSOR_OP_PP CURSOR WITH HOLD FOR
      *     SELECT  CLIENTS.IDCLIENT 
      *            ,NOM
      *            ,PRENOM
      *            ,RAISON_SOCIALE
      *            ,TYPECLIENT
      *            ,COMPTES.COMPTEID
      *            ,SOLDE
      *            ,COMPTES.IDCLIENT
      *            ,IDOPERATION
      *            ,LIBELLE
      *            ,MONTANT
      *            ,DATEOP
      *      FROM   CLIENTS 
      *      JOIN   OPERATIONS
      *        ON   CLIENTS.IDCLIENT = OPERATIONS.IDCLIENT
      *      JOIN   COMPTES 
      *        ON   OPERATIONS.COMPTEID = COMPTES.COMPTEID
      **    WHERE    STATUSOP <> 'EN ATTENTE' AND
      *    WHERE   ( IDOPERATION ) < : HV-OPERATIONS.IDOPERATION
      *   ORDER BY  IDOPERATION         ASC
      *     END-EXEC
                    

       
      *> cursor for paging last

          
      *EXEC SQL DECLARE  CURSOR_OP_PL CURSOR WITH HOLD FOR
      *      SELECT  CLIENTS.IDCLIENT 
      *             ,NOM
      *             ,PRENOM
      *             ,RAISON_SOCIALE
      *             ,TYPECLIENT
      *             ,COMPTES.COMPTEID
      *             ,SOLDE
      *             ,COMPTES.IDCLIENT
      *             ,IDOPERATION
      *             ,LIBELLE
      *             ,MONTANT
      *             ,DATEOP
      *       FROM   CLIENTS 
      *       JOIN   OPERATIONS
      *         ON   CLIENTS.IDCLIENT = OPERATIONS.IDCLIENT
      *       JOIN   COMPTES 
      *         ON   OPERATIONS.COMPTEID = COMPTES.COMPTEID
      **     WHERE    STATUSOP <> 'EN ATTENTE'
      *    ORDER BY  IDOPERATION         DESC
      *     END-EXEC
                    
             
      *> cursor for list first

           
      *EXEC SQL DECLARE   CURSOR_CLIENT_LF CURSOR WITH HOLD FOR
      *     SELECT   CLIENTS.IDCLIENT 
      *             ,NOM
      *             ,PRENOM
      *             ,RAISON_SOCIALE
      *             ,TYPECLIENT
      *             ,COMPTEID
      *             ,SOLDE
      *           FROM   CLIENTS 
      *          INNER JOIN   COMPTES 
      *             ON   CLIENTS.IDCLIENT = COMPTES.IDCLIENT                  
      *      ORDER BY  NOM          ASC
      *              , PRENOM       ASC
      *     END-EXEC
                    

      *> cursor for list next

           
      *EXEC SQL DECLARE   CURSOR_CLIENT_LN CURSOR WITH HOLD FOR
      *     SELECT   CLIENTS.IDCLIENT 
      *              ,NOM
      *              ,PRENOM
      *              ,RAISON_SOCIALE
      *              ,TYPECLIENT
      *              ,COMPTES.COMPTEID
      *              ,SOLDE        
      *           FROM   CLIENTS 
      *           INNER JOIN   COMPTES 
      *          ON   CLIENTS.IDCLIENT = COMPTES.IDCLIENT  
      *                  
      *     WHERE (
      *               NOM
      *              ,PRENOM
      *           ) > (
      *              :HV-CLIENT.CLIENT-NOM
      *             ,:HV-CLIENT.CLIENT-PRENOM
      *
      *           )
      *       
      *      ORDER BY  NOM          ASC
      *              , PRENOM       ASC
      *     END-EXEC
                    
       
       

      *> cursor for list previous
           
      *EXEC SQL DECLARE   CURSOR_CLIENT_LP CURSOR WITH HOLD FOR
      *    SELECT CLIENTS.IDCLIENT 
      *        ,NOM
      *        ,PRENOM
      *        ,RAISON_SOCIALE
      *        ,TYPECLIENT
      *        ,COMPTES.COMPTEID
      *        ,SOLDE       
      *     FROM   CLIENTS 
      *       INNER JOIN   COMPTES 
      *       ON   CLIENTS.IDCLIENT = COMPTES.IDCLIENT 
      *     
      *     
      *     WHERE (
      *         NOM
      *        ,PRENOM
      *     ) > (
      *        :HV-CLIENT.CLIENT-NOM
      *       ,:HV-CLIENT.CLIENT-PRENOM
      *     )
      * 
      *    ORDER BY  NOM          ASC
      *        , PRENOM       ASC
      *      END-EXEC
                     
      
      
      *> cursor for list last

           
      *EXEC SQL DECLARE   CURSOR_CLIENT_LL CURSOR WITH HOLD FOR
      * SELECT CLIENTS.IDCLIENT 
      *        ,NOM
      *        ,PRENOM
      *        ,RAISON_SOCIALE
      *        ,TYPECLIENT
      *        ,COMPTES.COMPTEID
      *        ,SOLDE
      * 
      *     FROM   CLIENTS 
      *       INNER JOIN   COMPTES 
      *       ON   CLIENTS.IDCLIENT = COMPTES.IDCLIENT 
      *     
      *       ORDER BY  NOM          DESC
      *               , PRENOM       DESC
      *     END-EXEC
                    

       
       LINKAGE SECTION.
       COPY "LNACTIVITY2.cpy".
       
       PROCEDURE DIVISION USING LN-MOD.
      
      *>------------------------------------------------------------------------
       MAIN-MODACTIVITY2 SECTION.
      *>-----------------------------------------------------------------------



           INITIALIZE LN-MSG
      
           EVALUATE TRUE
          
             WHEN V-LN-FNC-CONNECT
               PERFORM CONNECT
          
             WHEN V-LN-FNC-ADD-NEW-CLIENT
                PERFORM ADD-NEW-CLIENT

             WHEN V-LN-FNC-SEE-ONE-CLIENT
                PERFORM SEE-ONE-CLIENT

      *       VOIR LE PAGING DES OPERATIONS
      *       WHEN V-LN-FNC-SEE-ALL-WAIT-OP
      *          PERFORM SEE-ALL-WAIT-OP
                
             WHEN V-LN-FNC-UPDATE_OP
                
                
                
      *>     paging functions 
             WHEN V-LN-FNC-PAGING-FIRST
                PERFORM PAGING-FIRST
                
             WHEN V-LN-FNC-PAGING-NEXT
                PERFORM PAGING-NEXT

             WHEN V-LN-FNC-PAGING-PREVIOUS
                PERFORM PAGING-PREVIOUS

             WHEN V-LN-FNC-PAGING-LAST
                PERFORM PAGING-LAST

      *>     list functions --list all clients
             WHEN V-LN-FNC-LIST-FIRST
                PERFORM LIST-FIRST
                
             WHEN V-LN-FNC-LIST-NEXT
                PERFORM LIST-NEXT

             WHEN V-LN-FNC-LIST-PREVIOUS
                PERFORM LIST-PREVIOUS

             WHEN V-LN-FNC-LIST-LAST
                PERFORM LIST-LAST
                
             WHEN OTHER
                MOVE "Wrong linkage function" 
                  TO LN-MSG-1 OF LN-MOD
          END-EVALUATE
      
          GOBACK
      
          .
       MAIN-MODACTIVITY2-EX.
          EXIT.
          


      *>------------------------------------------------------------------------
       CONNECT SECTION.
      *>------------------------------------------------------------------------
          MOVE LN-DBALIAS OF LN-MOD TO HV-DBALIAS-BUF
          MOVE FUNCTION STORED-CHAR-LENGTH(HV-DBALIAS-BUF) 
            TO HV-DBALIAS-LEN
          
          MOVE LN-USERID  OF LN-MOD TO HV-USERID-BUF
          MOVE FUNCTION STORED-CHAR-LENGTH(HV-USERID-BUF) 
            TO HV-USERID-LEN
          
          MOVE LN-PSWD    OF LN-MOD TO HV-PSWD-BUF   
          MOVE FUNCTION STORED-CHAR-LENGTH(HV-PSWD-BUF) 
            TO HV-PSWD-LEN
       
          PERFORM SQL-CONNECT
          PERFORM COPY-SQL-MSG-IN-LINKAGE
          
          .
       CONNECT-EX.
          EXIT. 


      *>------------------------------------------------------------------------
       SEE-ONE-CLIENT SECTION.
      *>------------------------------------------------------------------------

          INITIALIZE HV-CLIENT
          INITIALIZE HV-COMPTE
          INITIALIZE HV-OPERATIONS
          MOVE LN-INP-CLIENTID             OF LN-MOD 
            TO CLIENT-ID                    OF HV-CLIENT
      
          PERFORM SQL-SELECT-CLIENT

          PERFORM COPY-SQL-MSG-IN-LINKAGE

          EVALUATE TRUE
          WHEN     SQL-STATUS-OK
             PERFORM COPY-HV-DATA-IN-LINKAGE
             DISPLAY LN-OUT-CLIENTID       
             DISPLAY LN-OUT-NOM            
             DISPLAY LN-OUT-PRENOM         
             DISPLAY LN-OUT-RAISON-SOCIALE 
             DISPLAY LN-OUT-TYPECLIENT     
      
          WHEN     SQL-STATUS-NOT-FOUND
             MOVE "Aucun client avec ce num de compte "
               TO LN-MSG-1                OF LN-MOD
             MOVE CLIENT-ID                OF HV-CLIENT  
               TO LN-MSG-2                OF LN-MOD
             DISPLAY "RIEN N A ETE TROUVE"
          WHEN  OTHER
              DISPLAY "AUTRE CHOSE A ETE TROUVE"

             CONTINUE
          END-EVALUATE
          
          .
       SEE-ONE-CLIENT-EX.
          EXIT.

      *>------------------------------------------------------------------------
       ADD-NEW-CLIENT SECTION.
      *>------------------------------------------------------------------------

          INITIALIZE HV-CLIENT
          MOVE LN-INP-NOM             OF LN-MOD 
            TO CLIENT-NOM                    OF HV-CLIENT
          MOVE LN-INP-PRENOM          OF LN-MOD   
            TO CLIENT-PRENOM                 OF HV-CLIENT
          MOVE LN-INP-RAISON-SOCIALE   OF LN-MOD
            TO CLIENT-RSOCIALE          OF HV-CLIENT
          MOVE LN-INP-TYPECLIENT      OF LN-MOD
            TO CLIENT-TYPECLIENT             OF HV-CLIENT
          MOVE LN-INP-EXTERNE         OF LN-MOD
            TO CLIENT-EXTERNE                OF HV-CLIENT
            
          PERFORM SQL-INSERT-CLIENT

          PERFORM COPY-SQL-MSG-IN-LINKAGE
          
          EVALUATE TRUE
          WHEN     SQL-STATUS-OK
             PERFORM SQL-COMMIT
             PERFORM COPY-SQL-MSG-IN-LINKAGE
      
          WHEN     SQL-STATUS-DUP
             PERFORM SQL-ROLLBACK
             MOVE "Un client similaire existe déjà "
               TO LN-MSG-1                OF LN-MOD
             MOVE CLIENT-NOM                    OF HV-CLIENT  
               TO LN-MSG-2                OF LN-MOD
      
          WHEN     OTHER
             PERFORM SQL-ROLLBACK
          END-EVALUATE
          
          .
       ADD-NEW-CLIENT-EX.
          EXIT.






      *>------------------------------------------------------------------------
       PAGING-FIRST SECTION.
      *>------------------------------------------------------------------------

          INITIALIZE HV-CLIENT
          INITIALIZE HV-COMPTE
          INITIALIZE HV-OPERATIONS
      
          PERFORM SQL-OPEN-CURSOR-OP-PF

          PERFORM COPY-SQL-MSG-IN-LINKAGE

          IF SQL-STATUS-OK
          THEN
             PERFORM SQL-FETCH-CURSOR-OP-PF

             PERFORM COPY-SQL-MSG-IN-LINKAGE
             
             EVALUATE TRUE
             WHEN     SQL-STATUS-OK
                PERFORM COPY-HV-DATA-IN-LINKAGE
                MOVE "First op selected."
                  TO LN-MSG-1          OF LN-MOD
                MOVE SPACES
                  TO LN-MSG-2          OF LN-MOD
         
             WHEN     SQL-STATUS-NOT-FOUND
                MOVE "No first op found."
                  TO LN-MSG-1          OF LN-MOD
                MOVE SPACES
                  TO LN-MSG-2          OF LN-MOD
         
             WHEN     OTHER
                CONTINUE
             END-EVALUATE
          END-IF      
          
      *>  always try to close the cursor, also in error cases    
          PERFORM SQL-CLOSE-CURSOR-OP-PF
          
          .
       PAGING-FIRST-EX.
          EXIT.

      *>------------------------------------------------------------------------
       PAGING-NEXT SECTION.
      *>------------------------------------------------------------------------

          INITIALIZE HV-CLIENT
          INITIALIZE HV-COMPTE
          INITIALIZE HV-OPERATIONS
      *>  current value as restart point          
          MOVE LN-INP-IDOPERATION      OF LN-MOD 
            TO IDOPERATION             OF HV-OPERATIONS
      
          PERFORM SQL-OPEN-CURSOR-OP-PN

          PERFORM COPY-SQL-MSG-IN-LINKAGE

          IF SQL-STATUS-OK
          THEN
             PERFORM SQL-FETCH-CURSOR-OP-PN

             PERFORM COPY-SQL-MSG-IN-LINKAGE
             
             EVALUATE TRUE
             WHEN     SQL-STATUS-OK
                PERFORM COPY-HV-DATA-IN-LINKAGE
                MOVE "Next op selected."
                  TO LN-MSG-1          OF LN-MOD
                MOVE SPACES
                  TO LN-MSG-2          OF LN-MOD
         
             WHEN     SQL-STATUS-NOT-FOUND
                MOVE "No next op found."
                  TO LN-MSG-1          OF LN-MOD
                MOVE SPACES
                  TO LN-MSG-2          OF LN-MOD
         
             WHEN     OTHER
                CONTINUE
             END-EVALUATE
          END-IF      
          
      *>  always try to close the cursor, also in error cases    
          PERFORM SQL-CLOSE-CURSOR-OP-PN
      
          .
       PAGING-NEXT-EX.
          EXIT.

      *>------------------------------------------------------------------------
       PAGING-PREVIOUS SECTION.
      *>------------------------------------------------------------------------

           INITIALIZE HV-CLIENT
           INITIALIZE HV-COMPTE
           INITIALIZE HV-OPERATIONS
      *>  current value as restart point          
          MOVE LN-INP-IDOPERATION      OF LN-MOD 
            TO IDOPERATION             OF HV-OPERATIONS
      
          PERFORM SQL-OPEN-CURSOR-OP-PP

          PERFORM COPY-SQL-MSG-IN-LINKAGE

          IF SQL-STATUS-OK
          THEN
             PERFORM SQL-FETCH-CURSOR-OP-PP

             PERFORM COPY-SQL-MSG-IN-LINKAGE
             
             EVALUATE TRUE
             WHEN     SQL-STATUS-OK
                PERFORM COPY-HV-DATA-IN-LINKAGE
                MOVE "Previous op selected."
                  TO LN-MSG-1          OF LN-MOD
                MOVE SPACES
                  TO LN-MSG-2          OF LN-MOD
         
             WHEN     SQL-STATUS-NOT-FOUND
                MOVE "No previous op found."
                  TO LN-MSG-1          OF LN-MOD
                MOVE SPACES
                  TO LN-MSG-2          OF LN-MOD
         
             WHEN     OTHER
                CONTINUE
             END-EVALUATE
          END-IF      
          
      *>  always try to close the cursor, also in error cases    
          PERFORM SQL-CLOSE-CURSOR-OP-PP
      
          .
       PAGING-PREVIOUS-EX.
          EXIT.

      *>------------------------------------------------------------------------
       PAGING-LAST SECTION.
      *>------------------------------------------------------------------------
      
           INITIALIZE HV-CLIENT
           INITIALIZE HV-COMPTE
           INITIALIZE HV-OPERATIONS
      
          PERFORM SQL-OPEN-CURSOR-OP-PL

          PERFORM COPY-SQL-MSG-IN-LINKAGE

          IF SQL-STATUS-OK
          THEN
             PERFORM SQL-FETCH-CURSOR-OP-PL

             PERFORM COPY-SQL-MSG-IN-LINKAGE
             
             EVALUATE TRUE
             WHEN     SQL-STATUS-OK
                PERFORM COPY-HV-DATA-IN-LINKAGE
                MOVE "Last op selected."
                  TO LN-MSG-1          OF LN-MOD
                MOVE SPACES
                  TO LN-MSG-2          OF LN-MOD
         
             WHEN     SQL-STATUS-NOT-FOUND
                MOVE "No last op found."
                  TO LN-MSG-1          OF LN-MOD
                MOVE SPACES
                  TO LN-MSG-2          OF LN-MOD
         
             WHEN     OTHER
                CONTINUE
             END-EVALUATE
          END-IF      
          
      *>  always try to close the cursor, also in error cases    
          PERFORM SQL-CLOSE-CURSOR-OP-PL
      
          .
       PAGING-LAST-EX.
          EXIT.

      *>------------------------------------------------------------------------
       LIST-FIRST SECTION.
      *>------------------------------------------------------------------------

          INITIALIZE LN-OUTPUT
          INITIALIZE HV-CLIENT
          INITIALIZE HV-COMPTE

          PERFORM SQL-OPEN-CURSOR-CLIENT-LF

          PERFORM COPY-SQL-MSG-IN-LINKAGE

          IF SQL-STATUS-OK
          THEN
             PERFORM VARYING WS-IND-1 FROM 1 BY 1
               UNTIL WS-IND-1 > C-MAX-LINE-NR
          
                PERFORM SQL-FETCH-CURSOR-CLIENT-LF
      
                PERFORM COPY-SQL-MSG-IN-LINKAGE
                
                EVALUATE TRUE
                WHEN     SQL-STATUS-OK
                   MOVE WS-IND-1
                     TO LN-OUT-CLIENT-TAB-LINE-NR OF LN-MOD
                   
      *>           copy selected data in linkage
                   PERFORM COPY-LIST-IN-LINKAGE
                
                   MOVE "First client list selected."
                     TO LN-MSG-1       OF LN-MOD
                   MOVE SPACES
                     TO LN-MSG-2       OF LN-MOD
            
                WHEN     SQL-STATUS-NOT-FOUND
                   IF WS-IND-1 = 1
                   THEN
                      MOVE ZEROES
                        TO LN-OUT-CLIENT-TAB-LINE-NR OF LN-MOD
                        
                      MOVE "No first client list found."
                        TO LN-MSG-1    OF LN-MOD
                      MOVE SPACES
                        TO LN-MSG-2    OF LN-MOD
                   ELSE     
                      MOVE "First client list selected."
                        TO LN-MSG-1    OF LN-MOD
                      MOVE SPACES
                        TO LN-MSG-2    OF LN-MOD
                   END-IF  
                   EXIT PERFORM
            
                WHEN     OTHER
                   EXIT PERFORM
                END-EVALUATE
             END-PERFORM   
          END-IF      
          
      *>  always try to close the cursor, also in error cases    
          PERFORM SQL-CLOSE-CURSOR-CLIENT-LF
      
          .
       LIST-FIRST-EX.
          EXIT.

      *>------------------------------------------------------------------------
       LIST-NEXT SECTION.
      *>------------------------------------------------------------------------

          INITIALIZE LN-OUTPUT
          INITIALIZE HV-CLIENT
          INITIALIZE HV-COMPTE
          
      
      *>  current value as restart point          
          MOVE LN-INP-CLIENTID          OF LN-MOD 
            TO CLIENT-ID                 OF HV-CLIENT
          MOVE LN-INP-NOM               OF LN-MOD 
            TO CLIENT-NOM                      OF HV-CLIENT
          MOVE LN-INP-PRENOM            OF LN-MOD 
            TO CLIENT-PRENOM                     OF HV-CLIENT
          MOVE LN-INP-RAISON-SOCIALE     OF LN-MOD
            TO CLIENT-RSOCIALE            OF HV-CLIENT
          MOVE LN-INP-TYPECLIENT        OF LN-MOD
            TO CLIENT-TYPECLIENT               OF HV-CLIENT
      
          PERFORM SQL-OPEN-CURSOR-CLIENT-LN

          PERFORM COPY-SQL-MSG-IN-LINKAGE

          IF SQL-STATUS-OK
          THEN
             PERFORM VARYING WS-IND-1 FROM 1 BY 1
               UNTIL WS-IND-1 > C-MAX-LINE-NR
          
                PERFORM SQL-FETCH-CURSOR-CLIENT-LN
      
                PERFORM COPY-SQL-MSG-IN-LINKAGE
                
                EVALUATE TRUE
                WHEN     SQL-STATUS-OK
                   MOVE WS-IND-1
                     TO LN-OUT-CLIENT-TAB-LINE-NR OF LN-MOD
                   
      *>           copy selected data in linkage
                   PERFORM COPY-LIST-IN-LINKAGE
                
                   MOVE "Next client list selected."
                     TO LN-MSG-1       OF LN-MOD
                   MOVE SPACES
                     TO LN-MSG-2       OF LN-MOD
            
                WHEN     SQL-STATUS-NOT-FOUND
                   IF WS-IND-1 = 1
                   THEN
                      MOVE ZEROES
                        TO LN-OUT-CLIENT-TAB-LINE-NR OF LN-MOD
                        
                      MOVE "No next client list found."
                        TO LN-MSG-1    OF LN-MOD
                      MOVE SPACES
                        TO LN-MSG-2    OF LN-MOD
                   ELSE     
                      MOVE "Next client list selected."
                        TO LN-MSG-1    OF LN-MOD
                      MOVE SPACES
                        TO LN-MSG-2    OF LN-MOD
                   END-IF  
                   EXIT PERFORM
            
                WHEN     OTHER
                   EXIT PERFORM
                END-EVALUATE
             END-PERFORM   
          END-IF      
          
      *>  always try to close the cursor, also in error cases    
          PERFORM SQL-CLOSE-CURSOR-CLIENT-LN
      
          .
       LIST-NEXT-EX.
          EXIT.

      *>------------------------------------------------------------------------
       LIST-PREVIOUS SECTION.
      *>------------------------------------------------------------------------

          INITIALIZE LN-OUTPUT
          INITIALIZE HV-CLIENT
          INITIALIZE HV-COMPTE

      *>  current value as restart point          
           MOVE LN-INP-CLIENTID          OF LN-MOD 
              TO CLIENT-ID                 OF HV-CLIENT
            MOVE LN-INP-NOM               OF LN-MOD 
              TO CLIENT-NOM                      OF HV-CLIENT
            MOVE LN-INP-PRENOM            OF LN-MOD 
              TO CLIENT-PRENOM                     OF HV-CLIENT
            MOVE LN-INP-RAISON-SOCIALE     OF LN-MOD
              TO CLIENT-RSOCIALE            OF HV-CLIENT
            MOVE LN-INP-TYPECLIENT        OF LN-MOD
              TO CLIENT-TYPECLIENT               OF HV-CLIENT
          
          
          PERFORM SQL-OPEN-CURSOR-CLIENT-LP

          PERFORM COPY-SQL-MSG-IN-LINKAGE

          IF SQL-STATUS-OK
          THEN
             PERFORM VARYING WS-IND-1 FROM C-MAX-LINE-NR BY -1
               UNTIL WS-IND-1 < 1
          
                PERFORM SQL-FETCH-CURSOR-CLIENT-LP
      
                PERFORM COPY-SQL-MSG-IN-LINKAGE
                
                EVALUATE TRUE
                WHEN     SQL-STATUS-OK
                   MOVE C-MAX-LINE-NR
                     TO LN-OUT-CLIENT-TAB-LINE-NR OF LN-MOD
                   
      *>           copy selected data in linkage
                   PERFORM COPY-LIST-IN-LINKAGE
                
                   MOVE "Previous client list selected."
                     TO LN-MSG-1       OF LN-MOD
                   MOVE SPACES
                     TO LN-MSG-2       OF LN-MOD
            
                WHEN     SQL-STATUS-NOT-FOUND
                   IF WS-IND-1 = C-MAX-LINE-NR
                   THEN
                      MOVE ZEROES
                        TO LN-OUT-CLIENT-TAB-LINE-NR OF LN-MOD
                        
                      MOVE "No previous client list found."
                        TO LN-MSG-1    OF LN-MOD
                      MOVE SPACES
                        TO LN-MSG-2    OF LN-MOD
                   ELSE     
                      MOVE "Previous client list selected."
                        TO LN-MSG-1    OF LN-MOD
                      MOVE SPACES
                        TO LN-MSG-2    OF LN-MOD
                   END-IF  
                   EXIT PERFORM
            
                WHEN     OTHER
                   EXIT PERFORM
                END-EVALUATE
             END-PERFORM   
          END-IF      
          
      *>  always try to close the cursor, also in error cases    
          PERFORM SQL-CLOSE-CURSOR-CLIENT-LP
      
          .
       LIST-PREVIOUS-EX.
          EXIT.

      *>------------------------------------------------------------------------
       LIST-LAST SECTION.
      *>------------------------------------------------------------------------

          INITIALIZE LN-OUTPUT
          INITIALIZE HV-CLIENT
          INITIALIZE HV-COMPTE
        
      
          PERFORM SQL-OPEN-CURSOR-CLIENT-LL

          PERFORM COPY-SQL-MSG-IN-LINKAGE

          IF SQL-STATUS-OK
          THEN
             PERFORM VARYING WS-IND-1 FROM C-MAX-LINE-NR BY -1
               UNTIL WS-IND-1 < 1
          
                PERFORM SQL-FETCH-CURSOR-CLIENT-LL
      
                PERFORM COPY-SQL-MSG-IN-LINKAGE
                
                EVALUATE TRUE
                WHEN     SQL-STATUS-OK
                   MOVE C-MAX-LINE-NR
                     TO LN-OUT-CLIENT-TAB-LINE-NR OF LN-MOD
                   
      *>           copy selected data in linkage
                   PERFORM COPY-LIST-IN-LINKAGE
                
                   MOVE "Last client list selected."
                     TO LN-MSG-1       OF LN-MOD
                   MOVE SPACES
                     TO LN-MSG-2       OF LN-MOD
            
                WHEN     SQL-STATUS-NOT-FOUND
                   IF WS-IND-1 = C-MAX-LINE-NR
                   THEN
                      MOVE ZEROES
                        TO LN-OUT-CLIENT-TAB-LINE-NR OF LN-MOD
                        
                      MOVE "No last client list found."
                        TO LN-MSG-1    OF LN-MOD
                      MOVE SPACES
                        TO LN-MSG-2    OF LN-MOD
                   ELSE     
                      MOVE "Last client list selected."
                        TO LN-MSG-1    OF LN-MOD
                      MOVE SPACES
                        TO LN-MSG-2    OF LN-MOD
                   END-IF  
                   EXIT PERFORM
            
                WHEN     OTHER
                   EXIT PERFORM
                END-EVALUATE
             END-PERFORM   
          END-IF      
          
      *>  always try to close the cursor, also in error cases    
          PERFORM SQL-CLOSE-CURSOR-CLIENT-LL
      
          .
       LIST-LAST-EX.
          EXIT.
          
      *>------------------------------------------------------------------------
       COPY-SQL-MSG-IN-LINKAGE SECTION.
      *>------------------------------------------------------------------------

      *>  get SQL message with DB2 functions: sqlgintp, sqlggstt
          CALL "DB2SQLMSG" USING SQLCA
                                 LN-SQLMSG
          END-CALL
           
          MOVE SQLCODE         
            TO LN-SQLCODE              OF LN-MOD
          MOVE SQLSTATE       
            TO LN-SQLSTATE             OF LN-MOD
          MOVE LN-MSG-1                OF LN-SQLMSG         
            TO LN-MSG-1                OF LN-MOD
          MOVE LN-MSG-2                OF LN-SQLMSG         
            TO LN-MSG-2                OF LN-MOD
          MOVE LN-MSG-3                OF LN-SQLMSG         
            TO LN-MSG-3                OF LN-MOD
          MOVE LN-MSG-4                OF LN-SQLMSG         
            TO LN-MSG-4                OF LN-MOD
           
          .
       COPY-SQL-MSG-IN-LINKAGE-EX.
          EXIT.

      *>------------------------------------------------------------------------
       COPY-HV-DATA-IN-LINKAGE SECTION.
      *>------------------------------------------------------------------------


       *>  copy selected data in linkage
           MOVE CLIENT-ID                   OF  HV-CLIENT 
               TO LN-OUT-CLIENTID           OF  LN-MOD
           MOVE CLIENT-NOM                  OF  HV-CLIENT 
               TO LN-OUT-NOM                OF  LN-MOD
           MOVE CLIENT-PRENOM               OF  HV-CLIENT
               TO LN-OUT-PRENOM             OF  LN-MOD
           MOVE CLIENT-RSOCIALE             OF  HV-CLIENT
               TO LN-OUT-RAISON-SOCIALE           OF  LN-MOD
           MOVE CLIENT-TYPECLIENT           OF  HV-CLIENT
               TO LN-OUT-TYPECLIENT         OF  LN-MOD

           MOVE COMPTE-ID                 OF HV-COMPTE
             TO LN-OUT-COMPTE-ID           OF  LN-MOD
           MOVE COMPTE-SOLDE              OF HV-COMPTE
             TO LN-OUT-COMPTE-SOLDE           OF  LN-MOD
           MOVE CLIENT-ID2                OF HV-COMPTE
             TO LN-OUT-CLIENT-ID2           OF  LN-MOD

           MOVE IDOPERATION               OF HV-OPERATIONS
             TO LN-OUT-IDOPERATION           OF  LN-MOD
           MOVE LIBELLE-OP                OF HV-OPERATIONS
             TO LN-OUT-LIBELLE-OP           OF  LN-MOD
           MOVE MONTANT-OP                OF HV-OPERATIONS
             TO LN-OUT-MONTANT-OP           OF  LN-MOD
           MOVE DATE-OP                   OF HV-OPERATIONS
             TO LN-OUT-DATE-OP           OF  LN-MOD
       
          .
       COPY-HV-DATA-IN-LINKAGE-EX.
          EXIT.

      *>------------------------------------------------------------------------
       COPY-LIST-IN-LINKAGE SECTION.
      *>------------------------------------------------------------------------

      *>  copy selected data in linkage
          INITIALIZE LN-OUT-CLIENT-TAB-LINE(WS-IND-1)
          MOVE CLIENT-NOM                 OF HV-CLIENT
            TO LN-OUT-CLIENT-TAB-NOM(WS-IND-1)
          MOVE CLIENT-PRENOM              OF HV-CLIENT
            TO LN-OUT-CLIENT-TAB-PRENOM(WS-IND-1)
          MOVE CLIENT-ID            OF HV-CLIENT
            TO LN-OUT-CLIENT-TAB-IDCLIENT(WS-IND-1)
          MOVE CLIENT-RSOCIALE       OF HV-CLIENT
           TO LN-OUT-CLIENT-TAB-RSOCIALE(WS-IND-1)
          MOVE CLIENT-TYPECLIENT         OF HV-CLIENT
           TO LN-OUT-CLIENT-TAB-TYPECLIENT(WS-IND-1)
          MOVE CLIENT-EXTERNE            OF HV-CLIENT
           TO LN-OUT-CLIENT-TAB-EXTERNE(WS-IND-1)
      
          .
       COPY-LIST-IN-LINKAGE-EX.
          EXIT.
          

      *>------------------------------------------------------------------------
       SQL-CONNECT SECTION.
      *>------------------------------------------------------------------------
          
      *EXEC SQL CONNECT TO    :HV-DBALIAS 
      *                 USER  :HV-USERID
      *                 USING :HV-PSWD
      *    END-EXEC
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

           MOVE 9 TO SQL-HOST-VAR-LENGTH
           MOVE 448 TO SQL-DATA-TYPE
           MOVE 0 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE HV-DBALIAS
            BY VALUE 0
                     0

           MOVE 20 TO SQL-HOST-VAR-LENGTH
           MOVE 448 TO SQL-DATA-TYPE
           MOVE 1 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE HV-USERID
            BY VALUE 0
                     0

           MOVE 20 TO SQL-HOST-VAR-LENGTH
           MOVE 448 TO SQL-DATA-TYPE
           MOVE 2 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE HV-PSWD
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
                  
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-CONNECT-EX.
          EXIT.
      



      *>------------------------------------------------------------------------
       SQL-COMMIT SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL COMMIT
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 0 TO SQL-SECTIONUMBER 
           MOVE 21 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  

          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-COMMIT-EX.
          EXIT.

      *>------------------------------------------------------------------------
       SQL-ROLLBACK SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL ROLLBACK
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 0 TO SQL-SECTIONUMBER 
           MOVE 28 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  

          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-ROLLBACK-EX.
          EXIT.
          
      *>------------------------------------------------------------------------
       SQL-SELECT-CLIENT SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL SELECT IDCLIENT                
      *              , NOM              
      *              , PRENOM               
      *              , RAISON_SOCIALE            
      *              , TYPECLIENT                
      *                              
      *         INTO   :HV-CLIENT.CLIENT-ID   
      *              , :HV-CLIENT.CLIENT-NOM  
      *              , :HV-CLIENT.CLIENT-PRENOM               
      *              , :HV-CLIENT.CLIENT-RSOCIALE            
      *              , :HV-CLIENT.CLIENT-TYPECLIENT                              
      *         FROM   CLIENTS
      *         WHERE  IDCLIENT = :HV-CLIENT.CLIENT-ID
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 2 TO SQL-STMT-ID 
           MOVE 1 TO SQLDSIZE 
           MOVE 2 TO SQLDA-ID 

           CALL "sqlgaloc" USING
               BY VALUE SQLDA-ID 
                        SQLDSIZE
                        SQL-STMT-ID
                        0

           MOVE 3 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 0 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-ID
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 3 TO SQL-STMT-ID 
           MOVE 5 TO SQLDSIZE 
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
            BY REFERENCE CLIENT-ID
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 1 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-NOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 2 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-PRENOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 3 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-RSOCIALE
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 4 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-TYPECLIENT
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 3 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 2 TO SQL-INPUT-SQLDA-ID 
           MOVE 9 TO SQL-SECTIONUMBER 
           MOVE 24 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  

          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-SELECT-CLIENT-EX.
          EXIT.

      *>------------------------------------------------------------------------
       SQL-INSERT-CLIENT SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL INSERT INTO CLIENTS
      *         (  NOM        
      *          , PRENOM              
      *          , RAISON_SOCIALE               
      *          , TYPECLIENT            
      *          , EXTERNE                               
      *         )
      *         VALUES
      *         (  :HV-CLIENT.CLIENT-NOM
      *          , :HV-CLIENT.CLIENT-PRENOM  
      *          , :HV-CLIENT.CLIENT-RSOCIALE               
      *          , :HV-CLIENT.CLIENT-TYPECLIENT            
      *          , :HV-CLIENT.CLIENT-EXTERNE                           
      *         )
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 4 TO SQL-STMT-ID 
           MOVE 5 TO SQLDSIZE 
           MOVE 2 TO SQLDA-ID 

           CALL "sqlgaloc" USING
               BY VALUE SQLDA-ID 
                        SQLDSIZE
                        SQL-STMT-ID
                        0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 0 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-NOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 1 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-PRENOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 2 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-RSOCIALE
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 3 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-TYPECLIENT
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 1 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 4 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-EXTERNE
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 2 TO SQL-INPUT-SQLDA-ID 
           MOVE 10 TO SQL-SECTIONUMBER 
           MOVE 24 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  

          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-INSERT-CLIENT-EX.
          EXIT.





      *>------------------------------------------------------------------------
       SQL-OPEN-CURSOR-OP-PF SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL OPEN CURSOR_OP_PF
      *    END-EXEC
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
                  
          
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-OPEN-CURSOR-OP-PF-EX.
          EXIT.
          
      *>------------------------------------------------------------------------
       SQL-OPEN-CURSOR-OP-PN SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL OPEN CURSOR_OP_PN
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 5 TO SQL-STMT-ID 
           MOVE 1 TO SQLDSIZE 
           MOVE 2 TO SQLDA-ID 

           CALL "sqlgaloc" USING
               BY VALUE SQLDA-ID 
                        SQLDSIZE
                        SQL-STMT-ID
                        0

           MOVE 3 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 0 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE IDOPERATION
            OF
            HV-OPERATIONS
            BY VALUE 0
                     0

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 2 TO SQL-INPUT-SQLDA-ID 
           MOVE 2 TO SQL-SECTIONUMBER 
           MOVE 26 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  
          
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-OPEN-CURSOR-OP-PN-EX.
          EXIT.

      *>------------------------------------------------------------------------
       SQL-OPEN-CURSOR-OP-PP SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL OPEN CURSOR_OP_PP
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 6 TO SQL-STMT-ID 
           MOVE 1 TO SQLDSIZE 
           MOVE 2 TO SQLDA-ID 

           CALL "sqlgaloc" USING
               BY VALUE SQLDA-ID 
                        SQLDSIZE
                        SQL-STMT-ID
                        0

           MOVE 3 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 0 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE IDOPERATION
            OF
            HV-OPERATIONS
            BY VALUE 0
                     0

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 2 TO SQL-INPUT-SQLDA-ID 
           MOVE 3 TO SQL-SECTIONUMBER 
           MOVE 26 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  
          
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-OPEN-CURSOR-OP-PP-EX.
          EXIT.
          
      *>------------------------------------------------------------------------
       SQL-OPEN-CURSOR-OP-PL SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL OPEN CURSOR_OP_PL
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 4 TO SQL-SECTIONUMBER 
           MOVE 26 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  
          
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-OPEN-CURSOR-OP-PL-EX.
          EXIT.

      *>------------------------------------------------------------------------
       SQL-OPEN-CURSOR-CLIENT-LF SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL OPEN CURSOR_CLIENT_LF
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 5 TO SQL-SECTIONUMBER 
           MOVE 26 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  
          
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-OPEN-CURSOR-CLIENT-LF-EX.
          EXIT.

      *>------------------------------------------------------------------------
       SQL-OPEN-CURSOR-CLIENT-LN SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL OPEN CURSOR_CLIENT_LN
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 7 TO SQL-STMT-ID 
           MOVE 2 TO SQLDSIZE 
           MOVE 2 TO SQLDA-ID 

           CALL "sqlgaloc" USING
               BY VALUE SQLDA-ID 
                        SQLDSIZE
                        SQL-STMT-ID
                        0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 0 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-NOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 1 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-PRENOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 2 TO SQL-INPUT-SQLDA-ID 
           MOVE 6 TO SQL-SECTIONUMBER 
           MOVE 26 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  
          
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-OPEN-CURSOR-CLIENT-LN-EX.
          EXIT.

      *>------------------------------------------------------------------------
       SQL-OPEN-CURSOR-CLIENT-LP SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL OPEN CURSOR_CLIENT_LP
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 8 TO SQL-STMT-ID 
           MOVE 2 TO SQLDSIZE 
           MOVE 2 TO SQLDA-ID 

           CALL "sqlgaloc" USING
               BY VALUE SQLDA-ID 
                        SQLDSIZE
                        SQL-STMT-ID
                        0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 0 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-NOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 1 TO SQLVAR-INDEX
           MOVE 2 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-PRENOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 2 TO SQL-INPUT-SQLDA-ID 
           MOVE 7 TO SQL-SECTIONUMBER 
           MOVE 26 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  
          
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-OPEN-CURSOR-CLIENT-LP-EX.
          EXIT.
          
      *>------------------------------------------------------------------------
       SQL-OPEN-CURSOR-CLIENT-LL SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL OPEN CURSOR_CLIENT_LL
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 8 TO SQL-SECTIONUMBER 
           MOVE 26 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  
          
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-OPEN-CURSOR-CLIENT-LL-EX.
          EXIT.
          
      *>------------------------------------------------------------------------
       SQL-FETCH-CURSOR-OP-PF SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL FETCH CURSOR_OP_PF
      *         INTO      :Hv-CLIENT.CLIENT-ID
      *                 ,:HV-CLIENT.CLIENT-NOM
      *                 ,:HV-CLIENT.CLIENT-PRENOM
      *                 ,:HV-CLIENT.CLIENT-RSOCIALE
      *                 ,:HV-CLIENT.CLIENT-TYPECLIENT
      *                 ,:HV-COMPTE.COMPTE-ID
      *                 ,:HV-COMPTE.COMPTE-SOLDE
      *                 ,:HV-COMPTE.CLIENT-ID2
      *                 ,:HV-OPERATIONS.IDOPERATION
      *                 ,:HV-OPERATIONS.LIBELLE-OP
      *                 ,:HV-OPERATIONS.MONTANT-OP
      *                 ,:HV-OPERATIONS.DATE-OP
      *                                                                          
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 9 TO SQL-STMT-ID 
           MOVE 12 TO SQLDSIZE 
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
            BY REFERENCE CLIENT-ID
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 1 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-NOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 2 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-PRENOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 3 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-RSOCIALE
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 4 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-TYPECLIENT
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 3 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 5 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE COMPTE-ID
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 524 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 6 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE COMPTE-SOLDE
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 3 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 7 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-ID2
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 3 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 8 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE IDOPERATION
            OF
            HV-OPERATIONS
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 9 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE LIBELLE-OP
            OF
            HV-OPERATIONS
            BY VALUE 0
                     0

           MOVE 522 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 10 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE MONTANT-OP
            OF
            HV-OPERATIONS
            BY VALUE 0
                     0

           MOVE 10 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 11 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE DATE-OP
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
                  
          
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-FETCH-CURSOR-OP-PF-EX.
          EXIT.
          
      *>------------------------------------------------------------------------
       SQL-FETCH-CURSOR-OP-PN SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL FETCH CURSOR_OP_PN
      *         INTO  :Hv-CLIENT.CLIENT-ID
      *              ,:HV-CLIENT.CLIENT-NOM
      *              ,:HV-CLIENT.CLIENT-PRENOM
      *              ,:HV-CLIENT.CLIENT-RSOCIALE
      *              ,:HV-CLIENT.CLIENT-TYPECLIENT
      *              ,:HV-COMPTE.COMPTE-ID
      *              ,:HV-COMPTE.COMPTE-SOLDE
      *              ,:HV-COMPTE.CLIENT-ID2
      *              ,:HV-OPERATIONS.IDOPERATION
      *              ,:HV-OPERATIONS.LIBELLE-OP
      *              ,:HV-OPERATIONS.MONTANT-OP
      *              ,:HV-OPERATIONS.DATE-OP
      *            
      *                                                                          
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 10 TO SQL-STMT-ID 
           MOVE 12 TO SQLDSIZE 
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
            BY REFERENCE CLIENT-ID
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 1 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-NOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 2 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-PRENOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 3 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-RSOCIALE
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 4 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-TYPECLIENT
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 3 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 5 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE COMPTE-ID
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 524 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 6 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE COMPTE-SOLDE
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 3 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 7 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-ID2
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 3 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 8 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE IDOPERATION
            OF
            HV-OPERATIONS
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 9 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE LIBELLE-OP
            OF
            HV-OPERATIONS
            BY VALUE 0
                     0

           MOVE 522 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 10 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE MONTANT-OP
            OF
            HV-OPERATIONS
            BY VALUE 0
                     0

           MOVE 10 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 11 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE DATE-OP
            OF
            HV-OPERATIONS
            BY VALUE 0
                     0

           MOVE 3 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 2 TO SQL-SECTIONUMBER 
           MOVE 25 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  
          
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-FETCH-CURSOR-OP-PN-EX.
          EXIT.
          
      *>------------------------------------------------------------------------
       SQL-FETCH-CURSOR-OP-PP SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL FETCH CURSOR_OP_PP
      *         INTO   :Hv-CLIENT.CLIENT-ID
      *               ,:HV-CLIENT.CLIENT-NOM
      *               ,:HV-CLIENT.CLIENT-PRENOM
      *               ,:HV-CLIENT.CLIENT-RSOCIALE
      *               ,:HV-CLIENT.CLIENT-TYPECLIENT
      *               ,:HV-COMPTE.COMPTE-ID
      *               ,:HV-COMPTE.COMPTE-SOLDE
      *               ,:HV-COMPTE.CLIENT-ID2
      *               ,:HV-OPERATIONS.IDOPERATION
      *               ,:HV-OPERATIONS.LIBELLE-OP
      *               ,:HV-OPERATIONS.MONTANT-OP
      *               ,:HV-OPERATIONS.DATE-OP
      *
      *                                                                          
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 11 TO SQL-STMT-ID 
           MOVE 12 TO SQLDSIZE 
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
            BY REFERENCE CLIENT-ID
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 1 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-NOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 2 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-PRENOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 3 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-RSOCIALE
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 4 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-TYPECLIENT
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 3 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 5 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE COMPTE-ID
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 524 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 6 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE COMPTE-SOLDE
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 3 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 7 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-ID2
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 3 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 8 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE IDOPERATION
            OF
            HV-OPERATIONS
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 9 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE LIBELLE-OP
            OF
            HV-OPERATIONS
            BY VALUE 0
                     0

           MOVE 522 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 10 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE MONTANT-OP
            OF
            HV-OPERATIONS
            BY VALUE 0
                     0

           MOVE 10 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 11 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE DATE-OP
            OF
            HV-OPERATIONS
            BY VALUE 0
                     0

           MOVE 3 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 3 TO SQL-SECTIONUMBER 
           MOVE 25 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  
          
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-FETCH-CURSOR-OP-PP-EX.
          EXIT.
          
      *>------------------------------------------------------------------------
       SQL-FETCH-CURSOR-OP-PL SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL FETCH CURSOR_OP_PL
      *         INTO  :Hv-CLIENT.CLIENT-ID
      *              ,:HV-CLIENT.CLIENT-NOM
      *              ,:HV-CLIENT.CLIENT-PRENOM
      *              ,:HV-CLIENT.CLIENT-RSOCIALE
      *              ,:HV-CLIENT.CLIENT-TYPECLIENT
      *              ,:HV-COMPTE.COMPTE-ID
      *              ,:HV-COMPTE.COMPTE-SOLDE
      *              ,:HV-COMPTE.CLIENT-ID2
      *              ,:HV-OPERATIONS.IDOPERATION
      *              ,:HV-OPERATIONS.LIBELLE-OP
      *              ,:HV-OPERATIONS.MONTANT-OP
      *              ,:HV-OPERATIONS.DATE-OP                                 
      *              
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 12 TO SQL-STMT-ID 
           MOVE 12 TO SQLDSIZE 
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
            BY REFERENCE CLIENT-ID
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 1 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-NOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 2 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-PRENOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 3 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-RSOCIALE
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 4 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-TYPECLIENT
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 3 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 5 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE COMPTE-ID
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 524 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 6 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE COMPTE-SOLDE
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 3 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 7 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-ID2
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 3 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 8 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE IDOPERATION
            OF
            HV-OPERATIONS
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 9 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE LIBELLE-OP
            OF
            HV-OPERATIONS
            BY VALUE 0
                     0

           MOVE 522 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 10 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE MONTANT-OP
            OF
            HV-OPERATIONS
            BY VALUE 0
                     0

           MOVE 10 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 11 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE DATE-OP
            OF
            HV-OPERATIONS
            BY VALUE 0
                     0

           MOVE 3 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 4 TO SQL-SECTIONUMBER 
           MOVE 25 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  
          
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-FETCH-CURSOR-OP-PL-EX.
          EXIT.

      *>------------------------------------------------------------------------
       SQL-FETCH-CURSOR-CLIENT-LF SECTION.
      *>------------------------------------------------------------------------


          
      *EXEC SQL FETCH CURSOR_CLIENT_LF
      *         INTO  :Hv-CLIENT.CLIENT-ID
      *               ,:HV-CLIENT.CLIENT-NOM
      *               ,:HV-CLIENT.CLIENT-PRENOM
      *               ,:HV-CLIENT.CLIENT-RSOCIALE
      *               ,:HV-CLIENT.CLIENT-TYPECLIENT
      *               ,:Hv-COMPTE.COMPTE-ID
      *               ,:HV-COMPTE.COMPTE-SOLDE
      *                      
      *               
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 13 TO SQL-STMT-ID 
           MOVE 7 TO SQLDSIZE 
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
            BY REFERENCE CLIENT-ID
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 1 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-NOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 2 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-PRENOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 3 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-RSOCIALE
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 4 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-TYPECLIENT
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 3 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 5 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE COMPTE-ID
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 524 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 6 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE COMPTE-SOLDE
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 3 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 5 TO SQL-SECTIONUMBER 
           MOVE 25 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  
          
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-FETCH-CURSOR-CLIENT-LF-EX.
          EXIT.

      *>------------------------------------------------------------------------
       SQL-FETCH-CURSOR-CLIENT-LN SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL FETCH CURSOR_CLIENT_LN
      *         INTO   :Hv-CLIENT.CLIENT-ID
      *               ,:HV-CLIENT.CLIENT-NOM
      *               ,:HV-CLIENT.CLIENT-PRENOM
      *               ,:HV-CLIENT.CLIENT-RSOCIALE
      *               ,:HV-CLIENT.CLIENT-TYPECLIENT
      *               ,:Hv-COMPTE.COMPTE-ID
      *               ,:HV-COMPTE.COMPTE-SOLDE
      *                              
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 14 TO SQL-STMT-ID 
           MOVE 7 TO SQLDSIZE 
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
            BY REFERENCE CLIENT-ID
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 1 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-NOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 2 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-PRENOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 3 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-RSOCIALE
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 4 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-TYPECLIENT
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 3 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 5 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE COMPTE-ID
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 524 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 6 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE COMPTE-SOLDE
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 3 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 6 TO SQL-SECTIONUMBER 
           MOVE 25 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  
          
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-FETCH-CURSOR-CLIENT-LN-EX.
          EXIT.

      *>------------------------------------------------------------------------
       SQL-FETCH-CURSOR-CLIENT-LP SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL FETCH CURSOR_CLIENT_LP
      *         INTO   :Hv-CLIENT.CLIENT-ID
      *                ,:HV-CLIENT.CLIENT-NOM
      *                ,:HV-CLIENT.CLIENT-PRENOM
      *                ,:HV-CLIENT.CLIENT-RSOCIALE
      *                ,:HV-CLIENT.CLIENT-TYPECLIENT
      *                ,:Hv-COMPTE.COMPTE-ID
      *                ,:HV-COMPTE.COMPTE-SOLDE
      *                                                
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 15 TO SQL-STMT-ID 
           MOVE 7 TO SQLDSIZE 
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
            BY REFERENCE CLIENT-ID
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 1 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-NOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 2 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-PRENOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 3 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-RSOCIALE
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 4 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-TYPECLIENT
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 3 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 5 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE COMPTE-ID
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 524 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 6 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE COMPTE-SOLDE
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 3 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 7 TO SQL-SECTIONUMBER 
           MOVE 25 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  
          
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-FETCH-CURSOR-CLIENT-LP-EX.
          EXIT.
          
      *>------------------------------------------------------------------------
       SQL-FETCH-CURSOR-CLIENT-LL SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL FETCH CURSOR_CLIENT_LL
      *         INTO   :Hv-CLIENT.CLIENT-ID
      *                ,:HV-CLIENT.CLIENT-NOM
      *                ,:HV-CLIENT.CLIENT-PRENOM
      *                ,:HV-CLIENT.CLIENT-RSOCIALE
      *                ,:HV-CLIENT.CLIENT-TYPECLIENT
      *                ,:Hv-COMPTE.COMPTE-ID
      *                ,:HV-COMPTE.COMPTE-SOLDE
      *                        
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 16 TO SQL-STMT-ID 
           MOVE 7 TO SQLDSIZE 
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
            BY REFERENCE CLIENT-ID
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 1 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-NOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 2 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-PRENOM
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 3 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-RSOCIALE
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 50 TO SQL-HOST-VAR-LENGTH
           MOVE 452 TO SQL-DATA-TYPE
           MOVE 4 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE CLIENT-TYPECLIENT
            OF
            HV-CLIENT
            BY VALUE 0
                     0

           MOVE 3 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 5 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE COMPTE-ID
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 524 TO SQL-HOST-VAR-LENGTH
           MOVE 484 TO SQL-DATA-TYPE
           MOVE 6 TO SQLVAR-INDEX
           MOVE 3 TO SQLDA-ID

           CALL "sqlgstlv" USING 
            BY VALUE SQLDA-ID
                     SQLVAR-INDEX
                     SQL-DATA-TYPE
                     SQL-HOST-VAR-LENGTH
            BY REFERENCE COMPTE-SOLDE
            OF
            HV-COMPTE
            BY VALUE 0
                     0

           MOVE 3 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 8 TO SQL-SECTIONUMBER 
           MOVE 25 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  
          
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-FETCH-CURSOR-CLIENT-LL-EX.
          EXIT.
          
      *>------------------------------------------------------------------------
       SQL-CLOSE-CURSOR-OP-PF SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL CLOSE CURSOR_OP_PF
      *    END-EXEC
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
                  
          
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-CLOSE-CURSOR-OP-PF-EX.
          EXIT.
          
      *>------------------------------------------------------------------------
       SQL-CLOSE-CURSOR-OP-PN SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL CLOSE CURSOR_OP_PN
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 2 TO SQL-SECTIONUMBER 
           MOVE 20 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  
          
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-CLOSE-CURSOR-OP-PN-EX.
          EXIT.
          
      *>------------------------------------------------------------------------
       SQL-CLOSE-CURSOR-OP-PP SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL CLOSE CURSOR_OP_PP
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 3 TO SQL-SECTIONUMBER 
           MOVE 20 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  
          
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-CLOSE-CURSOR-OP-PP-EX.
          EXIT.
          
      *>------------------------------------------------------------------------
       SQL-CLOSE-CURSOR-OP-PL SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL CLOSE CURSOR_OP_PL
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 4 TO SQL-SECTIONUMBER 
           MOVE 20 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  
          
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-CLOSE-CURSOR-OP-PL-EX.
          EXIT.
      
      *>------------------------------------------------------------------------
       SQL-CLOSE-CURSOR-CLIENT-LF SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL CLOSE CURSOR_CLIENT_LF
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 5 TO SQL-SECTIONUMBER 
           MOVE 20 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  
          
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-CLOSE-CURSOR-CLIENT-LF-EX.
          EXIT.

      *>------------------------------------------------------------------------
       SQL-CLOSE-CURSOR-CLIENT-LN SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL CLOSE CURSOR_CLIENT_LN
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 6 TO SQL-SECTIONUMBER 
           MOVE 20 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  
          
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-CLOSE-CURSOR-CLIENT-LN-EX.
          EXIT.

      *>------------------------------------------------------------------------
       SQL-CLOSE-CURSOR-CLIENT-LP SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL CLOSE CURSOR_CLIENT_LP
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 7 TO SQL-SECTIONUMBER 
           MOVE 20 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  
          
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-CLOSE-CURSOR-CLIENT-LP-EX.
          EXIT.
          
      *>------------------------------------------------------------------------
       SQL-CLOSE-CURSOR-CLIENT-LL SECTION.
      *>------------------------------------------------------------------------

          
      *EXEC SQL CLOSE CURSOR_CLIENT_LL
      *    END-EXEC
           CALL "sqlgstrt" USING
              BY CONTENT SQLA-PROGRAM-ID
              BY VALUE 0
              BY REFERENCE SQLCA

           MOVE 0 TO SQL-OUTPUT-SQLDA-ID 
           MOVE 0 TO SQL-INPUT-SQLDA-ID 
           MOVE 8 TO SQL-SECTIONUMBER 
           MOVE 20 TO SQL-CALL-TYPE 

           CALL "sqlgcall" USING
            BY VALUE SQL-CALL-TYPE 
                     SQL-SECTIONUMBER
                     SQL-INPUT-SQLDA-ID
                     SQL-OUTPUT-SQLDA-ID
                     0

           CALL "sqlgstop" USING
            BY VALUE 0
                  
          
          MOVE SQLCODE TO WS-SQL-STATUS
          
          .
       SQL-CLOSE-CURSOR-CLIENT-LL-EX.
          EXIT.
          
       END PROGRAM modactivity2.
       