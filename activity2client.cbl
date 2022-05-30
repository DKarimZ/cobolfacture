       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACTIVITY2CLIENT.
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
          CRT STATUS IS WS-FNC-KEY.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
             SELECT PRINT-RELEVE ASSIGN TO "PRINT-RELEVE.DAT"
                 ORGANIZATION IS SEQUENTIAL.
             SELECT PRINT-FILE ASSIGN TO "PRINTFILE.DAT"
                 ORGANIZATION IS LINE SEQUENTIAL.
             SELECT GENERESQL  ASSIGN TO "GENERESQL.SQL"
                ORGANIZATION IS LINE SEQUENTIAL.
             SELECT GENERUPDATE ASSIGN TO "GENERUPDATE.SQL"
             ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
          FD PRINT-RELEVE.
              01 RELEVE-OP      PIC X(38).
          FD GENERESQL.
              01 PRINT-LINE     PIC X(200).
          FD GENERUPDATE.
              01 PRINT-UP-LINE  PIC X(200).
          FD PRINT-FILE.
              01 DETAILS-LINE.
               88 ENDOffiLE                 VALUE HIGH-VALUES.
               05 D-CLIENT-ID               PIC S9(3).
               05 D-CLIENT-NOM              PIC X(20).
               05 D-CLIENT-PRENOM           PIC X(20).
               05 D-CLIENT-RSOCIALE         PIC X(20).
               05 D-CLIENT-TYPECLIENT       PIC X(20).
               05 D-CLIENT-EXTERNE          PIC 9.
               05 D-COMPTE-ID               PIC S9(3).
               05 D-COMPTE-IBAN             PIC X(25).
               05 D-COMPTE-DTOUV            PIC X(14).
               05 D-COMPTE-SOLDE            PIC 9(5).99.
               05 FILLER                    PIC X(3).
               05 D-COMPTE-CLIENTID         PIC S9(3).

       WORKING-STORAGE SECTION.
       01 WS-FNC-KEY                   PIC 9(4).
          88 V-FNC-F1                  VALUE 1001.
          88 V-FNC-F2                  VALUE 1002.
          88 V-FNC-F3                  VALUE 1003.
          88 V-FNC-F4                  VALUE 1004.
          88 V-FNC-F5                  VALUE 1005.
          88 V-FNC-F6                  VALUE 1006.
          88 V-FNC-F7                  VALUE 1007.
          88 V-FNC-F8                  VALUE 1008.
          88 V-FNC-F9                  VALUE 1009.
          88 V-FNC-F10                 VALUE 1010.
       01 WS-ACCEPT-FNC-KEY            PIC X.

       01 WS-MSG.
         05 WS-SQLCODE                 PIC S9(10).
         05 WS-SQLSTATE                PIC X(5).
         05 WS-MSG-1                   PIC X(80).
         05 WS-MSG-2                   PIC X(80).
         05 WS-MSG-3                   PIC X(80).
         05 WS-MSG-4                   PIC X(80).

       01 WS-CONNECT.
         02 WS-DBALIAS                 PIC X(9).
         02 WS-USERID                  PIC X(20).
         02 WS-PSWD                    PIC X(20).

       01 WS-INP-CLIENT.  
         05 WS-INP-CLIENTID           PIC 9(3). 
         05 WS-INP-NOM                PIC X(50).
         05 WS-INP-PRENOM             PIC X(50).
         05 WS-INP-RAISONSOCIALE      PIC X(50).
         05 WS-INP-TYPECLIENT         PIC X(50).
         05 WS-INP-EXTERNE            PIC 9(1).

       01 WS-INP-COMPTE.  
         05 WS-INP-COMPTEID            PIC 9(3). 
         05 WS-INP-IBAN                PIC X(50).
         05 WS-INP-DATEOUV             PIC X(10).
         05 WS-INP-SOLDE               PIC 9(10)V99.
         05 WS-INP-IDCLIENT            PIC 9(3).
         
       01 WS-INP-OPERATIONS.  
         05 WS-INP-IDOPERATION         PIC 9(3). 
         05 WS-INP-TYPE                PIC X(50).
         05 WS-INP-LIBELLE             PIC X(50).
         05 WS-INP-MONTANT             PIC 9(8)V99.
         05 WS-INP-COMPTEID            PIC 9(3).
         05 WS-INP-IDCLIENT            PIC 9(3).
         05 WS-INP-DATEOP              PIC X(10).
         05 WS-INP-STATUS              PIC X(20).

       01 WS-OUT-CLIENT.  
         05 WS-OUT-CLIENTID           PIC 9(3). 
         05 WS-OUT-NOM                PIC X(50).
         05 WS-OUT-PRENOM             PIC X(50).
         05 WS-OUT-RAISONSOCIALE      PIC X(50).
         05 WS-OUT-TYPECLIENT         PIC X(50).
         05 WS-OUT-EXTERNE            PIC 9(1).

       01 WS-OUT-COMPTE.  
         05 WS-OUT-COMPTEID            PIC 9(3). 
         05 WS-OUT-IBAN                PIC X(50).
         05 WS-OUT-DATEOUV             PIC X(10).
         05 WS-OUT-SOLDE               PIC 9(10)V99.
         05 WS-OUT-IDCLIENT            PIC 9(3).
         
       01 WS-OUT-OPERATIONS.  
         05 WS-OUT-IDOPERATION         PIC 9(3). 
         05 WS-OUT-TYPE                PIC X(50).
         05 WS-OUT-LIBELLE             PIC X(50).
         05 WS-OUT-MONTANT             PIC 9(8)V99.
         05 WS-OUT-COMPTEID2            PIC 9(3).
         05 WS-OUT-IDCLIENT            PIC 9(3).
         05 WS-OUT-DATEOP              PIC X(10).
        



      *> max number of lines for the list screen   
       78 C-MAX-LINE-NR                VALUE 10.  
      *> line data for the list screen   
       01 WS-OUT-CLIENT-TABLE.
         02 WS-OUT-CLIENT-TAB-LINE-NR    PIC 9(2).
         02 WS-OUT-CLIENT-TAB OCCURS C-MAX-LINE-NR TIMES. 
           03 WS-OUT-CLIENT-TAB-LINE.
             04 WS-OUT-CLIENT-TAB-NOM         PIC X(50).
             04 WS-OUT-CLIENT-TAB-PRENOM      PIC X(50).
             04 WS-OUT-CLIENT-TAB-RSOCIALE    PIC X(50).
             04 WS-OUT-CLIENT-TAB-TYPECLIENT  PIC X(50).
             04 WS-OUT-CLIENT-TAB-EXTERNE     PIC 9(1).
             04 WS-OUT-CLIENT-TAB-IDCLIENT    PIC 9(3).

      *> line number var for the list screen   
       01 SC-LINE-NR                   PIC 99.  

      *> indices for cycles
       01 WS-IND-1                     PIC S9(4) COMP.
       

       01 Ws-FIELDS.
             05 RESULTATCRE    PIC 9(5).99.
             05 RESULTATDEB    PIC 9(5).99.
             05 SOLDE          PIC 9(5).99.
             05 CREDIT         PIC 9(5).99.
             05 DEBIT          PIC 9(5).99.
         01 WS-VALUES.
             05 MONSOLDE        PIC 9(5).99.
             05 voiture         PIC 9(5) VALUE 30000.
             05 Moto            PIC 9(5) VALUE 15000.
             05 velo            PIC 9(5) VALUE 500.
             05 MSG-CREDIT      PIC X(30).
             05 MSG-SOMME       PIC 9(5).99.
             05 WSMSG           PIC X(50).
             05 LINE-NUMBER     PIC 99.
             05 SOMMECREDITEE   PIC 9(5).99.
             05 SOMMEDEPOSEE    PIC 9(5).
             05 NEWSOLDE        PIC 9(5).99.
             05 SOMMEEPARGNEE   PIC 9(5).
             05 SOMMESFR        PIC 9(5).99.
             05 LIBELLEVIREM    PIC X(10) JUST RIGHT.
             05 DATEVIREM       PIC X(10).
             05 DATEPRELEVSFR   PIC X(10).
             05 REPONSESFR      PIC X(3).
             05 PRODUITACHETEE  PIC X(15).
             05 PRIXPRODUIT     PIC 9(5).
             05 NOMMAGASIN      PIC X(15).
             05 C-LIBELLE       PIC X(35).
             05 LE-LIBELLE       PIC X(20).
             05 C-SUCCESS       PIC X(3).
             05 C-DATE          PIC X(10).
             05 LA-DATE          PIC X(10).
             05 C-TYPEOP        PIC X(25).
             05 C-MONTANT       PIC 9(5).9(2). 
             05 C-SOMME         PIC X(10). 
             05 C-COMM          PIC 9V9(2). 
             05 C-FRAGFINANC    PIC X(3).
             05 C-PLAFOND       PIC 9.
             05 SOMMEOPP        PIC 9(3)B9(3)B9(4). 
             05 ENTREEAR        PIC 9(5).
             05 WS-DATE2        PIC X(10).
             77 ptr             PIC 99.
             
           01 mouse-flags   PIC 9(4).
            01 WS-FIELDS2.
               05 L-CLIENT-ID               PIC S9(3).
               05 L-CLIENT-NOM              PIC X(20).
               05 L-CLIENT-PRENOM           PIC X(20).
               05 L-CLIENT-RSOCIALE         PIC X(20).
               05 L-CLIENT-TYPECLIENT       PIC X(20).
               05 L-CLIENT-EXTERNE          PIC 9 .
               05 L-COMPTE-ID               PIC S9(3).
               05 L-COMPTE-IBAN             PIC X(25).
               05 L-COMPTE-DTOUV            PIC X(14).
               05 L-COMPTE-SOLDE            PIC 9(5).99.
               05 L-COMPTE-CLIENTID         PIC S9(3).
              
         01 FIELDS-TEST.
             05 LIBELLE-1      PIC X(25).
             05 SOMME-1      PIC X(25).
             05 CLIENT-ID-1      PIC X(25).
             05 LIBELLE-1      PIC X(25).
         
         01 DETAIL-LINE.
             05 DET-NAME-OP       PIC X(25).
             05 FILLER            PIC X(3) VALUES SPACES.
             05 DET-MONTANT-OP    PIC 9(5)V99.
             05 FILLER            PIC X(1) VALUe ' '.
             05 DET-TIME          PIC 9(2).

         01  WS-CURRENT-DATE-FIELDS.
             05  WS-DATE.
                 10  WS-YEAR    PIC  9(4).
                 10  WS-MONTH   PIC  9(2).
                 10  WS-DAY     PIC  9(2).
             05  WS-TIME.
                 10  WS-HOUR    PIC  9(2).
                 10  WS-MINUTE  PIC  9(2).
                 10  WS-SECOND  PIC  9(2).
                 10  WS-MS      PIC  9(2).
             05  WS-DIFF-FROM-GMT       PIC S9(4).   


         01 SQL-DETAIL-LINE.
           05 DET-START           PIC X(89) VALUE 
       'INSERT INTO OPERATIONS (TYPE,LIBELLE,MONTANT,COMPTEID,IDCLIENT,
      -'DATEOP,STATUSOP) VALUES('.
          05 FILLER              PIC X   VALUE "'".
          05 DET-TYPE            PIC X(25).
          05 FILLER              PIC X(3) VALUE "','".
          05 DET-LIBELLE         PIC X(30).
          05 FILLER              PIC X(3) VALUE "',".
          05 DET-MONTANT         PIC X(8). 
          05 FILLER              PIC X VALUE ','.
          05 DET-COMPTEID        PIC 9(3).
          05 FILLER              PIC X VALUE ','.
          05 DET-CLIENTID        PIC 9(3). 
          05 FILLER              PIC X(2) VALUE ",'".
          05 DET-DATEOP          PIC X(10).
          05 FILLER              PIC X(14) VALUE "','VALIDEE ');". 

         01  SQL-UPDATE-LINE.
              05 FILLER      PIC X(33) VALUE
              'UPDATE COMPTES SET SOLDE = SOLDE '.
              05 DET-OPERATOR        PIC X.
              05 DET-uP-SOMME        PIC 9(5).99.
              05 FILLER              VALUE 
              ' WHERE COMPTEID = '.
              05 DET-UP-COMPTEID     PIC 9(3).
              05 FILLER              PIC X VALUE ";".
         

      *> linkage 
       COPY "LNACTIVITY2.cpy".
       
      *> colors
       COPY SCREENIO.
      
       SCREEN SECTION.
      
       01 WELCOME-SCREEN.
            05 FILLER LINE 10 COLUMN 23
            VALUE "BIENVENUE A LA BANQUE KARIMUS"
            BLANK SCREEN
            BACKGROUND-COLOR COB-COLOR-WHITE
            FOREGROUND-COLOR COB-COLOR-BLACK.

       01  LEAVE-SCREEN.
           05 FILLER LINE 10 COLUMN 23
           VALUE "MERCI A VOUS ET A BIENTOT"
           BLANK SCREEN
           BACKGROUND-COLOR COB-COLOR-WHITE
           FOREGROUND-COLOR COB-COLOR-BLACK.

       01 HEADER-SCREEN.
          05 FILLER LINE 2 COLUMN 23
          VALUE "Simulateur d'operations bancaire"
          BLANK SCREEN
          FOREGROUND-COLOR COB-COLOR-YELLOW.
      
      
       01 MAIN-FUNCTION-SCREEN.
          05 FILLER LINE 5 COLUMN 1    
          VALUE "F1 - Faire un virement occasionnel"
          FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 7 COLUMN 1    
          VALUE "F2 - Faire opposition"
          FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 9 COLUMN 1                     
          VALUE "F3 - Valider un achat internet"
          FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 11 COLUMN 1    
          VALUE "F4 - Retirer de l'argent -- EN PANNE"
          FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 13 COLUMN 1    
          VALUE "F5 - Crediter mon compte"
          FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 17 COLUMN 1    
          VALUE "F9 - Quitter le simulateur bancaire"
          FOREGROUND-COLOR COB-COLOR-GREEN.
          
          05 FILLER PIC 9(6) TO WS-ACCEPT-FNC-KEY SECURE.
              
      
        01 EPARGNER-SCREEN.
          05 FILLER LINE 5 COLUMN 1
          VALUE "QUEL EST LE MONTANT DU VIREMENT?:"
          FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC 9(5) TO SOMMEEPARGNEE 
          LINE 5 COLUMN 38    
          FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 6 COLUMN 1    
          VALUE "AJOUTER UN LIBELLE (OPTIONNEL)"
          FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X(10) TO LIBELLEVIREM
          LINE 6 COLUMN 38    
          FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X(50) FROM WSMSG
          LINE 15 COLUMN 1
          FOREGROUND-COLOR COB-COLOR-RED. 
          05 FILLER LINE 18 COLUMN 1    
          VALUE "F1 : Valider - F10 : Revenir au sommaire"
          FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X TO WS-ACCEPT-FNC-KEY SECURE
          LINE 18 COLUMN 79
          FOREGROUND-COLOR COB-COLOR-GREEN. 

         01 CREDITER-SCREEN.
          05 FILLER LINE 5 COLUMN 1
          VALUE "COMBIEN VOULEZ-VOUS CREDITER ?:"
          FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC 9(5) TO SOMMEDEPOSEE 
          LINE 5 COLUMN 38    
          FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 6 COLUMN 1    
          VALUE "VEUILLEZ INTRODUIRE LA SOMME INDIQUEE"
          FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X(50) FROM WSMSG
          LINE 15 COLUMN 1
          FOREGROUND-COLOR COB-COLOR-RED. 
          05 FILLER LINE 18 COLUMN 1    
          VALUE "F1 : Valider - F10 : Revenir au sommaire"
          FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X TO WS-ACCEPT-FNC-KEY SECURE
          LINE 18 COLUMN 79
          FOREGROUND-COLOR COB-COLOR-GREEN. 



       01 OPPOSITION-SCREEN.
          05 FILLER LINE 5 COLUMN 1
          VALUE "SUR QUEL PRODUIT SOUHAITEZ-VOUS FAIRE OPPOSITION ?:"
          FOREGROUND-COLOR COB-COLOR-GREEN.  
          05 FILLER LINE 7 COLUMN 1    
          VALUE "(SI CHEQUE) NUMERO DU CHEQUE A OPPOSER ?"
          FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER  PIC 9(3)B9(3)B9(4) TO SOMMEOPP
          LINE 9 COLUMN 1    
          FOREGROUND-COLOR COB-COLOR-GREEN. 
          05 FILLER LINE 11 COLUMN 1
          VALUE "F1 - Faire opposition sur ce cheque "
          FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 12 COLUMN 1    
          VALUE "F2 - Declarer la perte de ma carte bancaire"
          FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 18 COLUMN 1    
          VALUE "F1 : Valider - F10 : Revenir au sommaire"
          FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X(50) FROM WSMSG
          LINE 15 COLUMN 1
          FOREGROUND-COLOR COB-COLOR-RED. 
          05 FILLER PIC X TO WS-ACCEPT-FNC-KEY SECURE
          LINE 18 COLUMN 79
          FOREGROUND-COLOR COB-COLOR-GREEN. 
      
      
        01 ACHAT-CHEZ-PARTNER-SCREEN.
           05 FILLER LINE 5 COLUMN 1
           VALUE "QUE VOULEZ VOUS ACHETER ?:"
           FOREGROUND-COLOR COB-COLOR-GREEN.
           05 FILLER PIC X(15) TO PRODUITACHETEE
           LINE 5 COLUMN 34    
           FOREGROUND-COLOR COB-COLOR-GREEN.
           05 FILLER LINE 6 COLUMN 1    
           VALUE "QUEL EST SON PRIX ?"
           FOREGROUND-COLOR COB-COLOR-GREEN.
           05 FILLER PIC 9(5).99 TO PRIXPRODUIT 
           LINE 6 COLUMN 38    
           FOREGROUND-COLOR COB-COLOR-GREEN.
           05 FILLER PIC X(50) FROM WSMSG
           LINE 15 COLUMN 1
           FOREGROUND-COLOR COB-COLOR-RED.
           05 FILLER LINE 18 COLUMN 1    
          VALUE "F1 : Valider - F10 : Revenir au sommaire"
           FOREGROUND-COLOR COB-COLOR-GREEN.
           05 FILLER PIC X TO WS-ACCEPT-FNC-KEY SECURE
           LINE 18 COLUMN 79
           FOREGROUND-COLOR COB-COLOR-GREEN. 
          
             
       01 MESSAGE-SCREEN.
      *> line 20
          05 FILLER LINE 20 COLUMN 1
             VALUE "SQLCODE: "
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC -Z(9)9 FROM WS-SQLCODE OF WS-MSG
             LINE 20 COLUMN 10
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 20 COLUMN 30
             VALUE "SQLSTATE: "
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X(5) FROM WS-SQLSTATE OF WS-MSG
             LINE 20 COLUMN 40
             FOREGROUND-COLOR COB-COLOR-GREEN.
      *> line 21
          05 FILLER PIC X(80) FROM WS-MSG-1 OF WS-MSG
             LINE 21 COLUMN 1
             FOREGROUND-COLOR COB-COLOR-GREEN.
      *> line 22
          05 FILLER PIC X(80) FROM WS-MSG-2 OF WS-MSG
             LINE 22 COLUMN 1
             FOREGROUND-COLOR COB-COLOR-GREEN.
      *> line 23
          05 FILLER PIC X(80) FROM WS-MSG-3 OF WS-MSG
             LINE 23 COLUMN 1
             FOREGROUND-COLOR COB-COLOR-GREEN.
      *> line 24
          05 FILLER PIC X(80) FROM WS-MSG-4 OF WS-MSG
             LINE 24 COLUMN 1
             FOREGROUND-COLOR COB-COLOR-GREEN.


        01 CONNECT-SCREEN.
          05 FILLER LINE 4 COLUMN 1
             VALUE "DBALIAS:"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X(9) TO WS-DBALIAS
             LINE 4 COLUMN 10
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 4 COLUMN 50
             VALUE "eg.: testdb"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 5 COLUMN 1
             VALUE "USERID:"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X(20) TO WS-USERID
             LINE 5 COLUMN 10
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 5 COLUMN 50
             VALUE "eg.: LASZLO.ERDOES"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 6 COLUMN 1
             VALUE "PSWD:"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X(20) TO WS-PSWD SECURE
             LINE 6 COLUMN 10
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 6 COLUMN 50
             VALUE "eg.: laszlopw"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 18 COLUMN 1    
             VALUE "F1 - Connect to DB2"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 18 COLUMN 25    
             VALUE "F10 - Back to main"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X TO WS-ACCEPT-FNC-KEY SECURE
             LINE 18 COLUMN 79
           FOREGROUND-COLOR COB-COLOR-GREEN.



             
       PROCEDURE DIVISION.
      
      *>------------------------------------------------------------------------
       MAIN-ACTIVITY2TEST SECTION.
      *>------------------------------------------------------------------------
      
          OPEN OUTPUT GENERESQL.
           OPEN OUTPUT  GENERUPDATE.
           COMPUTE MOUSE-FLAGS = COB-AUTO-MOUSE-HANDLING
                   + COB-ALLOW-LEFT-DOWN  + COB-ALLOW-MIDDLE-DOWN  
                    + COB-ALLOW-RIGHT-DOWN
                   + COB-ALLOW-LEFT-UP     + COB-ALLOW-MIDDLE-UP    
                    + COB-ALLOW-RIGHT-UP
                    + COB-ALLOW-LEFT-DOUBLE + COB-ALLOW-MIDDLE-DOUBLE
                    + COB-ALLOW-RIGHT-DOUBLE
                    + COB-ALLOW-MOUSE-MOVE

           SET environment "COB_MOUSE_FLAGS"  to MOUSE-FLAGS.

           DISPlAY WELCOME-SCREEN.
           CALL "C$SLEEP" USING 2 END-CALL.
              MOVE 001 TO L-CLIENT-ID.
           CALL 'fcdatasclient' USING
              L-CLIENT-ID         
              L-CLIENT-NOM       
              L-CLIENT-PRENOM    
              L-CLIENT-RSOCIALE  
              L-CLIENT-TYPECLIENT
              L-CLIENT-EXTERNE   
              L-COMPTE-ID        
              L-COMPTE-IBAN      
              L-COMPTE-DTOUV     
              L-COMPTE-SOLDE      
              L-COMPTE-CLIENTID.


            PERFORM FOREVER
             DISPLAY HEADER-SCREEN END-DISPLAY  
             DISPLAY MAIN-FUNCTION-SCREEN END-DISPLAY
             DISPLAY MESSAGE-SCREEN END-DISPLAY 
             ACCEPT MAIN-FUNCTION-SCREEN END-ACCEPT
      
      *>     init message       
             INITIALIZE WS-MSG
             DISPLAY MESSAGE-SCREEN END-DISPLAY 
             
             EVALUATE TRUE
                WHEN V-FNC-F1
                   PERFORM FNC-CREDITER-CC-SCREEN
      
                WHEN V-FNC-F2
                   PERFORM FNC-OPPOSITION-SCREEN

                WHEN V-FNC-F3
                   PERFORM FNC-ACHAT-PART-SCREEN
                   
                WHEN V-FNC-F4
                   PERFORM FNC-CONNECT-SCREEN
                
                WHEN V-FNC-F5 
                   PERFORM FNC-CREDITER-COMPTE-SCREEN

*
                WHEN V-FNC-F6
                  PERFORM FNC-ENTREE-ARGENT-SCREEN  
*
      *          WHEN V-FNC-F5
                WHEN V-FNC-F9
                    EXIT PERFORM
                    DISPLAY LEAVE-SCREEN
                    CALL "C$SLEEP" USING 2 END-CALL
                    
                   
                WHEN OTHER
                   MOVE "Please select a valid function key" 
                     TO WS-MSG-1 OF WS-MSG
             END-EVALUATE
            END-PERFORM
           CLOSE GENERESQL,GENERUPDATE.
          STOP RUN
      
          .
       MAIN-ACTIVITY2TEST-EX.
          EXIT.
       

      *>------------------------------------------------------------------------
       FNC-CONNECT-SCREEN SECTION.
      *>------------------------------------------------------------------------
            
          PERFORM FOREVER
             DISPLAY HEADER-SCREEN END-DISPLAY  
             DISPLAY CONNECT-SCREEN END-DISPLAY
             DISPLAY MESSAGE-SCREEN END-DISPLAY 
             ACCEPT CONNECT-SCREEN END-ACCEPT
            
      *>     init message       
             INITIALIZE WS-MSG
             DISPLAY MESSAGE-SCREEN END-DISPLAY 
             
             EVALUATE TRUE
                WHEN V-FNC-F1
                   PERFORM FNC-CONNECT
            
                WHEN V-FNC-F10
                   EXIT PERFORM
                   
                WHEN OTHER
                   MOVE "Please select a valid function key" 
                     TO WS-MSG-1 OF WS-MSG
             END-EVALUATE
          END-PERFORM
          
          .
       FNC-CONNECT-SCREEN-EX.
          EXIT.
      *>------------------------------------------------------------------------
       FNC-CONNECT SECTION.
      *>------------------------------------------------------------------------
            
          INITIALIZE LN-MOD
          INITIALIZE WS-MSG
          SET V-LN-FNC-CONNECT OF LN-MOD TO TRUE
          MOVE WS-CONNECT TO LN-CONNECT OF LN-MOD
            
          CALL 'modactivity2' USING LN-MOD END-CALL
            
          PERFORM COPY-LN-MSG-IN-WS-MSG
          
          .
       FNC-CONNECT-EX.
          EXIT.


      *>------------------------------------------------------------------------
       FNC-CREDITER-CC-SCREEN SECTION.
      *>------------------------------------------------------------------------
      
          PERFORM FOREVER
             
             DISPLAY HEADER-SCREEN END-DISPLAY  
             DISPLAY EPARGNER-SCREEN END-DISPLAY
             DISPLAY MESSAGE-SCREEN END-DISPLAY 
             ACCEPT EPARGNER-SCREEN END-ACCEPT
      
      *>     init message       
             INITIALIZE WS-MSG
             DISPLAY MESSAGE-SCREEN END-DISPLAY 
             
             EVALUATE TRUE
                WHEN V-FNC-F1
                   PERFORM FNC-CREDITER-CC
      
                WHEN V-FNC-F10
                   EXIT PERFORM
                   
                WHEN OTHER
                   MOVE "Please select a valid function key" 
                     TO WS-MSG-1 OF WS-MSG
             END-EVALUATE
          END-PERFORM
          
          .
       FNC-CREDITER-CC-SCREEN-EX.
          EXIT.
          
      *>------------------------------------------------------------------------
       FNC-CREDITER-CC SECTION.
      *>------------------------------------------------------------------------
      
          INITIALIZE LN-MOD
          INITIALIZE WS-MSG
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS
           MOVE 001 TO DET-UP-COMPTEID.
           MOVE 'VIREMENT OCCASIONNEL' TO C-TYPEOP.
           MOVE 5.99 tO C-COMM.
           MOVE WS-DATE TO  C-DATE.
           MOVE '-' TO DET-OPERATOR.
                      
                      
           CALL 'opprelev' USING L-COMPTE-SOLDE, SOMMEEPARGNEE,
           NEWSOLDE,
             C-TYPEOP, C-SUCCESS, C-COMM , C-DATE, C-LIBELLE.
           MOVE C-TYPEOP TO DET-TYPE.
           IF(LIBELLEVIREM IS ALPHABETIC)
               STRING FUNCTION TRIM(C-TYPEOP) ' ' 
               FUNCTION TRIM(LIBELLEVIREM)  DELIMITED BY SIZE 
               INTO DET-LIBELLE
           ELSE 
               MOVE "rester dans l'alphabet" TO WSMSG
           END-IF.
           IF(SOMMEEPARGNEE IS NUMERIC)
               STRING '-' FUNCTION TRIM(SOMMEEPARGNEE)
               DELIMITED BY SIZE 
               INTO DET-MONTANT
               MOVE SOMMEEPARGNEE TO DET-uP-SOMME 
           ELSE 
               MOVE "Veuillez entrer un nombre" TO WSMSG
           END-IF.
          IF(LIBELLEVIREM IS ALPHABETIC) AND (SOMMEEPARGNEE IS NUMERIC)
               MOVE "Le virement a bien ete effectue" TO WSMSG 
           MOVE L-CLIENT-ID TO DET-CLIENTID.
           MOVE L-COMPTE-ID TO DET-COMPTEID.


          STRING WS-YEAR '-' WS-MONTH '-' WS-DAY 
          DELIMITED BY SIZE INTO DET-DATEOP.
      *    MOVE C-DATE TO DET-DATEOP.
           MOVE SQL-DETAIL-LINE TO PRINT-LINE.
           WRITE PRINT-LINE.
           MOVE SQL-UPDATE-LINE TO PRINT-UP-LINE .
           WRITE PRINT-UP-LINE.
      
          PERFORM COPY-LN-MSG-IN-WS-MSG

          
          .
       FNC-CREDITER-CC-EX.
          EXIT.


      *>----------------------------------------------------------------
       FNC-CREDITER-COMPTE-SCREEN SECTION.
      *>----------------------------------------------------------------
            
          PERFORM FOREVER
             
             DISPLAY HEADER-SCREEN END-DISPLAY  
             DISPLAY CREDITER-SCREEN END-DISPLAY
             DISPLAY MESSAGE-SCREEN END-DISPLAY 
             ACCEPT CREDITER-SCREEN END-ACCEPT
            
      *>     init message       
             INITIALIZE WS-MSG
             DISPLAY MESSAGE-SCREEN END-DISPLAY 
             
             EVALUATE TRUE
                WHEN V-FNC-F1
                   PERFORM FNC-CREDITER-COMPTE
            
                WHEN V-FNC-F10
                   EXIT PERFORM
                   
                WHEN OTHER
                   MOVE "Please select a valid function key" 
                     TO WS-MSG-1 OF WS-MSG
             END-EVALUATE
          END-PERFORM
          
          .
       FNC-CREDITER-COMPTE-SCREEN-EX.
          EXIT.
          
      *>----------------------------------------------------------------
       FNC-CREDITER-COMPTE SECTION.
      *>----------------------------------------------------------------
            
          INITIALIZE LN-MOD
          INITIALIZE WS-MSG
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS
           MOVE 001 TO DET-UP-COMPTEID.
           MOVE 'DEPOT D ESPECE' TO C-TYPEOP.
           MOVE WS-DATE TO  C-DATE.
           MOVE '+' TO DET-OPERATOR.
                      
                      
           CALL 'opcredit' USING L-COMPTE-SOLDE, SOMMEDEPOSEE,
           NEWSOLDE.
             
           MOVE C-TYPEOP TO DET-TYPE.
           
               MOVE FUNCTION TRIM(C-TYPEOP) TO DET-LIBELLE.
           
           IF(SOMMEEPARGNEE IS NUMERIC)
               STRING '+' FUNCTION TRIM(SOMMEDEPOSEE)
               DELIMITED BY SIZE 
               INTO DET-MONTANT
               MOVE SOMMEDEPOSEE TO DET-UP-SOMME 
           ELSE 
               MOVE "Veuillez entrer un nombre" TO WSMSG
           END-IF.
          IF(LIBELLEVIREM IS ALPHABETIC) AND (SOMMEEPARGNEE IS NUMERIC)
               MOVE "Le virement a bien ete effectue" TO WSMSG 
           MOVE L-CLIENT-ID TO DET-CLIENTID.
           MOVE L-COMPTE-ID TO DET-COMPTEID.
           STRING WS-YEAR '-' WS-MONTH '-' WS-DAY 
           DELIMITED BY SIZE INTO DET-DATEOP.
           MOVE SQL-DETAIL-LINE TO PRINT-LINE.
           WRITE PRINT-LINE.
           MOVE SQL-UPDATE-LINE TO PRINT-UP-LINE .
           WRITE PRINT-UP-LINE.
            
          PERFORM COPY-LN-MSG-IN-WS-MSG
          
          .
       FNC-CREDITER-COMPTE-EX.
          EXIT.
      

      *>------------------------------------------------------------------------
       FNC-OPPOSITION-SCREEN SECTION.
      *>------------------------------------------------------------------------
      
          PERFORM FOREVER
             ACCEPT WS-CURRENT-DATE-FIELDS FROM DATE
             DISPLAY HEADER-SCREEN END-DISPLAY  
             DISPLAY OPPOSITION-SCREEN END-DISPLAY
             DISPLAY MESSAGE-SCREEN END-DISPLAY 
             ACCEPT OPPOSITION-SCREEN END-ACCEPT
      
      *>     init message       
             INITIALIZE WS-MSG
             DISPLAY MESSAGE-SCREEN END-DISPLAY 
             
             EVALUATE TRUE
                WHEN V-FNC-F1
                WHEN V-FNC-F2
                   PERFORM FNC-OPPOSITION
      
                WHEN V-FNC-F10
                   EXIT PERFORM
                   
                WHEN OTHER
                   MOVE "Please select a valid function key" 
                     TO WS-MSG-1 OF WS-MSG
             END-EVALUATE
          END-PERFORM
          
          .
       FNC-OPPOSITION-SCREEN-EX.
          EXIT.
          
      *>------------------------------------------------------------------------
       FNC-OPPOSITION SECTION.
      *>------------------------------------------------------------------------
      
          INITIALIZE LN-MOD
          INITIALIZE WS-MSG
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS

    
               If(SOMMEOPP IS ALPHABETIC)
               MOVE SOMMEOPP TO C-SOMME
            ELSE
             MOVE 'Veuillez entrer un numéro de chèque au bon format'
             TO WSMSG
            END-IF.
               
               MOVE L-COMPTE-CLIENTID TO DET-COMPTEID.
               MOVE L-CLIENT-ID TO DET-CLIENTID.
              MOVE WS-DATE TO  C-DATE.
              
               STRING WS-YEAR '-' WS-MONTH '-' WS-DAY 
               DELIMITED BY SIZE INTO DET-DATEOP.
               EVALUATE TRUE 
                   WHEN V-FNC-F1 
                MOVE 'OPPOSITION CHEQUE BANQUE ' TO C-TYPEOP
                MOVE "opposition sur cheque prise en compte" TO WSMSG
                
                   WHEN V-FNC-F2 
                 MOVE 'OPPOSITION CARTE BANCAIRE' TO C-TYPEOP
                 MOVE "Opposition sur carte prise en compte" TO WSMSG
                      
                END-EVALUATE.
                
               CALL 'commisionfrais' USING C-MONTANT, C-SOMME,
             , C-TYPEOP, C-FRAGFINANC, C-PLAFOND, C-DATE,C-LIBELLE.
             MOVE C-LIBELLE TO DET-LIBELLE.
             MOVE C-TYPEOP TO DET-TYPE.
             STRING '-' C-MONTANT DELIMITED BY SIZE INTO DET-MONTANT.
             MOVE SQL-DETAIL-LINE TO PRINT-LINE.
               WRITE PRINT-LINE.
             MOVE '-' TO DET-OPERATOR.
             MOVE C-MONTANT TO DET-UP-SOMME.
             MOVE 001 TO DET-UP-COMPTEID.
             MOVE SQL-UPDATE-LINE TO PRINT-UP-LINE.
               WRITE PRINT-UP-LINE.
      
          PERFORM COPY-LN-MSG-IN-WS-MSG
          
          .
       FNC-INSERT-CLIENT-EX.
          EXIT.
       

      *>------------------------------------------------------------------------
       FNC-ACHAT-PART-SCREEN SECTION.
      *>------------------------------------------------------------------------
      
          PERFORM FOREVER

             DISPLAY HEADER-SCREEN END-DISPLAY  
             DISPLAY ACHAT-CHEZ-PARTNER-SCREEN END-DISPLAY
             DISPLAY MESSAGE-SCREEN END-DISPLAY 
             ACCEPT ACHAT-CHEZ-PARTNER-SCREEN END-ACCEPT
      
      *>     init message       
             INITIALIZE WS-MSG
             DISPLAY MESSAGE-SCREEN END-DISPLAY 
             
             EVALUATE TRUE
                WHEN V-FNC-F1
                WHEN V-FNC-F2
                WHEN V-FNC-F3
                WHEN V-FNC-F4
                   PERFORM FNC-ACHAT-CHEZ-PARTNER
      
                WHEN V-FNC-F10
                   EXIT PERFORM
                   
                WHEN OTHER
                   MOVE "Please select a valid function key" 
                     TO WS-MSG-1 OF WS-MSG
             END-EVALUATE
          END-PERFORM
          
          .
       FNC-PAGING-OP-SCREEN-EX.
          EXIT.
          
      *>------------------------------------------------------------------------
       FNC-ACHAT-CHEZ-PARTNER SECTION.
      *>------------------------------------------------------------------------
      
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS

          INITIALIZE LN-MOD
          INITIALIZE WS-MSG
          
            MOVE L-COMPTE-CLIENTID TO DET-COMPTEID.
            MOVE L-CLIENT-ID TO DET-CLIENTID.
            
             STRING WS-YEAR '-' WS-MONTH '-' WS-DAY 
             DELIMITED BY SIZE INTO DET-DATEOP. 

            IF(PRIXPRODUIT IS NUMERIC)
               MOVE PRIXPRODUIT TO C-SOMME
            ELSE 
               MOVE 'Veuillez entrer uniquement des chiffres au prix'
               TO WSMSG. 
            CALL 'opdebit' USING MONSOLDE, C-SOMME, NEWSOLDE.
            MOVE 'ACHAT CB' TO DET-TYPE.
            IF(PRODUITACHETEE IS ALPHABETIC )
               STRING FUNCTION TRIM(DET-TYPE) ' ' FUNCTION 
               TRIM(PRODUITACHETEE) DELIMITED BY SIZE INTO DET-LIBELLE
            ELSE   
               MOVE 'Veuillez entrer uniquement des lettres au produit'
               TO WSMSG. 
            STRING '-' C-SOMME DELIMITED BY SIZE INTO DET-MONTANT.
            MOVE SQL-DETAIL-LINE TO PRINT-LINE.
            WRITE PRINT-LINE.
            
            MOVE '-' TO DET-OPERATOR.
            MOVE C-SOMME TO DET-UP-SOMME.
            MOVE 001 TO DET-UP-COMPTEID.
            MOVE SQL-UPDATE-LINE TO PRINT-UP-LINE.
              WRITE PRINT-UP-LINE.
                       
          
      
          PERFORM COPY-LN-MSG-IN-WS-MSG
          
          .
       FNC-PAGING-BOOK-EX.
          EXIT.

      *>------------------------------------------------------------------------
       FNC-ENTREE-ARGENT-SCREEN SECTION.
      *>------------------------------------------------------------------------
      
         
      
          PERFORM FOREVER
             DISPLAY HEADER-SCREEN END-DISPLAY  
          END-PERFORM
          
          .
       FNC-LIST-SCREEN-EX.
          EXIT.
          
      *>------------------------------------------------------------------------
       FNC-ENTREE-ARGENT SECTION.
      *>------------------------------------------------------------------------
      
          INITIALIZE LN-MOD
          INITIALIZE WS-MSG
          
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS
           MOVE 1500 TO ENTREEAR.
           
           

           MOVE 'SALAIRE TLR' to C-LIBELLE.
           MOVE 'ENTREE ARGENT' TO C-TYPEOP.
           CALL 'entreeargent' USING MONSOLDE, ENTREEAR, NEWSOLDE,
           LA-DATE, LE-LIBELLE.
              
           STRING WS-YEAR '-' WS-MONTH '-' WS-DAY 
           DELIMITED BY SIZE INTO DET-DATEOP.
          
           MOVE C-TYPEOP TO DET-TYPE.
           STRING LE-LIBELLE ' ' C-LIBELLE DELIMITED BY SIZE 
           INTO DET-LIBELLE.
           STRING '+' ENTREEAR DELIMITED BY SIZE INTO DET-MONTANT.
                      
           MOVE 001 TO  DET-COMPTEID
           MOVE WS-DATE TO DET-DATEOP.
           MOVE SQL-DETAIL-LINE TO PRINT-LINE.
            WRITE PRINT-LINE.
           MOVE '+' TO DET-OPERATOR.
           MOVE ENTREEAR TO DET-UP-SOMME.
           MOVE 001 TO DET-UP-COMPTEID.
           MOVE SQL-UPDATE-LINE TO PRINT-UP-LINE.
             WRITE PRINT-UP-LINE. 
             
      
          PERFORM COPY-LN-MSG-IN-WS-MSG

          
          .
       FNC-LIST-CLIENT-EX.
          EXIT.
          
      *>------------------------------------------------------------------------
       COPY-LN-MSG-IN-WS-MSG SECTION.
      *>------------------------------------------------------------------------
      
          MOVE LN-MSG                  OF LN-OUTPUT         
            TO WS-MSG                  
          
          .
       COPY-LN-MSG-IN-WS-MSG-EX.
          EXIT.
          
       END PROGRAM ACTIVITY2CLIENT. 
