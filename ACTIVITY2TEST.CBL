       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACTIVITY2TEST.
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
          CRT STATUS IS WS-FNC-KEY.
       
       DATA DIVISION.
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
             04 WS-OUT-CLIENT-TAB-SOLDE       PIC 9(5).

      *> line number var for the list screen   
       01 SC-LINE-NR                   PIC 99.  

      *> indices for cycles
       01 WS-IND-1                     PIC S9(4) COMP.
       
      *> linkage 
       COPY "LNACTIVITY2.cpy".
       
      *> colors
       COPY SCREENIO.
      
       SCREEN SECTION.
       01 HEADER-SCREEN.
          05 FILLER LINE 2 COLUMN 13
             VALUE "DB2 sample program, please select a function"
      *>     this deletes the screen       
             BLANK SCREEN
             FOREGROUND-COLOR COB-COLOR-GREEN.
      
       01 MAIN-FUNCTION-SCREEN.
          05 FILLER LINE 4 COLUMN 5
             VALUE "F1 - Consulter le fichier clientele"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 5 COLUMN 5
             VALUE "F2 - Ajouter un nouveau client --EN PANNE"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 6 COLUMN 5
             VALUE "F3 - Acceder au compte d'un client"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 7 COLUMN 5
             VALUE "F4 - Voir toutes les operations"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 8 COLUMN 5
             VALUE "F5 - Acceder a ma messagerie (BIENTOT)"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 9 COLUMN 5
           VALUE "F6 - Connexion BDD"
           FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 10 COLUMN 5
             VALUE "F9 - Exit"
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


       01 SELECT-CLIENT-SCREEN.
          05 FILLER LINE 4 COLUMN 1
             VALUE "CLIENTID:"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC 9(3) TO  WS-INP-CLIENTID
             LINE 4 COLUMN 12
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 4 COLUMN 50
             VALUE "ex.: 001"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 5 COLUMN 1
             VALUE "NOM:"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X(50) FROM WS-OUT-NOM
             LINE 5 COLUMN 10
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 6 COLUMN 1
             VALUE "Prenom:"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X(50) FROM WS-OUT-PRENOM
             LINE 6 COLUMN 10
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 7 COLUMN 1
             VALUE "Raison Sociale:"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X(50) FROM WS-OUT-RAISONSOCIALE
             LINE 7 COLUMN 18
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 8 COLUMN 1
             VALUE "CATEGORIE"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X(50) FROM WS-OUT-TYPECLIENT
             LINE 8 COLUMN 10
             FOREGROUND-COLOR COB-COLOR-GREEN.

         05 FILLER LINE 18 COLUMN 1    
             VALUE "F1 - Select with CLIENT ID "
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 18 COLUMN 25    
             VALUE "F10 - Back to main"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X TO WS-ACCEPT-FNC-KEY SECURE
             LINE 18 COLUMN 79
             FOREGROUND-COLOR COB-COLOR-GREEN.

       01 INSERT-CLIENT-SCREEN.
          05 FILLER LINE 4 COLUMN 1
             VALUE "NOM:"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X(50) TO WS-INP-NOM
             LINE 4 COLUMN 10
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 5 COLUMN 1
             VALUE "PRENOM:"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X(50) TO WS-INP-PRENOM
             LINE 5 COLUMN 10
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 6 COLUMN 1
             VALUE "RAISON SOCIALE:"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X(50) TO WS-INP-RAISONSOCIALE
             LINE 6 COLUMN 17
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 7 COLUMN 1
             VALUE "CATEGORIE:"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X(50) TO WS-INP-TYPECLIENT
             LINE 7 COLUMN 10
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 8 COLUMN 1
             VALUE "EXTERNE:"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC 9(1) TO WS-INP-EXTERNE
             LINE 8 COLUMN 10
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 18 COLUMN 1    
             VALUE "F1 - AJOUTER"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 18 COLUMN 25    
             VALUE "F10 - Back to main"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X TO WS-ACCEPT-FNC-KEY SECURE
             LINE 18 COLUMN 79
             FOREGROUND-COLOR COB-COLOR-GREEN.



       01 PAGING-OP-SCREEN.
          05 FILLER LINE 4 COLUMN 1
             VALUE "NOM"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC 9(3) FROM WS-OUT-NOM
             LINE 4 COLUMN 10
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 5 COLUMN 1
             VALUE "Prenom:"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X(50) FROM WS-OUT-PRENOM
             LINE 5 COLUMN 10
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 6 COLUMN 1
             VALUE "Raison Sociale:"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X(50) FROM WS-OUT-RAISONSOCIALE
             LINE 6 COLUMN 18
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 7 COLUMN 1
             VALUE "Categorie:"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X(50) FROM WS-OUT-TYPECLIENT
             LINE 7 COLUMN 15
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 8 COLUMN 1
             VALUE "Numero de Compte:"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC 9(3) FROM WS-OUT-COMPTEID
             LINE 8 COLUMN 20
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 10 COLUMN 1
             VALUE "Solde"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC 9(10)V99 FROM WS-OUT-SOLDE
             LINE 10 COLUMN 10
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 11 COLUMN 1
             VALUE "ID Operation:"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC 9(3) USING WS-INP-IDOPERATION
             LINE 11 COLUMN 20
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 12 COLUMN 1
             VALUE "Type d'operation:"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X(50) FROM WS-OUT-TYPE
             LINE 12 COLUMN 24
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 13 COLUMN 1
             VALUE "Libelle operation:"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X(50) FROM WS-OUT-LIBELLE
             LINE 13 COLUMN 24
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 14 COLUMN 1
             VALUE "Montant operation:"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC 9(8)V99 FROM WS-OUT-MONTANT
             LINE 14 COLUMN 24
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 15 COLUMN 1
             VALUE "Date operation:"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X(10) FROM WS-OUT-DATEOP
             LINE 15 COLUMN 24
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 18 COLUMN 1    
             VALUE "F1 - First, F2 - Next, F3 - Previous, F4 - Last,"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 18 COLUMN 55    
             VALUE "F10 - Back to main"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X TO WS-ACCEPT-FNC-KEY SECURE
             LINE 18 COLUMN 79
             FOREGROUND-COLOR COB-COLOR-GREEN.

       01 LIST-CLIENTS-SCREEN.
      *>  column headers 
          05 FILLER LINE 4 COLUMN 1
             VALUE "Nom"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 4 COLUMN 15
             VALUE "Prenom"
             FOREGROUND-COLOR COB-COLOR-GREEN.
             05 FILLER LINE 4 COLUMN 29
             VALUE "raison Sociale"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 4 COLUMN 50
             VALUE "Categorie"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 4 COLUMN 67
             VALUE "Solde"
             FOREGROUND-COLOR COB-COLOR-GREEN.
      *    05 FILLER LINE 4 COLUMN 80
      *       VALUE "Opération en attente"
      *       FOREGROUND-COLOR COB-COLOR-GREEN.

      *>  bottom part             
          05 FILLER LINE 18 COLUMN 1    
             VALUE "F1 - First, F2 - Next, F3 - Previous, F4 - Last,"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER LINE 18 COLUMN 55    
             VALUE "F10 - Back to main"
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 FILLER PIC X TO WS-ACCEPT-FNC-KEY SECURE
             LINE 18 COLUMN 79
             FOREGROUND-COLOR COB-COLOR-GREEN.
       01 LIST-CLIENT-LINE.           
          05 SC-NOM                PIC X(15)
             LINE SC-LINE-NR COLUMN 1
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 SC-PRENOM                  PIC X(15)
             LINE SC-LINE-NR COLUMN 15
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 SC-RSOCIALE                   PIC X(13) 
             LINE SC-LINE-NR COLUMN 29
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 SC-TYPECLIENT                  PIC X(15)
             LINE SC-LINE-NR COLUMN 50
             FOREGROUND-COLOR COB-COLOR-GREEN.
          05 SC-SOLDE                  PIC 9(5)V99
              LINE SC-LINE-NR COLUMN 67
              FOREGROUND-COLOR COB-COLOR-GREEN.
      *     05 SC-OPATTENTE                  PIC X(3)
      *        LINE SC-LINE-NR COLUMN 80
      *        FOREGROUND-COLOR COB-COLOR-GREEN.
             
       PROCEDURE DIVISION.
      
      *>------------------------------------------------------------------------
       MAIN-ACTIVITY2TEST SECTION.
      *>------------------------------------------------------------------------
      
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
                   PERFORM FNC-LIST-CLIENTS-SCREEN
      
                WHEN V-FNC-F2
                   PERFORM FNC-INSERT-CLIENT-SCREEN

                WHEN V-FNC-F3
                   PERFORM FNC-SELECT-CLIENT-SCREEN
                   
                WHEN V-FNC-F4
                   PERFORM FNC-PAGING-OP-SCREEN

                WHEN V-FNC-F6
                   PERFORM FNC-CONNECT-SCREEN   

                WHEN V-FNC-F5
                WHEN V-FNC-F9
                   EXIT PERFORM
                   
                WHEN OTHER
                   MOVE "Please select a valid function key" 
                     TO WS-MSG-1 OF WS-MSG
             END-EVALUATE
          END-PERFORM
      
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
       FNC-SELECT-CLIENT-SCREEN SECTION.
      *>------------------------------------------------------------------------
      
          PERFORM FOREVER
             DISPLAY HEADER-SCREEN END-DISPLAY  
             DISPLAY SELECT-CLIENT-SCREEN END-DISPLAY
             DISPLAY MESSAGE-SCREEN END-DISPLAY 
             ACCEPT SELECT-CLIENT-SCREEN END-ACCEPT
      
      *>     init message       
             INITIALIZE WS-MSG
             DISPLAY MESSAGE-SCREEN END-DISPLAY 
             
             EVALUATE TRUE
                WHEN V-FNC-F1
                   PERFORM FNC-SELECT-CLIENT
      
                WHEN V-FNC-F10
                   EXIT PERFORM
                   
                WHEN OTHER
                   MOVE "Please select a valid function key" 
                     TO WS-MSG-1 OF WS-MSG
             END-EVALUATE
          END-PERFORM
          
          .
       FNC-SELECT-CLIENT-SCREEN-EX.
          EXIT.
          
      *>------------------------------------------------------------------------
       FNC-SELECT-CLIENT SECTION.
      *>------------------------------------------------------------------------
      
          INITIALIZE LN-MOD
          INITIALIZE WS-MSG
          INITIALIZE WS-INP-CLIENT
          SET V-LN-FNC-SEE-ONE-CLIENT OF LN-MOD TO TRUE
          MOVE 001 TO LN-INP-CLIENTID
      
          CALL 'modactivity2' USING LN-MOD END-CALL
      
          PERFORM COPY-LN-MSG-IN-WS-MSG

          MOVE LN-OUT-CLIENT             OF LN-OUTPUT
            TO WS-OUT-CLIENT          
          
          .
       FNC-SELECT-CLIENT-EX.
          EXIT.

      *>------------------------------------------------------------------------
       FNC-INSERT-CLIENT-SCREEN SECTION.
      *>------------------------------------------------------------------------
      
          PERFORM FOREVER
             DISPLAY HEADER-SCREEN END-DISPLAY  
             DISPLAY INSERT-CLIENT-SCREEN END-DISPLAY
             DISPLAY MESSAGE-SCREEN END-DISPLAY 
             ACCEPT INSERT-CLIENT-SCREEN END-ACCEPT
      
      *>     init message       
             INITIALIZE WS-MSG
             DISPLAY MESSAGE-SCREEN END-DISPLAY 
             
             EVALUATE TRUE
                WHEN V-FNC-F1
                   PERFORM FNC-INSERT-CLIENT
      
                WHEN V-FNC-F10
                   EXIT PERFORM
                   
                WHEN OTHER
                   MOVE "Please select a valid function key" 
                     TO WS-MSG-1 OF WS-MSG
             END-EVALUATE
          END-PERFORM
          
          .
       FNC-INSERT-CLIENT-SCREEN-EX.
          EXIT.
          
      *>------------------------------------------------------------------------
       FNC-INSERT-CLIENT SECTION.
      *>------------------------------------------------------------------------
      
          INITIALIZE LN-MOD
          INITIALIZE WS-MSG
          SET V-LN-FNC-ADD-NEW-CLIENT OF LN-MOD TO TRUE
          MOVE WS-INP-CLIENT TO LN-INP-CLIENT
      *>  for the fields INSERT-USER, LUPD-USER in the table CLIENT  
      *>    MOVE WS-CONNECT TO LN-CONNECT OF LN-MOD
      
          CALL 'modactivity2' USING LN-MOD END-CALL
      
          PERFORM COPY-LN-MSG-IN-WS-MSG
          
          .
       FNC-INSERT-CLIENT-EX.
          EXIT.
       

      *>------------------------------------------------------------------------
       FNC-PAGING-OP-SCREEN SECTION.
      *>------------------------------------------------------------------------
      
          PERFORM FOREVER
             DISPLAY HEADER-SCREEN END-DISPLAY  
             DISPLAY PAGING-OP-SCREEN END-DISPLAY
             DISPLAY MESSAGE-SCREEN END-DISPLAY 
             ACCEPT PAGING-OP-SCREEN END-ACCEPT
      
      *>     init message       
             INITIALIZE WS-MSG
             DISPLAY MESSAGE-SCREEN END-DISPLAY 
             
             EVALUATE TRUE
                WHEN V-FNC-F1
                WHEN V-FNC-F2
                WHEN V-FNC-F3
                WHEN V-FNC-F4
                   PERFORM FNC-PAGING-OP
      
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
       FNC-PAGING-OP SECTION.
      *>------------------------------------------------------------------------
      
          INITIALIZE LN-MOD
          INITIALIZE WS-MSG
          EVALUATE TRUE
             WHEN V-FNC-F1
                SET V-LN-FNC-PAGING-FIRST    OF LN-MOD TO TRUE
                
             WHEN V-FNC-F2
                SET V-LN-FNC-PAGING-NEXT     OF LN-MOD TO TRUE
      *>        current value as restart point          
                MOVE WS-INP-IDOPERATION      OF WS-INP-OPERATIONS 
                  TO LN-INP-IDOPERATION      OF LN-INP-OPERATION
          
             WHEN V-FNC-F3
                SET V-LN-FNC-PAGING-PREVIOUS OF LN-MOD TO TRUE
      *>        current value as restart point          
                 MOVE WS-INP-IDOPERATION      OF WS-INP-OPERATIONS 
                   TO LN-INP-IDOPERATION      OF LN-INP-OPERATION
             
             WHEN V-FNC-F4
                SET V-LN-FNC-PAGING-LAST     OF LN-MOD TO TRUE
          END-EVALUATE
      
          CALL 'modactivity2' USING LN-MOD END-CALL
      
          PERFORM COPY-LN-MSG-IN-WS-MSG

          MOVE LN-OUT-CLIENT         OF LN-OUTPUT
            TO WS-OUT-CLIENT
          MOVE LN-OUT-COMPTE         OF LN-OUTPUT
            TO WS-OUT-COMPTE
          MOVE LN-OUT-OPERATION      OF LN-OUTPUT
            TO WS-OUT-OPERATIONS          
      *>  copy selected ISBN as new input value      
          MOVE WS-OUT-IDOPERATION      OF WS-OUT-OPERATIONS
            TO WS-INP-IDOPERATION      OF WS-INP-OPERATIONS
          
          .
       FNC-PAGING-BOOK-EX.
          EXIT.

      *>------------------------------------------------------------------------
       FNC-LIST-CLIENTS-SCREEN SECTION.
      *>------------------------------------------------------------------------
      
          INITIALIZE WS-OUT-CLIENT-TABLE
      
          PERFORM FOREVER
             DISPLAY HEADER-SCREEN END-DISPLAY  
             DISPLAY LIST-CLIENTS-SCREEN END-DISPLAY
             
      *>     copy table data to screen
             PERFORM VARYING WS-IND-1 FROM 1 BY 1
               UNTIL WS-IND-1 > C-MAX-LINE-NR
               OR    WS-IND-1 > WS-OUT-CLIENT-TAB-LINE-NR
                COMPUTE SC-LINE-NR = WS-IND-1 + 5 END-COMPUTE
                MOVE WS-OUT-CLIENT-TAB-NOM(WS-IND-1)
                  TO SC-NOM
                MOVE WS-OUT-CLIENT-TAB-PRENOM(WS-IND-1)
                  TO SC-PRENOM
                MOVE WS-OUT-CLIENT-TAB-RSOCIALE(WS-IND-1)
                  TO SC-RSOCIALE
                MOVE WS-OUT-CLIENT-TAB-TYPECLIENT(WS-IND-1)
                  TO SC-TYPECLIENT 
                 MOVE WS-OUT-CLIENT-TAB-SOLDE(WS-IND-1)
                 TO SC-SOLDE 

                DISPLAY LIST-CLIENT-LINE END-DISPLAY
             END-PERFORM
             
             DISPLAY MESSAGE-SCREEN END-DISPLAY 
             ACCEPT LIST-CLIENTS-SCREEN END-ACCEPT
      
      *>     init message       
             INITIALIZE WS-MSG
             DISPLAY MESSAGE-SCREEN END-DISPLAY 
             
             EVALUATE TRUE
                WHEN V-FNC-F1
                WHEN V-FNC-F2
                WHEN V-FNC-F3
                WHEN V-FNC-F4
                   PERFORM FNC-LIST-CLIENT
      
                WHEN V-FNC-F10
                   EXIT PERFORM
                   
                WHEN OTHER
                   MOVE "Please select a valid function key" 
                     TO WS-MSG-1 OF WS-MSG
             END-EVALUATE
          END-PERFORM
          
          .
       FNC-LIST-SCREEN-EX.
          EXIT.
          
      *>------------------------------------------------------------------------
       FNC-LIST-CLIENT SECTION.
      *>------------------------------------------------------------------------
      
          INITIALIZE LN-MOD
          INITIALIZE WS-MSG
          EVALUATE TRUE
             WHEN V-FNC-F1
                SET V-LN-FNC-LIST-FIRST    OF LN-MOD TO TRUE
                
             WHEN V-FNC-F2
                SET V-LN-FNC-LIST-NEXT     OF LN-MOD TO TRUE
      *>        current value as restart point 
      *>        searching for the valid first line from bottom      
                PERFORM VARYING WS-IND-1 FROM C-MAX-LINE-NR BY -1
                  UNTIL WS-IND-1 < 1
                   IF WS-OUT-CLIENT-TAB-IDCLIENT(WS-IND-1) NOT = ZEROES
                   THEN
                      MOVE WS-OUT-CLIENT-TAB-NOM(WS-IND-1)
                        TO LN-INP-NOM  OF LN-INP-CLIENT
                      MOVE WS-OUT-CLIENT-TAB-PRENOM(WS-IND-1)
                        TO LN-INP-PRENOM    OF LN-INP-CLIENT
                      MOVE WS-OUT-CLIENT-TAB-RSOCIALE(WS-IND-1)
                        TO LN-INP-RAISON-SOCIALE     OF LN-INP-CLIENT
                      MOVE WS-OUT-CLIENT-TAB-TYPECLIENT(WS-IND-1)
                        TO LN-INP-TYPECLIENT     OF LN-INP-CLIENT               
                      MOVE WS-OUT-CLIENT-TAB-SOLDE(WS-IND-1)
                         TO LN-INP-COMPTE-SOLDE     OF LN-INP-COMPTE
                      

                      EXIT PERFORM
                   END-IF  
                END-PERFORM
             
             WHEN V-FNC-F3
                SET V-LN-FNC-LIST-PREVIOUS OF LN-MOD TO TRUE
      *>        current value as restart point          
      *>        searching for the valid first line from top      
                PERFORM VARYING WS-IND-1 FROM 1 BY 1
                  UNTIL WS-IND-1 > C-MAX-LINE-NR
                   IF WS-OUT-CLIENT-TAB-IDCLIENT(WS-IND-1) NOT = ZEROES
                   THEN
                    MOVE WS-OUT-CLIENT-TAB-NOM(WS-IND-1)
                     TO LN-INP-NOM  OF LN-INP-CLIENT
                   MOVE WS-OUT-CLIENT-TAB-PRENOM(WS-IND-1)
                     TO LN-INP-PRENOM    OF LN-INP-CLIENT
                   MOVE WS-OUT-CLIENT-TAB-RSOCIALE(WS-IND-1)
                     TO LN-INP-RAISON-SOCIALE     OF LN-INP-CLIENT
                   MOVE WS-OUT-CLIENT-TAB-TYPECLIENT(WS-IND-1)
                     TO LN-INP-TYPECLIENT     OF LN-INP-CLIENT 
                   MOVE WS-OUT-CLIENT-TAB-SOLDE(WS-IND-1)
                      TO LN-INP-COMPTE-SOLDE     OF LN-INP-COMPTE
                        
                      EXIT PERFORM
                   END-IF  
                END-PERFORM
             
             WHEN V-FNC-F4
                SET V-LN-FNC-LIST-LAST     OF LN-MOD TO TRUE
          END-EVALUATE
      
          CALL 'modactivity2' USING LN-MOD END-CALL
      
          PERFORM COPY-LN-MSG-IN-WS-MSG

      *>  copy table data from linkage
          MOVE LN-OUT-CLIENT-TABLE       OF LN-OUTPUT
            TO WS-OUT-CLIENT-TABLE          
          
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
          
       END PROGRAM ACTIVITY2TEST. 
