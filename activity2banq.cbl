       IDENTIFICATION DIVISION.
       PROGRAM-ID. activity2.
       AUTHOR. D.KISAMA.

      * Ce programme permet dà partir d'écrans de procéder à différentes
      *> opérations bancaires telles que
      *> _ créditer une some
      *> _ Faire un achat d'un nvéhicule
      *> _ afficher la derière opération bacaire effectué

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
          SPECIAL-NAMES.
      *       CRT STATUS IS WS-FNC-KEY. 
             CRT STATUS IS wRetCode  
             CURSOR     IS wRowCol .  
       
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

             
       
       
           *>  Exception keys for mouse handling
       
       01  wTTT          pic 9(15) value 0.
       01  wDummy        PIC XXX.
       01  wRetCode      PIC 9(4) value 0000.
       01  wRetCodeDescr pic x(23).
       01  wRowCol       PIC 9(6) value 0000.
       01  redefines wRowCol .
           05 wRowR      Pic 9(3).
           05 wColR      Pic 9(3).
       
       01   wInt         BINARY-SHORT SIGNED.
       01   wRow        pic 9(02) value  0.
       01   wRow2       pic 9(02) value  0.
       01   wCol        pic 9(02) value  0.
       01   wCol2       pic 9(02) value  0.
       01   wClicked    pic 9(02) value  0.

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
           05 WSMSG           PIC X(25).
           05 LINE-NUMBER     PIC 99.
           05 SOMMECREDITEE   PIC 9(5).99.
           05 NEWSOLDE        PIC 9(5).99.
           05 SOMMEEPARGNEE   PIC 9(5).99 .
           05 SOMMESFR        PIC 9(5).99.
           05 LIBELLEVIREM    PIC X(10) JUST RIGHT.
           05 DATEVIREM       PIC X(10).
           05 DATEPRELEVSFR   PIC X(10).
           05 REPONSESFR      PIC X(3).
           05 PRODUITACHETEE  PIC X(15).
           05 PRIXPRODUIT     PIC 9(5).99.
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
           05 NUMCOMPTE        PIC 9(3).
           05 LOGIN        PIC X(10).
           05 MOTDEPASSE        PIC X(10).
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

       01 SQL-DETAIL-LINE.
           05 DET-START           PIC X(79) VALUE 
       'INSERT INTO OPERATIONS (TYPE,LIBELLE,MONTANT,COMPTEID,IDCLIENT,
      -'DATEOP) VALUES('.
           05 FILLER              PIC X   VALUE "'".
           05 DET-TYPE            PIC X(25).
           05 FILLER              PIC X(3) VALUE "','".
           05 DET-LIBELLE         PIC X(30).
           05 FILLER              PIC X(3) VALUE "','".
           05 DET-MONTANT         PIC X(8). 
           05 FILLER              PIC X VALUE ','.
           05 DET-COMPTEID        PIC 9(3).
           05 FILLER              PIC X VALUE ','.
           05 DET-CLIENTID        PIC 9(3). 
           05 FILLER              PIC X(2) VALUE ",".
           05 DET-DATEOP          PIC X(6).
           05 FILLER              PIC X(3) VALUE "');". 


       01  SQL-UPDATE-LINE.
            05 FILLER      PIC X(33) VALUE
            'UPDATE COMPTES SET SOLDE = SOLDE '.
            05 DET-OPERATOR        PIC X.
            05 DET-uP-SOMME        PIC 9(5).99.
            05 FILLER              VALUE 
            ' WHERE COMPTEID = '.
            05 DET-UP-COMPTEID     PIC 9(3).
            05 FILLER              PIC X VALUE ";".


           copy SCREENIO.

        SCREEN SECTION.

         01 WELCOME-SCREEN.

           05 FILLER LINE 10 COLUMN 23
           VALUE "BIENVENUE A LA BANQUE KARIMUS"
           BLANK SCREEN
           BACKGROUND-COLOR COB-COLOR-WHITE
           FOREGROUND-COLOR COB-COLOR-BLACK.

         01 LEAVE-SCREEN.

           05 FILLER LINE 10 COLUMN 23
           VALUE "VOUS ALLEZ ETRE DECONNECTE"
           BLANK SCREEN
           BACKGROUND-COLOR COB-COLOR-WHITE
           FOREGROUND-COLOR COB-COLOR-BLACK.


         01 HEADER-SCREEN.

           05 FILLER LINE 2 COLUMN 23
           VALUE "SIMULATEUR GESTION CLIENTELE"
           BLANK SCREEN
           FOREGROUND-COLOR COB-COLOR-YELLOW.

         01 MAIN-FUNCTION-SCREEN.

           05 FILLER LINE 5 COLUMN 1    
           VALUE "F1 - CONSULTER LE FICHIER CLIENTELE"
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER LINE 5 COLUMN 90    
           VALUE "F2 - AJOUTER UN NOUVEAU CLIENT"
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER LINE 10 COLUMN 1                                           
           VALUE "F3 - ACCEDER A UN COMPTE"
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER LINE 10 COLUMN 90    
           VALUE "F4 - VOIR TOUTES LES OPERATIONS EN ATTENTE"
           FOREGROUND-COLOR COB-COLOR-GREEN.
           
           05 FILLER LINE 15 COLUMN 1    
           VALUE "F5 - ACCEDER A LA MESSAGERIE"
           FOREGROUND-COLOR COB-COLOR-GREEN.


           05 FILLER LINE 15 COLUMN 90    
           VALUE "F9 - Quitter le simulateur bancaire"
           FOREGROUND-COLOR COB-COLOR-GREEN.
           
           05 FILLER PIC 9(6) TO wRetCode SECURE
           LINE 40 COLUMN 79
           FOREGROUND-COLOR COB-COLOR-GREEN. 


        01 AJOUT-CLIENT-SCREEN.

            05 FILLER LINE 5 COLUMN 1
            VALUE "AJOUTER UN NOUVEAU CLIENT"
            FOREGROUND-COLOR COB-COLOR-YELLOW.

            05 FILLER LINE 7 COLUMN 1
            VALUE "SUITE BIENTOT"
            FOREGROUND-COLOR COB-COLOR-YELLOW.
                 
            05 FILLER LINE 18 COLUMN 1    
            VALUE "F9 - Revenir au sommaire "
            FOREGROUND-COLOR COB-COLOR-GREEN.

            05 FILLER PIC X TO WS-ACCEPT-FNC-KEY SECURE
            LINE 18 COLUMN 79
            FOREGROUND-COLOR COB-COLOR-GREEN. 

           
       01 FICHIER-CLIENTS-SCREEN.

           05 FILLER LINE 5 COLUMN 1
           VALUE "COMBIEN VOULEZ-VOUS EPARGNER ?:"
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

           05 FILLER PIC X(25) FROM WSMSG
           LINE 15 COLUMN 1
           FOREGROUND-COLOR COB-COLOR-RED. 

           05 FILLER LINE 18 COLUMN 1    
           VALUE "F1 : Valider - F9 : Revenir au sommaire"
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER PIC X TO WS-ACCEPT-FNC-KEY SECURE
           LINE 18 COLUMN 79
           FOREGROUND-COLOR COB-COLOR-GREEN. 

        01 ACCEDER-UN-COMPTE-SCREEN.

           05 FILLER LINE 5 COLUMN 1
           VALUE "VEUILLEZ ENTRER LE NUMERO DU COMPTE : "
           FOREGROUND-COLOR COB-COLOR-GREEN.  


           05 FILLER  PIC 9(3) TO NUMCOMPTE
           LINE 5 COLUMN 40    
           FOREGROUND-COLOR COB-COLOR-GREEN. 

           05 FILLER LINE 9 COLUMN 1
           VALUE "F1 - VOIR LE DETAIL DES OPERATIONS "
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER LINE 10 COLUMN 1    
           VALUE "F2 - VOIR LES OPERATIONS EN ATTENTE"
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER LINE 10 COLUMN 1    
           VALUE "F2 - VOIR TOUTES LES INFORMATONS"
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER PIC X(50) FROM WSMSG
           LINE 15 COLUMN 1
           FOREGROUND-COLOR COB-COLOR-RED. 

           05 FILLER PIC X TO WS-ACCEPT-FNC-KEY SECURE
           LINE 18 COLUMN 79
           FOREGROUND-COLOR COB-COLOR-GREEN. 

              
         01 TOUTES-OP-ATTENTE-SCREEN.
        
             05 FILLER LINE 5 COLUMN 1
             VALUE "OPERATIONS EN ATTENTE"
             FOREGROUND-COLOR COB-COLOR-YELLOW.
        
             05 FILLER LINE 7 COLUMN 1
             VALUE "SUITE BIENTOT"
             FOREGROUND-COLOR COB-COLOR-YELLOW.
         
        
             05 FILLER LINE 18 COLUMN 1    
             VALUE "F9 - Revenir au sommaire "
             FOREGROUND-COLOR COB-COLOR-GREEN.
        
             05 FILLER PIC X TO WS-ACCEPT-FNC-KEY SECURE
             LINE 18 COLUMN 79
             FOREGROUND-COLOR COB-COLOR-GREEN. 
        
        01 MESSAGERIE-SCREEN.
       
             05 FILLER LINE 5 COLUMN 1
             VALUE "VEUILLEZ ENTRER VOTRE LOGIN"
             FOREGROUND-COLOR COB-COLOR-GREEN.
       
             05 FILLER PIC X(10) TO LOGIN
             LINE 5 COLUMN 58    
             FOREGROUND-COLOR COB-COLOR-GREEN.
       
             05 FILLER LINE 6 COLUMN 1    
             VALUE "VEUILLEZ ENTRER VOTRE MOT DE PASSE"
             FOREGROUND-COLOR COB-COLOR-GREEN.
       
             05 FILLER PIC X(10) TO MOTDEPASSE SECURE
             LINE 6 COLUMN 531    
             FOREGROUND-COLOR COB-COLOR-GREEN.
       
             05 FILLER LINE 18 COLUMN 1    
             VALUE "F1 : Valider - F9 : Revenir au sommaire"
             FOREGROUND-COLOR COB-COLOR-GREEN.
       
             05 FILLER PIC X TO WS-ACCEPT-FNC-KEY SECURE
             LINE 18 COLUMN 79
             FOREGROUND-COLOR COB-COLOR-GREEN. 
       

       PROCEDURE DIVISION.

       0100-MAIN-PROCEDURE.
           OPEN EXTEND GENERESQL.
           OPEN OUTPUT  GENERUPDATE.

           COMPUTE MOUSE-FLAGS = COB-AUTO-MOUSE-HANDLING
                      + COB-ALLOW-LEFT-DOWN   + COB-ALLOW-MIDDLE-DOWN  
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
             


           DISPLAY HEADER-SCREEN
           DISPLAY MAIN-FUNCTION-SCREEN.
           ACCEPT MAIN-FUNCTION-SCREEN

           if(wRowR >=3 and wRowR <= 7) and (wColR >= 0 
           and wColR <= 70)
              PERFORM 0200-VOIR-FICHIER-CLIENTELE.
              Display '--------------------------' at LINE 1 COLUMN 6.

           if(wRowR >=3 and wRowR <= 7) and (wColR >= 80 
           and wColR <= 130)
           PERFORM 0220-AJOUTER-NOUVEAU-CLIENT.

           if(wRowR >=8 and wRowR <= 12) and (wColR >= 0 
           and wColR <= 70)
           PERFORM 0240-ACCEDER-UN-COMPTE.

           if(wRowR >=8 and wRowR <= 12) and (wColR >= 80 
           and wColR <= 130)
           PERFORM 0260-VOIR-TOUTES-OP-ATTENTE.

           if(wRowR >=13 and wRowR <= 17) and (wColR >= 0 
           and wColR <= 70)
           PERFORM 0280-ACCEDER-MESSAGERIE.


           if(wRowR >=13 and wRowR <= 17) and (wColR >= 80 
           and wColR <= 130)
              DISPLAY LEAVE-SCREEN.
              CALL "C$SLEEP" USING 2 END-CALL.
              STOP RUN.
           

       0200-VOIR-FICHIER-CLIENTELE.
           ACCEPT WS-CURRENT-DATE-FIELDS FROM DATE.
           DISPLAY HEADER-SCREEN.
           DISPLAY FICHIER-CLIENTS-SCREEN.
           ACCEPT FICHIER-CLIENTS-SCREEN.

       0220-AJOUTER-NOUVEAU-CLIENT.

           ACCEPT WS-CURRENT-DATE-FIELDS FROM DATE.

           DISPLAY HEADER-SCREEN.
           DISPLAY AJOUT-CLIENT-SCREEN.
           ACCEPT AJOUT-CLIENT-SCREEN.


       0240-ACCEDER-UN-COMPTE.
           
           ACCEPT WS-CURRENT-DATE-FIELDS FROM DATE.

           DISPLAY HEADER-SCREEN.
           DISPLAY ACCEDER-UN-COMPTE-SCREEN.
           ACCEPT ACCEDER-UN-COMPTE-SCREEN.

           

       0260-VOIR-TOUTES-OP-ATTENTE.

           ACCEPT WS-CURRENT-DATE-FIELDS FROM DATE.

           DISPLAY HEADER-SCREEN.
           DISPLAY TOUTES-OP-ATTENTE-SCREEN.
           ACCEPT TOUTES-OP-ATTENTE-SCREEN.


       0280-ACCEDER-MESSAGERIE.
            ACCEPT WS-CURRENT-DATE-FIELDS FROM DATE.

            DISPLAY HEADER-SCREEN.
            DISPLAY MESSAGERIE-SCREEN.
            ACCEPT MESSAGERIE-SCREEN.
                   

           
           
       0900-STOP-RUN.
           STOP RUN.
           CLOSE GENERESQL.
       end program activity2.
          


             
