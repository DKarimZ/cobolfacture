       IDENTIFICATION DIVISION.
       PROGRAM-ID. facturee.
       AUTHOR. D.KISAMA.

      * Ce programme permet dà partir d'écrans de procéder à différentes
      *> opérations bancaires telles que
      *> _ créditer une some
      *> _ Faire un achat d'un nvéhicule
      *> _ afficher la derière opération bacaire effectué

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
          SPECIAL-NAMES.
             CRT STATUS IS WS-FNC-KEY. 
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
             SELECT PRINT-RELEVE ASSIGN TO "PRINT-RELEVE.DAT"
                 ORGANIZATION IS SEQUENTIAL.1

             SELECT PRINT-FILE ASSIGN TO "PRINTFILE.DAT"
                 ORGANIZATION IS LINE SEQUENTIAL.

             SELECT GENERESQL  ASSIGN TO "GENERESQL.DAT"
                ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION. 

       FILE SECTION.
       FD PRINT-RELEVE.
           01 RELEVE-OP      PIC X(38).

       FD GENERESQL.
           01 PRINT-LINE     PIC X(350).

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
            05 D-COMPTE-SOLDE            PIC 9(5)V99.
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


       01 Ws-FIELDS.
           05 RESULTATCRE    PIC 9(5)V99.
           05 RESULTATDEB    PIC 9(5)V99.
           05 SOLDE          PIC 9(5)V99.
           05 CREDIT         PIC 9(5)V99.
           05 DEBIT          PIC 9(5)V99.

       01 WS-VALUES.
           05 MONSOLDE       PIC 9(5)V99.
           05 voiture        PIC 9(5) VALUE 30000.
           05 Moto           PIC 9(5) VALUE 15000.
           05 velo           PIC 9(5) VALUE 500.
           05 MSG-CREDIT      PIC X(30).
           05 MSG-SOMME       PIC 9(5)V99.
           05 WSMSG           PIC X(50).
           05 LINE-NUMBER     PIC 99.
           05 SOMMECREDITEE   PIC 9(5)V99.
           05 SOMMEEPARGNEE   PIC 9(5)V99.
           05 SOMMESFR        PIC 9(5)V99.
           05 LIBELLEVIREM    PIC X(50).
           05 DATEVIREM       PIC X(10).
           05 DATEPRELEVSFR   PIC X(10).
           05 REPONSESFR      PIC X(3).
           05 PRODUITACHETEE  PIC X(15).
           05 NOMMAGASIN      PIC X(15).


          01 WS-FIELDS2.
             05 CLIENT-ID               PIC S9(3).
             05 CLIENT-NOM              PIC X(20).
             05 CLIENT-PRENOM           PIC X(20).
             05 CLIENT-RSOCIALE         PIC X(20).
             05 CLIENT-TYPECLIENT       PIC X(20).
             05 CLIENT-EXTERNE          PIC 9.
             05 COMPTE-ID               PIC S9(3).
             05 COMPTE-IBAN             PIC X(25).
             05 COMPTE-DTOUV            PIC X(14).
             05 COMPTE-SOLDE            PIC 9(5)V99.
             05 FILLER                  PIC X(3).
             05 COMPTE-CLIENTID         PIC S9(3).
            

       
       01 DETAIL-LINE.
           05 DET-NAME-OP       PIC X(25).
           05 FILLER            PIC X(3) VALUES SPACES.
           05 DET-MONTANT-OP    PIC 9(5)V99.
           05 FILLER            PIC X(1) VALUe ' '.
           05 DET-TIME          PIC 9(2).

       01  WS-CURRENT-DATE-FIELDS.
           05  WS-DATE.
               10  WS-DAY    PIC  9(2).
               10  WS-MONTH   PIC  9(2).
               10  WS-YEAR     PIC  9(2).
           05  WS-TIME.
               10  WS-HOUR    PIC  9(2).
               10  WS-MINUTE  PIC  9(2).
               10  WS-SECOND  PIC  9(2).
               10  WS-MS      PIC  9(2).
           05  WS-DIFF-FROM-GMT       PIC S9(4).

       01 SQL-DETAIL-LINE.
           05 FILLER          PIC X VALUE
           'INSERT INTO OPERATIONS (IDOPERATION,TYPE,LIBELLE,MONTANT,
      -     'COMPTEID,IDCLIENT,DATEOP) VALUES('.
           05 DET-IDOPERAATION    PIC 9(3).
           05 FILLER              PIC X VALUE ','.
           05 DET-TYPE            PIC X(50).
           05 FILLER              PIC X VALUE ','.
           05 DET-LIBELLE         PIC X(50).
           05 FILLER              PIC X VALUE ','.
           05 DET-MONTANT         PIC 9(6)V9(2). 
           05 FILLER              PIC X VALUE ','.
           05 DET-COMPTEID        PIC 9(3).
           05 FILLER              PIC X VALUE ','.
           05 DET-CLIENTID        PIC 9(3). 
           05 FILLER              PIC X VALUE ','.
           05 DET-DATEOP          PIC X(10).
           05 FILLER              PIC X(2) VALUE ');'. 

           copy SCREENIO.

       SCREEN SECTION.

         01 HEADER-SCREEN.

           05 FILLER LINE 2 COLUMN 13
           VALUE "Simulateur d'opérations bancaire
           BLANK SCREEN
           FOREGROUND-COLOR COB-COLOR-YELLOW.

       01 MAIN-FUNCTION-SCREEN.

           05 FILLER LINE 5 COLUMN 1    
           VALUE "F1 - crediter mon compte courant depuis mon épargne)"
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER LINE 6 COLUMN 1    
           VALUE "F2 - Créditer mon compte épargne depuis mon CCourant)"
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER LINE 7 COLUMNbv                                                                                                                                                       11    
           VALUE "F3 - Cosulter mon relevé"
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER LINE 8 COLUMN 1    
           VALUE "F4 - Retirer de l'argent"
           FOREGROUND-COLOR COB-COLOR-GREEN.
           
           05 FILLER LINE 9 COLUMN 1    
           VALUE "F5 - Recharger ma carte SFR"
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER LINE 10 COLUMN 1    
           VALUE "F6 - faire un achat chez un partenaire"
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER LINE 11 COLUMN 1    
           VALUE "F9 - Quitter le simulateur bancaire"
           FOREGROUND-COLOR COB-COLOR-GREEN.
           
           05 FILLER PIC X TO WS-ACCEPT-FNC-KEY SECURE
           LINE 18 COLUMN 79
           FOREGROUND-COLOR COB-COLOR-GREEN. 

           
       01 EPARGNER-SCREEN.

           05 FILLER LINE 5 COLUMN 1
           VALUE "COMBIEN VOULEZ-VOUS EPARGNER ?:"
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER PIC 9(5)V99 TO SOMMEEPARGNEE
           LINE 5 COLUMN 38    
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER LINE 6 COLUMN 1    
           VALUE "AJOUTER UN LIBELLE (OPTIONNEL)"
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER PIC X(50) TO LIBELLEVIREM
           LINE 6 COLUMN 38    
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER LINE 7 COLUMN 1    
           VALUE "AJOUTER UNE DATE (OPTIONNEL)"
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER PIC X(10) TO DATEVIREM
           LINE 7 COLUMN 38    
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER LINE 18 COLUMN 1    
           VALUE "F1 : Valider - F9 : Revenir au sommaire"
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER PIC X TO WS-ACCEPT-FNC-KEY SECURE
           LINE 18 COLUMN 79
           FOREGROUND-COLOR COB-COLOR-GREEN. 

       01 CREDITERCC-SCREEN.

           05 FILLER LINE 5 COLUMN 1
           VALUE "COMBIEN VOULEZ-VOUS CREDITER SUR LE COMPTE COURANT ?:"
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER PIC 9(5)V99 TO SOMMECREDITEE
           LINE 5 COLUMN 58    
           FOREGROUND-COLOR COB-COLOR-GREEN.1111111

           05 FILLER LINE 6 COLUMN 1    
           VALUE "AJOUTER UN LIBELLE (OPTIONNEL)"
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER PIC X(50) TO LIBELLEVIREM
           LINE 6 COLUMN 38    
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER LINE 7 COLUMN 1    
           VALUE "AJOUTER UNE DATE (OPTIONNEL)"
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER PIC X(10) TO DATEVIREM
           LINE 7 COLUMN 38    
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER LINE 18 COLUMN 1    
           VALUE "F1 : Valider - F9 : Revenir au sommaire"
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER PIC X TO WS-ACCEPT-FNC-KEY SECURE
           LINE 18 COLUMN 79
           FOREGROUND-COLOR COB-COLOR-GREEN. 

              
       01 RELEVE-SCREEN.

           05 FILLER LINE 5 COLUMN 1
           VALUE "DETAIL DES OPERATIONS"
           FOREGROUND-COLOR COB-COLOR-YELLOW.

           05 FILLER LINE 7 COLUMN 1
           VALUE "SOMME DISPONIBLE :"
           FOREGROUND-COLOR COB-COLOR-YELLOW.

           05 FILLER PIC 9(5) FROM SOLDE 
           LINE 7 COLUMN 21
           FOREGROUND-COLOR COB-COLOR-WHITE. 


           05 FILLER LINE 18 COLUMN 1    
           VALUE "F9 - Revenir au sommaire "
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER PIC X TO WS-ACCEPT-FNC-KEY SECURE
           LINE 18 COLUMN 79
           FOREGROUND-COLOR COB-COLOR-GREEN. 

       01 CARTESFR-SCREEN.

            05 FILLER LINE 5 COLUMN 1
            VALUE "COMBIEN SOUHAITEZ-VOUS CREDITER SUR VOTRE CARTE SFR"
            FOREGROUND-COLOR COB-COLOR-GREEN.

            05 FILLER PIC 9(2) TO SOMMESFR
            LINE 5 COLUMN 58    
            FOREGROUND-COLOR COB-COLOR-GREEN.1

            05 FILLER LINE 6 COLUMN 1    
            VALUE "SOUHAITEZ-VOUS ETRE PRELEVE MENSUELLEMENT ?"
            FOREGROUND-COLOR COB-COLOR-GREEN.

            05 FILLER PIC X(3) TO REPONSESFR
            LINE 6 COLUMN 531    
            FOREGROUND-COLOR COB-COLOR-GREEN.

            05 FILLER LINE 7 COLUMN 1    
            VALUE "AJOUTER UNE DATE (OPTIONNEL)"
            FOREGROUND-COLOR COB-COLOR-GREEN.

            05 FILLER PIC X(10) TO DATEPRELEVSFR
            LINE 7 COLUMN 38    
            FOREGROUND-COLOR COB-COLOR-GREEN.

            05 FILLER LINE 18 COLUMN 1    
            VALUE "F1 : Valider - F9 : Revenir au sommaire"
            FOREGROUND-COLOR COB-COLOR-GREEN.

            05 FILLER PIC X TO WS-ACCEPT-FNC-KEY SECURE
            LINE 18 COLUMN 79
            FOREGROUND-COLOR COB-COLOR-GREEN. 


        01 ACHAT-CHEZ-PARTNER.

           05 FILLER LINE 5 COLUMN 1
           VALUE "QUE VOULEZ VOUS ACHETER ?:"
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER PIC X(15) TO PRODUITACHETEE
           LINE 5 COLUMN 34    
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER LINE 6 COLUMN 1    
           VALUE "QUEL EST SON PRIX ?"
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER PIC 9(5)V99 TO PRIXPRODUIT
           LINE 6 COLUMN 38    
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER LINE 7 COLUMN 1    
           VALUE "NOM DU MAGASIN (POUR REDUCTION PARTENAIRE)"
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER PIC X(15) TO NOMMAGASIN
           LINE 7 COLUMN 45    
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER LINE 18 COLUMN 1    
           VALUE "F1 : Valider - F9 : Revenir au sommaire"
           FOREGROUND-COLOR COB-COLOR-GREEN.

           05 FILLER PIC X TO WS-ACCEPT-FNC-KEY SECURE
           LINE 18 COLUMN 79
           FOREGROUND-COLOR COB-COLOR-GREEN. 


       PROCEDURE DIVISION  .

       0100-MAIN-PROCEDURE.
           OPEN INPUT PRINT-FILE.
           READ PRINT-FILE 
            AT END SET ENDOFFILE TO TRUE
            END-READ.
           MOVE D-COMPTE-SOLDE TO COMPTE-SOLDE.
           MOVE D-CLIENT-ID TO CLIENt-ID.
           
           OPEN OUTPUT PRINT-RELEVE.
           MOVE 8 TO LINE-NUMBER.

           PERFORM FOREVER

           DISPLAY HEADER-SCREEN
           DISPLAY MAIN-FUNCTION-SCREEN
           ACCEPT MAIN-FUNCTION-SCREEN

           EVALUATE TRUE
               WHEN V-FNC-F1
                  PERFORM 0200-CREDITER-CC
               WHEN V-FNC-F2    
                   PERFORM 0220-CREDITER-CE
               WHEN V-FNC-F3    
                    PERFORM 0230-CONSULTER-SOLDE
               WHEN V-FNC-F3    
                    PERFORM 0240-RETIRER-ARGENT-CC
               WHEN V-FNC-F3    
                    PERFORM 0250-RECHARGE-SFR
               WHEN V-FNC-F3    
                    PERFORM 0260-ACHAT-PARTENAIRE
               WHEN V-FNC-F9
                    EXIT PERFORM

               WHEN OTHER
                  DISPLAY "Please select a valid function key" 
           END-EVALUATE

           END-PERFORM.
           CLOSE PRINT-RELEVE.
           EXIT PROGRAM.

       0200-CREDITER-CC.
           DISPLAY HEADER-SCREEN.
           DISPLAY CREDITERCC-SCREEN.
           ACCEPT CREDItERCC-SCREEN.
           CALL 'opcredit' USING COMPTE-SOLDE, SOLDECREDITEE,
           COMPTE-SOLDE.
           MOVE  "VIREMENT 012154"  TO LIBELLE-1. 
           MOVE  SOMMECREDITEE TO SOMME-1.
           MOVE CLIENT-ID TO CLIENT-ID-1. 
           MOVE MSG-SOMME TO DET-MONTANT-OP. 
           MOVE DETAIL-LINE TO RELEVE-OP.
           WRITE RELEVE-OP.
          

       0220-ACHETER.

           PERFORM FOREVER

           DISPLAY HEADER-SCREEN
           DISPLAY ACHAT-SCREEN
           ACCEPT ACHAT-SCREEN

           EVALUATE TRUE
               WHEN V-FNC-F1
                   MOVE 30000 TO DEBIT
               WHEN V-FNC-F2    
                   MOVE 15000 TO DEBIT
               WHEN V-FNC-F3    
                   MOVE 500 TO DEBIT
               WHEN V-FNC-F9
                  EXIT PERFORM
              WHEN OTHER
                DISPLAY "Please select a valid function key"
               END-EVALUATE

           END-PERFORM.

           ACCEPT WS-TIME FROM DATE.
           MOVE WS-TIME TO DET-TIME.
           
          
           CALL 'opdebit' USING SOLDE, DEBIT, RESULTATCRE.

           MOVE RESULTATCRE TO SOLDE.
           MOVE  "COMPTE DEBITE DE   "  TO MSG-CREDIT. 
           MOVE  DEBIt TO MSG-SOMME. 
           MOVE MSG-CREDIT TO DET-NAME-OP. 
           MOVE MSG-SOMME TO DET-MONTANT-OP.
           MOVE DETAIL-LINE TO RELEVE-OP.
           WRITE RELEVE-OP . 
          

       0230-RELEVE.

           PERFORM FOREVER

           DISPLAY HEADER-SCREEN
           DISPLAY RELEVE-SCREEN
           ACCEPT RELEVE-SCREEN

           EVALUATE TRUE
              WHEN V-FNC-F9
                 EXIT PERFORM
              WHEN OTHER
               DISPLAY "Please select a valid function key"
           END-EVALUATE

           END-PERFORM.

       end program facturee.
          


             
