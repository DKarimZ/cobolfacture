       IDENTIFICATION DIVISION.
       PROGRAM-ID. opcredittest.
       AUTHOR D.KISAMA.

      *> Ce programme permet d'effectuer des tests unitaires portant
      *> sur l'opération de crédit d'un compte

       DATA DIVISION. 
       WORKING-STORAGE SECTION. 

         01  WS-FIELDS.
           05 I-SOLDE              PIC 9(5)V99.
           05 I-CREDIT             PIC 9(5)V99.
           05 I-NEWSOLDE-EXP       PIC 9(5)V99. 
           05 I-NEWSOLDE           PIC 9(5)V99. 

       LINKAGE SECTION. 

        COPY "test-context.cpy".

        PROCEDURE DIVISION USING TEST-CONTEXT.

        0100-MAIN-TEST.
           PERFORM 0400-AJOUT-PETITE-SOMME.
           PERFORM 0420-AJOUT-GROSSE-SOMME.

           GOBACK.

        0400-AJOUT-PETITE-SOMME.
           INITIALIZE WS-FIELDS.
           MOVE 3207.24 TO I-SOLDE.
           MOVE 39.99 TO I-CREDIT.
           MOVE 3247.23 TO I-NEWSOLDE-EXP.

           CALL 'opcredit' USING I-SOLDE, I-CREDIT, I-NEWSOLDE.

           CALL 'ASSERT-EQUAL' USING TEST-CONTEXT, 
              '0400-AJOUT-PETITE-SOMME', I-NEWSOLDE-EXP, I-NEWSOLDE.


        0420-AJOUT-GROSSE-SOMME.
            INITIALIZE WS-FIELDS.
             MOVE 15460.24 TO I-SOLDE.
             MOVE 90789.06 TO I-CREDIT.
             MOVE 106249.30 TO I-NEWSOLDE-EXP.

           CALL 'opcredit' USING I-SOLDE, I-CREDIT, I-NEWSOLDE.

           CALL 'ASSERT-EQUAL' USING TEST-CONTEXT, 
              '0400-AJOUT-GROSSE-SOMME', I-NEWSOLDE-EXP, I-NEWSOLDE.  

           END PROGRAM opcredittest.
