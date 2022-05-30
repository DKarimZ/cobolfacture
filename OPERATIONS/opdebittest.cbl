       IDENTIFICATION DIVISION.
       PROGRAM-ID. opdebittest.
       AUTHOR D.KISAMA.

      * Ce programme permet d'effectuer un test unitaire sur une 
      *> opération de débit.

       DATA DIVISION. 
       WORKING-STORAGE SECTION. 
         01  WS-FIELDS.
           05 I-SOLDE              PIC 9(5)V99.
           05 I-DEBIT              PIC 9(5)V99.
           05 I-NEWSOLDE-EXP       PIC 9(5)V99. 
           05 I-NEWSOLDE           PIC 9(5)V99. 

       LINKAGE SECTION. 

        COPY "test-context.cpy".

        PROCEDURE DIVISION USING TEST-CONTEXT.
        0100-MAIN-TEST.

           PERFORM 0400-DEBIT-PETITE-SOMME.
           PERFORM 0420-DEBIT-GROSSE-SOMME.
           GOBACK.

        0400-DEBIT-PETITE-SOMME.
           INITIALIZE WS-FIELDS.
           MOVE 3200.15 TO I-SOLDE.
           MOVE 139.78 TO I-DEBIT.
           MOVE 3060.37 TO I-NEWSOLDE-EXP.
           CALL 'opdebit' USING I-SOLDE, I-DEBIT, I-NEWSOLDE.
           CALL 'ASSERT-EQUAL' USING TEST-CONTEXT, 
              '0400-DEBIT-PETITE-SOMME', I-NEWSOLDE-EXP, I-NEWSOLDE.

        0420-DEBIT-GROSSE-SOMME.
           INITIALIZE WS-FIELDS.
           MOVE 58460.17 TO I-SOLDE.
           MOVE 41099.70 TO I-DEBIT.
           MOVE 17360.47 TO I-NEWSOLDE-EXP.
           CALL 'opdebit' USING I-SOLDE, I-DEBIT, I-NEWSOLDE.
           CALL 'ASSERT-EQUAL' USING TEST-CONTEXT, 
              '0420-DEBIT-GROSSE-SOMME', I-NEWSOLDE-EXP, I-NEWSOLDE. 

           END PROGRAM opdebittest.
