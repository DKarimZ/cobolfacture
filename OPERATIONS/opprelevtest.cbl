       IDENTIFICATION DIVISION.
       PROGRAM-ID. opprelevtest.
       AUTHOR D.KISAMA.

      *> Ce programme permet d'effectuer des tests unitaires portant
      *> sur l'opération de crédit d'un compte

       DATA DIVISION. 
       WORKING-STORAGE SECTION. 

         01  WS-FIELDS.
           05  I-SOLDE          PIC 9(5)V99.
           05  I-SOMME          PIC 9(5)V99.
           05  I-NEWSOLDE       PIC 9(5)V99.
           05  I-NEWSOLDE-EXP   PIC 9(5)V99.
           05  I-SUCCESS        PIC X(3).
           05  I-SUCCESS-EXP    PIC X(3).
           05  I-COMM           PIC 9V99.
           05  I-DATE           PIC X(10).
           05  I-TYPEOP         PIC X(20).
           05  I-LIBELLE        PIC X(25).
           05  I-LIBELLE-EXP    PIC X(25).
              

       LINKAGE SECTION. 

        COPY "test-context.cpy".

        PROCEDURE DIVISION USING TEST-CONTEXT.

        0100-MAIN-TEST.
           PERFORM 0400-TEST-PREL-REUSSI.
      *     PERFORM 0410-TEST-PREL-RATE.

           GOBACK.

        0400-TEST-PREL-REUSSI.
           INITIALIZE WS-FIELDS.
           MOVE 750.24 TO I-SOLDE.
           MOVE 39.99 TO I-SOMME.
           MOVE 'VIREMENT OCCASIONNEL' TO I-TYPEOP.
           MOVE 710.25 TO I-NEWSOLDE-EXP.
           MOVE 'OUI' TO I-SUCCESS-EXP.
           MOVE 'OP VIREMENT OCCASIONNEL' TO I-LIBELLE-EXP.
           MOVE 0 TO I-COMM.
           MOVE '' TO I-DATE.

           CALL 'opprelev' USING I-SOLDE, I-SOMME,I-NEWSOLDE,
           I-TYPEOP,I-SUCCESS, I-COMM , I-DATE, I-LIBELLE.

           CALL 'ASSERT-EQUAL2' USING TEST-CONTEXT, 
              'TEST-PREL-REUSSI', I-NEWSOLDE-EXP, I-NEWSOLDE,
              I-SUCCESS-EXP, I-SUCCESS, I-LIBELLE-EXP, I-LIBELLE.


      *  0410-TEST-PREL-RATE.
      *      INITIALIZE WS-FIELDS.
      *      MOVE 50.24 TO I-SOLDE.
      *      MOVE 69.99 TO I-SOMME.
      *      MOVE 45.74 TO I-NEWSOLDE-EXP.
      *      MOVE 'NOM' TO I-SUCCESS-EXP.
      *      MOVE 4.50 TO I-COMM.
      *      MOVE '' TO I-DATE.
      *      CALL 'opprelev' USING I-SOLDE, I-SOMME, I-NEWSOLDE,
      *      I-SUCCESS, I-COMM , I-DATE.
      *      CALL 'ASSERT-EQUAL2' USING TEST-CONTEXT, 
      *         'TEST-PREL-RATE', I-NEWSOLDE-EXP, I-NEWSOLDE,
      *         I-SUCCESS-EXP, I-SUCCESS.








  

           END PROGRAM opprelevtest.
