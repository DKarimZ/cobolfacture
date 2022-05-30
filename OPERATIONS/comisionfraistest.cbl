       IDENTIFICATION DIVISION.
       PROGRAM-ID. comisionfraistest.
       AUTHOR D.KISAMA.

      *> Ce programme permet d'effectuer des tests unitaires portant
      *> sur l'opération de crédit d'un compte

       DATA DIVISION. 
       WORKING-STORAGE SECTION. 

         01  WS-FIELDS.
           05 I-SOLDE        PIC 9(5)V99.
           05 I-FRAIS        PIC 9(5)V99.
           05 I-MONTANT      PIC 9(5)V99.
           05 I-MONTANTEXP   PIC 9(5)V99.
           05 I-SOMME        PIC 9(5)V99.
           05 I-TYPEOP       PIC X(30).
           05 I-FRAGFINANC   pIc X(3).
           05 I-PLAFOND      PIC  9 VALUE 8.
           05 I-TAUX         PIC 9V99.
           05 I-DATE         PIC X(10).
           05 I-LIBELLE      PIC X(30).
           05 I-LIBELLEEXP   PIC X(30).
              

       LINKAGE SECTION. 

        COPY "test-context.cpy".

        PROCEDURE DIVISION USING TEST-CONTEXT.

        0100-MAIN-TEST.
           PERFORM 0400-TEST-COMMISSION.
      *     PERFORM 0410-TEST-PREL-RATE.

           GOBACK.

        0400-TEST-COMMISSION.
           INITIALIZE WS-FIELDS.
           MOVE 1529.99 TO I-SOMME.
           MOVE 'OPPOSITION CARTE BANCAIRE' TO I-TYPEOP.
           MOVE 2.99 TO I-MONTANTEXP.
           MOVE 'OP OPPOSITION CARTE BANCAIRE' TO I-LIBELLEEXP.

           CALL 'commisionfrais' USING I-MONTANT, I-SOMME,
           I-TYPEOP,I-FRAGFINANC  , I-PLAFOND, I-DATE, I-LIBELLE.

           CALL 'ASSERT-EQUAL3' USING TEST-CONTEXT, 
              'TEST-COMMISSION', I-MONTANTEXP, I-MONTANT,
               I-LIBELLEEXP, I-LIBELLE.

  

           END PROGRAM comisionfraistest.
