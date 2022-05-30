       IDENTIFICATION DIVISION. 
       PROGRAM-ID. optauxtest.
       AUTHOR. D.KISAMA. 

      *> Ce program permet de réaliser un test unitaire par rapport au
      *> calcul du nouveau solde après un ajout d'untérêts 

       DATA DIVISION. 
       WORKING-STORAGE SECTION.

       01 WS-FIELDS.
        05 I-SOLDE            PIC 9(5)V99.
        05 I-INTERETS         PIC 9(5)V99.
        05 I-INTERETS-EXP     PIC 9(5)V99.
        05 I-TAUXINTERET      PIC 9V99.  

       LINKAGE SECTION.

       COPY "test-context.cpy".

       PROCEDURE DIVISION USING TEST-CONTEXT.

       0100-MAIN-TEST.
           PERFORM 0400-BON-TAUX.
           PERFORM 0420-BON-TAUX-2.

           GOBACK.

       0400-BON-TAUX.
          INITIALIZE WS-FIELDS.
           MOVE 7.28 TO I-TAUXINTERET.    
           MOVE 1589.21 TO I-SOLDE.
           MOVE 115.69 TO I-INTERETS-EXP.

           CALL 'optaux' USING I-SOLDE, I-TAUXINTERET,I-INTERETS.

           CALL 'ASSERT-EQUAL' USING TEST-CONTEXT, '0400-BON-TAUX',
           I-INTERETS-EXP, I-INTERETS.

       0420-BON-TAUX-2.
          INITIALIZE WS-FIELDS.
           MOVE 4.81 TO I-TAUXINTERET.
           MOVE 2078.10 TO I-SOLDE.
           MOVE 99.95 TO I-INTERETS-EXP.

            CALL 'optaux' USING I-SOLDE, I-TAUXINTERET,I-INTERETS.

            CALL 'ASSERT-EQUAL' USING TEST-CONTEXT, '0420-BON-TAUX-2',
           I-INTERETS-EXP, I-INTERETS.

           END PROGRAM optauxtest.

