       IDENTIFICATION DIVISION.
       PROGRAM-ID. commisionfrais.
       AUTHOR. D.KISAMA.

      *> Ce programme permet d'effectuer une opération bancaire dans
      *> le but de cérditer un compte

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION. 
       LINKAGE SECTION. 
        77 L-SOLDE        PIC 9(5).99.
        77 L-MONTANT      PIC 9(5).99.
        77 L-SOMME        PIC X(10).
        77 L-TYPEOP       PIC X(25).
        77 L-FRAGFINANC   pIc X(3).
        77 L-PLAFOND      PIC 9 VALUE 8.
        77 L-DATE         PIC X(10).
        77 L-LIBELLE      PIC X(30).
        

        PROCEDURE DIVISION USING L-MONTANT, L-SOMME
         , L-TYPEOP, L-FRAGFINANC, L-PLAFOND, L-DATE,L-LIBELLE.

        0100-MAIN-MPROCEDURE.
           
            IF(L-TYPEOP = 'REJET PRELEVEMENT') THEN

              MOVE 4.99  TO L-MONTANT
              MOVE 'OP REJET PRELEVEMENT' TO L-LIBELLE 

            ELSE IF(L-TYPEOP = 'REJET CHEQUE BANQUE') THEN
                      
               MOVE 5.99 TO L-MONTANT
               STRING 'OP REJET CHEQUE BANQUE ' L-SOMME 
               DELIMITED BY SIZE INTO L-LIBELLE
               
               

            ELSE IF(L-TYPEOP = 'OPPOSITION CARTE BANCAIRE') 

                MOVE 6.99 TO L-MONTANT
                
                MOVE 'OP OPPOSITION CARTE BANCAIRE' TO L-LIBELLE

            ELSE IF(L-TYPEOP = 'OPPOSITION CHEQUE BANQUE ')


                MOVE 7.99 TO L-MONTANT 
              If L-MONTANT > 8.00 MOVE 8.00 TO L-MONTANT

              MOVE 5.99 TO L-MONTANT
                 STRING 'OP OPPOSITION CHEQUE BANQUE ' L-SOMME
                 DELIMITED BY SIZE INTO L-LIBELLE

            ELSE
              
              COMPUTE L-MONTANT = 1.99
              MOVE 'OP OPERATION SANS SOLDE' TO L-LIBELLE
         
           
           END-IF.
           
           

       END PROGRAM commisionfrais.
