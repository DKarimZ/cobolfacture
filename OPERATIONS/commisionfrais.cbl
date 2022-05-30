       IDENTIFICATION DIVISION.
       PROGRAM-ID. commisionfrais.
       AUTHOR. D.KISAMA.

      *> Ce programme permet d'effectuer une opération bancaire dans
      *> le but de cérditer un compte

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION. 
       LINKAGE SECTION. 
        77 L-SOLDE        PIC 9(5)V99.
        77 L-MONTANT      PIC 9(5)V99.
        77 L-SOMME        PIC 9(5)V99.
        77 L-TYPEOP       PIC X(30).
        77 L-FRAGFINANC   pIc X(3).
        77 L-PLAFOND      PIC 9 VALUE 8.
        77 L-DATE         PIC X(10).
        77 L-LIBELLE      PIC X(30).
        

        PROCEDURE DIVISION USING L-MONTANT, L-SOMME,
         , L-TYPEOP, L-FRAGFINANC, L-PLAFOND, L-DATE,L-LIBELLE.

        0100-MAIN-MPROCEDURE.
           
            IF(L-TYPEOP = 'REJET PRELEVEMENT') THEN

              COMPUTE L-MONTANT =  4.99 + (0.1 * L-SOMME)
              MOVE 'OP REJET PRELEVEMENT' TO L-LIBELLE 

            ELSE IF(L-TYPEOP = 'REJET CHEQUE BANQUE') THEN
                      
               COMPUTE L-MONTANT = 5.99 + (0.1 * L-SOMME)
               MOVE 'OP REJET CHEQUE BANQUE' TO L-LIBELLE
                           

            ELSE IF(L-TYPEOP = 'OPPOSITION CARTE BANCAIRE') 

                IF (L-SOMME > 15000.00)
                   COMPUTE L-MONTANT = 5.99 
                ELSE
                   COMPUTE L-MONTANT = 2.99
                END-IF

                MOVE 'OP OPPOSITION CARTE BANCAIRE' TO L-LIBELLE

            ELSE IF(L-TYPEOP = 'OPPOSITION CHEQUE BANQUE')


              COMPUTE L-MONTANT = 2.99 + (0.05 * L-SOMME)
              MOVE 'OP OPPOSITION CHEQUE BANQUE' TO L-LIBELLE

            ELSE
              
              COMPUTE L-MONTANT = 1.99
              MOVE 'OP OPERATION SNAS SOLDE' TO L-LIBELLE
         
           
           END-IF.
           
           

       END PROGRAM commisionfrais.
