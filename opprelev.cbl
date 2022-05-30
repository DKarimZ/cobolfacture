       IDENTIFICATION DIVISION.
       PROGRAM-ID. opprelev.
       AUTHOR. D.KISAMA.

      *> Ce programme permet d'effectuer une opération bancaire dans
      *> le but de cérditer un compte

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION. 
       LINKAGE SECTION. 
        77 L-SOLDE        PIC 9(5)V99.
        77 L-SOMME        PIC 9(5)V99.
        77 L-NEWSOLDE     PIC 9(5)V99.
        77 L-TYPEOP       PIC X(20).
        77 L-SUCCESS      PIC X(3).
        77 L-COMM         PIC 9V99.
        77 L-DATE         PIC X(10).
        77 L-LIBELLE      PIC X(25).
        

        PROCEDURE DIVISION USING L-SOLDE, L-SOMME,L-NEWSOLDE, L-TYPEOP, 
        L-SUCCESS, L-COMM, L-DATE, L-LIBELLE.

        0100-MAIN-MPROCEDURE.
           IF(L-SOLDE >= L-SOMME)
              COMPUTE L-NEWSOLDE = L-SOLDE - L-SOMME
              MOVE 'OUI' TO L-SUCCESS
              If(L-TYPEOP = 'VIREMENT OCCASIONNEL')
                 MOVE 'OP VIREMENT OCCASIONNEL' TO L-LIBELLE 
              ELSE IF(L-TYPEOP = 'VIREMENT PERMANENT')
                 MOVE 'OP VIREMENT PERMANENT' TO L-LIBELLE
              ELSE
                  MOVE 'OP VIREMENT ' TO L-LIBELLE
             END-IF 
           ELSE
              COMPUTE L-NEWSOLDE = L-SOLDE - L-COMM
              MOVE 'NON' TO L-SUCCESS
              MOVE 'FRAIS REJET PRELEVEMENT'  TO L-LIBELLE
           END-IF.

       END PROGRAM opprelev.
