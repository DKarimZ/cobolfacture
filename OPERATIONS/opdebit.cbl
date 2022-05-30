       IDENTIFICATION DIVISION.
       PROGRAM-ID. opdebit.
       AUTHOR. D.KISAMA.

      * Ce programme permet d'effectuer une opération bancaire dans le
      * but de débiter un compte

       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION. 

       LINKAGE SECTION. 
        77 L-SOLDE        PIC 9(5)V99.
        77 L-CREDIT       PIC 9(5)V99.
        77 L-NEWSOLDE     PIC 9(5)V99.

        PROCEDURE DIVISION USING L-SOLDE, L-CREDIT, L-NEWSOLDE.
        0100-MAIN-MPROCEDURE.

           COMPUTE L-NEWSOLDE = L-SOLDE - L-CREDIT.

       END PROGRAM opdebit.
       