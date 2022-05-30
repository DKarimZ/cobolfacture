       IDENTIFICATION DIVISION. 
       PROGRAM-ID. optaux.
       AUTHOR. D.KISAMA.

      *> Ce  programme permet de simuler une opération de calcul d'un 
      *> taux bancaire, un nouveau solde est proposé en ajoutant 
      *> un certain taux d'interet 

       DATA DIVISION.

       WORKING-STORAGE SECTION. 
      

       LINKAGE SECTION. 
          77 L-SOLDE            PIC 9(5)V99. 
          77 L-TAUXINTERET      PIC 9V99.     
          77 L-INTERETS         PIC 9(5)V99.     
            

       PROCEDURE DIVISION USING L-SOLDE,L-TAUXINTERET,
           L-INTERETS .

       0100-MAIN-PROCEDURE.

           PERFORM 0120-CALCUL-INTERETS.
        

       0120-CALCUL-INTERETS.
           COMPUTE L-INTERETS =  L-SOLDE * ( L-TAUXINTERET / 100).
           
       END PROGRAM optaux.
     
       