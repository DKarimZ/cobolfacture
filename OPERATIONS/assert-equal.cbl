       IDENTIFICATION DIVISION. 
       PROGRAM-ID. ASSERT-EQUAL.

      *> Ce programme sert à nofifier les erreurs lors de l'execution
      *> des testts unitaires 

       DATA DIVISION. 
       WORKING-STORAGE SECTION.
       LINKAGE SECTION. 
              
       COPY 'test-context.cpy'.

       01 TEST-NAME    PIC x(30).
       01 EXPECTED     PIC 9(5)V99.
       01 ACTUAL       PIC 9(5)V99.

       PROCEDURE DIVISION USING TEST-CONTEXT,TEST-NAME, EXPECTED,
        ACTUAL.

       0100-MAIN-PROCEDURE.
           ADD 1 TO TESTS-RUN.
           IF ACTUAL = EXPECTED then ADD 1 TO PASSES
           ELSE
           DISPLAY 'FAILED: ' TEST-NAME '. résultat attendu: ' EXPECTED 
              ' résultat obtenu ' ACTUAL
              ADD 1 TO FAILURES.
           GOBACK.
       END PROGRAM ASSERT-EQUAL.
