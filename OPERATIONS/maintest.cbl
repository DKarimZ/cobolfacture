       IDENTIFICATION DIVISION. 
       PROGRAM-ID. MAINTEST.
       *> Ce programme a pour but de lancer les tests unitaires
       *> en faisant appel aux différentssous-programmes 
       DATA DIVISION. 
       FILE SECTION. 
       WORKING-STORAGE SECTION. 
       COPY "test-context.cpy".
       PROCEDURE DIVISION.
       0100-MAIN.
           DISPLAY "Runniing the validation tests....".
           CALL 'optauxtest' USING TEST-CONTEXT.
           CALL 'opcredittest' USING TEST-CONTEXT.
           CALL 'opdebittest' USING TEST-CONTEXT.
           CALL 'opprelevtest' USING TEST-CONTEXT.
           CALL 'comisionfraistest' USING TEST-CONTEXT.
           DISPLAY 'Tests run: ' TESTS-RUN.
           DISPLAY 'Passed: ' PASSES.
           DISPLAY 'Failed: ' FAILURES.
           
       END PROGRAM MAINTEST.