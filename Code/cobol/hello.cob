      *> filename: hello.cob
      *> cobc -x -free hello.cob -o hi
      *> ./hi
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLOWORLD.
       PROCEDURE DIVISION.
       DISPLAY 'Hello, World!'.
       STOP RUN.
      *> Outputs: Hello, World!
