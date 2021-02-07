# CMSC389B Special Topics in Computer Science; A Tour of Programming Languages
## Procedural Languages
**Procedure**: set of computational steps. Idea is to write code as a list of computational step.
### Imperative Languages
Superset of Procedural Languages in that they use statements and code to change a program's state. Imperative languages are typically machine-readable and are build for hardware execution. However, these languages are typically abstracted into things like Procedural Languages or Object Oriented Programming.
### Structure and Control Flow
**Control Flow**: how a language flows through execution. Can be:
* **structured**: uses if-then-else, while loops, or for loops to handle control flow
* **unstructured**: uses jump labels instruction address to handle control flow
### COBOL
Common Business-Oriented Language can either be written in
* **fixed** format: each typed statement must fit in an 80-character row
* **free** format: has no size restriction
```COBOL
*> filename: hello.cob
 IDENTIFICATION DIVISION.
 PROGRAM-ID. HELLOWORLD.
 PROCEDURE DIVISION.
 DISPLAY 'Hello, World!'.
 STOP RUN.
```
Can be compiled using `cobc -x -free hello.cob -o hi` and run using `./hi`. Outputs:
```
Hello, World!
```
COBOL is **compiled** using cobc. The -free tag tells cobc that we are using free format. COBOL programs are separated into **divisions**:
* **Identification** division: the only mandatory division that gives information about the program itself.
* **Environment** division: gives information about the computer and necessary I/O
* **Data** division: contains variables
* **Procedure** division: contains the execution statements and procedures.
#### Arithmetic/Data
COBOL variables are globally scoped: they must be defined in the `WORKING-STORAGE SECTION` inside the `DATA DIVISION` and are visible in the entire `PROCEDURE DIVISION`. Variables can be declared like so:
```COBOL
01 VAR-NAME Pic X(14) VALUE 'hunter2'.
```
* `01`: says it's a top-level variable. Used for record like variable construction (more below)
* `VAR-NAME`: the variable name
* `PIC`: allows you to specify variable type
* `X(14)`: actual variable type
* (optional) `VALUE`: whatever follows is put into the variable

Basic data types. The length in bytes follows the data type declaration in parenthesis.
* 9 $\rightarrow$ numeric
* A $\rightarrow$ alphabetic
* X $\rightarrow$ alphanumeric
* V, S, P $\rightarrow$ special decimal/sign/precision types

Level numbers (these allow for record-like structure in variables):
* 01: record/group desc entry
* 02-49: group/elementary items
* 66: rename clause items
* 77: items without sub-divsion
* 88: condition name entry

```COBOL
*> cobc -x -free variables.cob
 IDENTIFICATION DIVISION.
 PROGRAM-ID. VARIABLEES.
 DATA DIVISION.
     WORKING-STORAGE SECTION.
     01 VAR-STR PIC X(8) VALUE '1234abcd'.
     01 VAR-YEET.
         02 YEET-A PIC 9(2).
         02 YEET-X.
             05 YEET-X-A PIC A(8) VALUE 'sub'.
             05 YEET-X-B PIC A(8) VALUE 'part'.
         02 YEET-B PIC X(4) VALUE '$2ab'.
 PROCEDURE DIVISION.
     DISPLAY VAR-STR
     DISPLAY VAR-YEET
     DISPLAY YEET-A YEET-X YEET-B
     DISPLAY YEET-X-A ' - ' YEET-X-B
     STOP RUN.
```
This will produce:
```
1234abcd
00sub     part    $2ab
00sub     part    $2ab
sub      - part    
```
**Note** there are spaces and zeros for uninitialized data.

For simple arithmetic:
```COBOL
ADD a TO b          *> b = b + a
SUBTRACT a FROM b   *> b = b - a
MULTIPLY a BY b     *> b = b * a
DIVIDE a INTO b     *> b = b / a (integer division)
```
Moving data (copy var1 into var2):
```COBOL
MOVE var1 TO var2    
```
Creating tables (arrays). Use the `OCCURS` keyword to repeat a variable and allow indexing (e.e `table(2)`).

**Note** indexing is 1-based, not 0-based).
```COBOL
...
WORKING-STORAGE SECTION.
01 table1.
    03 table1-row PIC 9(2) OCCURS 3 TIMES.
...
PROCEDURE DIVISION.
    DISPLAY table1-row(3)
```
#### Control Flow
**Procedures** (functions) are defined with a label followed by a period (this tells the compiler to only run the following lines when it reaches the label `PERFORM <procedure-name>.`).
* Can break out of a procedure early using `EXIT PARAGRAPH.`
* Can break out of the entire `PROCEDURE DIVISION` using `STOP RUN.`
```COBOL
...
WORKING-STORAGE SECTION.
    01 alpha PIC 9(4).
    01 beta  PIC 9(4).
    01 gamma PIC 9(4).
...
PROCEDURE DIVISION.
    PERFORM func1.
    PERFORM func-c THRU func-b
    fun1.
    DISPLAY "Hello, World!".

    func-c.
    EXIT PARAGRAPH.
    DISPLAY "this is not called".

    func-b.
    STOP RUN.
    DISPLAY "this is not called".
```
