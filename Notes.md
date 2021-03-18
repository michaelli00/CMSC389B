# CMSC389B Special Topics in Computer Science; A Tour of Programming Languages
## Table of Contents
1. [Procedural Languages/COBOL](#ProcLang)
2. [Scripting Languages/Lua](#ScriptLang)
3. [Logic Languages/Prolog](#LogicLang)
4. [Stack Languages/Forth](#StackLang)
5. [Array Languages/J](#ArrayLang)
6. [Security Languages/Rust](#SecurityLang)
<a name="ProcLang"></a>
## 1. Procedural Languages/COBOL
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
Conditionals and looping:
```COBOL
*> conditional
IF ... THEN
    ...
END-IF.

IF ... THEN
    ...
ELSE
    ...
END-IF.

*> switch
EVALUATE ...
    WHEN ...
        ...
    WHEN ...
        CONTINUE
    ...
    WHEN OTHER
        ...
END-EVALUATE.
```
Comparison operators:
```
<>   (not equals)
=    (equals)
<, <=, >=, >    (as expected)

OR, AND, NOT    (as expected)
```
Looping is implemented using `PERFORM` and `UNTIL` or `TIMES`:
```COBOL
*> proc == procedure label
*> condition == some boolean condition
PERFORM proc UNTIL condition.
PERFORM proc n TIMES.
```
<a name="ScriptLang"></a>
## 2. Scripting Languages/Lua
No high-level programming language can directly run on a CPU. There are 2 **bridges** that connect a programming language to a CPU
* **Interpreters**: take code written in 1 language and execute meaningful code in some other language.
  * Runs much slow at **runtime** than compiled code (has to parse code every time the user runs the program).
  * Compiles code much faster than compiled code (only need to process 1 line at a time).
  * Usually just directly parse and execute the code
* **Compilers**: take code written in 1 language and output code in some other language.
  * Runs much faster at **runtime** than interpreted code (only has to parse the code once).
  * Takes longer to compile since we have to load, parse, and translate the entire code at once.
  * Usually perform **optimizations**

**Note** most scripted languages are **interpreted**
### REPL
Read-Eval-Print Loop
* **Read**: read user input
* **Eval**: evaluate user input (verify input has correct syntax and execute it)
* **Print**: print the calculated output
* **Loop**: do it all again
### Type Systems
**Type**: refers to type of variable in a language (e.g. integer). 

**Type Systems**: categorize variable implementation
* **Static** typing: require variables to declare their type upon instantiation and must stay the same type until their death
* **Dynamic** typing: variable types are **inferred** and **mutable**

**Note** most scripting languages are dynamically typed (aids in the "speed" of scripting).
### Lua
Terminology:
* **rocks**: modules that you can import
* **chunks**: execution blocks (a single line or an entire file)    

Lua has both **global** and **local** scope
#### Arithmetic/Data
Variable declaration: `varName = value` (defaults to `nil` if you declare a variable without a value)   
Boolean values: `false` and `true`  
Logical operators: `and`, `or`, `not` 
String concatenation: `str1 .. str2`  
Tables (hashmaps):
```Lua
a = {}
a[0] = "zero"
a["one"] = 1
#a -- 2, length of table (number of keys)
```
**Note** Lua doesn't have an array type although tables can be treated as arrays (indexing starts at 1)
#### Control Flow
Because Lua executes code line-by-line, you need to instantiated data before you can access them. 
Scoping follows lexicographically: local variables will shadow global variables of the same name.   
Blocks are created by wrapping code with `do-end`
```Lua
g = "g-global"
print(g)
do
  local g = "g-local"
  print(g)
  f = "f-global"
end
print(f) -- it exists, even though f was created in a different scope
```
**Note** variables will persist out of the code block they originated from    
Flow keywords
* `if ... then ... elseif ... then ... else ... end`
* `while ... do ... end`
* `repeat ... until ...`
* `for ... do ... end...`
* `break`
* `return`
Example for loops:
```Lua
lowVal = 0
highVal = 4
step = 2
-- create varName, set to lowVal, execute body, varName = varName + step
-- repeat until varName = highVal (inclusive)
for varName=lowVal, highVal, step do
    print(varName)
end
print("---")
for i=10, 3, -1 do -- can go backwards using negative step
    print(i)
end
print("---")
for i=2, 10 do -- default step increment is +1
    print(i)
end
print("------")

a = {"hi", "hey", "hello"} -- array
t = {hi=2, hey=3, hello=5} -- table
-- generic for
for key in pairs(t) do -- iterates through table keys (NO GUARANTEED ORDER)
    print(key, t[key])
end
print("---")
for idx, val in ipairs(a) do -- iterate through indices/values in array
    print(idx, val, a[idx])
end
print("---")
for k, v in ipairs(t) do -- ignores non-numeric keys (no numeric keys exist in this table)
    print(k, v, t[v])
end
```

**Note** `break` and `return` must be at the end of the current scope otherwise you'll get syntax errors. Can wrap in a `do-end` to get around errors.  
**Note** functions return implicitly  
Functions are created as `function funcName(arglist) ... end`   
* Any argument that is omitted is treated as `nil` in the function
* Functions can return multiple results, using commas
* Functions are **first-class**: they can be passed as variables
<a name="LogicLang"></a>
## 3. Logic Languages/Prolog
### Propositional Logic
**Proposition**: any statement that has a truth value.
* $p \wedge q$
* s
* r -> t
### Predicate Logic
Introduces predicates, quantifiers, and functions
* Quantifiers (forall, exists)
* Predicates (denote relationships)
### Prolog
Goal is to acertain if a question is true or false, or, find values for given variables that make the predicate true.   
**Objects**: used to represent something, entities assigned to a variable, represented using lowercase (not objects in Java)   
**Relationships**: relates two objects `sisters(daphne, astoria)`   
**Variables**: represented using capital letters and serve as placeholders `loves(john, X)`   
3 basic operations:
* Declaring facts:
* Declaring rules: declaration that some fact is depending on something else `sisters(X,Y) :- parent(Z,X), parent(Z,Y).`
* Asking questions: `sisters(daphne, astoria).`
#### Procedures
7 main operators:
* `=`: used for *if and only if*. Does not evaluate expressions on either side of the operator. `?- 9=9` returns `true` but `?- 7+2=9` returns `false`
* `is`: used for arithmetic expressions and only evaluates the **right hand** side of the expression. `7+2 is 9` evaluates to `false` but `9 is 7+2` evaluates `true`
* `==`: used when checking if 2 things are identical. `X==2` is only true if `X` is assigned `2`.
* `=:=`: compares arithmetic expressions
* `\+`: NOT operator. `\+ false` evaluates to `true`
* `,`: used to `and` goals
* `;`: used to `or` goals

Since Prolog only deals with `true` and `false`, return values need to be part of the query.
* If a variable is part of the query: `increment(1, Z).` returns `Z=2`
* If no variable is part of the query: `increment(1, 2).` returns `true`

For recursion we can define a base case and a recursive case:
```Prolog
facotorial(0,1).
factorial(N,F) :- N > 0, N1 is N-1, factorial(N1, F1), F is N*F1).
```
#### Asking/Answering Questions
* Questions with no variables will evaluate as `true` or `false`: `sisters(daphne, astoria).` evaluates to `true`   
* Questions with variables will be evaluated and variables that make the statement true are returned: `sisters(X, astoria).` returns `daphne`
#### Backracking
Backtracking: use previous statements to see what has been done to satisfy the question. Prolog uses backtracking for goal satisfaction (finding all possible solutions).   
We can use the cut operator (`!`) to prevent unnecessary backtracking.
## Stack Languages/Forth
Stacks use **First In, Last Out (FILO)**. Operations include
* pop: remove and returns the top value
* push: adds an item to the top of the stack
* peek: returns but does not remove top value of stack
* remove: removes an item from the stack

Types of order notation:
* postfix notation: operator after operands `10 25 +` (analogous to using a stack)
* infix notation: operator between operands `10 + 25`
* prefix notation: operator before operands `+ 25 10`
### Forth
#### Stack/arithmetic operations:
* `dup`: duplicates the top item of the stack
* `drop`: removes the top item from the stack
* `.`: pops top item from stack
* `swap`: swaps top 2 items from stack
* `over`: duplicates 2nd from top item from stack
* `rot`: places 3rd item of stack on top
* `nip`: removes 2nd from top item from stack
* `tuck`: copies top item from stack and places it under the top 2 items
#### Functions
Procedures are called **colon definitions** and are closed using semicolons `;`
```Forth
: add1
  1 + ;
```
#### Conditionals
Conditionals are dependent on flags. `0` is false and `-1`, or any non-zero value, is true.
```Forth
: min
  2dup < if
    drop
  else
    nip
  endif ;
```
Note that comparison operators will consume values on the stack
```Forth
1 2 ( put 1 then 2 on the stack )
>   ( pop off 2 and 1 then compare )
.s  ( print out the stack, in this case -1 )
```
#### Looping
```Forth
BEGIN
  code1
WHILE
  code2
REPEAT
```
`code` is executed if a true flag is on top of the stack. `code2` is run and the loop restarts.
#### Memory
Forth supports heap memory allocations (used for global variables or arrays). Global variables can be stored or fetched from memory using `!` or `@`, respectively.
```Forth
variable v
v . \ prints the memory address 
5 v ! \ places 5 in v and removes 5 from the stack
v @ \ will place the value in v on the top of the stack.
```
Array structures can be allocates by allotting cells in memory. Pointer arithmetic can be used to access values
```Forth
create arr 20 cells allot
389 arr 3 cells + ! \ places 389 in the cell with index 3 after arr.
arr 3 cells + @ \ pushes the value in the cell at index 3 to the stack.
```
`char` size and `cell` size differ:
```Forth
\ I'm like pretty sure these two will allot the same amount of space
create str 16 chars allot
create str2 2 cells allot

\ to add a string we want to use the 'chars' keyword
97 str !
98 str 1 chars + !

str 2 type \ will print 2 chracters which are stored in 'str'.
str 2 chars dump \ should do the same thing but also show the actual memory
```
## 5. Array Languages/J
Array Programming is a way to apply operations on an entire set of values at once in a **functional** manner.
### J
Data is stored in arrays and computations are performed by applying functions to arrays.
#### Arithmetic/Data
Sample code for an array `[1,2,3]` and indexing it. Tabbed lines are input, untabbed lines are output.
```J
  a =: 1 2 3
  0 { a
1
  a { a
2
  2 { a
3
  3 { a
|index error
```

Arrays also have **dimension** and **rank** (number of dimension). Arrays also have **shape**. `$` gives the right-argument's dimensions
```J
  $ a
3
  i. 3 4
0 1  2  3
4 5  6  7
8 9 10 11
  $ i. 3 4
3 4
  i. 2 2 2
0 1
2 3

4 5
6 7
  $ i. 2 2 2
2 2 2
```
`i.` outputs an array of the inputted dimensions filled in-order

Dyadic `$` will put the right-arg input into an array of the left-arg shape
```J
  2 2 $ 1 2 3 4
1 2
3 4
```
Arithmetic is applied to arrays element-wise
```J
   2 3 * 4 5
8 15
```
Information is stored using `=:` (or `=.` for local variables)
```J
  a =: 'hello!'
  a
hello!
  a =: 1 2 3
  b =: 4 5 6
5 7 9
```
#### Control Flow
Functions
```
func_name =: 4 : <fxn operations>
```
A **monadic** functions takes only 1 argument (usually right-arg).

A **Dyadic** functions takes 2 arguments (1 left, 1 right)

Control flow key terms
* `if. do. elseif. do. else. end`
* `while. do. end`
* `for. A do. B end.`
* `for _a. A do. B. end.`
* `return.`
* `break.`
* `continue.`
* `select. case. do. fcase. do. end.`
Iterator:
```J
for_item. 4 5 6 do.
  smoutput item ; item_index
end.
)

4 0
5 1
6 2
```
## 6. Security Languages/Rust
When a security attack occurs, 1 of 3 things usually happen:
* Data that shouldn't have been read is read 
* Data that shouldn't have been modified is modified
* Data that should be public is blocked

These attacks are usually a result of **memory misuses** (dangling pointers, memory leaks)
### Types of Attacks
#### Buffer Overflow
data past the buffer is overwritten. Rust mitigates this issue with **splices** and **vectors**
```C
char buffer[5];
scanf("%s", buffer);
// If we input 'hackerman' the content beyond the buffer is overwritten
```
#### Memory
One example is dangling pointers. Rust helps prevent (but doesn't fully stop vulnerabilities) using **limetimes** and **ownership**
```C
struct Node {
  int value;
  sturct Node* next;
}

struct Node *head = malloc(sizeof(struct Node);
struct Node *tail = malloc(sizeof(struct Node);
head->value = 10;
tail->value = 14;
head->next = tail;
free(tail);
// tail is already free'd but we're doing some type of operation on it (dangling pointer)
if (head->next == NULL) {
  do_something();
}
```
#### Time Attacks
Using multithreading to access data we aren't supposed to be accessing (e.g. passwords). Rust mitigates this using **ownership** and **borrowing rules**.
```C
if (access("/home/student/file.txt", R_OK) != 0) {
  exit(1);
}
// comment 1
fd = open("/home/student/fil.txt", R_WRONLY);
read(fd, buffer, 1023);
```
```C
symlink("/secret/root/file", "home/student/file.txt");
```
In this example, if the first program execution was paused at `// comment`, then the second program (multithreading) ran, `file.txt` points to a different file. When the first program resumes, it will open a potentially open a file with private information.

### Rust
#### Ownership and Borrowing
Each piece of data has both an **owner** and a **lifetime**
* Only the owner can change the data. Ownership refers to which variable has the privilege of pointing to a piece of data with its memory address.
  * **Reference**, or **borrowing**, refers to temporarily giving privilege to another variable for this data.
* Lifetime determines where the data can be seen or how long it lasts in memory
```Rust
let x = 32;
x = 22; //not okay (not mutable)
--------------------------------
let mut x = 32;
x = 23; // okay
```
Reference can be created using `&`. To create a mutable reference use `&mut`.
```Rust
let mut s1 = String::from("hello");
let s2 = &mut s1;
// s1 is the mutable owner. s2 is given a mutable reference
// both s1 and s2 can modify the data so long s2 is not borrowed
-------------------------------------
let s1 = String::from("hello");
let s2 = &mut s1; // not allowed since s1 is not mutable
------------------------------------
let mut s1 = String::from("hello");
let s2 = &s1;
// s1 is mutable owner and s2 is immutable reference so only s1 can modify it
```
#### Slices
Contain metadata about the data they refer to
```Rust
let mystr = "Hello World".to_srting();
let myslice = &mystr[4..9]; //o Wor"
```
Mutable slices can be created, so long as the original data is mutable
```Rust
let mut mystr = "Hello World".to_srting();
let myslice = &mut mystr[4..9]; // this is fine
-----------------------------------
let mystr = "Hello World".to_srting();
let myslice = &mut mystr[4..9]; // this is not ok
```
