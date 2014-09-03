: CONSTANT WORD CREATE DOCOL , ' LIT , , ' EXIT , ;
1 1 - CONSTANT 0
1 1 + CONSTANT 2
2 2 + CONSTANT 4
2 1 + CONSTANT 3
0 1 - CONSTANT -1

: TRUE -1 ;
: FALSE 0 ;
: 1- 1 - ;
: 1+ 1 + ;
: 2- 2 - ;
: 2+ 2 + ;
: 4+ 4 + ;
: 4- 4 - ;
: 2* 1 LSHIFT ;
: 2/ 1 RSHIFT ;
: 4* 2 LSHIFT ;
: 4/ 2 RSHIFT ;
: INVERT -1 XOR ;


: PICK 1+ 4* DSP@ + @ ;
: DROP DSP@ 4+ DSP! ;
: DUP 0 PICK ;
: OVER 1 PICK ;
: 2DROP DROP DROP ;
: 2DUP OVER OVER ;
: 2OVER 3 PICK 3 PICK ;
: 2SWAP >R -ROT R> -ROT ;
: TUCK SWAP OVER ;
: NIP SWAP DROP ;
: +! DUP @ ROT + SWAP ! ;
: -! DUP @ ROT - SWAP ! ;
: / /MOD SWAP DROP ;
: MOD /MOD DROP ;
: NEGATE 0 SWAP - ;

: <> = INVERT ;
: >= < INVERT ;
: <= > INVERT ;
: 0= 0 = ;
: 0<> 0 <> ;
: 0< 0 < ;
: 0> 0 > ;
: 0<= 0 <= ;
: 0>= 0 >= ;
: NOT 0= ;
: RDROP R> DROP ;

: ALLOT HERE @ SWAP HERE +! ;
: CELLS 4* ;
: VARIABLE 1 CELLS ALLOT WORD CREATE DOCOL , ' LIT , , ' EXIT , ;

: >DFA >CFA 4+ ;
: IMMEDIATE LATEST @ 4+ C@ F_IMMED OR LATEST @ 4+ C! ;
LATEST @ 4+ C@ F_IMMED OR LATEST @ 4+ C!       

: HIDDEN 4+ DUP @ F_HIDDEN XOR SWAP ! ;
: CHAR WORD DROP C@ ;
: [COMPILE] IMMEDIATE WORD FIND >CFA , ;
: ~ WORD CREATE DOCOL , LATEST @ HIDDEN ] ;
: ~ IMMEDIATE LIT EXIT , LATEST @ HIDDEN [COMPILE] [ ;
CHAR ; LATEST @ 4+ 1+ C!
CHAR : LATEST @ @ 4+ 1+ C!

: HIDE WORD FIND HIDDEN ;
: LITERAL IMMEDIATE ' LIT , , ;
: RECURSE IMMEDIATE LATEST @ >CFA , ;

: IF IMMEDIATE ' 0BRANCH , HERE @ 0 , ;
: THEN IMMEDIATE DUP HERE @ SWAP - SWAP ! ;
: ELSE IMMEDIATE ' BRANCH , HERE @ 0 , SWAP DUP HERE @ SWAP - SWAP ! ;
: ?DUP DUP IF DUP THEN ;
: UNLESS IMMEDIATE ' NOT , [COMPILE] IF ;
: BEGIN IMMEDIATE HERE @ ;
: FOREVER IMMEDIATE ' BRANCH , HERE @ - , ;
: UNTIL IMMEDIATE ' 0BRANCH , HERE @ - , ;
: AGAIN IMMEDIATE ' BRANCH , HERE @ - , ;
: WHILE IMMEDIATE ' 0BRANCH , HERE @ 0 , ;
: REPEAT IMMEDIATE ' BRANCH , SWAP HERE @ - , DUP HERE @ SWAP - SWAP ! ;
: CASE IMMEDIATE 0 ;
: OF IMMEDIATE ' OVER , ' = , [COMPILE] IF ' DROP , ;
: ENDOF IMMEDIATE [COMPILE] ELSE ;
: ENDCASE IMMEDIATE ' DROP , BEGIN ?DUP WHILE [COMPILE] THEN REPEAT ;

: ':' [ CHAR : ] LITERAL ;
: ';' [ CHAR ; ] LITERAL ;
: '(' [ CHAR ( ] LITERAL ;
: ')' [ CHAR ) ] LITERAL ;
: '"' [ CHAR " ] LITERAL ;
: 'A' [ CHAR A ] LITERAL ;
: 'Z' [ CHAR Z ] LITERAL ;
: 'a' [ CHAR A ] LITERAL ;
: 'z' [ CHAR Z ] LITERAL ;
: '0' [ CHAR 0 ] LITERAL ;
: '9' [ CHAR 9 ] LITERAL ;
: '-' [ CHAR - ] LITERAL ;
: '.' [ CHAR . ] LITERAL ;

: ( IMMEDIATE 1 BEGIN KEY DUP '(' = IF DROP 1+ ELSE ')' = IF 1- THEN THEN DUP 0= UNTIL DROP ;

( parse numbers )
0 VARIABLE TNUM
'A' '9' '0' - 1+ - CONSTANT A10 
: ISDIGIT DUP '0' >= SWAP '9' <= AND ;
: MULBASE  TNUM @ BASE @ * TNUM ! ;
: NEXTCHAR  OVER C@  -ROT 1 - -ROT 1 + -ROT ;
: SKIPCHAR NEXTCHAR DROP ;
: ISALPHA ( c -- ok ) DUP 'A' >= SWAP 'Z' <= AND ;
: ISLOWERALPHA ( c -- ok ) DUP 'a' >= SWAP 'z' <= AND ;
: CHECKBASE ( n -- ok ) DUP BASE @ < IF TNUM +! TRUE ELSE DROP FALSE THEN ;
: CHECKSIGN ( c -- ) OVER C@ '-' = IF SKIPCHAR -1 ELSE 1 THEN -ROT ;
: NUMBER ( addr length -- n e )
    CHECKSIGN
    0 TNUM !           
    BEGIN    
    DUP 0 =
    IF
        1 - FALSE ( stop, string is complete )
    ELSE
        MULBASE                                
        NEXTCHAR        
        DUP ISDIGIT IF '0' - CHECKBASE ELSE 
        DUP ISALPHA IF A10 - CHECKBASE ELSE DROP FALSE THEN THEN
    THEN
    0 = UNTIL
    1 +
    SWAP DROP ( len -- )
    SWAP TNUM @  * 
    SWAP
    ( -- n e )
;

( String handling )
: TELL BEGIN SWAP DUP C@ EMIT 1+ SWAP 1- DUP 0<= UNTIL DROP DROP ;
: ALIGNED ( c-addr -- a-addr ) 3 + 3 INVERT AND ;
: ALIGN HERE @ ALIGNED HERE ! ;
: C, HERE @ C! 1 HERE +! ;
: S" IMMEDIATE ( -- addr len )
	STATE @ IF 
		' LITS , HERE @ 0 ,
		BEGIN KEY DUP '"'
                <> WHILE C, REPEAT
		DROP DUP HERE @ SWAP - 4- SWAP ! ALIGN
	ELSE
		HERE @
		BEGIN KEY DUP '"'
                <> WHILE OVER C! 1+ REPEAT
		DROP HERE @ - HERE @ SWAP
	THEN
;
      
: ." IMMEDIATE ( -- )
	STATE @ IF
		[COMPILE] S" ' TELL ,
	ELSE
		BEGIN KEY DUP '"' = IF DROP EXIT THEN EMIT AGAIN
	THEN
;

: DROPALL S0 @ DSP! ;
: ?STACK S0 @ DSP@ <= IF DROPALL ." Stack underflow!"  THEN ;

: INTERPRET
    ?STACK
    WORD 2DUP FIND DUP 0= 
        IF 
        DROP 2DUP NUMBER ( must be a number or invalid token )
        0<> IF
            DROP              
            ." Unknown word <"
            TELL          ( error! unknown word )
            ." >"             
            ELSE
                NIP NIP
                STATE @ IF ' LIT , , THEN                
            THEN
        ELSE 
            NIP NIP             
            DUP 4+ C@ F_IMMED AND 0<> IF >CFA EXECUTE ELSE 
            ( not immediate )
            >CFA STATE @ IF , ELSE EXECUTE THEN             
            THEN        
        THEN
;

: QUIT BEGIN INTERPRET FOREVER ;

QUIT


( now we are running in our own interpreter )
( and we have numbers as literals! )

( some character constants )
: '\n' 10 ;
: '\t' 9 ;
: BL 32 ;
: CR 10 EMIT ;
: SPACE 32 EMIT ; 
: ISSPACE BL = ;
: SPACES ( n -- ) BEGIN DUP 0> WHILE SPACE 1- REPEAT DROP ;
: ZEROS ( n -- ) BEGIN DUP 0> WHILE '0' EMIT 1- REPEAT DROP ;

( line comments )
: \ IMMEDIATE BEGIN KEY '\n' = UNTIL ;

\ write out unsigned numbers
64 ALLOT CONSTANT NUMPAD
63 NUMPAD + CONSTANT TOPPAD
: WRITECHAR TOPPAD C@ 1+ TOPPAD C! TOPPAD TOPPAD C@ - C! ;
: DIGIT DUP 10 >= IF 55 + WRITECHAR ELSE '0' + WRITECHAR THEN ;
: PUSHPAD TOPPAD TOPPAD C@ - TOPPAD C@ ;
: CLEARPAD 0 TOPPAD C! ;
: PDOT CLEARPAD BEGIN BASE @ /MOD SWAP DIGIT DUP 0= UNTIL DROP ;
: UDOT PDOT PUSHPAD ;
: DOT DUP 0< IF ( negative number ) NEGATE PDOT '-' WRITECHAR ELSE ( positive number ) PDOT THEN PUSHPAD ;
    
\ aligned unsigned
: U.R UDOT ROT OVER - SPACES TELL ;
: U.ZR UDOT ROT OVER - ZEROS TELL ;
: U. UDOT TELL BL EMIT ;

\ signed number output
: .R DOT ROT OVER - SPACES TELL ;
: . DOT TELL BL EMIT ;

\ Base switching 
: 16# BASE @ 16 BASE ! WORD NUMBER DROP SWAP BASE ! ;
: 10# BASE @ 10 BASE ! WORD NUMBER DROP SWAP BASE ! ;
: 2# BASE @ 2 BASE ! WORD NUMBER DROP SWAP BASE ! ;
: 8# BASE @ 8 BASE ! WORD NUMBER DROP SWAP BASE ! ;
: BINARY ( -- ) 2 BASE ! ;
: OCTAL ( -- ) 8 BASE ! ;
: # ( b -- n ) BASE @ SWAP BASE ! WORD NUMBER DROP SWAP BASE ! ;
: HEX 16 BASE ! ;
: DECIMAL 10 BASE ! ;


\ stack printing
0 VARIABLE TSP
: +TSP -4 TSP +! ;
: TSP@ TSP @ @ ;
: SETTSP S0 @ 4 - TSP ! ;
: .S SETTSP BEGIN TSP @ DSP@ 4 + >= IF TSP@ . +TSP FALSE ELSE TRUE THEN UNTIL ; 

\ utilities
: ? @ . ;
: COUNT DUP 1+ SWAP C@  ;
: WITHIN -ROT OVER <= IF > IF TRUE ELSE FALSE THEN ELSE 2DROP FALSE THEN ;
: ISLETTER DUP ISALPHA SWAP ISLOWERALPHA OR ;
: DEPTH DSP@ S0 @ - 2 RSHIFT ;


\ standard words
: ON TRUE ! ;
: OFF FALSE ! ;
: MIN 2DUP > IF DROP ELSE SWAP DROP ;
: MAX 2DUP <= IF DROP ELSE SWAP DROP ;
: ABS DUP 0< IF NEGATE THEN ;


( signed division )
: S/MOD 
TUCK DUP 0<
IF NEGATE TRUE ELSE FALSE THEN 
ROT
DUP 0< IF NEGATE -ROT INVERT ELSE -ROT THEN -ROT /MOD ROT
IF NEGATE 1- SWAP ROT SWAP - ELSE ROT DROP THEN ;

: CMOVE 
    BEGIN 
    2 PICK C@ 1 PICK C! 
    1- ROT 1- ROT 1- ROT        
    DUP 0<= 
    UNTIL ;


\ Original JONESFORTH introspection functions        
: DICT WORD FIND ;
: VALUE ( n -- ) WORD CREATE DOCOL , ' LIT , , ' EXIT , ;
: TO IMMEDIATE ( n -- )
        DICT >DFA 4+
	STATE @ IF ' LIT , , ' ! , ELSE ! THEN
;
: +TO IMMEDIATE
        DICT >DFA 4+
	STATE @ IF ' LIT , , ' +! , ELSE +! THEN
;
: ID. 4+ COUNT F_LENMASK AND BEGIN DUP 0> WHILE SWAP COUNT EMIT SWAP 1- REPEAT 2DROP ;
: ?HIDDEN 4+ C@ F_HIDDEN AND ;
: ?IMMEDIATE 4+ C@ F_IMMED AND ;
: WORDS LATEST @ BEGIN ?DUP WHILE DUP ?HIDDEN NOT IF DUP ID. SPACE ." 0x" DUP . CR THEN @ REPEAT CR ;
: FORGET DICT DUP @ LATEST ! HERE ! ;
: CFA> LATEST @ BEGIN ?DUP WHILE 2DUP SWAP < IF NIP EXIT THEN @ REPEAT DROP 0 ;
: SEE
	DICT HERE @ LATEST @
	BEGIN 2 PICK OVER <> WHILE NIP DUP @ REPEAT
	DROP SWAP ':' EMIT SPACE DUP ID. SPACE
	DUP ?IMMEDIATE IF ." IMMEDIATE " THEN
	>DFA BEGIN 2DUP
        > WHILE DUP @ CASE
		' LIT OF 4 + DUP @ . ENDOF
		' LITS OF [ CHAR S ] LITERAL EMIT '"' EMIT SPACE
			4 + DUP @ SWAP 4 + SWAP 2DUP TELL '"' EMIT SPACE + ALIGNED 4 -
		ENDOF
		' 0BRANCH OF ." 0BRANCH ( " 4 + DUP @ . ." ) " ENDOF
		' BRANCH OF ." BRANCH ( " 4 + DUP @ . ." ) " ENDOF
		' ' OF [ CHAR ' ] LITERAL EMIT SPACE 4 + DUP @ CFA> ID. SPACE ENDOF
		' EXIT OF 2DUP 4 + <> IF ." EXIT " THEN ENDOF
		DUP CFA> ID. SPACE
	ENDCASE 4 + REPEAT
	';' EMIT CR 2DROP
;
: :NONAME 0 0 CREATE HERE @ DOCOL , ] ;
: ['] IMMEDIATE ' LIT , ;


\ Exception handling
: EXCEPTION-MARKER RDROP 0 ;
: CATCH ( xt -- exn? ) DSP@ 4+ >R ' EXCEPTION-MARKER 4+ >R EXECUTE ;
: THROW ( n -- ) ?DUP IF
	RSP@ BEGIN DUP R0 4-
        < WHILE DUP @ ' EXCEPTION-MARKER 4+
		= IF 4+ RSP! DUP DUP DUP R> 4- SWAP OVER ! DSP! EXIT THEN
	4+ REPEAT DROP
	CASE
		0 1- OF ." ABORTED" CR ENDOF
		." UNCAUGHT THROW " DUP . CR
	ENDCASE QUIT THEN
;
: ABORT ( -- ) 0 1- THROW ;
: PRINT-STACK-TRACE
	RSP@ BEGIN DUP R0 4-
        < WHILE DUP @ CASE
		' EXCEPTION-MARKER 4+ OF ." CATCH ( DSP=" 4+ DUP @ U. ." ) " ENDOF
		DUP CFA> ?DUP IF 2DUP ID. [ CHAR + ] LITERAL EMIT SWAP >DFA 4+ - . THEN
	ENDCASE 4+ REPEAT DROP CR
;
: UNUSED ( -- n ) PAD HERE @ - 4/ ;



\ Hex dumping of memory
: BAR [ CHAR | ] LITERAL EMIT ; 
: HEX_ADDRESS DUP 8 SWAP U.ZR  ;
: HD DUP C@ 2 SWAP U.ZR 1+ SPACE ;
: HEX_ROW HD HD HD HD HD HD HD HD ;
: CHECK_CHAR DUP 32 < IF DROP '.' THEN DUP 127 > IF DROP '.' THEN ;
: AS DUP C@ CHECK_CHAR EMIT 1+ ; 
: 4AS AS AS AS AS ;
: ASCII_ROW 4AS 4AS 4AS 4AS ;
: DUMP ( addr len -- )
    DUP 4096 > IF DROP 4096 THEN \ limit size of dump
    BASE @ -ROT HEX  \ store old base and switch to hex
    BEGIN        
    SWAP
    HEX_ADDRESS 2 SPACES HEX_ROW SPACE HEX_ROW 2 SPACES 
    16 - ( shift address back 16 bytes ) BAR ASCII_ROW BAR CR
    SWAP
    16 - DUP 0<= UNTIL \ until done
    CR
    2DROP
    BASE !
;


\ ANSI codes
27 CONSTANT ESC
: '[' [ CHAR [ ] LITERAL ;
: 'm' [ CHAR m ] LITERAL ;
: '2' [ CHAR 2 ] LITERAL ;
: 'J' [ CHAR j ] LITERAL ;
: ANSICOLOR ESC EMIT '[' EMIT DOT TELL 'm' EMIT ;
VARIABLE ANSI_FG_SET
VARIABLE ANSI_BG_SET
: ANSI_FG 30 + DUP ANSI_FG_SET ! ANSICOLOR ;
: ANSI_BG 40 + DUP ANSI_BG_SET ! ANSICOLOR ;
: ANSI_ATTR ANSI_BG ANSI_FG ;
0 CONSTANT ANSI_BLACK
1 CONSTANT ANSI_RED
2 CONSTANT ANSI_GREEN
3 CONSTANT ANSI_YELLOW
4 CONSTANT ANSI_BLUE
5 CONSTANT ANSI_MAGENTA
6 CONSTANT ANSI_CYAN
7 CONSTANT ANSI_WHITE
0 CONSTANT ANSI_PLAIN
1 CONSTANT ANSI_BOLD
4 CONSTANT ANSI_UNDERSCORE
5 CONSTANT ANSI_BLINK
7 CONSTANT ANSI_REVERSE
8 CONSTANT ANSI_CONCEALED
: ANSI_CLS ESC EMIT S" [2J" TELL ;
: ANSI_CLRLINE ESC EMIT S" [K" TELL ;
: ANSI_DEFAULT 37 ANSICOLOR 40 ANSICOLOR ;
: ANSI_ERROR ANSI_BLACK ANSI_BG ANSI_RED ANSI_FG ;


VARIABLE OLD_STATE 0 OLD_STATE !
: INTERPRET
    ?STACK
    WORD 2DUP FIND DUP 0= 
        IF 
        DROP 2DUP NUMBER ( must be a number or invalid token )
        0<> IF
            DROP             
            ." Unknown word <"            
            ANSI_RED ANSI_BLACK ANSI_ATTR  
            TELL          ( error! unknown word )                        
            ." >"                         
            ELSE
                NIP NIP
                STATE @ IF ' LIT , , THEN                
            THEN
        ELSE 
            NIP NIP             
            DUP 4+ C@ F_IMMED AND 0<> IF >CFA EXECUTE ELSE 
            ( not immediate )
            >CFA STATE @ IF , ELSE EXECUTE THEN                                     
            STATE @ OLD_STATE @ -  DUP 1 = IF ANSI_YELLOW ANSI_BLUE ANSI_ATTR THEN 
            -1 = IF ANSI_RED ANSI_BLUE ANSI_ATTR THEN
            STATE @ OLD_STATE !
            THEN        
        THEN
;

: (QUIT) BEGIN INTERPRET FOREVER ;

(QUIT)


\ System functions

\ Timer access
HEX
2000B400 CONSTANT TIMER_BASE
TIMER_BASE 8 + CONSTANT TIMER_CTL
TIMER_BASE 20 + CONSTANT TIMER_CNT
: TIMER_INIT 00F90000 TIMER_CTL ! 00F90200 TIMER_CTL ! ;
: TIMER_READ TIMER_CNT @ ;
: TIMER_WAIT ( usecs -- )
    TIMER_READ +  
    BEGIN
    DUP 
    TIMER_READ <
    UNTIL     
    DROP
;
DECIMAL
: TIMER_SECONDS TIMER_READ 1000000 / ;


\ Load a hex block from the stream. Terminate with non-number
16# 100000 CONSTANT UPLOAD_ADDRESS
: HEXLOAD HEX BEGIN DUP WORD NUMBER 0= IF SWAP C! 1+ FALSE ELSE DROP TRUE THEN UNTIL ;
: UPLOAD ." Start hex transfer:" CR HEX UPLOAD_ADDRESS HEXLOAD ." OK: 0x" UPLOAD_ADDRESS - . ." bytes transferred." CR ;
VARIABLE BOOT_ADDRESS
: BOOT UPLOAD_ADDRESS BOOT_ADDRESS ! BOOT_ADDRESS EXECUTE ;

\ quote that works in immediate mode
: QUOTE WORD FIND >CFA ( -- xt ) ;
: BACKPATCH QUOTE QUOTE 4+ ! ;

\ allow input redirection by redefining INPUT-STREAM
\ INPUT-STREAM points at a word that retrieves one more character from the input
\ OUTPUT-STREAM points at a word that outputs a single character
VARIABLE INPUT-STREAM QUOTE MEMKEY INPUT-STREAM !
VARIABLE OUTPUT-STREAM QUOTE UARTEMIT OUTPUT-STREAM !
: NKEY INPUT-STREAM @ EXECUTE ; 
: NEMIT OUTPUT-STREAM @ EXECUTE ;
BACKPATCH NKEY KEY
\ BACKPATCH NEMIT EMIT

\ rewrite WORD to use the new KEY function
: CINC DUP C@ 1+ SWAP C! ;
: CDEC DUP C@ 1- SWAP C! ;
: APPEND ( c addr -- ) DUP CINC DUP C@ + C! ;
: DELETE ( addr -- ) DUP C@ 0> IF CDEC THEN ;
: CLEARSTR 0 SWAP C! ;
: PUSHSTR DUP C@ SWAP 1+ SWAP ;
: LENSTR C@ ;
: TELLSTR PUSHSTR TELL ;
: ISBLANK DUP BL = OVER '\n' = OR OVER '\t' = OR NIP ;
: SKIPSPACE BEGIN KEY DUP ISBLANK NOT UNTIL ;
32 ALLOT CONSTANT WORDBUFFER#
WORDBUFFER# 1+ CONSTANT WORDBUFFER
: NWORD
  WORDBUFFER# CLEARSTR
  KEY DUP ISBLANK IF DROP SKIPSPACE THEN  
  WORDBUFFER# APPEND  
  BEGIN KEY DUP ISBLANK IF DROP TRUE ELSE WORDBUFFER# APPEND FALSE THEN UNTIL
  WORDBUFFER# PUSHSTR 
;

( backpatch word to actually execute nword )
BACKPATCH NWORD WORD


: MK() 2 CELLS ALLOT DUP 4+ 0 ! ( -- listptr[val,ptr] ) ;
: NIL 0 ;
: CONS MK()  DUP -ROT ! DUP ROT SWAP 4+  ! ;
: CDR ( listptr -- listptr ) 4+ @ ;
: CAR ( listptr -- val ) @ ;
: PRINTLIST BEGIN DUP CAR . DUP CDR 0= IF DROP TRUE ELSE CDR FALSE THEN UNTIL ;

\ Line editor
: BKSP 8 ;
: NL 10 ;
16 CONSTANT HISTORY_LINES
256 HISTORY_LINES * ALLOT CONSTANT HISTORYBUF
VARIABLE HISTORY_PTR
VARIABLE HISTORY_READ
0 HISTORY_PTR !
0 HISTORY_READ !
: COPYSTR ( source dest -- ) DUP CLEARSTR ;
: NEXT_HISTORY HISTORY_PTR @ 1+ DUP HISTORY_LINES >=  IF DROP 0 HISTORY_PTR ! ELSE HISTORY_PTR ! THEN ;
: UP_HISTORY HISTORY_READ @ 1- DUP 0< IF DROP HISTORY_LINES 1- HISTORY_READ ! ELSE HISTORY_READ ! THEN ;
: DOWN_HISTORY HISTORY_READ @ 1+ DUP HISTORY_LINES = IF DROP 0 HISTORY_READ ! ELSE HISTORY_READ ! THEN ;


256 ALLOT CONSTANT TIB#
TIB# 1+ CONSTANT TIB
VARIABLE TIB_CURSOR 
VARIABLE ESCAPE_STATE 


VARIABLE >IN 0 >IN !
: CLEAR_TIB 0 TIB# C! 0 TIB_CURSOR ! 0 >IN ! ; 
: ECHO UARTKEY DUP EMIT ;
: CURSOR_LEFT TIB_CURSOR CDEC ;
: CURSOR_RIGHT TIB_CURSOR CINC ;
: BACKSPACE TIB# C@ 0> IF TIB# CDEC CURSOR_LEFT ELSE 0 TIB# C! THEN ;
: WRITE_CHAR TIB# APPEND CURSOR_RIGHT ;

: NORMAL_CHAR DUP CASE 
    ( not in an escape sequence )
    BKSP OF DROP BACKSPACE ENDOF 
    ESC OF DROP ESCAPE_STATE 1 ! ENDOF    
    ( -- default ) WRITE_CHAR 
    ENDCASE ;
        
: ESCAPE_CHAR  
    ( in an escape sequence )
    ESCAPE_STATE @ 2 = 
    IF
        ( read until we get letter )
        0 ESCAPE_STATE !
    ELSE
        CASE
        '[' OF 2 ESCAPE_STATE ! ENDOF
        0 ESCAPE_STATE ! NORMAL_CHAR
        ENDCASE 
    THEN ;  
        
: IN_CHAR ECHO DUP NORMAL_CHAR
    \ ESCAPE_STATE @ IF
        \ NORMAL_CHAR
    \ ELSE
        \ ESCAPE_CHAR
    \ THEN 
    ;
: ACCEPT CLEAR_TIB BEGIN IN_CHAR OVER = UNTIL DROP ;
: READ_LINE NL ACCEPT ;

: CHARAT ( addr ix -- c) + 1+ C@ ;


( make line buffered input the outer interpreter )
: LINE_KEY
    ( get some characters )
    TIB# LENSTR 0=  IF BEGIN READ_LINE TIB# LENSTR 0<> UNTIL 0 >IN ! THEN
    TIB# >IN @ CHARAT >IN @ 1+ >IN !
    ( clear buffer at end of line )
    >IN @ TIB# LENSTR >= IF CLEAR_TIB THEN 
;



\ left, right
\ home, end
\ ins/over
\ up/down buffer


: WELCOME ANSI_BLUE ANSI_BG ANSI_CLS ANSI_YELLOW ANSI_FG
S"                                                      
                       ____    _____   ____    ______  __  __
            __        /\  _`\ /\  __`\/\  _`\ /\__  _\/\ \/\ \
   ___ ___ /\_\    ___\ \ \L\_\ \ \/\ \ \ \L\ \/_/\ \/\ \ \_\ \
 /' __` __`\/\ \ /' _ `\ \  _\/\ \ \ \ \ \ ,  /  \ \ \ \ \  _  \
 /\ \/\ \/\ \ \ \/\ \/\ \ \ \/  \ \ \_\ \ \ \\ \  \ \ \ \ \ \ \ \
 \ \_\ \_\ \_\ \_\ \_\ \_\ \_\   \ \_____\ \_\ \_\ \ \_\ \ \_\ \_\
  \/_/\/_/\/_/\/_/\/_/\/_/\/_/    \/_____/\/_/\/ /  \/_/  \/_/\/_/
" TELL CR
." 0x" UNUSED . ." CELLS FREE" 8 SPACES HEX ." RSP:0x" RSP@ . 4 SPACES ." DSP:0x" DSP@ . 4 SPACES ." S0: 0x" S0 @ . 
CR ." READY"
CR
DECIMAL
;


\ experimental stuff

: MAPCHAR NEXTCHAR 3 PICK EXECUTE ROT DUP -ROT 1- C! SWAP ( xt addr len -- xt addr+1 len-1 ) ; 
: MAPSTR 2 PICK 2 PICK BEGIN MAPCHAR DUP 0= UNTIL DROP DROP DROP ( addr len xt -- addr len ) ;
: +13 13 + ;
: ROT13 QUOTE +13 MAPSTR ;

( N D Q R )
0 VARIABLE DIVD
0 VARIABLE DIVN

: DIVSTEP RSHIFT DIVN @  1 AND OR DUP 4 PICK >= IF 2 PICK - SWAP 1 OR SWAP THEN ;
: DIVI 
    DIVN ! DIVD !
    DIVD @ 0= IF ." Divide by zero" ABORT THEN
    0 0
    
    DIVSTEP   
;

\ redirect input

WELCOME
CLEAR_TIB
QUOTE UARTEKEY INPUT-STREAM !

\ ARM opcodes
\ : BIN-> BASE @ BINARY WORD NUMBER CONSTANT BASE ! ;

\ 0 VARIABLE OPC
\ : ENUM 0 OPC ! BEGIN OPC @ CONSTANT OPC 1 +! 1- 0= UNTIL ;
\ 16 ENUM EQ NE CS CC MI PL VS VC HI LS GE LT GT LE AL UNC
\ data processing opcodes
\ 16 ENUM AND EOR SUB RSB ADD ADC SBC RSC TST TEQ CMP CMN ORR MOV BIC MVN 

\ BIN-> 1010 B 
\ BIN-> 1011 BL 





\ TODO
\ signed division
\ color change on : 
\ line editor
\ quotations
\ read-line as accept (char to terminate)
\ private namespaces
\ ?do / +LOOP / LEAVE / UNLOOP
\ alloc / free / resize
\ inline / code
\ fixed point: */ .FX / SIN.COS / SQRT / EXP
\ toupper / tolower
\ word completion
\ struct
\ clear / cmove / fill / blank
\ strcompare
\ make word names indirect : | FLAGS | STRPTR | CFA | | CODE ...

\ namespaces
\ input streams (evaluate...)\
\ output streams
\ some string handling
\ structs, lists



