***********************************************************************
*
* Title:          Calculator and clock
*
* Objective:     CMPEN 472 Homework 8
*
* Revision:       V3.2  for CodeWarrior 5.2 Debugger Simulation
*
* Date:	          Oct. 30, 2021
*
* Programmer:     Yao Xu
*
* Company:        The Pennsylvania State University
*                 Department of Computer Science and Engineering
*
* Program:        Simple Calculator and clock program
*                 
*
* Algorithm:      Simple Serial I/O use, typewriter
*
* Register use:	  A: Serial port data
*                 X,Y: Delay loop counters
*
* Memory use:     RAM Locations from $3000 for data, 
*                 RAM Locations from $3100 for program
*
* Output:         
*                 PORTB bit 7 to bit 4, 7-segment MSB
*                 PORTB bit 3 to bit 0, 7-segment LSB
*
* Observation:    This is a typewriter program that displays ASCII
*                 data on PORTB - 7-segment displays.
*
***********************************************************************
* Parameter Declearation Section
*
* Export Symbols
            XDEF        pstart       ; export 'pstart' symbol
            ABSENTRY    pstart       ; for assembly entry point
  
* Symbols and Macros
PORTB       EQU         $0001        ; i/o port B addresses
DDRB        EQU         $0003

SCIBDH      EQU         $00C8        ; Serial port (SCI) Baud Register H
SCIBDL      EQU         $00C9        ; Serial port (SCI) Baud Register L
SCICR2      EQU         $00CB        ; Serial port (SCI) Control Register 2
SCISR1      EQU         $00CC        ; Serial port (SCI) Status Register 1
SCIDRL      EQU         $00CF        ; Serial port (SCI) Data Register

CRGFLG      EQU         $0037        ; Clock and Reset Generator Flags
CRGINT      EQU         $0038        ; Clock and Reset Generator Interrupts
RTICTL      EQU         $003B        ; Real Time Interrupt Control

CR          equ         $0d          ; carriage return, ASCII 'Return' key
LF          equ         $0a          ; line feed, ASCII 'next line' character
staddr      equ         $3020        ; start address of the buffer
s           equ         $73          ; s
W           equ         $57          ; W
q           equ         $71          ; q
one         equ         $31          ; 1
two         equ         $32          ; 2
three       equ         $33          ; 3
four        equ         $34          ; 4
dolsign     equ         $24          ; $
equalsign   equ         $3d          ; =
zero        equ         $0           ; 0
space       equ         $20          ; 20
minus       equ         $2D          ; -
plus        equ         $2B          ; +
star        equ         $2A          ; *
div         equ         $2F          ; /
stopw       equ         $cf          ; inverse reading stop sign
upper       equ         $FFFF        ; upper bound
timerup     equ         $3024        ; timer upper bound

***********************************************************************
* Data Section: address used [ $3000 to $30FF ] RAM memory
*
            ORG         $3000        ; Reserved RAM memory starting address 
                                     ;   for Data for CMPEN 472 class
Counter1    DC.W        $008F        ; X register count number for time delay
                                     ;   inner loop for msec
Counter2    DC.W        $000C        ; Y register count number for time delay
                                     ;   outer loop for sec
msg1        DC.B        'Hello', $00
msg2        DC.B        'You may type below', $00
curaddr     DC.W        $3020        ; pointer for the putcharcmd
CCount      DC.B        $00          ; start at $3019
Buff        DS.B        8            ; srart at $3020

            ORG         $2000
msg3        DC.B        'Welcome to the elementary calculator!', $00
msg4        DC.B        'Please enter commands and hit Enter', $00
msgbuffof   DC.B        'Please input two maximum three digit numbers and one operator OR correct s/q commond, try again.',$00
msgof       DC.B        'Overflow error',$00
invalidform DC.B        'Invalid input format',$00
invalidset  DC.B        'Invalid time format. Correct example => 0 to 59',$00
noopr       DC.B        'No operator, please reenter',$00
dividing0   DC.B        'dividing zero, error',$00
tottypewriter DC.B      'Stop clock and calculator, start Typewrite program',$00
ecalc       DC.B        'Tcalc>',$00 ; information for users
oprflag     DC.B        $0           ; operator flag
recordaddr  DC.W        $00          ; recorded address
bufferptr   DC.W        $3022        ; buffer pointer
bias        DC.W        $30          ; bias
ansbufptr   DC.W        $3050        ; answer buffer pointer
oprpos      DC.W        $0           ; operator positions
lensecnum   DC.B        $4           ; second number length
biasl       DC.B        $30          ; low bias
setupper    DC.W        $003b        ; upper bound of seeting clock

            ORG         $2600
clockBuff   DS.B        4            ; clock buffer srart at $2600
clockBufptr DC.W        $2600        ; clock buffer pointer

            ORG         $2700
rr          DC.b        12           ; not used
ctr2p5m     DS.W        1            ; interrupt counter for 2.5 mSec. of time
times       DS.B        1            ; counter for seconds
timem       DS.B        1            ; counter for minutes
timeh       DS.B        1            ; counter for hours

; Each message ends with $00 (NULL ASCII character) for your program.
;
; There are 256 bytes from $3000 to $3100.  If you need more bytes for
; your messages, you can put more messages 'msg3' and 'msg4' at the end of 
; the program - before the last "END" line.
                                     ; Remaining data memory space for stack,
                                     ;   up to program memory start
;*******************************************************
; interrupt vector section
            ORG         $FFF0        ; RTI interrupt vector setup for the simulator
;           ORG         $3FF0        ; RTI interrupt vector setup for the CSM-12C128 board
            DC.W        rtiisr       ; load address to the interrupt vector

;*******************************************************
; code section

*
***********************************************************************
* Program Section: address used [ $3100 to $3FFF ] RAM memory
*
            ORG        $3100         ; Program start address, in RAM
pstart      LDS        #$3100        ; initialize the stack pointer

            LDAA       #%11111111    ; Set PORTB bit 0,1,2,3,4,5,6,7
            STAA       DDRB          ; as output

            LDAA       #%00000000
            STAA       PORTB         ; clear all bits of PORTB

            ldaa       #$0C          ; Enable SCI port Tx and Rx units
            staa       SCICR2        ; disable SCI interrupts

            ldd        #$0001        ; Set SCI Baud Register = $0001 => 2M baud at 24MHz (for simulation)
;           ldd        #$0002        ; Set SCI Baud Register = $0002 => 1M baud at 24MHz
;           ldd        #$000D        ; Set SCI Baud Register = $000D => 115200 baud at 24MHz
;           ldd        #$009C        ; Set SCI Baud Register = $009C => 9600 baud at 24MHz
            std        SCIBDH        ; SCI port baud rate change
            
            ldx        #msg3         ; print message3
            jsr        printmsg
            
            ldaa       #CR           ; move the cursor to beginning of the line
            jsr        putchar       ;   Cariage Return/Enter key
            ldaa       #LF           ; move the cursor to next line, Line Feed
            jsr        putchar

            ldx        #msg4         ; print message4
            jsr        printmsg

            ldaa       #CR           ; move the cursor to beginning of the line
            jsr        putchar       ;   Cariage Return/Enter key
            ldaa       #LF           ; move the cursor to next line, Line Feed
            jsr        putchar
            
            bset       RTICTL,%00011001 ; set RTI: dev=10*(2**10)=2.555msec for C128 board
                                        ; 4MHz quartz oscillator clock
            bset       CRGINT,%10000000 ; enable RTI interrupt
            bset       CRGFLG,%10000000 ; clear RTI IF (Interrupt Flag)
            
            ldx        #0
            stx        ctr2p5m          ; initialize interrupt counter with 0.
            ldaa       #0
            staa       times
            cli
                        
init        ldy        #$3020        ; initialize the buffer
            sty        $301D
            ldab       #$00
            stab       $301f
            ldaa       #CR           ; move the cursor to beginning of the line
            jsr        putchar       ;   Cariage Return/Enter key
            ldaa       #LF           ; move the cursor to next line, Line Feed
            jsr        putchar
            ldx        #ecalc        ; print the Ecalc message
            jsr        printmsg
            ldab       #0
            stab       oprflag

looop       jsr        LEDtoggle     ; if 0.5 second is up, toggle the LED 
            jsr        getchar       ; type writer - check the key board
            cmpa       #$00          ; if nothing typed, keep checking
            beq        looop         ; otherwise - what is typed on key board
            jsr        putcharcmd    ; is displayed on the terminal window - echo print            
            cmpa       #CR
            bne        looop         ; if Enter/Return key is pressed, move the
            ldaa       #LF           ; cursor to next line
            jsr        putchar
            ldaa       $3020
            cmpa       #s            ; check the s and q commond
            lbeq       scommond
            cmpa       #q
            lbeq       qcommond
            lbra       calculator    ; long branch to the calculator application
            
errormsg    ldaa       #CR           ; move the cursor to beginning of the line
            jsr        putchar       ;   Cariage Return/Enter key
            ldaa       #LF           ; move the cursor to next line, Line Feed
            jsr        putchar
            ldx        #invalidcmd   ; print the invalid message
            jsr        printmsg
            ldaa       #CR           ; move the cursor to beginning of the line
            jsr        putchar       ; Cariage Return/Enter key
            ldaa       #LF           ; move the cursor to next line, Line Feed
            jsr        putchar
            
            bra        init


typewriter  ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF 
            ldx   #msg1              ; print message1
            jsr   printmsg
            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar
            ldx   #msg2              ; print message2
            jsr   printmsg
            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar
            
tploop      jsr   getchar            ; type writer - check the key board
            cmpa  #$00               ;  if nothing typed, keep checking
            beq   tploop             ;  otherwise - what is typed on key board
            jsr   putchar            ; is displayed on the terminal window - echo print
            staa  PORTB              ; show the character on PORTB
            cmpa  #CR
            bne   tploop             ; if Enter/Return key is pressed, move the
            ldaa  #LF 
            jsr   putchar            ; cursor to next line
            bra   tploop

;subroutine section below
;***********printmsg***************************
;* Program: Output character string to SCI port, print message
;* Input:   Register X points to ASCII characters in memory
;* Output:  message printed on the terminal connected to SCI port
;* 
;* Registers modified: CCR
;* Algorithm:
;     Pick up 1 byte from memory where X register is pointing
;     Send it out to SCI port
;     Update X register to point to the next byte
;     Repeat until the byte data $00 is encountered
;       (String is terminated with NULL=$00)
;**********************************************
NULL           equ     $00
printmsg       psha                   ;Save registers
               pshx
printmsgloop   ldaa    1,X+           ;pick up an ASCII character from string
                                      ;   pointed by X register
                                      ;then update the X register to point to
                                      ;   the next byte
               cmpa    #NULL
               beq     printmsgdone   ;end of strint yet?
               jsr     putchar        ;if not, print character and do next
               bra     printmsgloop

printmsgdone   pulx 
               pula
               rts
;***********end of printmsg********************





;***********RTI interrupt service routine***************
rtiisr      bset   CRGFLG,%10000000 ; clear RTI Interrupt Flag - for the next one
            ldx    ctr2p5m          ; every time the RTI occur, increase
            inx                     ;    the 16bit interrupt count
            stx    ctr2p5m
rtidone     RTI
;***********end of RTI interrupt service routine********





;***************LEDtoggle**********************
;* Program: toggle LED if 0.5 second is up
;* Input:   ctr2p5m variable
;* Output:  ctr2p5m variable and LED1
;* Registers modified: CCR
;* Algorithm:
;    Check for 0.5 second passed
;      if not 0.5 second yet, just pass
;      if 0.5 second has reached, then toggle LED and reset ctr2p5m
;**********************************************
LEDtoggle   psha
            pshx

            ldx      ctr2p5m          ; check for 0.5 sec
;           cpx      #200             ; 2.5msec * 200 = 0.5 sec
            cpx      #80              ; 1 sec adjusted according to my system speed
            blo      doneLED          ; NOT yet

            ldx      #0               ; 1sec is up,
            stx      ctr2p5m          ; clear counter to restart
            
            ldaa     times            ; if the clock is 59 second, reset it 
            cmpa     #59
            bhs      resetsec    
            inca                      ; otherwise, increase it 
            staa     times
            bra      displaysec 
resetsec    ldaa     #0               ; reset the clock to 0
            staa     times             

displaysec  tfr      a,b              ; display the number in decimal
            clra
            jsr      decdisplay

doneLED     pulx
            pula
            rts
;***************end of LEDtoggle***************






;***********decdisplay***************************
;* Program:Convert the second to a decimal 
;          number which will be displayed on
;          the 7-segment display
;**********************************************
decdisplay     pshx
               pshd
               pshy
decdisplayloop                       ; using dividing to convert the number to deciaml
               ldx     #$A              
               idiv    
               cpx     #$0
               beq     decdisplayloopend
               ldy     clockBufptr
               std     y
               iny
               iny   
               sty     clockBufptr
               tfr     x,d
               bra     decdisplayloop
               
decdisplayloopend                   ; store the number in a buffer
               ldy     clockBufptr
               std     y
               lsl     $2603        ; use logical shift to make the number in one byte
               lsl     $2603
               lsl     $2603
               lsl     $2603
               ldaa    $2603
               eora    $2601
               staa    PORTB
               ldy     #$2600
               sty     clockBufptr
               puly
               puld 
               pulx
               rts    
;***********decdisplay***************************
 
 
 


;subroutine section below
;***********printmsgin***************************
;* Program: Output character string to SCI port, print message
;* Input:   Register X points to ASCII characters in memory
;* Output:  message printed on the terminal connected to SCI port
;* 
;* Registers modified: CCR
;* Algorithm:
;     Pick up 1 byte from memory where X register is pointing
;     Send it out to SCI port
;     Update X register to point to the next byte
;     Repeat until the byte data $00 is encountered
;       (String is terminated with NULL=$00)
;**********************************************
printmsgin     psha                   ;Save registers
               pshx
printmsginloop ldaa  1,X-           ;pick up an ASCII character from string
                                      ;   pointed by X register
                                      ;then update the X register to point to
                                      ;   the next byte
               cmpa  #$cf
               beq   printmsgindone   ;end of strint yet?
               jsr   putchar        ;if not, print character and do next
               bra   printmsginloop

printmsgindone pulx 
               pula
               rts
;***********end of printmsgin********************





;***************putchar************************
;* Program: Send one character to SCI port, terminal
;* Input:   Accumulator A contains an ASCII character, 8bit
;* Output:  Send one character to SCI port, terminal
;* Registers modified: CCR
;* Algorithm:
;    Wait for transmit buffer become empty
;      Transmit buffer empty is indicated by TDRE bit
;      TDRE = 1 : empty - Transmit Data Register Empty, ready to transmit
;      TDRE = 0 : not empty, transmission in progress
;**********************************************
putchar        brclr SCISR1,#%10000000,putchar   ; wait for transmit buffer empty
               staa  SCIDRL
               rts
;***************end of putchar*****************





;***************putcharcmd************************
;* Program: Send one character to SCI port, terminal
;* Input:   Accumulator A contains an ASCII character, 8bit
;* Output:  Send one character to SCI port, terminal
;* Registers modified: CCR
;* Algorithm:
;    Wait for transmit buffer become empty
;      Transmit buffer empty is indicated by TDRE bit
;      TDRE = 1 : empty - Transmit Data Register Empty, ready to transmit
;      TDRE = 0 : not empty, transmission in progress
;**********************************************
putcharcmd     brclr SCISR1,#%10000000,putchar   ; wait for transmit buffer empty
               staa  SCIDRL                      ; send a character
               pshx                              
               ldx   curaddr                        
               staa  x                           ; store the char into the buffer
               inc   CCount
               psha  
               ldaa  CCount
               cmpa  #9
               pula
               beq   bufferoverflow
               inx
               stx   curaddr
               pulx
               rts
;***************end of putcharcmd*****************




;***************bufferoverflow************************
;* Program: handling buffer overflow
;**********************************************   
bufferoverflow ldaa  #CR                ; move the cursor to beginning of the line
               jsr   putchar            ;   Cariage Return/Enter key
               ldaa  #LF                ; move the cursor to next line, Line Feed
               jsr   putchar
               ldx   #msgbuffof         ; print the second message
               jsr   printmsg
               lbra  init
;***************end of bufferoverflow*****************
;***************invalidformat************************
;* Program: handling buffer overflow
;**********************************************   
invalidformp   ldaa  #CR                ; move the cursor to beginning of the line
               jsr   putchar            ;   Cariage Return/Enter key
               ldaa  #LF                ; move the cursor to next line, Line Feed
               jsr   putchar
               ldx   #invalidform       ; print the invalidformat message
               jsr   printmsg
               lbra  init

overflow       ldaa  #CR                ; move the cursor to beginning of the line
               jsr   putchar            ;   Cariage Return/Enter key
               ldaa  #LF                ; move the cursor to next line, Line Feed
               jsr   putchar
               ldx   #msgof             ; print the overflow message
               jsr   printmsg
               lbra  init
               
noopre         ldaa  #CR                ; move the cursor to beginning of the line
               jsr   putchar            ;   Cariage Return/Enter key
               ldaa  #LF                ; move the cursor to next line, Line Feed
               jsr   putchar
               ldx   #noopr             ; print the no operator message
               jsr   printmsg
               lbra  init

divzero        ldaa  #CR                ; move the cursor to beginning of the line
               jsr   putchar            ;   Cariage Return/Enter key
               ldaa  #LF                ; move the cursor to next line, Line Feed
               jsr   putchar
               ldx   #dividing0         ; print the dividing zero message
               jsr   printmsg
               lbra  init
               
invalidsetting 
               ldaa  #CR                ; move the cursor to beginning of the line
               jsr   putchar            ;   Cariage Return/Enter key
               ldaa  #LF                ; move the cursor to next line, Line Feed
               jsr   putchar
               ldx   #invalidset        ; print the invalid setting message
               jsr   printmsg
               lbra  init
               
returntotypewriter 
               ldaa  #CR                ; move the cursor to beginning of the line
               jsr   putchar            ;   Cariage Return/Enter key
               ldaa  #LF                ; move the cursor to next line, Line Feed
               jsr   putchar
               ldx   #tottypewriter        ; print the invalid setting message
               jsr   printmsg
               lbra  typewriter
;***************end of invalidformat*****************





;***************scommond*****************

  
scommond                                ; processing the s commond
               ldaa   $3021
               cmpa   #space
               lbne   invalidsetting
               ldy    #$3022
scommondloop   ldaa   y                 ; validating the commond
               cmpa   #CR
               beq    settime
               suba   #$30
               lblo   invalidsetting
               ldaa   y
               suba   #$39
               lbhi   invalidsetting
               iny
               bra    scommondloop
settime        tfr    y,d               
               subd   #$3024
               lbhi   invalidsetting


                    
               ldx    #$0
               ldd    #$0
               ldy    #$3022
               sty    bufferptr
               
settimeloop    ldy    bufferptr         ; converting the number from ascii to hex decimal number
               ldab   y
               subb   biasl  
               abx
               iny
               sty    bufferptr
               ldab   y    
               cmpb   #CR
               beq    quitSet
               ldy    #$A
               tfr    x,d
               emul
               tfr    d,x
               bra    settimeloop
                                        ; set the timer
quitSet        tfr    x,d
               subd   #59
               lbhi   invalidsetting
               tfr    x,d
               ldy    #$3022
               sty    bufferptr
               decb
               stab   times           
               lbra   init                                                 
                
;***************end of scommond*****************




;***************qcommond*****************
qcommond       lbra    returntotypewriter       ; quit the clock and Tcal and return to the type writer
;***************end of qcommond*****************






;***************calculator************************
;* Program: identify the second char
;**********************************************
         
calculator     ;ldaa  #$31
               ;staa  $3020
               ;ldaa  #$35
               ;staa  $3021
               ;ldaa  #plus
               ;staa  $3022
               ;ldaa  #$31
               ;staa  $3023
               ;ldaa  #$35
               ;staa  $3024
               ;ldaa  #CR
               ;staa  $3025
               ldx   #$0
               
               jsr   validationbuffer
               ldaa  #equalsign
               jsr   putchar  
               ldy   #$3020
               sty   bufferptr
               
calloop        ldy   bufferptr       ; identify the operator and go to a subroutine
               ldab  y
               subb  #$30
               abx
               iny
               sty   bufferptr
               ldab  y    
               cmpb  #plus
               lbeq  calplus
               cmpb  #minus
               lbeq  calminus
               cmpb  #star
               lbeq  calmult
               cmpb  #div
               lbeq  caldiv
               ldy   #$A
               tfr   x,d
               emul
               tfr   d,x
               bra   calloop
;***************end of calculator*****************





;***************calplus*****************
calplus        stx   $3060          ; process addition
               ldx   #$0
               ldy   bufferptr
               iny
               sty   bufferptr
               
plusloop       ldy   bufferptr
               ldab  y
               subb  #$30
               abx
               iny
               sty   bufferptr
               ldab  y    
               cmpb  #CR
               lbeq  plend
               ldy   #$A
               tfr   x,d
               emul
               tfr   d,x
               bra   plusloop

plend          tfr    x,d
               addd   $3060
               
               ldx    #$3050
               stx    ansbufptr
               tfr    d,y
               ldab   #$cf
               stab   $3050
               tfr    y,d
               inx
               stx    ansbufptr
               lbra   divloop
               
;***************calminus*****************
calminus       stx   $3060        ; process subtraction
               ldx   #$0
               ldy   bufferptr
               iny
               sty   bufferptr
               
minusloop      ldy   bufferptr
               ldab  y
               subb  #$30
               abx
               iny
               sty   bufferptr
               ldab  y    
               cmpb  #CR
               lbeq  mlend
               ldy   #$A
               tfr   x,d
               emul
               tfr   d,x
               bra   minusloop

mlend          tfr    x,d
               tfr    d,y
               ldd    $3060
               sty    $3060
               subd   $3060
               lblo   negitive
               
mlendback      ldx    #$3050
               stx    ansbufptr
               tfr    d,y
               ldab   #$cf
               stab   $3050
               tfr    y,d
               inx
               stx    ansbufptr
               lbra   divloop
               
negitive       
               std    $3060
               com    $3060
               com    $3061
               inc    $3061
               ldaa   #minus
               jsr    putchar
               ldd    $3060
               bra    mlendback               
                              
;***************calmult*****************
calmult        stx   $3060        ; process multipilication
               ldx   #$0
               ldy   bufferptr
               iny
               sty   bufferptr
               
multloop       ldy   bufferptr
               ldab  y
               subb  #$30
               abx
               iny
               sty   bufferptr
               ldab  y    
               cmpb  #CR
               lbeq  mllend
               ldy   #$A
               tfr   x,d
               emul
               tfr   d,x
               bra   multloop

mllend         tfr    x,d
               ldy    #$0
               ldy    $3060
               emul
               cpy    #$0
               lbne   overflow
               
               ldx    #$3050
               stx    ansbufptr
               tfr    d,y
               ldab   #$cf
               stab   $3050
               tfr    y,d
               inx
               stx    ansbufptr
               lbra   divloop
               
;***************caldiv*****************
caldiv         stx   $3060       ; process division
               ldx   #$0
               ldy   bufferptr
               iny
               sty   bufferptr
               
divlloop       ldy   bufferptr
               ldab  y
               subb  #$30
               abx
               iny
               sty   bufferptr
               ldab  y    
               cmpb  #CR
               lbeq  dlend
               ldy   #$A
               tfr   x,d
               emul
               tfr   d,x
               bra   divlloop

dlend         tfr    x,d
              ldd   $3060
              cpx   #$0
              lbeq  divzero
              idiv
              
              tfr    x,d
               
               ldx    #$3050
               stx    ansbufptr
               tfr    d,y
               ldab   #$cf
               stab   $3050
               tfr    y,d
               inx
               stx    ansbufptr
               lbra    divloop
               
               
;***************divloop*****************       
divloop        ldx    #$A              
               idiv   
               cpx    #$0
               beq    printdec
               addd   bias
               ldy    ansbufptr
               std    y
               iny
               iny   
               sty    ansbufptr
               tfr    x,d
               bra    divloop
               
printdec       ldy    ansbufptr              
               addd   bias               ; print information
               std    y
               iny
               sty    ansbufptr
               ldx    ansbufptr
               jsr    printmsgin
               ldaa   #space
               jsr    putchar
               
               lbra   init               
          
;***************validationbuffer************************
;* Program: identify the second char
;**********************************************              
validationbuffer                         ; used to validate the content in the buffer
               pshy
               psha
               ;ldaa   #$2A
               ;staa   $3020
               ldy    #$3020
               ldaa   y
               jsr    putchar
               suba   #$30
               lblo   invalidformp
               ldaa   y
               suba   #$39
               lbhi   invalidformp
               ldy    #$3021
vbloop         ldaa   y
               cmpa   #CR
               beq    vbend
               jsr    putchar
               suba   #$39
               lbhi   invalidformp
               ldaa   y
               suba   #$2A
               lblo   invalidformp
               ldaa   y
               suba   #$30
               lblo   checkopr
checkoprback   iny
               bra    vbloop
               
vbend          tfr    y,d
               subd   oprpos
               subb   lensecnum
               lbhi   invalidformp    
               pula
               puly
               rts              
;***************end of validationbuffer*****************




;***************checkopr************************
;* Program: identify the second char
;**********************************************              
checkopr       sty    oprpos           ; validate the operator
               ldaa   y
               ldab   oprflag
               cmpb   #1
               lbeq   invalidformp
               ldab   #1
               stab   oprflag
               cpy    #$3023
               lbhi   invalidformp
               cmpa   #plus
               beq    checkoprback
               cmpa   #minus
               beq    checkoprback
               cmpa   #star
               beq    checkoprback
               cmpa   #div
               beq    checkoprback
               
               lbra   invalidformp
                       
;***************end of checkopr*****************


;****************getchar***********************
;* Program: Input one character from SCI port (terminal/keyboard)
;*             if a character is received, other wise return NULL
;* Input:   none    
;* Output:  Accumulator A containing the received ASCII character
;*          if a character is received.
;*          Otherwise Accumulator A will contain a NULL character, $00.
;* Registers modified: CCR
;* Algorithm:
;    Check for receive buffer become full
;      Receive buffer full is indicated by RDRF bit
;      RDRF = 1 : full - Receive Data Register Full, 1 byte received
;      RDRF = 0 : not full, 0 byte received
;**********************************************
getchar        brclr SCISR1,#%00100000,getchar7
               ldaa  SCIDRL
               rts
getchar7       clra
               rts
;****************end of getchar**************** 





;OPTIONAL
;more variable/data section below
; this is after the program code section
; of the RAM.  RAM ends at $3FFF
; in MC9S12C128 chip

msg12          DC.B    'Enter your command below:', $00
invalidcmd     DC.B    'Error: Invalid command, wrong data or address', $00

end
               END               ; this is end of assembly source file
                                 ; lines below are ignored - not assembled/compiled
