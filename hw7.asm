***********************************************************************
*
* Title:          SCI Serial Port and 7-segment Display at PORTB
*
* Objective:     Homework 7
*
* Revision:       V3.2  for CodeWarrior 5.2 Debugger Simulation
*
* Date:	          Oct. 22, 2020
*
* Programmer:     Yao Xu
*
* Company:        The Pennsylvania State University
*                 Department of Computer Science and Engineering
*
* Program:        Simple SCI Serial Port I/O and Demonstration
*                 Typewriter program and 7-Segment display, at PORTB
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

CR          equ         $0d          ; carriage return, ASCII 'Return' key
LF          equ         $0a          ; line feed, ASCII 'next line' character
staddr      equ         $3020        ; start address of the buffer
S           equ         $53          ; S
W           equ         $57          ; W
Q           equ         $51          ; Q
one         equ         $31          ; 1
two         equ         $32          ; 2
three       equ         $33          ; 3
four        equ         $34          ; 4
dolsign     equ         $24          ; $
equalsign   equ         $3d          ; =
zero        equ         $0           ; 0
space       equ         $20          ; 20
minus       equ         $2D          ; -
plus        equ         $2B
star        equ         $2A
div         equ         $2F
stopw        equ         $cf
upper       equ         $FFFF

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
msgbuffof   DC.B        'Please input two maximum three digit numbers and one operator, try again.',$00
msgof       DC.B        'Overflow error',$00
invalidform DC.B        'Invalid input format',$00
noopr       DC.B        'No operator, please reenter',$00
dividing0       DC.B        'dividing zero, error',$00
ecalc       DC.B        'Ecalc>',$00
oprflag     DC.B        $0
recordaddr  DC.W        $00
bufferptr   DC.W        $3022
bias        DC.W        $30
ansbufptr   DC.W        $3050

; Each message ends with $00 (NULL ASCII character) for your program.
;
; There are 256 bytes from $3000 to $3100.  If you need more bytes for
; your messages, you can put more messages 'msg3' and 'msg4' at the end of 
; the program - before the last "END" line.
                                     ; Remaining data memory space for stack,
                                     ;   up to program memory start

*
***********************************************************************
* Program Section: address used [ $3100 to $3FFF ] RAM memory
*
            ORG        $3100        ; Program start address, in RAM
pstart      LDS        #$3100       ; initialize the stack pointer
            ldaa       #$6a
            staa       $3500
            LDAA       #%11111111   ; Set PORTB bit 0,1,2,3,4,5,6,7
            STAA       DDRB         ; as output

            LDAA       #%00000000
            STAA       PORTB        ; clear all bits of PORTB

            ldaa       #$0C         ; Enable SCI port Tx and Rx units
            staa       SCICR2       ; disable SCI interrupts

            ldd        #$0001       ; Set SCI Baud Register = $0001 => 2M baud at 24MHz (for simulation)
;            ldd        #$0002       ; Set SCI Baud Register = $0002 => 1M baud at 24MHz
;            ldd        #$000D       ; Set SCI Baud Register = $000D => 115200 baud at 24MHz
;            ldd        #$009C       ; Set SCI Baud Register = $009C => 9600 baud at 24MHz
            std        SCIBDH       ; SCI port baud rate change
            
            ldx   #msg3              ; print message3
            jsr   printmsg
            
            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar

            ldx   #msg4              ; print message4
            jsr   printmsg

            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar
            
            
init        ldy   #$3020             ; initialize the buffer
            sty   $301D
            ldab  #$00
            stab  $301f
            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar
            ldx   #ecalc             ; print the Ecalc message
            jsr   printmsg
            ldab  #0
            stab  oprflag


looop       jsr   getchar            ; type writer - check the key board
            cmpa  #$00               ; if nothing typed, keep checking
            beq   looop
                                     ; otherwise - what is typed on key board
            jsr   putcharcmd         ; is displayed on the terminal window - echo print
            
            cmpa  #CR
            bne   looop              ; if Enter/Return key is pressed, move the
            ldaa  #LF                ; cursor to next line
            jsr   putchar
            
            lbra  calculator        ; long branch to the calculator application
            
errormsg    ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar
            ldx   #invalidcmd        ; print the second message
            jsr   printmsg

            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar
            
            bra   init


typewriter
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
            beq   tploop
                                     ;  otherwise - what is typed on key board
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
printmsginloop ldaa    1,X-           ;pick up an ASCII character from string
                                      ;   pointed by X register
                                      ;then update the X register to point to
                                      ;   the next byte
               cmpa    #$cf
               beq     printmsgindone   ;end of strint yet?
               jsr     putchar        ;if not, print character and do next
               bra     printmsginloop

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
               ldx   #invalidform       ; print the second message
               jsr   printmsg
               lbra  init
;***************end of invalidformat*****************
overflow       ldaa  #CR                ; move the cursor to beginning of the line
               jsr   putchar            ;   Cariage Return/Enter key
               ldaa  #LF                ; move the cursor to next line, Line Feed
               jsr   putchar
               ldx   #msgof         ; print the second message
               jsr   printmsg
               lbra  init
               
noopre          ldaa  #CR                ; move the cursor to beginning of the line
               jsr   putchar            ;   Cariage Return/Enter key
               ldaa  #LF                ; move the cursor to next line, Line Feed
               jsr   putchar
               ldx   #noopr         ; print the second message
               jsr   printmsg
               lbra  init

divzero          ldaa  #CR                ; move the cursor to beginning of the line
               jsr   putchar            ;   Cariage Return/Enter key
               ldaa  #LF                ; move the cursor to next line, Line Feed
               jsr   putchar
               ldx   #dividing0         ; print the second message
               jsr   printmsg
               lbra  init


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
               
calloop        ldy   bufferptr
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
calplus        stx   $3060
               ldx   $0
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
calminus       stx   $3060
               ldx   $0
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
               lbra    divloop
               
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
calmult        stx   $3060
               ldx   $0
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
               lbra    divloop
               
               
               
               
;***************caldiv*****************
caldiv         stx   $3060
               ldx   $0
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
validationbuffer
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
               
vbend          
               pula
               puly
               rts              
;***************end of validationbuffer*****************




;***************checkopr************************
;* Program: identify the second char
;**********************************************              
checkopr       ldaa   y
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
