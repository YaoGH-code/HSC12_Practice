***********************************************************************
*
* Title:          SCI Serial Port and 7-segment Display at PORTB
*
* Objective:      CMPEN 472 Homework 6, in-class-room demonstration
*                 program
*
* Revision:       V3.2  for CodeWarrior 5.2 Debugger Simulation
*
* Date:	          Oct. 15, 2020
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
curaddr     DC.W        $3020
CCount      DC.B        $00
Buff        DS.B        5
            ORG         $2000
msg3        DC.B        'Welcome to the Simple Memory Access Program!', $00
msg4        DC.B        'Please enter commands and hit Enter', $00

level       DC.b        $0
levelu      DC.b        $64
upper       DC.b        $64
lower       DC.b        $0
biasl       DC.b        $30
biash       DC.b        $37
bufferptr   DC.W        $3022

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
            
            
init        ldy   #$3020
            sty   $301D
            ldab  #$00
            stab  $301f

looop       jsr   getchar            ; type writer - check the key board
            cmpa  #$00               ;  if nothing typed, keep checking
            beq   looop
                                     ;  otherwise - what is typed on key board
            jsr   putcharcmd         ; is displayed on the terminal window - echo print
            
            cmpa  #CR
            bne   looop              ; if Enter/Return key is pressed, move the
            ldaa  #LF                ; cursor to next line
            jsr   putchar
            
            ldaa  $3020              ; load the first char in the buffer
            cmpa  #S                 ; compare with S
            lbeq   secondcharS       ; find the second char

            cmpa  #W                 ; compare with W
            lbeq   secondcharW       ; find the second char
            
            cmpa  #Q                 ; compare with Q
            lbeq   typewriter        ; find the second char
            
errormsg    ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar
            ldx   #msg13             ; print the second message
            jsr   printmsg

            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar
            
            ldy    #$3022
            sty    bufferptr
            
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





;***************putchar2************************
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
               inx
               stx   curaddr
               pulx
               rts
;***************end of putchar2*****************





;***************secondcharS************************
;* Program: identify the second char
;**********************************************              
secondcharS    psha
               pshb
               pshx
               pshd
               pshy
               ldaa   #dolsign
               jsr    putchar   
               ldx    #$0
               
               ldab   $3021
               cmpb   #$24
               lbne   errormsg
               
scsloop        ldy    bufferptr
               ldab   y
               subb   #$46
               lbhi   errormsg
               ldaa   y               ;minus bias
               jsr    putchar
               ldab   y
               subb   #$40
               lbhs   highbias
               ldab   y
               subb   biasl  
scsback        abx
               iny
               sty    bufferptr
               ldab   y    
               cmpb   #CR
               beq    quitS
               ldy    #$10
               tfr    x,d
               emul
               tfr    d,x
               bra    scsloop
                             
quitS          ldaa   #$0
               staa   $3021
               ldy    #$3022
               sty    bufferptr
               ldaa   #equalsign
               jsr    putchar
               ldaa   #dolsign
               jsr    putchar
               ldab   x                  ; put data into d from input address
               clra
               
               pshd
               
               
divloop        ldx    #$10               ; division to get each digit
               idiv   
               cpx    #zero
               beq    printdic
               tfr    b,a
               subb   #$A
               bhs    highbias2
               adda   biasl
scsback2       ldy    bufferptr
               staa   y
               iny   
               sty    bufferptr
               tfr    x,d
               bra    divloop
               
printdic       ldy    bufferptr              
               tfr    b,a
               adda   biasl               ; print information
               staa   y
               ldx    bufferptr
               jsr    printmsgin
               ldaa   #space
               jsr    putchar               
          
                                
               puld
               staa   $3021
               ldy    #$3022
               sty    bufferptr                      

divloopdec     ldx    #$A               ; division to get each digit
               idiv   
               cpx    #zero
               beq    printdicdec
               tfr    b,a
               adda   biasl
               ldy    bufferptr
               staa   y
               iny   
               sty    bufferptr
               tfr    x,d
               bra    divloopdec
               
printdicdec    ldy    bufferptr              
               tfr    b,a
               adda   biasl
               staa   y
               ldx    bufferptr
               jsr    printmsgin

               ldaa   #CR
               jsr    putchar
               ldy    #$3022
               sty    bufferptr               
               
               puly
               puld
               pulx
               pulb
               pula
               lbra    init
;***************end of secondcharS*****************




;***************biascalculator************************
;* Program: identify the second char
;**********************************************   
highbias       ldab  y
               subb  biash
               lbra   scsback    

highbias2      adda  biash
               lbra   scsback2
;***************end of biascalculator****************               
               
 ;***************biascalculator************************
;* Program: identify the second char
;**********************************************   
highbiasw      ldab  y
               subb  biash
               lbra   scwback    

highbias2w     ldab  y
               subb  biash
               lbra   scwback2
;***************end of biascalculator****************  



;***************secondcharF************************
;* Program: identify the second char
;**********************************************              


secondcharW    

               psha
               pshb
               pshx                   ;push registers
               pshd
               pshy
               ldaa   #dolsign
               jsr    putchar   
               ldx    #$0
               
               ldab   $3021
               cmpb   #$24
               lbne    errormsg
               
scwloop        ldy    bufferptr
               ldab   y
               subb   #$46
               lbhi   errormsg
               ldaa   y
               jsr    putchar        ;minus bias
               ldab   y
               subb   #$40
               bhs    highbiasw
               ldab   y
               subb   biasl  
scwback        abx
               iny
               sty    bufferptr
               ldab   y    
               cmpb   #space
               beq    addresspro    ;process the address
               ldy    #$10
               tfr    x,d
               emul
               tfr    d,x
               bra    scwloop
               
addresspro     pshx  
               ldy    bufferptr
               iny
               sty    bufferptr
               ldab   y
               cmpb   #dolsign       ;print some signs
               bne    decimalbra

               ldx    #$0
               ldd    #$0
               ldy    bufferptr
               iny
               
scwloop2       ldy    bufferptr
               ldab   y
               subb   #$40
               lbhs   highbias2w
               ldab   y
               subb   biasl  
scwback2       abx
               iny                   ;process input values
               sty    bufferptr
               ldab   y    
               cmpb   #CR
               beq    quitW
               ldy    #$10
               tfr    x,d
               emul
               tfr    d,x
               bra    scwloop2
quitW          tfr    x,d
               pulx
               stab   x
               lbra    quitS

decimalbra     cmpb   #minus         ;if input is in decimal
               lbeq    errormsg
               ldx    #$0
               ldd    #$0
               
scwloopdec     ldy    bufferptr
               ldab   y
               subb   biasl  
               abx
               iny
               sty    bufferptr
               ldab   y    
               cmpb   #CR            ; convert to hex
               beq    quitWdec
               ldy    #$A
               tfr    x,d
               emul
               tfr    d,x
               bra    scwloopdec
quitWdec       tfr    x,d
               pulx
               stab   x
               lbra    quitS
                  
;***************end of secondcharF*****************





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
               cmpa    #NULL
               beq     printmsgindone   ;end of strint yet?
               jsr     putchar        ;if not, print character and do next
               bra     printmsginloop

printmsgindone pulx 
               pula
               rts
;***********end of printmsgin********************





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
msg13          DC.B    'Error: Invalid command, wrong data or address', $00

end

               END               ; this is end of assembly source file
                                 ; lines below are ignored - not assembled/compiled
