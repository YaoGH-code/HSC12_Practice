***********************************************************************
*
* Title:          SCI Serial Port and 7-segment Display at PORTB
*
* Objective:      Homework 5
*
* Revision:       V3.2  for CodeWarrior 5.2 Debugger Simulation
*
* Date:	          Sep. 16, 2020
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
L           equ         $4c          ; L
F           equ         $46          ; F
Q           equ         $51          ; Q
one         equ         $31          ; 1
two         equ         $32          ; 2
three       equ         $33          ; 3
four        equ         $34          ; 4

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
msg3        DC.B        'L1:Turn on LED1', $00
msg4        DC.B        'F1:Turn off LED1', $00
msg5        DC.B        'L2:LED 2 dim up', $00
msg6        DC.B        'F2:LED 2 dim down', $00
msg7        DC.B        'L3:Turn on LED3', $00
msg8        DC.B        'F3:Turn off LED3', $00
msg9        DC.B        'L4:Turn on LED4', $00
msg10       DC.B        'F4:Turn off LED4', $00
msg11       DC.B        'QUIT:Quit menu program', $00
level       DC.b        $0
levelu      DC.b        $64
upper       DC.b        $64
lower       DC.b        $0

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

            
            ldx   #msg3              ; print the first message, 'Hello'
            jsr   printmsg
            
            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar

            ldx   #msg4              ; print the second message
            jsr   printmsg

            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar
             
            ldx   #msg5              ; print the first message, 'Hello'
            jsr   printmsg
            
            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar

            ldx   #msg6              ; print the second message
            jsr   printmsg

            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar
            
            ldx   #msg7              ; print the first message, 'Hello'
            jsr   printmsg
            
            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar

            ldx   #msg8              ; print the second message
            jsr   printmsg

            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar
            
            ldx   #msg9              ; print the first message, 'Hello'
            jsr   printmsg
            
            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar

            ldx   #msg10             ; print the second message
            jsr   printmsg

            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar
            
            ldx   #msg11             ; print the second message
            jsr   printmsg

            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar
            
            ldx   #msg12             ; print the second message
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
            cmpa  #L                 ; compare with L
            lbeq   secondcharL       ; find the second char

            cmpa  #F                 ; compare with F
            lbeq   secondcharF       ; find the second char
            
            cmpa  #Q                 ; compare with Q
            lbeq   typewriter        ; find the second char
            
            ldx   #msg13             ; print the second message
            jsr   printmsg

            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar
            
            bra   init


typewriter
            ldx   #msg1              ; print the first message, 'Hello'
            jsr   printmsg
            
            ldaa  #CR                ; move the cursor to beginning of the line
            jsr   putchar            ;   Cariage Return/Enter key
            ldaa  #LF                ; move the cursor to next line, Line Feed
            jsr   putchar

            ldx   #msg2              ; print the second message
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





;***************LEDcontrols************************
;* Program: subroutines for LEDs control
;**********************************************              
LED1on         bset  PORTB,%00000001     ; set LED1 on
               lbra   init
               
LED1off        bclr  PORTB,%00000001     ; set LED1 off
               lbra   init
               
LED3on         bset  PORTB,%00000100     ; set LED3 on
               lbra   init
               
LED3off        bclr  PORTB,%00000100     ; set LED3 off
               lbra   init  
                            
LED4on         bset  PORTB,%00001000     ; set LED4 on
               lbra   init
                              
LED4off        bclr  PORTB,%00001000     ; set LED4 off
               lbra   init
;***************end of LEDcontrols*****************





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





;***************secondcharL************************
;* Program: identify the second char
;**********************************************              
secondcharL    ldaa  $3021                 ; different cases for the seconf char in the buffer
               cmpa  #one
               beq   LED1on
               cmpa  #two
               beq   LED2dimdownu
               cmpa  #three
               beq   LED3on
               cmpa  #four
               beq   LED4on
;***************end of secondcharL*****************





;***************secondcharF************************
;* Program: identify the second char
;**********************************************              
secondcharF    ldaa  $3021                 ; different cases for the seconf char in the buffer
               cmpa  #one
               beq   LED1off
               cmpa  #two
               beq   LED2dimdownh
               cmpa  #three
               beq   LED3off
               cmpa  #four
               beq   LED4off
;***************end of secondcharF*****************





;***************LED2dimup************************
;* Program: LED2dimup
;**********************************************              
LED2dimup 
               LDAA      level
               CMPA      upper             ; compare is level at 100 or not
               LBEQ      init              ; brunch back to the main loop
               JSR       dim               ; jump to dim
               INC       level             ; inc the level
               BRA       LED2dimup       
;***************end of LED2dimup*****************





;***************LED2dimdownh************************
;* Program: LED2dimdownh
;*********************************************
LED2dimdownh  pshb
              ldab      #$64
              staa      level              ; update the level value
              pulb
              bra       LED2dimdown
;***************end of LED2dimdownh*****************
;***************LED2dimdownu************************
;* Program: LED2dimdownu
;*********************************************
LED2dimdownu  pshb
              ldab      #$00               ; update the level value 
              staa      level
              pulb
              bra       LED2dimup
;***************end of LED2dimdownu*****************




;***************LED2dimdown************************
;* Program: LED2dimdown
;**************************************************              
LED2dimdown  
             LDAA      level               
             CMPA      lower               ; check the level is at 0 or not
             LBEQ      init                ; branch back
             JSR       dim                 ; jump to the subloop
             DEC       level               ; dec the level
             BRA       LED2dimdown     
;***************end of LED2dimdown****************





;***************dim************************
;* Program: dim
;**************************************************   
dim         PSHA
            PSHB
            LDAA      level               ;level number
            LDAB      upper               ;for 100-level
            SUBB      level

            CMPA      lower
            BEQ       offLED3
            BSET      PORTB,%00000010     ; set LED3 light 
                               
onnloop     JSR       delay10usc          ; jump to delay10usc
            DECA                          ; decrease X by 1
            BNE       onnloop             ; check the value of X
            
            CMPB      lower
            BEQ       quit
            
offLED3     BCLR      PORTB,%00000010     ; clear bits
            BSET      PORTB,%00000000 
            
offloop     JSR       delay10usc          ; jump to delay10usc
            DECB                          ; decrease Y by 1
            BNE       offloop             ; check the value of Y
            
quit        LDAA      level               ; quit the dim loop
            CMPA      #99
            BEQ       seton
bc          PULA
            PULB
            RTS

seton       BSET      PORTB,%00000010
            BRA       bc
;***************end of dim****************





;***************delay10usc************************
;* Program: delay10usc
;**************************************************   
delay10usc
            PSHX                           ; save X
            LDX   #58                      ; short delay
                                           ; based on my calculation, 57.5 loops will ahieve a 10us delay so I use 58 
Loop        DEX
            BNE   Loop
            PULX                           ; restore X
            RTS 
;***************end of delay10usc****************





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
msg13          DC.B    'Error: Invalid command', $00


               END               ; this is end of assembly source file
                                 ; lines below are ignored - not assembled/compiled
