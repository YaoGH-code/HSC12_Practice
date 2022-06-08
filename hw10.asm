;*******************************************************
;* Homework 9: Timer Interrupt Sample Program, 
;* MC9S12C128 Program (set to MC9S12C32 for Simulation/Debug)
;* CodeWarrior Simulator/Debug edition, not for CSM-12C128 board
;* 
;* This program is a 1024 data transfer program running on the 
;*   CodeWarrior Debugger/Simulator as follows: 
;*   1. Program starts with print messages on the simulator Terminal,
;*      an intro message at 2M baud (this program will not work 
;*      on the CSM-12C128 board - 2M baud too fast).
;*   2. Then user may hit any key, it's a typewriter program at 2M baud.
;*      But hitting the Enter key will terminate the typewriter mode with 
;*      the instruction message print.
;*   3. Two messages are (1) start terminal data capture into a file and
;*      (2) hit Enter key for the 1024 data transfer to begin. 
;*   4. At this time, user setup the Terminal Output file, data capture to a file.
;*   5. User hits an Enter key to send 1024 data, to the Terminal and
;*      the data saved in to a file named RxData3.txt  which may be looked at 
;*      or plotted using Excel sheet.
;*   6. User may repeat the step 3 above as many times as he/she like.
;*      User plots or prints the data to verify the correct data transmit.
;* 
;* We assumed 24MHz bus clock and 4MHz external resonator clock frequency.  
;* 
;*******************************************************
;*******************************************************

; export symbols - program starting point
            XDEF        Entry   ; export 'Entry' symbol
            ABSENTRY    Entry   ; for assembly entry point

; include derivative specific macros
PORTB       EQU         $0001
DDRB        EQU         $0003

ATDCTL2     EQU  $0082            ; Analog-to-Digital Converter (ADC) registers
ATDCTL3     EQU  $0083
ATDCTL4     EQU  $0084
ATDCTL5     EQU  $0085
ATDSTAT0    EQU  $0086
ATDDR0H     EQU  $0090
ATDDR0L     EQU  $0091
ATDDR7H     EQU  $009e
ATDDR7L     EQU  $009f

SCIBDH      EQU         $00C8   ; Serial port (SCI) Baud Register H
SCIBDL      EQU         $00C9   ; Serial port (SCI) Baud Register L
SCICR2      EQU         $00CB   ; Serial port (SCI) Control Register 2
SCISR1      EQU         $00CC   ; Serial port (SCI) Status Register 1
SCIDRL      EQU         $00CF   ; Serial port (SCI) Data Register

CRGFLG      EQU         $0037        ; Clock and Reset Generator Flags
CRGINT      EQU         $0038        ; Clock and Reset Generator Interrupts
RTICTL      EQU         $003B        ; Real Time Interrupt Control

TIOS        EQU         $0040   ; Timer Input Capture (IC) or Output Compare (OC) select
TIE         EQU         $004C   ; Timer interrupt enable register
TCNTH       EQU         $0044   ; Timer free runing main counter
TSCR1       EQU         $0046   ; Timer system control 1
TSCR2       EQU         $004D   ; Timer system control 2
TFLG1       EQU         $004E   ; Timer interrupt flag 1
TC6H        EQU         $005C   ; Timer channel 2 register
wave        equ         $3070   ; indecator of the waveform
     ;
CR          equ         $0d     ; carriage return, ASCII 'Return' key
LF          equ         $0a     ; line feed, ASCII 'next line' character
s           equ         $73          ; s
W           equ         $57          ; W
w           equ         $77          ; w
t           equ         $74          ; t
q           equ         $71          ; q
g           equ         $67
d_l         equ         $64
c           equ         $63             
a_l         equ         $61
space       equ         $20          ; 20
two         equ         $32


;DATAmax     equ         1024    ; Data count maximum, 1024 constant

;*******************************************************
; variable/data section
            ORG    $3000
Counter1    DC.W        $008F        ; X register count number for time delay inner loop for msec
Counter2    DC.W        $000C        ; Y register count number for time delay outer loop for sec
msg1p       DC.B        'Hello', $00
msg2p       DC.B        'You may type below', $00
curaddr     DC.W        $3020        ; pointer for the putcharcmd
CCount      DC.B        $00          ; start at $3019
Buff        DS.B        8            ; srart at $3020

            ORG         $2600
clockBuff   DS.B        4            ; clock buffer srart at $2600
clockBufptr DC.W        $2600        ; clock buffer pointer

            ORG         $2700
ctr2p5m     DS.W        1            ; interrupt counter for 2.5 mSec. of time
times       DS.B        1            ; counter for seconds

            ORG    $2800        ; RAMStart defined as $3000in MC9S12C128 chip
ctr125u     DS.W   1            ; 16bit interrupt counter for 125 uSec. of time
BUF         DS.B   6            ; character buffer for a 16bit number in decimal ASCII
CTR         DS.B   1            ; character buffer fill count
msg1        DC.B   'Hello, this is 1024 data transmit program.', $00
msg2        DC.B   'When ready, enter adc command to start the 1024 point ADC data capture.', $00
bufferptr   DC.W   $3022        ; buffer pointer
biasl       DC.B   $30          ; low bias
tricounter  DC.B   $ff
triflag     DC.B   $0
sqrflag1     DC.B  $0
sqrflag     DC.B   $0
sqrupper    DC.B   $ff
doubleflag  DC.B   $0
DATAmax     DC.w   1024    ; Data count maximum, 1024 constant
prompt      DC.B   'HW10>', $00      


;*******************************************************
; interrupt vector section

            ORG     $FFE2        ; Timer channel 6 interrupt vector setup, on simulator
            DC.W    oc6isr
            ORG     $FFF0        ; RTI interrupt vector setup for the simulator
            DC.W    rtiisr       ; load address to the interrupt vector

;*******************************************************
; code section

            ORG    $3100
Entry
            LDS    #Entry       ; initialize the stack pointer
            
            ; ATD initialization
            LDAA  #%11000000       ; Turn ON ADC, clear flags, Disable ATD interrupt
            STAA  ATDCTL2
            LDAA  #%00001000       ; Single conversion per sequence, no FIFO
            STAA  ATDCTL3
            LDAA  #%10000111       ; 8bit, ADCLK=24MHz/16=1.5MHz, sampling time=2*(1/ADCLK)
            STAA  ATDCTL4          ; for SIMULATION




            LDAA   #%11111111   ; Set PORTB bit 0,1,2,3,4,5,6,7
            STAA   DDRB         ; as output
            LDAA   #%00000000   ; Clear PORTB bit 0,1,2,3,4,5,6,7
            STAA   PORTB        ; Clear all bits of PORTB, initialize

            ldaa   #$0C         ; Enable SCI port Tx and Rx units
            staa   SCICR2       ; disable SCI interrupts

            ldd    #$0001       ; Set SCI Baud Register = $0001 => 2M baud at 24MHz (for simulation)
;            ldd    #$0002       ; Set SCI Baud Register = $0002 => 1M baud at 24MHz
;            ldd    #$000D       ; Set SCI Baud Register = $000D => 115200 baud at 24MHz
;            ldd    #$009C       ; Set SCI Baud Register = $009C => 9600 baud at 24MHz
            std    SCIBDH       ; SCI port baud rate change

            ldx     #msg1            ; print the first message, '1024 data transmit'
            jsr     printmsg
            jsr     nextline
            
            ldx     #msg20            ; print the first message, '1024 data transmit'
            jsr     printmsg
            jsr     nextline
            ldx     #msg21          ; print the first message, '1024 data transmit'
            jsr     printmsg
            jsr     nextline

            ldx     #msg2            ; print the second message, user instruction,
            jsr     printmsg         ;   hit 'Enter'
            
            bset    RTICTL,%00011001 ; set RTI: dev=10*(2**10)=2.555msec for C128 board
                                        ; 4MHz quartz oscillator clock
            bset    CRGINT,%10000000 ; enable RTI interrupt
            bset    CRGFLG,%10000000 ; clear RTI IF (Interrupt Flag)
            ldx     #0
            stx     ctr2p5m          ; initialize interrupt counter with 0.
            ldaa    #0
            staa    times
            cli

            
init        ldy     #$3020           ; initialize the buffer
            sty     $301D
            ldab    #$00
            stab    $301f
            ldab    #$00
            stab    $3020
            ldab    #$00
            stab    $3021
            ldaa    #CR             
            jsr     putchar
            ldaa    #LF              
            jsr     putchar
            ldx     #prompt           
            jsr     printmsg
            

mloop1      jsr     LEDtoggle     ; if 0.5 second is up, toggle the LED 
            jsr     getchar
            cmpa    #0
            beq     mloop1
            jsr     putcharcmd       ; type writer, with echo print
            cmpa    #CR
            bne     mloop1           ; if Enter/Return key is pressed, move the
            ldaa    #LF              ; cursor to next line
            jsr     putchar
            ldaa    $3020
            cmpa    #a_l           ; check the g commond   -> adc
            lbeq    acommond
            cmpa    #s            ; check the s commond
            lbeq    scommond
            cmpa    #q            ; check the q commond
            lbeq    qcommond
            lbra    invalidformp
transmit    jsr     delay1ms         ; flush out SCI serial port 
                                     ; wait to finish sending last characters

            ldx     #0               ; Enter/Return key hit
            stx     ctr125u
            jsr     StartTimer6oc


loop1024    jsr     LEDtoggle        ; if 0.5 second is up, toggle the LED
            ldd     ctr125u
            cpd     DATAmax         ; 1024 bytes will be sent, the receiver at Windows PC 
            bhs     loopTxON         ; will only take 1024 bytes.
            bra     loop1024         ; set Terminal Cache Size to 10000 lines, update from 1000 lines

loopTxON
            LDAA    #%00000000
            STAA    TIE              ; disable OC6 interrupt

            jsr     nextline
            jsr     nextline

            ldx     #msg5            ; print '> Done!  Close Output file.'
            jsr     printmsg
            jsr     nextline

            ldx     #msg6            ; print '> Ready for next data transmission'
            jsr     printmsg
            jsr     nextline
            
            
            ldx    #prompt          ; cursor to next line
            jsr     putchar
            ldd   #1024
            std   DATAmax
            LBRA     init

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

;***********Timer OC6 interrupt service routine***************
oc6isr      ldx   ctr125u
            inx                      ; update OC6 (125usec) interrupt counter
            stx   ctr125u
            ldd   #3000              ; 125usec with (24MHz/1 clock)
            addd  TC6H               ;    for next interrupt
            std   TC6H               ; 
            bset  TFLG1,%01000000    ; clear timer CH6 interrupt flag, not needed if fast clear enabled

            jsr   go2ADC
 
oc2done     RTI
;***********end of Timer OC6 interrupt service routine********


            
;*******************************************************
; subroutine section




;***********single AD conversiton*********************
; This is a sample, non-interrupt, busy wait method
;
go2ADC
            PSHA                   ; Start ATD conversion
            LDAA  #%10000111       ; right justified, unsigned, single conversion,
            STAA  ATDCTL5          ; single channel, CHANNEL 7, start the conversion

adcwait     ldaa  ATDSTAT0         ; Wait until ATD conversion finish
            anda  #%10000000       ; check SCF bit, wait for ATD conversion to finish
            beq   adcwait

            ldab   ATDDR0L          ; for SIMULATOR, pick up the lower 8bit result
            clra
            jsr   pnum10

            PULA
            RTS
;***********end of AD conversiton**************   



;***********RTI interrupt service routine***************
rtiisr      bset   CRGFLG,%10000000 ; clear RTI Interrupt Flag - for the next one
            ldx    ctr2p5m          ; every time the RTI occur, increase
            inx                     ;    the 16bit interrupt count
            stx    ctr2p5m
rtidone     RTI
;***********end of RTI interrupt service routine********


;***************StartTimer6oc************************
;* Program: Start the timer interrupt, timer channel 6 output compare
;* Input:   Constants - channel 6 output compare, 125usec at 24MHz
;* Output:  None, only the timer interrupt
;* Registers modified: D used and CCR modified
;* Algorithm:
;             initialize TIOS, TIE, TSCR1, TSCR2, TC2H, and TFLG1
;**********************************************
StartTimer6oc
            PSHD
            LDAA   #%01000000
            STAA   TIOS              ; set CH6 Output Compare
            STAA   TIE               ; set CH6 interrupt Enable
            LDAA   #%10000000        ; enable timer, Fast Flag Clear not set
            STAA   TSCR1
            LDAA   #%00000000        ; TOI Off, TCRE Off, TCLK = BCLK/1
            STAA   TSCR2             ;   not needed if started from reset

            LDD    #3000            ; 125usec with (24MHz/1 clock)
            ADDD   TCNTH            ;    for first interrupt
            STD    TC6H             ; 

            BSET   TFLG1,%01000000   ; initial Timer CH6 interrupt flag Clear, not needed if fast clear set
            LDAA   #%01000000
            STAA   TIE               ; set CH6 interrupt Enable
            PULD
            RTS
;***************end of StartTimer2oc*****************


;***********pnum10***************************
;* Program: print a word (16bit) in decimal to SCI port
;* Input:   Register D contains a 16 bit number to print in decimal number
;* Output:  decimal number printed on the terminal connected to SCI port
;* 
;* Registers modified: CCR
;* Algorithm:
;     Keep divide number by 10 and keep the remainders
;     Then send it out to SCI port
;  Need memory location for counter CTR and buffer BUF(6 byte max)
;**********************************************
pnum10          pshd                   ;Save registers
                pshx
                pshy
                clr     CTR            ; clear character count of an 8 bit number

                ldy     #BUF
pnum10p1        ldx     #10
                idiv
                beq     pnum10p2
                stab    1,y+
                inc     CTR
                tfr     x,d
                bra     pnum10p1

pnum10p2        stab    1,y+
                inc     CTR                        
;--------------------------------------

pnum10p3        ldaa    #$30                
                adda    1,-y
                jsr     putchar
                dec     CTR
                bne     pnum10p3
                jsr     nextline
                puly
                pulx
                puld
                rts
;***********end of pnum10********************

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
NULL            equ     $00
printmsg        psha                   ;Save registers
                pshx
printmsgloop    ldaa    1,X+           ;pick up an ASCII character from string
                                       ;   pointed by X register
                                       ;then update the X register to point to
                                       ;   the next byte
                cmpa    #NULL
                beq     printmsgdone   ;end of strint yet?
                bsr     putchar        ;if not, print character and do next
                bra     printmsgloop
printmsgdone    pulx 
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
putchar     brclr SCISR1,#%10000000,putchar   ; wait for transmit buffer empty
            staa  SCIDRL                      ; send a character
            rts
;***************end of putchar*****************




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
invalidsetting 
               ldaa  #CR                ; move the cursor to beginning of the line
               jsr   putchar            ;   Cariage Return/Enter key
               ldaa  #LF                ; move the cursor to next line, Line Feed
               jsr   putchar
               ldx   #invalidset        ; print the invalid setting message
               jsr   printmsg
               lbra  init
invalidformp   ldaa  #CR                ; move the cursor to beginning of the line
               jsr   putchar            ;   Cariage Return/Enter key
               ldaa  #LF                ; move the cursor to next line, Line Feed
               jsr   putchar
               ldx   #invalidform       ; print the invalidformat message
               jsr   printmsg
               lbra  init


;***************gcommond*****************
acommond      
              
              ldaa    $3021
              cmpa    #d_l            
              lbne    invalidformp
              ldaa    $3022
              cmpa    #c            
              lbne    invalidformp
              
              ldx     #adcinfo         ; print the second message, user instruction,
              jsr     printmsg         ;   hit 'Enter'
              jsr     nextline
              
              lbra    transmit

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
qcommond       lbra    typewriter       ; quit the clock and Tcal and return to the type writer
;***************end of qcommond*****************


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

getchar     brclr SCISR1,#%00100000,getchar7
            ldaa  SCIDRL
            rts
getchar7    clra
            rts
;****************end of getchar**************** 

;****************nextline**********************
nextline
            psha
            ldaa  #CR              ; move the cursor to beginning of the line
            jsr   putchar          ;   Cariage Return/Enter key
            ldaa  #LF              ; move the cursor to next line, Line Feed
            jsr   putchar
            pula
            rts
;****************end of nextline***************

;****************delay1ms**********************
delay1ms:   pshx
            ldx   #$1000           ; count down X, $8FFF may be more than 10ms 
d1msloop    nop                    ;   X <= X - 1
            dex                    ; simple loop
            bne   d1msloop
            pulx
            rts
;****************end of delay1ms***************
msgbuffof   DC.B   'Please input instruction with correct length.',$00
msg3        DC.B   '> Be sure to start saving Terminal data: open Output file = RxData3.txt', $00
msg4        DC.B   '> When ready, hit Enter/Return key for sawtooth wave, 1024 point print.', $00
msg5        DC.B   '> Finished!  You may close the Output file.', $00
msg6        DC.B   '> Ready for next data transmission or command, hit Enter key.', $00
invalidset  DC.B   'Invalid time format. Correct example => 0 to 59',$00
invalidform DC.B   'Invalid input format',$00
adcinfo     DC.B   'analog signal acqusition .... ',$00
msg20       DC.B   'saving Terminal output to file',$00
msg21       DC.B   'run analog signal command - connect an analog signal to ADC pin 7',$00

end            
            END                    ; this is end of assembly source file
                                   ; lines below are ignored - not assembled
