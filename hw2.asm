***********************************************************************
* Title: Yao Xu CMPEN472 Homework 2 - LED Light blinking
* 
* Date:              Sep. 8
*
* programmer:        Yao Xu
*
* Company:           The Pennsylvania State university
*
* Algorithm:         Simple Parallel I/O use and time delay-loop demo
*
* Register use:      A: LED Light on/off state and Switch 1 on/off state
*                    X,Y: Delay loop conuters
*                
* Memory use:        RAM Locations from $3000 for data 
*                    RAM Locations from $3100 for program
*
* Input:             Parameters hard coded in the program - PORTB
*                    Switch 1 at PORTB bit 0
*                    Switch 2 at PORTB bit 1
*                    Switch 3 at PORTB bit 2
*                    Switch 4 at PORTB bit 3
*
* Output:            LED 1 at PORTB bit 4
*                    LED 2 at PORTB bit 5
*                    LED 3 at PORTB bit 6
*                    LED 4 at PORTB bit 7
*
* Observation:       This program that blinks LEDs and blinking period canbe changed with the delay loop counter value

*
* Comments:          This program is developed and simulated using CodeWorrior
*                    development software and targrted for Axion Manufacturing's CSM-12C128 board running at 24MHz
*
***********************************************************************
* Parameter Declearation Section
*
* Export symbols
            XDEF      pstart              ; export 'pgstart' symbol
            ABSENTRY  pstart              ; for assembly entry point
* Symbols and Macros
PORTA       EQU       $0000               ; i/o port addresses
PORTB       EQU       $0001 
DDRA        EQU       $0002
DDRB        EQU       $0003
***********************************************************************
* Data Section: address used [ $3000 to $30FF ] RAM memory
*
            ORG       $3000               ; Reserved RAM memory staring address for Data for CMPEN472 class
            
Counter1    DC.W      $0100               ; X register count number for time delay inner loop for msec

Counter2    DC.W      $00BF               ; Y register count number for time delay outer loop for sec
                              
                                          ; Remaining data memory space for stack,
                                          ; up to program memory stack
*
***********************************************************************
* Program Section: address used [ $3100 to $3FFF ] RAM memory
*
            ORG       $3100               ; program start address, in RAM
pstart      LDS       #$3100              ; initialize the stack pointer

            LDAA      #%11110001          ; LED 1,2,3,4 at PORTB bit 4,5,6,7
            STAA      DDRB                ; set PORTB bit 4,5,6,7 as output
                                          ; plus the bit for switch 1
            LDAA      #%00000000
            STAA      PORTB               ; clear all bits for PORTB
            
mainLoop
            BSET      PORTB,%10000000     ; set LED4 on
            BCLR      PORTB,%00010000     ; set LED1 off
            JSR       delay1sec           ; delay for 1 sec
            
            BSET      PORTB,%00010000     ; set LED1 on 
            BCLR      PORTB,%10000000     ; set LED4 off
            JSR       delay1sec           ; delay for 1 sec
            
            LDAA      PORTB               ; check bit 0 for PORTB, switch1
            ANDA      #%00000001          ; if 0, the switch is not pushed 
            BNE       swlpushed           ; if 1, the switch is pushed
            
swlnotpsh   BCLR      PORTB,%00010000     ; if switch is not pushed
            BRA       mainLoop            ; go back to the mainloop
            
seqLoop     BCLR      PORTB,%11110000     ; set all LEDs off
            BSET      PORTB,%10000000     ; LED4 on
            JSR       delay1sec           ; delay for a sec
            
            BCLR      PORTB,%11110000     ; set all LEDs off
            BSET      PORTB,%01000000     ; LED3 on
            JSR       delay1sec           ; delay for a sec
            
            BCLR      PORTB,%11110000     ; set all LEDs off
            BSET      PORTB,%00100000     ; LED2 on 
            JSR       delay1sec           ; delay for a sec
            
            BCLR      PORTB,%11110000     ; set all LEDs off
            BSET      PORTB,%00010000     ; LED1 on
            JSR       delay1sec           ; delay for a sec
            
            
swlpushed   LDAA      PORTB               ; check bit 0 for PORTB, switch1
            ANDA      #%00000001          ; if 0, the switch is not pushed
            BNE       seqLoop             ; if 1, the switch is pushed, goes to seqloop
            BRA       mainLoop            ; if not pushed goes back to the mian loop

***********************************************************************
* Program Section: address used [ $3100 to $3FFF ] RAM memory
*

;**********************************************************
;delay1sec subroutine
;

delay1sec
           PSHY                           ; save Y
           LDY   Counter2                 ; long delay by
           
dly1Loop   JSR   delayMS                  ; total time delay = Y * delayMS
           DEY
           BNE   dly1Loop
           
           PULY                           ; restore Y
           RTS                            ; return
           
;**********************************************************
;delayMS subroutine
;

delayMS
           PSHX                           ; save X
           LDX   Counter1                 ; short delay
           
dlyMSLoop  NOP                            ; total tie dalay = X*NOP
           DEX
           BNE   dlyMSLoop
           
           PULX                           ; restore X
           RTS                            ; return
           
           end                            ; last line of a file