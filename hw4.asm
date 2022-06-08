***********************************************************************
* Title: Yao Xu Homework 4 - LED Light ON/OFF and Switch ON/OFF
* 
* Date:              Sep. 24
*
* programmer:        Yao Xu
*
* Company:           The Pennsylvania State university
*
* Program: LED 4 blink every 1 second
*                      ON for 0.2 sec, OFF for 0.8 sec when switch 1 is not pressed
*                      ON for 0.8 sec, OFF for 0.2 sec when switch 1 is pressed
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
PORTA       EQU       $0000               ; i/o port A addresses
PORTB       EQU       $0001               ; i/o port B addresses
DDRA        EQU       $0002
DDRB        EQU       $0003
***********************************************************************
* Data Section: address used [ $3000 to $30FF ] RAM memory
*
            ORG       $3000               ; Reserved RAM memory staring address for Data for CMPEN472 class
level       DC.b      $0
upper       DC.b      $64
lower       DC.b      $0
           
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
            
            BSET      PORTB,%00010000     ; LED 1 ON                
            
            

Dimup       LDAA      level
            SUBA      upper 
            BEQ       Dimdown
            JSR       dim40msec
            INC       level
            JSR       Dimup
                 
            
            
Dimdown     LDAA      level
            SUBA      lower 
            BEQ       Dimup
            JSR       dim40msec
            DEC       level
            JSR       Dimdown        
        
           
***********************************************************************
* Subroutine Section: address used [ $3100 to $3FFF ] RAM memory
*

;**********************************************************
;delay10usec subroutine
;

delay10usc
            PSHX                           ; save X
            LDX   #58                      ; short delay
                                           ; based on my calculation, 57.5 loops will ahieve a 10us delay so I use 58 
Loop                                      
            DEX
            BNE   Loop
            PULX                           ; restore X
            RTS                            ; return
           
  
dim40msec 
            PSHX      
            LDX       #40 
dimsub      JSR       dim
            DEX
            BNE       dimsub 
            PULX
            RTS  
            
                    
                       
dim         
            PSHA
            PSHB
            LDAA      upper                 ;OFF use x and y register to store number of cycles
            LDAB      level                 ;ON
            SBA 
                                          
ch1         BSET      PORTB,%01000000     ; set LED3 light
            
onnloop     
            JSR       delay10usc          ; jump to delay10usc
            DECB                          ; decrease X by 1
            BNE       onnloop             ; check the value of X

ch2         BCLR      PORTB,%01000000     ; clear bits
            BSET      PORTB,%00000000 
            
offloop            
            JSR       delay10usc          ; jump to delay10usc
            DECA                          ; decrease Y by 1
            BNE       offloop             ; check the value of Y
            
            PULA
            PULB
            RTS
           
            end                            ; last line of a file