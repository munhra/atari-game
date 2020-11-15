	processor 6502

    include "vcs.h"
	include "macro.h"
    SEG.U Variables
    ORG $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare the variables starting from address $80
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
buildingYPosition       byte
buildingXPosition       byte
buildingSpritePtr       word
buildingSpriteColorPtr  word
houseYPosition          byte
houseXPosition          byte
houseSpritePtr          word
houseSpriteColorPtr     word
treeYPosition           byte
treeXPosition           byte
treeSpritePtr           word
treeSpriteColorPtr      word
curvePtr                word
curveYPosition          byte
curve1Ptr               word
curve1YPosition         byte
currentScanLine         byte
temp1                   
PF0AUX                  byte
PF1AUX                  byte
PF2AUX                  byte

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
BUILDING_HEIGHT = 9
HOUSE_HEIGHT = 9
TREE_HEIGHT = 9
	seg code
    org $F000

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize RAM variables and TIA registers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Start:
	CLEAN_START                          ; call macro to safely clear RAM and TIA
    lda #50
    sta buildingYPosition
    
    LDA #35
    sta houseYPosition
    
    LDA #10
    sta treeYPosition

    LDA #174
    sta curveYPosition

    lda #156
    sta curve1YPosition

    LDA #<Building                       ;load in register the lo-byte of the address of the 
                                         ;lookup table
    STA buildingSpritePtr                ;store the lo-byte into the pointer
    LDA #>Building                       ;load in register the hi-byte of the address of the
                                         ;lookup table
    STA buildingSpritePtr+1              ;store the hi-byte on the pointer
    LDA #<BuildingColor      
    STA buildingSpriteColorPtr
    LDA #>BuildingColor     
    STA buildingSpriteColorPtr+1


    LDA #<House                          ;load in register the lo-byte of the address of the 
                                         ;lookup table
    STA houseSpritePtr                   ;store the lo-byte into the pointer
    LDA #>House                          ;load in register the hi-byte of the address of the
                                         ;lookup table
    STA houseSpritePtr+1                 ;store the hi-byte on the pointer
    LDA #<HouseColor      
    STA houseSpriteColorPtr
    LDA #>HouseColor     
    STA houseSpriteColorPtr+1

    LDA #<Tree                           ;load in register the lo-byte of the address of the 
                                         ;lookup table
    STA treeSpritePtr                   ;store the lo-byte into the pointer
    LDA #>Tree                          ;load in register the hi-byte of the address of the
                                         ;lookup table
    STA treeSpritePtr+1                 ;store the hi-byte on the pointer
    LDA #<TreeColor      
    STA treeSpriteColorPtr
    LDA #>TreeColor     
    STA treeSpriteColorPtr+1

    lda #<Curve
    sta curvePtr
    lda #>Curve
    sta curvePtr+1

    lda #<Curve1
    sta curve1Ptr
    lda #>Curve1
    sta curve1Ptr+1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start a new frame by turning on VBLANK and VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:
	lda #2           ; same as binary value %00000010
    sta VBLANK       ; turn on VBLANK
    sta VSYNC        ; turn on VSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate the three lines of VSYNC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	sta WSYNC        ; first scanline
    sta WSYNC        ; second scanline
    sta WSYNC        ; third scanline

	lda #0
	sta VSYNC        ; turn off VSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let the TIA output the recommended 37 scanlines of VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ldx #37          ; X = 37 (to count 37 scanlines)
LoopVBlank:
	sta WSYNC        ; hit WSYNC and wait for the next scanline
	dex				 ; X--
    bne LoopVBlank   ; loop while X != 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and tasks performed in the pre VBLANK
;; VBLANK is a good place to perform some calculations that can take some time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;lda houseXPosition
    ;ldy #1
    ;jsr SetObjectXPosition
    ;sta WSYNC
    ;sta HMOVE

	lda #0
	sta VBLANK		 ; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw 192 visible scanlines (kernel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ldx #192		    ; counter for 192 visible scanlines 192/2 = 96
    LDA #$C2           
    STA COLUPF              ;Set background to blue
    LDA #$08                ;Set color for the playfield
    STA COLUBK
    LDA #%00000001
    STA CTRLPF              ;Reflect player field
    LDA #%11110000                ;Setting PF0 pattern
    STA PF0
    LDA #%11111111
    STA PF1
    dec curveYPosition
    dec curve1YPosition
    ;dec curveYPosition
    ;dec curve1YPosition

    lda curveYPosition





LoopVisible:
    ;LDA #%11111111
    ;STA PF2
    ;sta WSYNC
    ;jsr DrawHouseSprite     ; wsync
    ;jsr DrawBuildingSprite  ; there is a problem here the second call erases the first
    ;jsr DrawTreeSprite

    TXA
    SEC                                  ;Transfer X to A  
    SBC curveYPosition  
    CMP #18                               ;are we inside the sprite boundary
    BCS .SkipDrawCurve                   ;if result < SpriteHeight call draw rountine
    
    TAY                                 ;transfer value of register A to register Y
                                        ;it is required to transfer as Y a index register
                                        ;for memory addressing   
    LDA (curvePtr),Y                   
    STA PF2                            ;set graphics for the player 0
.SkipDrawCurve

    TXA
    SEC                                  ;Transfer X to A  
    SBC curve1YPosition  
    CMP #18                               ;are we inside the sprite boundary
    BCS .SkipDrawCurve1                   ;if result < SpriteHeight call draw rountine
    
    TAY                                 ;transfer value of register A to register Y
                                        ;it is required to transfer as Y a index register
                                        ;for memory addressing   
    LDA (curve1Ptr),Y                   
    STA PF1                            ;set graphics for the player 0
.SkipDrawCurve1
    

    sta WSYNC

    dex				 ; X--
	bne LoopVisible  ; loop while X != 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output 30 more VBLANK lines (overscan) to complete our frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #2
	sta VBLANK       ; hit and turn on VBLANK again

	ldx #30			 ; counter for 30 scanlines
LoopOverscan:
	sta WSYNC		 ; wait for the next scanline
	dex				 ; X--
	bne	LoopOverscan ; loop while X != 0

	jmp StartFrame   ; go to next frame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example o how to define a sub-routine, this subroutine will process the
;; X position for all moveable objects according the parameters below
;; A Register contains the value of X to be positioned
;; Y = 0 : Player 0
;; Y = 1 : Player 1
;; Y = 2 : Missle 0
;; Y = 3 : Missle 1
;; Y = 4 : Ball 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
SetObjectXPosition:
    STA WSYNC
    SEC
.SpriteHotizontalLoop
    SBC #15
    BCS .SpriteHotizontalLoop
    EOR #7
    ASL
    ASL
    ASL
    ASL
    STA HMP0,Y
    STA RESP0,Y
    RTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw House sprite
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DrawHouseSprite:
    TXA                                 ;Transfer X to A    
    SEC                                 ;Reset carrier before subtraction
    SBC houseYPosition                  ;subtract the position
    CMP #HOUSE_HEIGHT                   ;are we inside the sprite boundary
    STA WSYNC
    BCS .SkipDrawHouse                   ;if result < SpriteHeight call draw rountine
    ldy #4
.HouseXloop
    dey
    bne .HouseXloop
    STA RESP1
    
    TAY                                 ;transfer value of register A to register Y
                                        ;it is required to transfer as Y a index register
                                        ;for memory addressing   
    LDA (houseSpritePtr),Y                   
    STA GRP1                            ;set graphics for the player 0
    LDA (houseSpriteColorPtr),Y         ;load sprite 0 color lookup table
    STA COLUP1                          ;store the color at the color register
.SkipDrawHouse
    RTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw Building sprite
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DrawTreeSprite:
    TXA                                 ;Transfer X to A    
    SEC                                 ;Reset carrier before subtraction
    SBC treeYPosition               ;subtract the position
    STA WSYNC
    CMP #TREE_HEIGHT                ;are we inside the sprite boundary
    BCS .SkipDrawTree                   ;if result < SpriteHeight call draw rountine
    ldy #2
.TreeXLoop
    dey
    bne .TreeXLoop
    STA RESP1
    TAY                                 ;transfer value of register A to register Y
                                        ;it is required to transfer as Y a index register
                                        ;for memory addressing 
    
    LDA (treeSpritePtr),Y               
    STA GRP1                            ;set graphics for the player 0
    LDA (treeSpriteColorPtr),Y      ;load sprite 0 color lookup table
    STA COLUP1                          ;store the color at the color register
.SkipDrawTree
    RTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Draw Tree sprite
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DrawBuildingSprite:
    TXA                                 ;Transfer X to A    
    SEC                                 ;Reset carrier before subtraction
    SBC buildingYPosition               ;subtract the position
    STA WSYNC
    CMP #BUILDING_HEIGHT                ;are we inside the sprite boundary
    BCS .SkipDrawBuilding                   ;if result < SpriteHeight call draw rountine
    ldy #4
.BuildingXLoop
    dey
    bne .BuildingXLoop
    STA RESP1
    TAY                                 ;transfer value of register A to register Y
                                        ;it is required to transfer as Y a index register
                                        ;for memory addressing 
    
    LDA (buildingSpritePtr),Y               
    STA GRP1                            ;set graphics for the player 0
    LDA (buildingSpriteColorPtr),Y      ;load sprite 0 color lookup table
    STA COLUP1                          ;store the color at the color register
.SkipDrawBuilding
    RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete my ROM size to 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Building
    .byte #%00000000
    .byte #%01101100;$1C
    .byte #%01111100;$88
    .byte #%01010100;$88
    .byte #%01111100;$98
    .byte #%01010100;$96
    .byte #%01111100;$84
    .byte #%01010100;$82
    .byte #%01111100;$42
BuildingColor
    .byte #$00
    .byte #$1C;
    .byte #$88;
    .byte #$88;
    .byte #$98;
    .byte #$96;
    .byte #$84;
    .byte #$82;
    .byte #$42;
House
    .byte #%00000000
    .byte #%11111111;$1C
    .byte #%10111001;$06
    .byte #%10111001;$06
    .byte #%11111111;$06
    .byte #%11111111;$06
    .byte #%11111011;$40
    .byte #%01110011;$30
    .byte #%00100011;$42
HouseColor
    .byte #$00
    .byte #$C4;
    .byte #$06;
    .byte #$06;
    .byte #$06;
    .byte #$06;
    .byte #$40;
    .byte #$30;
    .byte #$42;
Tree
    .byte #%00000000
    .byte #%00010000;$F0
    .byte #%00010000;$F0
    .byte #%00111000;$C6
    .byte #%01111100;$B4
    .byte #%01111100;$B8
    .byte #%00111000;$B4
    .byte #%00010000;$DE
    .byte #%00000000;--
TreeColor
    .byte #$00
    .byte #$F0;
    .byte #$F0;
    .byte #$C6;
    .byte #$B4;
    .byte #$B8;
    .byte #$B4;
    .byte #$DE;
    .byte #$0E;
Curve1
    .byte #%00000000;$F0
    .byte #%00000000;$F0
    .byte #%10000000;$F0
    .byte #%10000000;$F0
    .byte #%11000000;$F0
    .byte #%11000000;$F0
    .byte #%11100000;$C6
    .byte #%11100000;$C6
    .byte #%11110000;$B4
    .byte #%11110000;$B4
    .byte #%11111000;$B8
    .byte #%11111000;$B8
    .byte #%11111100;$B4
    .byte #%11111100;$B4
    .byte #%11111110;$DE
    .byte #%11111110;$DE
    .byte #%11111111;--
    .byte #%11111111;--
Curve
    .byte #%00000000;$F0
    .byte #%00000000;$F0
    .byte #%00000001;$F0
    .byte #%00000001;$F0
    .byte #%00000011;$F0
    .byte #%00000011;$F0
    .byte #%00000111;$C6
    .byte #%00000111;$C6
    .byte #%00001111;$B4
    .byte #%00001111;$B4
    .byte #%00001111;$B8
    .byte #%00001111;$B8
    .byte #%00001111;$B4
    .byte #%00001111;$B4
    .byte #%00001111;$DE
    .byte #%00001111;$DE
    .byte #%00001111;--
    .byte #%00001111;--


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete my ROM size to 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    org $FFFC
	.word Start
	.word Start
