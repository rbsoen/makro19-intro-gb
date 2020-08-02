SECTION "WRAM", WRAM0

wOAMBuffer::	ds $9F

wObjectIndex:: ds 1
wObjectOffset:: ds 1
wObjectCur:: ds 2 ; Y, X
wObject0:: ds 2 ; Y, X
wObject1:: ds 2 ; Y, X
wObject2:: ds 2 ; Y, X
wObject3:: ds 2 ; Y, X
wObject4:: ds 2 ; Y, X
wObject5:: ds 2 ; Y, X
wObject6:: ds 2 ; Y, X
wObject7:: ds 2 ; Y, X
wObject8:: ds 2 ; Y, X
wObject9:: ds 2 ; Y, X

wRAMEnd equ $DFFF
