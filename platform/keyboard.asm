.alias          CHAR                    $F9
.alias          KB_ACK                  %01000000

init_keyboard:
                ; 0 = input, 1 = output
                lda     #KB_ACK
                sta     IO_DDRB
                rts

keyboard_get_key:
                phx
                ; receive the character
                jsr     receive_nibble
                jsr     receive_nibble

                lda     CHAR            ; character is now in A, hold on to it
                pha

                jsr     receive_nibble  ; receive the flags
                pla                     ; this version ignores the flags
                plx
                rts

receive_nibble:
                lda     IO_PORTB           ; LDA loads bit 7 (avail) into N
                bpl     receive_nibble  ; repeat until avail is 1

                ldx     #4
_shift:
                asl                     ; move low nibble to high nibble
                dex
                bne     _shift

                ldx     #4
_rotate:
                asl                     ; shift bit into carry
                rol     CHAR            ; rotate carry into CHAR
                dex
                bne     _rotate

                lda     IO_PORTB           ; send ack signal to kb controller
                ora     #KB_ACK
                sta     IO_PORTB
_wait_avail_low:
                lda     IO_PORTB           ; wait for available to go low
                bmi     _wait_avail_low ; negative means bit 7 (avail) high

                lda     IO_PORTB           ; set ack low
                and     #[KB_ACK^$FF]
                sta     IO_PORTB
                rts