.alias          IO_PORTB                $6000                        ; Data port B
.alias          IO_PORTA                $6001                        ; Data port A
.alias          IO_DDRB                 $6002                        ; Data direction of port B
.alias          IO_DDRA                 $6003                        ; Data direction of port A
.alias          IO_PCR                  $600c                        ; Peripheral control register
.alias          IO_IFR                  $600d                        ; Interrupt flag register
.alias          IO_IER                  $600e                        ; Interrupt enable register

.alias          CLEAR_SCREEN            $0c
.alias          CHOOSE_CURSOR           2
.alias          CURSOR_CHAR             $db
.alias          CURSOR_BLINK            3

.alias          DATA_PINS               %11110000
.alias          AVAILABLE               %00000100
.alias          ACK                     %00001000
.alias          OUTPUT_PINS             DATA_PINS | AVAILABLE
.alias          UNUSED_PINS             %00000011

init_screen:
                ; set data direction for the 6 needed pins, while keeping
                ; the unused ones unchanged
                lda     IO_DDRA
                ora     #OUTPUT_PINS
                and     #[OUTPUT_PINS | UNUSED_PINS]
                sta     IO_DDRA

                ; set all used pins low, while keeping the unused ones unchanged
                lda     IO_PORTA
                and     #UNUSED_PINS
                sta     IO_PORTA

                lda     #CLEAR_SCREEN
                jsr     write_byte
                lda     #CHOOSE_CURSOR
                jsr     write_byte
                lda     #CURSOR_CHAR
                jsr     write_byte
                lda     #CURSOR_BLINK
                jsr     write_byte

                rts

write_byte:
                pha                     ; we pull off the arg twice, once for high
                pha                     ; nibble and once for low nibble

                lda     IO_PORTA
                and     #[DATA_PINS^$FF]; clear data
                sta     IO_PORTA

                jsr     wait_ack_low
                pla
                and     #%11110000      ; mask out low nibble
                ora     IO_PORTA
                sta     IO_PORTA

                ora     #AVAILABLE      ; flip available = high
                sta     IO_PORTA

                jsr     wait_ack_high

                and     #%00001111      ; clear data so we can ora with high nibble
                sta     IO_PORTA

                pla                     ; get the original byte back
                asl                     ; shift low nibble into high nibble
                asl                     
                asl
                asl                     

                ora     IO_PORTA
                sta     IO_PORTA

                and     #[AVAILABLE^$FF] ; flip available = low
                sta     IO_PORTA

                jsr     wait_ack_low

                rts


.scope
wait_ack_high:
                pha
_loop:
                lda     IO_PORTA
                and     #ACK
                beq     _loop
                pla
                rts
.scend

.scope
wait_ack_low:
                pha
_loop:
                lda     IO_PORTA
                and     #ACK
                bne     _loop
                pla
                rts
.scend