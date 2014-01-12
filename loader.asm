; loader.asm will be used to set up the boot image

; The direct "use16" specifies that NASM should generate code designed to run on a processor operating in 16-bit mode.
; It's used in place of "bits 16" for compatibility with other assemblers.
; See http://www.nasm.us/doc/nasmdoc6.html#section-6.1.1
use16

; The directive "org" stands for "origin", and specifies where NASM will look for the program
; when it loads the program into memory.
; See http://www.nasm.us/doc/nasmdoc7.html#section-7.1.1
; Why the value 0x7c00? That's where BIOS interrupt 19h typically loads the boot record.
; See http://wiki.osdev.org/Boot_Sequence#Master_Boot_Record and
; http://en.wikipedia.org/wiki/BIOS_interrupt_call
; For more details on the origin of this value, see http://www.glamenv-septzen.net/en/view/6
org 0x7c00

; This is a label. It's an easy way to point to the first instruction after the label
; See http://www.nasm.us/doc/nasmdoc3.html and http://stackoverflow.com/questions/14927277/what-is-label-address
boot:
    ; initialize segment registers
    xor ax, ax ; Common idiom to set register to 0. See http://stackoverflow.com/questions/8201676/xor-register-register-assembler
    mov ds, ax ; This register and the one below are also being set to 0
    mov es, ax
    mov ss, ax
    ; initialize stack
    mov ax, 0x7bff
    mov sp, ax
    ; load rust code into 0x7e00 so we can jump to it later
	; The address 0x7e00 is the next one after the stack pointer 0x7bff
	; The below ("ah", etc) are general purpose registers. See http://en.wikipedia.org/wiki/X86#x86_registers
    mov ah, 2       ; read
    mov al, 24      ; 24 sectors (12 KiB)
    mov ch, 0       ; cylinder & 0xff
    mov cl, 2       ; sector | ((cylinder >> 2) & 0xc0)
    mov dh, 0       ; head
    mov bx, 0x7e00  ; read buffer
    ; BIOS interrupt 13 is used for disk services. When ah is set to 2, it tells the BIOS to read the data
	; See http://en.wikipedia.org/wiki/BIOS_interrupt_call
	; and http://en.wikipedia.org/wiki/INT_13H#INT_13h_AH.3D02h:_Read_Sectors_From_Drive
	int 0x13
	; jc stands for "Jump if carry". The carry flag is a single bit and is
	;  either on or off (i.e. true or false). After using interrupt 13, the
	;  carry flag is set if there's an error, or clear if successful.
    jc error
    ; load protected mode GDT and a null IDT (we don't need interrupts)
    ;; The cli instruction clears the interrupt flag and disables interrupts.
    ;;  http://www.posix.nl/linuxassembly/nasmdochtml/nasmdoca.html#section-A.15
    cli
    ;; http://www.posix.nl/linuxassembly/nasmdochtml/nasmdoca.html#section-A.95
    lgdt [gdtr]
    lidt [idtr]
    ; set protected mode bit of cr0
    ;; The bits of control register cr0 can be manipulated to modify the
    ;;  operation of the processor. In this case, the value cr0 is copied to
    ;;  general purpose register eax, bit 1 is set to enable protected mode,
    ;;  and then this value is copied to cr0.
    ;;  http://en.wikipedia.org/wiki/Control_register#CR0
    mov eax, cr0
    or eax, 1
    mov cr0, eax
    ; far jump to load CS with 32 bit segment
    jmp 0x08:protected_mode

error:
    mov si, .msg
.loop:
    lodsb
    or al, al
    jz .done
    mov ah, 0x0e
    int 0x10
    jmp .loop
.done:
    jmp $
    .msg db "could not read disk", 0

protected_mode:
    use32
    ; load all the other segments with 32 bit data segments
    mov eax, 0x10
    mov ds, eax
    mov es, eax
    mov fs, eax
    mov gs, eax
    mov ss, eax
    ; set up stack
    mov eax, 0x7bff
    mov esp, eax
    ; clear the screen
    mov edi, 0xb8000
    mov ecx, 80*25*2
    mov al, 0
    rep stosb
    ; jump into rust
    jmp 0x7e00

gdtr:
    dw (gdt_end - gdt) + 1  ; size
    dd gdt                  ; offset

idtr:
    dw 0
    dd 0

gdt:
    ; null entry
    dq 0
    ; code entry
    dw 0xffff       ; limit 0:15
    dw 0x0000       ; base 0:15
    db 0x00         ; base 16:23
    db 0b10011010   ; access byte - code
    db 0x4f         ; flags/(limit 16:19). flag is set to 32 bit protected mode
    db 0x00         ; base 24:31
    ; data entry
    dw 0xffff       ; limit 0:15
    dw 0x0000       ; base 0:15
    db 0x00         ; base 16:23
    db 0b10010010   ; access byte - data
    db 0x4f         ; flags/(limit 16:19). flag is set to 32 bit protected mode
    db 0x00         ; base 24:31
gdt_end:

times 510-($-$$) db 0
db 0x55
db 0xaa
