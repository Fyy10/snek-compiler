section .text

extern print
global main

main:
    ; init value
    mov rax, 0x7FFFFFFFFFFFFFFE
    push rax
    ; print rax
    mov rdi, rax
    call print
    pop rax

    ; operation
    add rax, 0b10

    pushfq
    ; print rax
    mov rdi, rax
    call print
    popfq
    jo overflow

    ; no overflow
    mov rdi, 0b01
    call print
    jmp end_overflow

    ; overflow
    overflow:
        mov rdi, 0b11
        call print

    end_overflow:
    mov rax, 0
    ret
