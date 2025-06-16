EXTERN ExitProcess : PROC
EXTERN GetStdHandle : PROC
EXTERN CloseHandle : PROC
EXTERN ReadFile : PROC
EXTERN WriteFile : PROC
EXTERN CreateFileA : PROC
EXTERN LoadLibraryA : PROC

; Text encoding		[ cccccccccccccccccccccccccccccccccccccccccccccccccccccccc xxxxtttt ] (x = unused)
; Integer encoding	[ iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii ntttt ] (n = negative bit)
; Float encoding	[ ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff tttt ]

; Colours
WHITE	EQU 0
YELLOW	EQU 1
RED		EQU 2
GREEN	EQU 3
CYAN	EQU 4
MAGENTA EQU 5
ORANGE	EQU 6

; Tags
EXTEND			EQU 0
DEFINE_WORD		EQU 1	; Red
INTERP_WORD		EQU 2	; Yellow
INTERP_NUM		EQU 3	; Yellow
INTERP_FLOAT	EQU 4	; Yellow
COMPILE_WORD	EQU 5	; Green
COMPILE_NUM		EQU 6	; Green
COMPILE_FLOAT	EQU 7	; Green
COMPILE_MACRO	EQU 8	; Cyan
COMMENT			EQU	9	; White
VARIABLE		EQU 10	; Magenta
COMPILE_STRING	EQU 11	; Orange

; Registers
; RBX = Colour
; RSI = Stack Pointer

.DATA
input_buffer BYTE 4096 DUP(0)
input_ptr QWORD input_buffer
input_end QWORD input_buffer

stack dq 4096 DUP(0) ; rsi

macros				QWORD 0, 0
					QWORD 64757000h, _DUP
					QWORD 3b00h, _SEMICOLON
		last_macro	QWORD 6c696200h, _LIB
					QWORD 4096 DUP(0)
mp QWORD last_macro

words	QWORD 0, 0
		QWORD 4096 DUP(0)
wp QWORD words
dp QWORD wp

CODESEG SEGMENT EXECUTE READ WRITE
istream QWORD 4096 DUP(0)
current_inst QWORD istream
CODESEG ENDS

heap QWORD 4096 DUP(0) ; rdi

file_handle QWORD -10
pow10_lookup REAL8 1.0, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0, 10000000.0, 100000000.0, 1000000000.0
float_neg_mask QWORD 8000000000000000h

dispatch_table  QWORD _EXTEND
                QWORD _DEFINE_WORD
                QWORD _INTERP_WORD
                QWORD _INTERP_NUM
                QWORD _INTERP_FLOAT
                QWORD _COMPILE_WORD
                QWORD _COMPILE_NUM
                QWORD _COMPILE_FLOAT
                QWORD _COMPILE_MACRO
                QWORD _COMMENT
                QWORD _VARIABLE
                QWORD _COMPILE_STRING

.CODE
parse PROC ; cl = delimiter
	mov r12b, cl
	mov rcx, [input_ptr]
_start:
	cmp rcx, [input_end]
	je _load
	cmp byte ptr [rcx], ' '
	je _skip
	cmp byte ptr [rcx], 10
	je _skip
	cmp byte ptr [rcx], 13
	je _skip
	mov rdx, rcx
	jmp _read_char
_skip:
	inc rcx
	jmp _start
_load:
	sub rsp, 56
	mov rcx, [file_handle]
	cmp rcx, -10
	jne _got_handle
	call GetStdHandle
	mov rcx, rax
_got_handle:
	lea rdx, [input_buffer]
	mov r8, 4096
	lea r9, [rsp + 40]
	mov qword ptr [rsp + 32], 0
	call ReadFile
	xor rdx, rdx
	mov edx, dword ptr [rsp + 40]
	add rsp, 56

	test rax, rax
	jz _err

	lea rcx, [input_buffer]
	add rcx, rdx
	mov [input_end], rcx
	lea r8, [input_buffer]
	mov input_ptr, r8
	mov rcx, [input_ptr]
	jmp _start
_read_char:
	mov al, [rcx]
	inc rcx
	cmp rcx, [input_end]
	je _done_eof
	test al, al
	je _done
	cmp al, r12b
	je _done
	cmp al, 10
	je _done
	cmp al, 13
	je _done
	cmp al, '.'
	jmp _read_char
_done:
	mov [input_ptr], rcx
	dec rcx
	mov byte ptr [rcx], 0		; null-terminate token
	sub rcx, rdx
	ret
_done_eof: ; Also resets file 
	lea rax, [input_buffer]
	mov [input_ptr], rax
	mov [input_end], rax
	mov [file_handle], -10
	sub rcx, rdx
	ret
_err:
	int 3
parse ENDP ; rcx = length, rdx = address

write_word PROC	; rcx = length, rdx = address
	mov r8, rcx

	sub rsp, 56
	mov rcx, -11
	call GetStdHandle

	mov rcx, rax
	; rdx already set
	; r8 already set
	lea r9, [rsp + 40]
	mov qword ptr [rsp + 32], 0
	call WriteFile
	add rsp, 56

	test rax, rax
	jz err
	ret

err:
	int 3
write_word ENDP

convert_integer PROC ; rcx = length, rdx = address
	xor r8, r8
	xor r9, r9
	xor r10, r10

	cmp byte ptr [rdx], '-'
	sete r8b
	add r9, r8

	cmp rcx, 2
	jl _decimal

	cmp byte ptr [rdx + r9], '0'
	jne _decimal
	inc r9
	cmp byte ptr [rdx + r9], 'x'
	jne _decimal
	inc r9
	jmp _hex
_decimal:
	mov al, [rdx + r9]
	inc r9
	cmp al, '0'
	jl _err
	cmp al, '9'
	jg _err
	sub al, '0'
	imul r10, r10, 10
	movzx eax, al
	add r10, rax
	cmp r9, rcx
	jge _tag
	jmp _decimal
_hex:
	mov al, [rdx + r9]
	inc r9
	cmp al, '0'
	jl _err
	cmp al, '9'
	jle _hexd

	or al, 20h
	cmp al, 'a'
	jl _err
	cmp al, 'f'
	jg _err

	sub al, 'a'
	add al, 10
	jmp _hexinc

_hexd:
	sub al, '0'
_hexinc:
	shl r10, 4
	movzx eax, al
	add r10, rax
	cmp r9, rcx
	jge _tag
	jmp _hex

_tag:
	mov rax, r10
	shl rax, 5
	cmp rbx, YELLOW
	je _interp
	cmp rbx, GREEN
	je _compile
	jmp _done
_interp:
	or al, INTERP_NUM
	jmp _done
_compile:
	or al, COMPILE_NUM
_done:
	test r8b, r8b
	jnz _set_negative_bit
	ret
_set_negative_bit:
	or al, 00010000b
	ret

_err:
	int 3
convert_integer ENDP

convert_float PROC ; rcx = length, rdx = address
	xor r8, r8
	xor r9, r9
	xor r10, r10
	xor r11, r11
	xor r12, r12

	cmp byte ptr [rdx], '-'
	sete r8b
	add r10, r8

_integer_part:
	mov al, [rdx + r10]
	inc r10
	cmp al, '.'
	je _frac_part
	cmp al, '0'
	jl _err
	cmp al, '9'
	jg _err
	sub al, '0'
	imul r11, r11, 10
	movzx eax, al
	add r11, rax
	cmp r10, rcx
	jge _done
	jmp _integer_part

_frac_part:
	mov al, [rdx + r10]
	inc r10
	cmp al, '0'
	jl _err
	cmp al, '9'
	jg _err
	sub al, '0'
	imul r12, r12, 10
	movzx eax, al
	add r12, rax
	inc r9
	cmp r9, 9
	jge _done
	cmp r10, rcx
	jge _done
	jmp _frac_part

_done:
	cvtsi2sd xmm0, r11
	cvtsi2sd xmm1, r12

	lea rax, [pow10_lookup]
	movsd xmm2, qword ptr [rax + r9 * 8]

	divsd xmm1, xmm2
	addsd xmm0, xmm1

	test r8b, r8b
	jz _tag

	movq xmm1, [float_neg_mask]
	xorpd xmm0, xmm1

_tag:
	movq rax, xmm0
	shr rax, 4
	rol rax, 4
	cmp rbx, YELLOW
	je _interp
	cmp rbx, GREEN
	je _compile
	jmp _ret
_interp:
	or al, INTERP_FLOAT
	jmp _ret
_compile:
	or al, COMPILE_FLOAT

_ret:
	ret

_err:
	int 3
convert_float ENDP

convert_word PROC ; rcx = length, rdx = address
	xor rax, rax
	xor r9, r9
_copy:	
	mov r8b, [rdx + r9]
	inc r9
	or al, r8b
	rol rax, 8
	cmp r9, rcx
	jge _tag
	jmp _copy

_tag:
	cmp rbx, RED
	je _define
	cmp rbx, YELLOW
	je _interp
	cmp rbx, GREEN
	je _compile
	cmp rbx, CYAN
	je _compile_macro
	cmp rbx, ORANGE
	je _compile_string
	jmp _ret

_define:
	or al, DEFINE_WORD
	mov rbx, GREEN
	jmp _ret
_interp:
	or al, INTERP_WORD
	jmp _ret
_compile:
	or al, COMPILE_WORD
	jmp _ret
_compile_macro:
	or al, COMPILE_MACRO
	jmp _ret
_compile_string:
	or al, COMPILE_STRING
_ret:
	ret
convert_word ENDP

PUSH_ MACRO value:req
	lea rsi, [rsi - 8]
	mov [rsi], value
ENDM

_INTERPRET:
	mov cl, ' '
	call parse
	cmp rcx, 1
	jne _TOKEN
	cmp byte ptr [rdx], ':'
	je _RED
	cmp byte ptr [rdx], '['
	je _YELLOW
	cmp byte ptr [rdx], '('
	je _WHITE
	cmp byte ptr [rdx], ']'
	je _GREEN
	cmp byte ptr [rdx], '{'
	je _CYAN
	cmp byte ptr [rdx], '<'
	je _ORANGE
_TOKEN:
	cmp byte ptr [rdx], '#'
	je _INTEGER
	cmp byte ptr [rdx], '$'
	je _FLOAT
_WORD:
	call convert_word
	jmp _DISPATCH
_INTEGER:
	dec rcx
	inc rdx
	call convert_integer
	jmp _DISPATCH
_FLOAT:
	dec rcx
	inc rdx
	call convert_float
	
_DISPATCH:
	mov rcx, rax
	and rcx, 0Fh
	lea rdx, dispatch_table
	jmp QWORD PTR [rdx + rcx * 8]
_RED:
	mov rbx, RED
	jmp _INTERPRET
_YELLOW:
	mov rbx, YELLOW
	jmp _INTERPRET
_WHITE:
	mov rbx, WHITE
	jmp _INTERPRET
_GREEN:
	mov rbx, GREEN
	jmp _INTERPRET
_CYAN:
	mov rbx, CYAN
	jmp _INTERPRET
_ORANGE:
	mov rbx, ORANGE
	jmp _INTERPRET

.CODE
main PROC
	sub rsp, 8
	mov rbx, YELLOW
	lea rsi, [stack + 4096 * 8]
	lea rdi, [heap]
	jmp _INTERPRET
main ENDP

_EXTEND:
	jmp _INTERPRET

_DEFINE_WORD:
	and rax, 0FFFFFFFFFFFFFF00h
	mov rcx, [dp]
	mov rcx, [rcx]
	mov [rcx + 16], rax
	mov rdx, [current_inst]
	mov [rcx + 24], rdx
	mov rdx, [dp]
	add rcx, 16
	mov [rdx], rcx
	jmp _INTERPRET

_INTERP_WORD:
	and rax, 0FFFFFFFFFFFFFF00h
	mov rcx, [wp]

_word_search:
	mov rdx, [rcx]
	test rdx, rdx
	jz _word_not_found
	cmp rdx, rax
	je _exec_word
	sub rcx, 16
	jmp _word_search

_exec_word:
	call qword ptr [rcx + 8]
	jmp _INTERPRET

_word_not_found:
	int 3
	jmp _INTERPRET

_INTERP_NUM:
	shr rax, 4
	test rax, 1
	jz _skip_neg
	shr rax, 1
	neg rax
	jmp _continue
_skip_neg:
	shr rax, 1
_continue:
	PUSH_ rax
	jmp _INTERPRET

_INTERP_FLOAT:
	ror rax, 4
	shl rax, 4
	PUSH_ rax
	jmp _INTERPRET

_COMPILE_WORD:
	and rax, 0FFFFFFFFFFFFFF00h
	mov rcx, [mp]

_macro_search:
	mov rdx, [rcx]
	test rdx, rdx
	jz _macro_not_found
	cmp rdx, rax
	je _exec_macro
	sub rcx, 16
	jmp _macro_search

_exec_macro:
	mov rdx, rsi
	mov r8, rdi
	mov rax, [rcx + 8]
	mov rcx, [rax]
	mov rdi, [current_inst]
	lea rsi, [rax + 8]
	rep movsb
	mov [current_inst], rdi
	mov rsi, rdx
	mov rdi, r8
	jmp _INTERPRET

_macro_not_found:
	mov rcx, [wp]

_word_search2:
	mov rdx, [rcx]
	test rdx, rdx
	jz _word_not_found2
	cmp rdx, rax
	je _compile_word_
	sub rcx, 16
	jmp _word_search2

_compile_word_:
	; mov rax, [ address of word ]
	; call rax
	mov r8, rdi
	mov rax, [rcx + 8]
	mov rdi, [current_inst]
	mov byte ptr [rdi + 0], 048h
	mov byte ptr [rdi + 1], 0B8h
	mov qword ptr [rdi + 2], rax
	mov byte ptr [rdi + 10], 0FFh
	mov byte ptr [rdi + 11], 0D0h
	add rdi, 12
	mov [current_inst], rdi
	mov rdi, r8
	jmp _INTERPRET

_word_not_found2:
	int 3
	jmp _INTERPRET

_COMPILE_NUM:
	shr rax, 4
	test rax, 1
	jz _skip_neg2
	shr rax, 1
	neg rax
	jmp _continue2
_skip_neg2:
	shr rax, 1
_continue2:
	; mov rax, imm64
	; PUSH_ rax
	mov r8, rdi
	mov rdi, [current_inst]
	mov byte ptr [rdi], 048h
	mov byte ptr [rdi + 1], 0B8h
	mov qword ptr [rdi + 2], rax
	mov dword ptr [rdi + 10], 0F8768D48h
	mov dword ptr [rdi + 14], 068948h
	add rdi, 17
	mov [current_inst], rdi
	mov rdi, r8
	jmp _INTERPRET

_COMPILE_FLOAT:
	ror rax, 4
	shl rax, 4
	; mov rax, imm64
	; PUSH_ rax
	mov r8, rdi
	mov rdi, [current_inst]
	mov byte ptr [rdi], 048h
	mov byte ptr [rdi + 1], 0B8h
	mov qword ptr [rdi + 2], rax
	mov dword ptr [rdi + 10], 0F8768D48h
	mov dword ptr [rdi + 14], 068948h
	add rdi, 17
	mov [current_inst], rdi
	mov rdi, r8
	jmp _INTERPRET

_COMPILE_MACRO:
	and rax, 0FFFFFFFFFFFFFF00h
	mov rcx, [mp]

_macro_search2:
	mov rdx, [rcx]
	test rdx, rdx
	jz _macro_not_found2
	cmp rdx, rax
	je _compile_macro_
	sub rcx, 16
	jmp _macro_search2

_compile_macro_:
	jmp _INTERPRET

_macro_not_found2:
	int 3

	jmp _INTERPRET
_COMMENT:
	jmp _INTERPRET
_VARIABLE:
	jmp _INTERPRET

_COMPILE_STRING:
	mov cl, 56
_next_char:
	mov rdx, rax
	shr rdx, cl
	test dl, dl
	jnz _copy_char
	test cl, cl
	jz _compile_string_done
	sub cl, 8
	jmp _next_char
_copy_char:
	mov byte ptr [rdi], dl
	inc rdi
	sub cl, 8
	jmp _next_char
_compile_string_done:
	jmp _INTERPRET

_SEMICOLON:
	QWORD _SEMICOLON_END - $ - 8
_SEMICOLON_START:
	ret
_SEMICOLON_END:

_DUP:
	QWORD _DUP_END - $ - 8
_DUP_START:
	mov rcx, [rsi]
	PUSH_ rcx
_DUP_END:

_LIB:
	QWORD _LIB_END - $ - 8
_LIB_START:
	mov rcx, rdx
	call LoadLibraryA
_LIB_END:


END
