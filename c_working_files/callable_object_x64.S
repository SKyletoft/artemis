global call_x64_callable_no_stack
global call_x64_callable_with_stack
section .text

; typedef struct {
;	int64_t (*function)(); // Variadic, not (void)
;	int64_t count;
;	int64_t args;
; } Callable;

call_x64_callable_with_stack:
	; TODO: Load arguments to stack
	jmp call_x64_callable_no_stack

call_x64_callable_no_stack:
	; Copy registers that we're gonna overwrite to scratch registers
	mov	rax, rsi	; function 
	mov	r10, rdi	; count 
	mov	r11, rdx	; args

	; lea + mov = 7 bytes
	; Jump to `start + 7 * (6 - count)`
	; to skip loading nonexistant arguments
	sub	rax, 6
	neg	rax
	imul	rax, 7
	lea	rax, rax[start]
	jmp	rax

start:
	lea	r9, 40[r11]
	mov	r9, [r9]
	lea	r8, 32[r11]
	mov	r8, [r8]
	lea	rcx, 24[r11]
	mov	rcx, [rcx]
	lea	rdx, 16[r11]
	mov	rdx, [rdx]
	lea	rsi, 8[r11]
	mov	rsi, [rsi]
	lea	rdi, 0[r11]
	mov	rdi, [rdi]

	jmp r10
