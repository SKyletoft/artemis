global call_x64_callable_no_stack
global call_x64_callable_with_stack
section .text

; Magic number 6 is how many arguments we can fit in registers
; x86_64 has 8 byte words
; Stack alignment needs to be 16 bytes before `call`

; typedef struct {
;	int64_t (*function)(); // Variadic, not (void)
;	int64_t count;
;	int64_t args;
; } Callable;

call_x64_callable_with_stack:
	; rsi, rdi untouchable
	; rax should be 6 before leaving
	push	r15
	mov	r15, rsp
	sub	rsp, 8  	; Waste a byte on the stack for variadic functions
	mov	rax, rsi	; count 

stack_args_loop:
	sub	rsi, 1
	cmp	rsi, 5
	je	stack_args_loop_end

	lea	rcx, rdx[8*rsi]
	mov	rcx, [rcx]
	push	rcx

	jmp	stack_args_loop
stack_args_loop_end:

	mov	rax, 6
	mov	r10, rdi	; function 
	mov	r11, rdx	; args

	; lea + mov = 7 bytes
	; Jump to `start + 7 * (6 - count)`
	; to skip loading nonexistant arguments
	sub	rax, 6
	neg	rax
	imul	rax, 7
	lea	rax, rax[start2]
	jmp	rax

start2:
	lea	r9,  40[r11]
	mov	r9,  [r9]
	lea	r8,  32[r11]
	mov	r8,  [r8]
	lea	rcx, 24[r11]
	mov	rcx, [rcx]
	lea	rdx, 16[r11]
	mov	rdx, [rdx]
	lea	rsi, 8[r11]
	mov	rsi, [rsi]
	lea	rdi, 0[r11]
	mov	rdi, [rdi]
	
	mov	rax, 0      	; Something gcc does for variadics
	call	r10
	mov	rsp, r15
	pop	r15
	ret



call_x64_callable_no_stack:
	; Copy registers that we're gonna overwrite to scratch registers
	mov	rax, rsi	; count 
	mov	r10, rdi	; function 
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
	lea	r9,  40[r11]
	mov	r9,  [r9]
	lea	r8,  32[r11]
	mov	r8,  [r8]
	lea	rcx, 24[r11]
	mov	rcx, [rcx]
	lea	rdx, 16[r11]
	mov	rdx, [rdx]
	lea	rsi, 8[r11]
	mov	rsi, [rsi]
	lea	rdi, 0[r11]
	mov	rdi, [rdi]

	mov	rax, 0
	jmp	r10
