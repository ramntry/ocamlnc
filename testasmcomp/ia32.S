/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

/* $Id: i386.S 2553 1999-11-17 18:59:06Z xleroy $ */

/* Linux with ELF binaries does not prefix identifiers with _.
   Linux with a.out binaries, FreeBSD, and NextStep do. */

#if defined(SYS_linux_elf) || defined(SYS_bsd_elf) \
 || defined(SYS_solaris) || defined(SYS_beos) || defined(SYS_gnu)
#define G(x) x
#define FUNCTION_ALIGN 16
#else
#define G(x) _##x
#define FUNCTION_ALIGN 4
#endif

        .globl  G(call_gen_code)
        .align  FUNCTION_ALIGN
G(call_gen_code):
        pushl %ebp
        movl %esp,%ebp
        pushl %ebx
        pushl %esi
        pushl %edi
        subl $0xC, %esp
        movl 12(%ebp),%eax
        movl 16(%ebp),%ebx
        movl 20(%ebp),%ecx
        movl 24(%ebp),%edx
        call *8(%ebp)
        addl $0xC, %esp
        popl %edi
        popl %esi
        popl %ebx
        popl %ebp
        ret

        .globl  G(call_gen_code_float)
        .align  FUNCTION_ALIGN
G(call_gen_code_float):
        pushl %ebp
        movl %esp,%ebp
        pushl %ebx
        pushl %esi
        pushl %edi
        subl $0xC, %esp
        movl 12(%ebp),%eax
        movl 16(%ebp),%ebx
        movl 20(%ebp),%ecx
        movl 24(%ebp),%edx
        call *8(%ebp)
        movsd %xmm0, 0(%esp)
        fldl 0(%esp)
        addl $0xC, %esp
        popl %edi
        popl %esi
        popl %ebx
        popl %ebp
        ret

        .globl  G(caml_c_call)
        .align  FUNCTION_ALIGN
G(caml_c_call):
        jmp     *%eax

	.data
        .globl  G(caml_negf_mask)
        .align  FUNCTION_ALIGN
G(caml_negf_mask):
        .long   0, 0x80000000, 0, 0
        .globl  G(caml_absf_mask)
G(caml_absf_mask):
        .long   0xFFFFFFFF, 0x7FFFFFFF, 0, 0

        .comm   G(caml_exception_pointer), 4
        .comm   G(young_ptr), 4
        .comm   G(young_start), 4
