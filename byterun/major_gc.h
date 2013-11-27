/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

#ifndef CAML_MAJOR_GC_H
#define CAML_MAJOR_GC_H


#include "freelist.h"
#include "misc.h"

typedef struct {
  void *block;           /* address of the malloced block this chunk live in */
  asize_t alloc;         /* in bytes, used for compaction */
  asize_t size;          /* in bytes */
  char *next;
} heap_chunk_head;

#define Chunk_size(c) (((heap_chunk_head *) (c)) [-1]).size
#define Chunk_alloc(c) (((heap_chunk_head *) (c)) [-1]).alloc
#define Chunk_next(c) (((heap_chunk_head *) (c)) [-1]).next
#define Chunk_block(c) (((heap_chunk_head *) (c)) [-1]).block

extern int caml_gc_phase;
extern int caml_gc_subphase;
extern uintnat caml_allocated_words;
extern double caml_extra_heap_resources;
extern uintnat caml_dependent_size, caml_dependent_allocated;
extern uintnat caml_fl_size_at_phase_change;

#define Phase_mark 0
#define Phase_clean 1
#define Phase_sweep 2
#define Phase_idle 3

/* Subphase of sweep */
#define Subphase_mark_main 10 /* before marking finalized value */
/* Between this two subphases the set of marked blocks is an
   over-approximation of the set of alive blocks at the beginning of
   the marking phase */
#define Subphase_mark_final 11 /* after marking finalized value */

/* Subphase of clean */
#define Subphase_clean_weak  20 /* clean weak arrays */
#define Subphase_unlink_weak 21 /* remove dead weak arrays */

CAMLextern char *caml_heap_start;
extern uintnat total_heap_size;
extern char *caml_gc_sweep_hp;

void caml_init_major_heap (asize_t);           /* size in bytes */
asize_t caml_round_heap_chunk_size (asize_t);  /* size in bytes */
void caml_darken (value, value *);
intnat caml_major_collection_slice (intnat);
void major_collection (void);
void caml_finish_major_cycle (void);


#endif /* CAML_MAJOR_GC_H */
