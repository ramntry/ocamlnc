/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2000 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License.         */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Dynamic loading of C primitives. */

#ifndef _dynlink_
#define _dynlink_

/* Build the table of primitives, given a search path, a list
   of shared libraries, and a list of primitive names
   (all three 0-separated in char arrays).
   Abort the runtime system on error. */
extern void build_primitive_table(char * lib_path,
                                  char * libs,
                                  char * req_prims);

#endif
