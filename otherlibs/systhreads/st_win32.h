/***********************************************************************/
/*                                                                     */
/*                             Objective Caml                          */
/*                                                                     */
/*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 2009 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

/* $Id: posix.c 9270 2009-05-20 11:52:42Z doligez $ */

#include <windows.h>
#include <WinError.h>
#include <stdio.h>
#include <signal.h>

#define INLINE __inline

#if 1
#define TRACE(x)
#define TRACE1(x,y)
#else
#define TRACE(x) printf("%d: %s\n", GetCurrentThreadId(), x)
#define TRACE1(x,y) printf("%d: %s %d\n", GetCurrentThreadId(), x, y)
#endif

typedef DWORD st_retcode;

#define SIGPREEMPTION SIGTERM

/* Thread creation.  Created in detached mode if [res] is NULL. */

typedef HANDLE st_thread_id;

static DWORD st_thread_create(st_thread_id * res, 
                              LPTHREAD_START_ROUTINE fn, void * arg)
{
  HANDLE h = CreateThread(NULL, 0, fn, arg, 0, NULL);
  TRACE1("st_thread_create", h);
  if (h == NULL) return GetLastError();
  if (res == NULL)
    CloseHandle(h);
  else
    *res = h;
  return 0;
}

#define ST_THREAD_FUNCTION DWORD WINAPI

/* Thread termination */

static void st_thread_exit(void)
{
  TRACE("st_thread_exit");
  ExitThread(0);
}

static void st_thread_kill(st_thread_id thr)
{
  TRACE1("st_thread_kill", thr);
  TerminateThread(thr, 0);
  CloseHandle(thr);
}

/* Scheduling hints */

static INLINE void st_thread_yield(void)
{
  Sleep(0);
}

/* Thread-specific state */

typedef DWORD st_tlskey;

static DWORD st_tls_newkey(st_tlskey * res)
{
  *res = TlsAlloc();
  if (*res == TLS_OUT_OF_INDEXES)
    return GetLastError();
  else
    return 0;
}

static INLINE void * st_tls_get(st_tlskey k)
{
  return TlsGetValue(k);
}

static INLINE void st_tls_set(st_tlskey k, void * v)
{
  TlsSetValue(k, v);
}

/* The master lock.  */

typedef CRITICAL_SECTION st_masterlock;

static void st_masterlock_init(st_masterlock * m)
{
  TRACE("st_masterlock_init");
  InitializeCriticalSection(m);
  EnterCriticalSection(m);
}

static INLINE void st_masterlock_acquire(st_masterlock * m)
{
  TRACE("st_masterlock_acquire");
  EnterCriticalSection(m);
  TRACE("st_masterlock_acquire (done)");
}

static INLINE void st_masterlock_release(st_masterlock * m)
{
  LeaveCriticalSection(m);
  TRACE("st_masterlock_released");
}

static INLINE int st_masterlock_waiters(st_masterlock * m)
{
  return 1;                     /* info not maintained */
}
 
/* Mutexes */

typedef HANDLE st_mutex;

static DWORD st_mutex_create(st_mutex * res)
{
  st_mutex m = CreateMutex(0, FALSE, NULL);
  if (m == NULL) return GetLastError();
  TRACE1("st_mutex_create", m);
  *res = m;
  return 0;
}

static DWORD st_mutex_destroy(st_mutex m)
{
  TRACE1("st_mutex_destroy", m);
  if (CloseHandle(m))
    return 0;
  else
    return GetLastError();
}

static INLINE DWORD st_mutex_lock(st_mutex m)
{
  TRACE1("st_mutex_lock", m);
  if (WaitForSingleObject(m, INFINITE) == WAIT_FAILED) {
    TRACE1("st_mutex_lock (ERROR)", m);
    return GetLastError();
  } else {
    TRACE1("st_mutex_lock (done)", m);
    return 0;
  }
}

/* Error codes with the 29th bit set are reserved for the application */

#define PREVIOUSLY_UNLOCKED 0
#define ALREADY_LOCKED (1<<29)

static INLINE DWORD st_mutex_trylock(st_mutex m)
{
  DWORD rc;
  TRACE1("st_mutex_trylock", m);
  rc = WaitForSingleObject(m, 0);
  if (rc == WAIT_FAILED)
    return GetLastError();
  if (rc == WAIT_TIMEOUT)
    return ALREADY_LOCKED;
  else
    return PREVIOUSLY_UNLOCKED;
}

static INLINE DWORD st_mutex_unlock(st_mutex m)
{
  TRACE1("st_mutex_unlock", m);
  if (ReleaseMutex(m))
    return 0;
  else
    return GetLastError();
}

/* Condition variables, emulated using semaphores. */

typedef struct st_condvar_struct {
  LONG count;                /* number of waiting threads */
  HANDLE sem;                /* semaphore on which threads are waiting */
} * st_condvar;

/* Note: this is not a general emulation of condition variables:
   we assume that the master lock protects accesses to the "count" field.
   The master lock must be held when calling the "st_condvar_signal",
   "st_condvar_broadcast" and "st_condvar_prepare_wait" functions. */

static DWORD st_condvar_create(st_condvar * res)
{
  DWORD rc;
  st_condvar c = malloc(sizeof(struct st_condvar_struct));
  if (c == NULL) return ERROR_NOT_ENOUGH_MEMORY;
  c->sem = CreateSemaphore(NULL, 0, 0x7FFFFFFF, NULL);
  if (c->sem == NULL) return GetLastError();
  c->count = 0;
  *res = c;
  return 0;
}

static DWORD st_condvar_destroy(st_condvar c)
{
  if (CloseHandle(c->sem))
    return 0;
  else
    return GetLastError();
}

static DWORD st_condvar_signal(st_condvar c)
{
  if (c->count > 0) {
    c->count --;
    /* Increment semaphore by 1, waking up one waiter */
    if (ReleaseSemaphore(c->sem, 1, NULL))
      return 0;
    else
      return GetLastError();
  } else {
    return 0;
  }
}

static DWORD st_condvar_broadcast(st_condvar c)
{
  LONG n = c->count;
  if (n > 0) {
    c->count = 0;
    /* Increment semaphore by n, waking up all waiters */
    if (ReleaseSemaphore(c->sem, n, NULL))
      return 0;
    else
      return GetLastError();
  } else {
    return 0;
  }
}

static INLINE void st_condvar_prepare_wait(st_condvar c)
{
  c->count ++;
}

static DWORD st_condvar_wait(st_condvar c, st_mutex m)
{
  HANDLE handles[2];
  DWORD rc;

  if (! ReleaseMutex(m)) return GetLastError();
  /* Wait for semaphore to be non-null, and decrement it.
     Simultaneously, re-acquire mutex. */
  handles[0] = c->sem;
  handles[1] = m;
  rc = WaitForMultipleObjects(2, handles, TRUE, INFINITE);
  if (rc == WAIT_FAILED)
    return GetLastError();
  else
    return 0;
}

/* Triggered events */

typedef HANDLE st_event;

static DWORD st_event_create(st_event * res)
{
  st_event m =
    CreateEvent(NULL, TRUE/*manual reset*/, FALSE/*initially unset*/, NULL);
  TRACE1("st_event_create", m);
  if (m == NULL) return GetLastError();
  *res = m;
  return 0;
}

static DWORD st_event_destroy(st_event e)
{
  TRACE1("st_event_destroy", e);
  if (CloseHandle(e))
    return 0;
  else
    return GetLastError();
}

static DWORD st_event_trigger(st_event e)
{
  TRACE1("st_event_trigger", e);
  if (SetEvent(e))
    return 0;
  else
    return GetLastError();
}

static DWORD st_event_wait(st_event e)
{
  TRACE1("st_event_wait", e);
  if (WaitForSingleObject(e, INFINITE) == WAIT_FAILED)
    return GetLastError();
  else
    return 0;
}

/* Reporting errors */

static void st_check_error(DWORD retcode, char * msg)
{
  char errmsg[1024];

  if (retcode == 0) return;
  if (retcode == ERROR_NOT_ENOUGH_MEMORY) raise_out_of_memory();
  sprintf(errmsg, "%s: error code %lx", retcode);
  raise_sys_error(copy_string(errmsg));
}

/* The tick thread: posts a SIGPREEMPTION signal periodically */

static DWORD WINAPI caml_thread_tick(void * arg)
{
  while(1) {
    Sleep(Thread_timeout);
    /* The preemption signal should never cause a callback, so don't
     go through caml_handle_signal(), just record signal delivery via
     caml_record_signal(). */
    caml_record_signal(SIGPREEMPTION);
  }
  return 0;                     /* prevents compiler warning */
}

/* "At fork" processing -- none under Win32 */

static DWORD st_atfork(void (*fn)(void))
{
  return 0;
}

/* Signal handling -- none under Win32 */

value caml_thread_sigmask(value cmd, value sigs) /* ML */
{
  invalid_argument("Thread.sigmask not implemented");
  return Val_int(0);		/* not reached */
}

value caml_wait_signal(value sigs) /* ML */
{
  invalid_argument("Thread.wait_signal not implemented");
  return Val_int(0);		/* not reached */
}
