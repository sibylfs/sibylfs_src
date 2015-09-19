/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../../LICENSE.  */
/*                                                                     */
/***********************************************************************/

#define _GNU_SOURCE

#include <errno.h>
#include <sys/types.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <pwd.h>
#include <grp.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <sys/stat.h>

/* From <https://github.com/ocaml/ocaml/blob/4.01.0/otherlibs/unix/lseek.c> */
/* Modified to not use abstract commands */

#ifndef EOVERFLOW
#define EOVERFLOW ERANGE
#endif

CAMLprim value unix_lseek_int_command(value fd, value ofs, value cmd)
{
  off_t ret;
  caml_enter_blocking_section();
  ret = lseek(Int_val(fd), Long_val(ofs), Int_val(cmd));
  caml_leave_blocking_section();
  if (ret == -1) uerror("lseek", Nothing);
  if (ret > Max_long) unix_error(EOVERFLOW, "lseek", Nothing);
  return Val_long(ret);
}

/* Not included in the compiler's standard library distribution */

CAMLprim value unix_seteuid(value uid)
{
  if (seteuid(Int_val(uid)) == -1) uerror("seteuid", Nothing);
  return Val_unit;
}

/* Not included in the compiler's standard library distribution */

CAMLprim value unix_setegid(value gid)
{
  if (setegid(Int_val(gid)) == -1) uerror("setegid", Nothing);
  return Val_unit;
}

/* From <https://github.com/ocaml/ocaml/blob/4.01.0/otherlibs/unix/getpw.c> */
/* Modified to expose any resulting Unix_error */

static value alloc_passwd_entry(struct passwd *entry)
{
  value res;
  value name = Val_unit, passwd = Val_unit, gecos = Val_unit;
  value dir = Val_unit, shell = Val_unit;

  Begin_roots5 (name, passwd, gecos, dir, shell);
    name = copy_string(entry->pw_name);
    passwd = copy_string(entry->pw_passwd);
#if !defined(__BEOS__) && !defined(__ANDROID__)
    gecos = copy_string(entry->pw_gecos);
#else
    gecos = copy_string("");
#endif
    dir = copy_string(entry->pw_dir);
    shell = copy_string(entry->pw_shell);
    res = alloc_small(7, 0);
    Field(res,0) = name;
    Field(res,1) = passwd;
    Field(res,2) = Val_int(entry->pw_uid);
    Field(res,3) = Val_int(entry->pw_gid);
    Field(res,4) = gecos;
    Field(res,5) = dir;
    Field(res,6) = shell;
  End_roots();
  return res;
}

CAMLprim value unix_getpwnam_uerror(value name)
{
  struct passwd * entry;
  errno = 0;
  entry = getpwnam(String_val(name));
  if (entry == (struct passwd *) NULL) {
    if (errno == 0) raise_not_found();
    else uerror("getpwnam", name);
  }
  return alloc_passwd_entry(entry);
}

/* From <https://github.com/ocaml/ocaml/blob/4.01.0/otherlibs/unix/getgr.c> */
/* Modified to expose any resulting Unix_error */

static value alloc_group_entry(struct group *entry)
{
  value res;
  value name = Val_unit, pass = Val_unit, mem = Val_unit;

  Begin_roots3 (name, pass, mem);
    name = copy_string(entry->gr_name);
    pass = copy_string(entry->gr_passwd);
    mem = copy_string_array((const char**)entry->gr_mem);
    res = alloc_small(4, 0);
    Field(res,0) = name;
    Field(res,1) = pass;
    Field(res,2) = Val_int(entry->gr_gid);
    Field(res,3) = mem;
  End_roots();
  return res;
}

CAMLprim value unix_getgrnam_uerror(value name)
{
  struct group * entry;
  errno = 0;
  entry = getgrnam(String_val(name));
  if (entry == (struct group *) NULL) {
    if (errno == 0) raise_not_found();
    else uerror("getgrnam",name);
  }
  return alloc_group_entry(entry);
}

/* From <https://github.com/ocaml/ocaml/blob/4.01.0/otherlibs/unix/open.c> */
/* Modified to call the open libc function with arity 2 and remove CLOEXEC
   emulation */

#ifndef O_DSYNC
#define O_DSYNC 0
#endif
#ifndef O_RSYNC
#define O_RSYNC 0
#endif

/* Not included in the compiler's standard library distribution */
/* From <https://github.com/ocaml/ocaml/blob/4.01.0/otherlibs/unix/read.c> */
/* Modified to take a file offset argument and pass it to the pread libc
   function */

CAMLprim value unix_pread(value fd, value buf, value bofs, value len, value ofs)
{
  long numbytes;
  int ret;
  char iobuf[UNIX_BUFFER_SIZE];

  Begin_root (buf);
    numbytes = Long_val(len);
    if (numbytes > UNIX_BUFFER_SIZE) numbytes = UNIX_BUFFER_SIZE;
    enter_blocking_section();
    ret = pread(Int_val(fd), iobuf, (int) numbytes, (int) Long_val(ofs));
    leave_blocking_section();
    if (ret == -1) uerror("pread", Nothing);
    memmove (&Byte(buf, Long_val(bofs)), iobuf, ret);
  End_roots();
  return Val_int(ret);
}

/* Not included in the compiler's standard library distribution */
/* From <https://github.com/ocaml/ocaml/blob/4.01.0/otherlibs/unix/write.c> */
/* Modified to take a file offset argument and pass it to the pwrite libc
   function */

CAMLprim value unix_pwrite(value fd, value buf, value bofs, value vlen, value vofs)
{
  long len;
  int numbytes, ofs, ret;
  char iobuf[UNIX_BUFFER_SIZE];

  Begin_root (buf);
    ofs = Int_val(vofs);
    len = Long_val(vlen);
    ret = 0;
    if (len > 0) {
      numbytes = len > UNIX_BUFFER_SIZE ? UNIX_BUFFER_SIZE : len;
      memmove (iobuf, &Byte(buf, Long_val(bofs)), numbytes);
      enter_blocking_section();
      ret = pwrite(Int_val(fd), iobuf, numbytes, ofs);
      leave_blocking_section();
      if (ret == -1) uerror("pwrite", Nothing);
    }
  End_roots();
  return Val_int(ret);
}

// from https://github.com/janestreet/core/blob/master/lib/unix_stubs.c#L426
/* Replacement for broken stat functions */

static int file_kind_table[] = {
  S_IFREG, S_IFDIR, S_IFCHR, S_IFBLK, S_IFLNK, S_IFIFO, S_IFSOCK
};

#define Val_file_offset(fofs) caml_copy_int64(fofs)

static value cst_to_constr(int n, int *tbl, int size, int deflt)
{
  int i;
  for (i = 0; i < size; i++)
    if (n == tbl[i]) return Val_int(i);
  return Val_int(deflt);
}

static value core_stat_aux_64(struct stat64 *buf)
{
  CAMLparam0();
  CAMLlocal5(atime, mtime, ctime, offset, v);
  #if defined __linux|| defined _BSD_SOURCE || defined _SVID_SOURCE //FIXME not completely sure that the addition __linux makes this portable
  atime = caml_copy_double((double) buf->st_atime + (buf->st_atim.tv_nsec / 1000000000.0f));
  mtime = caml_copy_double((double) buf->st_mtime + (buf->st_mtim.tv_nsec / 1000000000.0f));
  ctime = caml_copy_double((double) buf->st_ctime + (buf->st_ctim.tv_nsec / 1000000000.0f));
  #elif defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__)
  atime = caml_copy_double((double) buf->st_atime + (buf->st_atimespec.tv_nsec / 1000000000.0f));
  mtime = caml_copy_double((double) buf->st_mtime + (buf->st_mtimespec.tv_nsec / 1000000000.0f));
  ctime = caml_copy_double((double) buf->st_ctime + (buf->st_ctimespec.tv_nsec / 1000000000.0f));
  #else
  atime = caml_copy_double((double) buf->st_atime + (buf->st_atime_nsec / 1000000000.0f));
  mtime = caml_copy_double((double) buf->st_mtime + (buf->st_mtime_nsec / 1000000000.0f));
  ctime = caml_copy_double((double) buf->st_ctime + (buf->st_ctime_nsec / 1000000000.0f));
  #endif
  offset = Val_file_offset(buf->st_size);
  v = caml_alloc_small(12, 0);
  Field (v, 0) = Val_int (buf->st_dev);
  Field (v, 1) = Val_int (buf->st_ino);
  Field (v, 2) = cst_to_constr(buf->st_mode & S_IFMT, file_kind_table,
                               sizeof(file_kind_table) / sizeof(int), 0);
  Field (v, 3) = Val_int (buf->st_mode & 07777);
  Field (v, 4) = Val_int (buf->st_nlink);
  Field (v, 5) = Val_int (buf->st_uid);
  Field (v, 6) = Val_int (buf->st_gid);
  Field (v, 7) = Val_int (buf->st_rdev);
  Field (v, 8) = offset;
  Field (v, 9) = atime;
  Field (v, 10) = mtime;
  Field (v, 11) = ctime;
  CAMLreturn(v);
}
static inline char * core_copy_to_c_string(value v_str)
{
  asize_t len = caml_string_length(v_str) + 1;
  char *p = caml_stat_alloc(len);
  memcpy(p, String_val(v_str), len);
  return p;
}
CAMLprim value core_unix_stat_64(value path)
{
  CAMLparam1(path);
  int ret;
  struct stat64 buf;
  char *p = core_copy_to_c_string(path);
  caml_enter_blocking_section();
  ret = stat64(p, &buf);
  caml_stat_free(p);
  caml_leave_blocking_section();
  if (ret == -1) uerror("stat", path);
  CAMLreturn(core_stat_aux_64(&buf));
}
CAMLprim value core_unix_lstat_64(value path)
{
  CAMLparam1(path);
  int ret;
  struct stat64 buf;
  char *p = core_copy_to_c_string(path);
  caml_enter_blocking_section();
  ret = lstat64(p, &buf);
  caml_stat_free(p);
  caml_leave_blocking_section();
  if (ret == -1) uerror("lstat", path);
  CAMLreturn(core_stat_aux_64(&buf));
}
