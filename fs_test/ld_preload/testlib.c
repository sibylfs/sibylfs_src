#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

char *interp = "/tmp/l/bitbucket/fs/fs_test/interp/interp.native";

char *log_txt = "/tmp/log.txt";

char *errarr[] = {
  /* 0 */ "FIXMEmissingerrorcode",
/* 1  */ "EPERM",
/* 2  */ "ENOENT",
/* 3  */ "ESRCH",
/* 4  */ "EINTR",
/* 5  */ "EIO",
/* 6  */ "ENXIO",
/* 7  */ "E2BIG",
/* 8  */ "ENOEXEC",
/* 9  */ "EBADF",
/* 10 */ "ECHILD",
/* 11 */ "EAGAIN",
/* 12 */ "ENOMEM",
/* 13 */ "EACCES",
/* 14 */ "EFAULT",
/* 15 */ "ENOTBLK",
/* 16 */ "EBUSY",
/* 17 */ "EEXIST",
/* 18 */ "EXDEV",
/* 19 */ "ENODEV",
/* 20 */ "ENOTDIR",
/* 21 */ "EISDIR",
/* 22 */ "EINVAL",
/* 23 */ "ENFILE",
/* 24 */ "EMFILE",
/* 25 */ "ENOTTY",
/* 26 */ "ETXTBSY",
/* 27 */ "EFBIG",
/* 28 */ "ENOSPC",
/* 29 */ "ESPIPE",
/* 30 */ "EROFS",
/* 31 */ "EMLINK",
/* 32 */ "EPIPE",
/* 33 */ "EDOM",
/* 34 */ "ERANGE",
/* 35 */ "EDEADLK",
/* 36 */ "ENAMETOOLONG",
/* 37 */ "ENOLCK",
/* 38 */ "ENOSYS",
/* 39 */ "ENOTEMPTY",
/* 40 */ "ELOOP",
  "FIXMEnoerrormsg",
/* 42 */ "ENOMSG",
/* 43 */ "EIDRM",
/* 44 */ "ECHRNG",
/* 45 */ "EL2NSYNC",
/* 46 */ "EL3HLT",
/* 47 */ "EL3RST",
/* 48 */ "ELNRNG",
/* 49 */ "EUNATCH",
/* 50 */ "ENOCSI",
/* 51 */ "EL2HLT",
/* 52 */ "EBADE",
/* 53 */ "EBADR",
/* 54 */ "EXFULL",
/* 55 */ "ENOANO",
/* 56 */ "EBADRQC",
/* 57 */ "EBADSLT",
  "FIXMEnoerrormsg",
/* 59 */ "EBFONT",
/* 60 */ "ENOSTR",
/* 61 */ "ENODATA",
/* 62 */ "ETIME",
/* 63 */ "ENOSR",
/* 64 */ "ENONET",
/* 65 */ "ENOPKG",
/* 66 */ "EREMOTE",
/* 67 */ "ENOLINK",
/* 68 */ "EADV",
/* 69 */ "ESRMNT",
/* 70 */ "ECOMM",
/* 71 */ "EPROTO",
/* 72 */ "EMULTIHOP",
/* 73 */ "EDOTDOT",
/* 74 */ "EBADMSG",
/* 75 */ "EOVERFLOW",
/* 76 */ "ENOTUNIQ",
/* 77 */ "EBADFD",
/* 78 */ "EREMCHG",
/* 79 */ "ELIBACC",
/* 80 */ "ELIBBAD",
/* 81 */ "ELIBSCN",
/* 82 */ "ELIBMAX",
/* 83 */ "ELIBEXEC",
/* 84 */ "EILSEQ",
/* 85 */ "ERESTART",
/* 86 */ "ESTRPIPE",
/* 87 */ "EUSERS",
/* 88 */ "ENOTSOCK",
/* 89 */ "EDESTADDRREQ",
/* 90 */ "EMSGSIZE",
/* 91 */ "EPROTOTYPE",
/* 92 */ "ENOPROTOOPT",
/* 93 */ "EPROTONOSUPPORT",
/* 94 */ "ESOCKTNOSUPPORT",
/* 95 */ "EOPNOTSUPP",
/* 96 */ "EPFNOSUPPORT",
/* 97 */ "EAFNOSUPPORT",
/* 98 */ "EADDRINUSE",
/* 99 */ "EADDRNOTAVAIL",
/* 100*/ "ENETDOWN",
/* 101*/ "ENETUNREACH",
/* 102*/ "ENETRESET",
/* 103*/ "ECONNABORTED",
/* 104*/ "ECONNRESET",
/* 105*/ "ENOBUFS",
/* 106*/ "EISCONN",
/* 107*/ "ENOTCONN",
/* 108*/ "ESHUTDOWN",
/* 109*/ "ETOOMANYREFS",
/* 110*/ "ETIMEDOUT",
/* 111*/ "ECONNREFUSED",
/* 112*/ "EHOSTDOWN",
/* 113*/ "EHOSTUNREACH",
/* 114*/ "EALREADY",
/* 115*/ "EINPROGRESS",
/* 116*/ "ESTALE",
/* 117*/ "EUCLEAN",
/* 118*/ "ENOTNAM",
/* 119*/ "ENAVAIL",
/* 120*/ "EISNAM",
/* 121*/ "EREMOTEIO",
/* 122*/ "EDQUOT",
/* 123*/ "ENOMEDIUM",
/* 124*/ "EMEDIUMTYPE",
/* 125*/ "ECANCELED",
/* 126*/ "ENOKEY",
/* 127*/ "EKEYEXPIRED",
/* 128*/ "EKEYREVOKED",
/* 129*/ "EKEYREJECTED",
/* 130*/ "EOWNERDEAD",
/* 131*/ "ENOTRECOVERABLE",
/* 132*/ "ERFKILL",
/* 133*/ "EHWPOISON"

};


typedef int (*realrename_t)(const char *oldpath, const char *newpath);
static realrename_t realrename;

void do_dynamic_call_rebinding()
{
  void *handle = NULL;
  int flags = RTLD_NOW | RTLD_GLOBAL;
  // the following line surely isn't correct, but seemed to be necessary to get stuff working on magrathea
  if ((handle = dlopen("/lib/i386-linux-gnu/i686/cmov/libc.so.6", flags)) == NULL) {
    if ((handle = dlopen("/lib/i386-linux-gnu/libc.so.6", flags)) == NULL) {
      if ((handle = dlopen("/lib/x86_64-linux-gnu/libc.so.6", flags)) == NULL) {
        printf("Failed to call dlopen in testlib: %s\n", dlerror());
        exit(1);
      }
    }
  }
  if ((realrename = dlsym(handle, "rename")) == NULL) {
    printf("Failed to rebind rename() in testlib: %s\n", dlerror());
    exit(1);
  }
}

int rename(const char *oldpath, const char *newpath)
{
  int retval;
  char buf[16536];
  char cwdbuf[4096];
  int i;
  int myerrno;
  FILE *fp;

  do_dynamic_call_rebinding();
  getcwd(cwdbuf,4096);

  // run external command
  sprintf(buf,"%s -m unix_impl_readonly -unix_root / -cwd %s \"rename \\\"%s\\\" \\\"%s\\\"\" >>%s",interp,cwdbuf,oldpath,newpath,log_txt);
  //printf("command: %s\n",buf);
  system(buf);
  retval = realrename(oldpath,newpath);
  if(retval==-1) { myerrno = errno; }

  // logging
  fp = fopen("/tmp/log.txt","a");
  fprintf(fp,"--\n");
  fprintf(fp,"Current cwd: %s\n",cwdbuf);
  //fprintf(fp,"Calling LD_PRELOAD library!\n");
  //fprintf(fp,"rename(%s,%s)\n",oldpath,newpath);
  //fprintf(fp,"Calling an external program...\n");
  //fprintf(fp,"Actual return value was %d\n",retval);
  if(retval==0) {     fprintf(fp,"None1\n"); }
  if(retval==-1) {
    fprintf(fp,"%s\n", errarr[myerrno]);
  }
  fprintf(fp,"----\n");
  i = fclose(fp);

  errno=myerrno;
  return retval; // do nothing!
}
