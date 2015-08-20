The Open Group Base Specifications Issue 7  
 IEEE Std 1003.1, 2013 Edition  
 Copyright © 2001-2013 The IEEE and The Open Group

* * * * *

#### NAME

> sys/stat.h - data returned by the stat() function

#### SYNOPSIS

> `#include <sys/stat.h>`

#### DESCRIPTION

> The *\<sys/stat.h\>* header shall define the structure of the data
> returned by the
> [*fstat*()](http://pubs.opengroup.org/onlinepubs/9699919799/functions/fstat.html),
> [*lstat*()](http://pubs.opengroup.org/onlinepubs/9699919799/functions/lstat.html),
> and
> [*stat*()](http://pubs.opengroup.org/onlinepubs/9699919799/functions/stat.html)
> functions.
>
> The *\<sys/stat.h\>* header shall define the **stat** structure, which
> shall include at least the following members:
>
>     dev_t st_dev            Device ID of device containing file. 
>     ino_t st_ino            File serial number. 
>     mode_t st_mode          Mode of file (see below). 
>     nlink_t st_nlink        Number of hard links to the file. 
>     uid_t st_uid            User ID of file. 
>     gid_t st_gid            Group ID of file. 
>     [XSI]
>     dev_t st_rdev           Device ID (if file is character or block special). 
>
>     off_t st_size           For regular files, the file size in bytes. 
>                             For symbolic links, the length in bytes of the 
>                             pathname contained in the symbolic link. 
>     [SHM]
>                             For a shared memory object, the length in bytes. 
>
>     [TYM]
>                             For a typed memory object, the length in bytes. 
>
>                             For other file types, the use of this field is 
>                             unspecified. 
>     struct timespec st_atim Last data access timestamp. 
>     struct timespec st_mtim Last data modification timestamp. 
>     struct timespec st_ctim Last file status change timestamp. 
>     [XSI]
>     blksize_t st_blksize    A file system-specific preferred I/O block size 
>                             for this object. In some file system types, this 
>                             may vary from file to file. 
>     blkcnt_t st_blocks      Number of blocks allocated for this object. 
>
> The *st\_ino* and *st\_dev* fields taken together uniquely identify
> the file within the system.
>
> The *\<sys/stat.h\>* header shall define the
> **dev\_t**, **ino\_t**, **mode\_t**, **nlink\_t**, **uid\_t**,
> **gid\_t**, **off\_t**, and **time\_t** types as described in
> *\<sys/types.h\>*.
>
> The *\<sys/stat.h\>* header shall define the **timespec** structure as
> described in [*\<time.h\>*].
> Times shall be given in seconds since the Epoch.
>
> Which structure members have meaningful values depends on the type of
> file. For further information, see the descriptions of
> [*fstat*()](http://pubs.opengroup.org/onlinepubs/9699919799/functions/fstat.html),
> [*lstat*()](http://pubs.opengroup.org/onlinepubs/9699919799/functions/lstat.html),
> and
> [*stat*()](http://pubs.opengroup.org/onlinepubs/9699919799/functions/stat.html)
> in the System Interfaces volume of POSIX.1-2008.
>
> For compatibility with earlier versions of this standard, the
> *st\_atime* macro shall be defined with the value *st\_atim.tv\_sec*.
> Similarly, *st\_ctime* and *st\_mtime* shall be defined as macros with
> the values *st\_ctim.tv\_sec* and *st\_mtim.tv\_sec*, respectively.  
>
> The *\<sys/stat.h\>* header shall define the following symbolic
> constants for the file types encoded in type **mode\_t**. The values
> shall be suitable for use in **\#if** preprocessing directives:
>
> S\_IFMT  
> Type of file.
>
> S\_IFBLK  
> Block special.
>
> S\_IFCHR  
> Character special.
>
> S\_IFIFO  
> FIFO special.
>
> S\_IFREG  
> Regular.
>
> S\_IFDIR  
> Directory.
>
> S\_IFLNK  
> Symbolic link.
>
> S\_IFSOCK  
> Socket.
>
>
> The *\<sys/stat.h\>* header shall define the following symbolic
> constants for the file mode bits encoded in type **mode\_t**, with the
> indicated numeric values. These macros shall expand to an expression
> which has a type that allows them to be used, either singly or OR'ed
> together, as the third argument to
> *open*()
> without the need for a **mode\_t** cast. The values shall be suitable
> for use in **\#if** preprocessing directives.
>
> **Name**
>
> **Numeric Value**
>
> **Description**
>
> S\_IRWXU
>
> 0700
>
> Read, write, execute/search by owner.
>
> S\_IRUSR
>
> 0400
>
> Read permission, owner.
>
> S\_IWUSR
>
> 0200
>
> Write permission, owner.
>
> S\_IXUSR
>
> 0100
>
> Execute/search permission, owner.
>
> S\_IRWXG
>
> 070
>
> Read, write, execute/search by group.
>
> S\_IRGRP
>
> 040
>
> Read permission, group.
>
> S\_IWGRP
>
> 020
>
> Write permission, group.
>
> S\_IXGRP
>
> 010
>
> Execute/search permission, group.
>
> S\_IRWXO
>
> 07
>
> Read, write, execute/search by others.
>
> S\_IROTH
>
> 04
>
> Read permission, others.
>
> S\_IWOTH
>
> 02
>
> Write permission, others.
>
> S\_IXOTH
>
> 01
>
> Execute/search permission, others.
>
> S\_ISUID
>
> 04000
>
> Set-user-ID on execution.
>
> S\_ISGID
>
> 02000
>
> Set-group-ID on execution.
>
> S\_ISVTX
>
> 01000
>
> On directories, restricted deletion flag.
>
> The following macros shall be provided to test whether a file is of
> the specified type. The value *m* supplied to the macros is the value
> of *st\_mode* from a **stat** structure. The macro shall evaluate to a
> non-zero value if the test is true; 0 if the test is false.
>
> S\_ISBLK(*m*)  
> Test for a block special file.
>
> S\_ISCHR(*m*)  
> Test for a character special file.
>
> S\_ISDIR(*m*)  
> Test for a directory.
>
> S\_ISFIFO(*m*)  
> Test for a pipe or FIFO special file.
>
> S\_ISREG(*m*)  
> Test for a regular file.
>
> S\_ISLNK(*m*)  
> Test for a symbolic link.
>
> S\_ISSOCK(*m*)  
> Test for a socket.
>
> The implementation may implement message queues, semaphores, or shared
> memory objects as distinct file types. The following macros shall be
> provided to test whether a file is of the specified type. The value of
> the *buf* argument supplied to the macros is a pointer to a **stat**
> structure. The macro shall evaluate to a non-zero value if the
> specified object is implemented as a distinct file type and the
> specified file type is contained in the **stat** structure referenced
> by *buf*. Otherwise, the macro shall evaluate to zero.
>
> S\_TYPEISMQ(*buf*)  
> Test for a message queue.
>
> S\_TYPEISSEM(*buf*)  
> Test for a semaphore.
>
> S\_TYPEISSHM(*buf*)  
> Test for a shared memory object.
>
> The implementation may
> implement typed memory objects as distinct file types, and the
> following macro shall test whether a file is of the specified type.
> The value of the *buf* argument supplied to the macros is a pointer to
> a **stat** structure. The macro shall evaluate to a non-zero value if
> the specified object is implemented as a distinct file type and the
> specified file type is contained in the **stat** structure referenced
> by *buf*. Otherwise, the macro shall evaluate to zero.
>
> S\_TYPEISTMO(*buf*)  
> Test macro for a typed memory object.
>
> > The *\<sys/stat.h\>* header shall define the following symbolic
> constants as distinct integer values outside of the range
> [0,999999999], for use with the
> [*futimens*()](http://pubs.opengroup.org/onlinepubs/9699919799/functions/futimens.html)
> and
> [*utimensat*()](http://pubs.opengroup.org/onlinepubs/9699919799/functions/utimensat.html)
> functions: UTIME\_NOW UTIME\_OMIT
>
> The following shall be declared as functions and may also be defined
> as macros. Function prototypes shall be provided.
>
>     int    chmod(const char *, mode_t);
>     int    fchmod(int, mode_t);
>     int    fchmodat(int, const char *, mode_t, int);
>     int    fstat(int, struct stat *);
>     int    fstatat(int, const char *restrict, struct stat *restrict, int);
>     int    futimens(int, const struct timespec [2]);
>     int    lstat(const char *restrict, struct stat *restrict);
>     int    mkdir(const char *, mode_t);
>     int    mkdirat(int, const char *, mode_t);
>     int    mkfifo(const char *, mode_t);
>     int    mkfifoat(int, const char *, mode_t);
>     [XSI]
>     int    mknod(const char *, mode_t, dev_t);
>     int    mknodat(int, const char *, mode_t, dev_t);
>
>     int    stat(const char *restrict, struct stat *restrict);
>     mode_t umask(mode_t);
>     int    utimensat(int, const char *, const struct timespec [2], int);

* * * * *

*The following sections are informative.*

#### APPLICATION USAGE

> Use of the macros is recommended for determining the type of a file.

#### RATIONALE

> A conforming C-language application must include *\<sys/stat.h\>* for
> functions that have arguments or return values of type **mode\_t**, so
> that symbolic values for that type can be used. An alternative would
> be to require that these constants are also defined by including
> [*\<sys/types.h\>*](http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/sys_types.h.html).
>
> The S\_ISUID and S\_ISGID bits may be cleared on any write, not just
> on
> [*open*()](http://pubs.opengroup.org/onlinepubs/9699919799/functions/open.html),
> as some historical implementations do.
>
> System calls that update the time entry fields in the **stat**
> structure must be documented by the implementors. POSIX-conforming
> systems should not update the time entry fields for functions listed
> in the System Interfaces volume of POSIX.1-2008 unless the standard
> requires that they do, except in the case of documented extensions to
> the standard.
>
> Upon assignment, file timestamps are immediately converted to the
> resolution of the file system by truncation (i.e., the recorded time
> can be older than the actual time). For example, if the file system
> resolution is 1 microsecond, then a conforming
> [*stat*()](http://pubs.opengroup.org/onlinepubs/9699919799/functions/stat.html)
> must always return an *st\_mtim.tv\_nsec* that is a multiple of 1000.
> Some older implementations returned higher-resolution timestamps while
> the *inode* information was cached, and then spontaneously truncated
> the *tv\_nsec* fields when they were stored to and retrieved from
> disk, but this behavior does not conform.
>
> Note that *st\_dev* must be unique within a Local Area Network (LAN)
> in a \`\`system'' made up of multiple computers' file systems
> connected by a LAN.
>
> Networked implementations of a POSIX-conforming system must guarantee
> that all files visible within the file tree (including parts of the
> tree that may be remotely mounted from other machines on the network)
> on each individual processor are uniquely identified by the
> combination of the *st\_ino* and *st\_dev* fields.
>
> The unit for the *st\_blocks* member of the **stat** structure is not
> defined within POSIX.1-2008. In some implementations it is 512 bytes.
> It may differ on a file system basis. There is no correlation between
> values of the *st\_blocks* and *st\_blksize*, and the *f\_bsize* (from
> [*\<sys/statvfs.h\>*](http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/sys_statvfs.h.html))
> structure members.
>
> Traditionally, some implementations defined the multiplier for
> *st\_blocks* in
> [*\<sys/param.h\>*](http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/sys_param.h.html)
> as the symbol DEV\_BSIZE.
>
> Some earlier versions of this standard did not specify values for the
> file mode bit macros. The expectation was that some implementors might
> choose to use a different encoding for these bits than the traditional
> one, and that new applications would use symbolic file modes instead
> of numeric. This version of the standard specifies the traditional
> encoding, in recognition that nearly 20 years after the first
> publication of this standard numeric file modes are still in
> widespread use by application developers, and that all conforming
> implementations still use the traditional encoding.

#### FUTURE DIRECTIONS

> No new S\_IFMT symbolic names for the file type values of **mode\_t**
> will be defined by POSIX.1-2008; if new file types are required, they
> will only be testable through *S\_ISxx()* or *S\_TYPEISxxx()* macros
> instead.

#### SEE ALSO

> [*\<sys/statvfs.h\>*](http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/sys_statvfs.h.html),
> [*\<sys/types.h\>*](http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/sys_types.h.html),
> [*\<time.h\>*](http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/time.h.html)
>
> XSH
> [*chmod*](http://pubs.opengroup.org/onlinepubs/9699919799/functions/chmod.html#tag_16_58),
> [*fchmod*](http://pubs.opengroup.org/onlinepubs/9699919799/functions/fchmod.html#),
> [*fstat*](http://pubs.opengroup.org/onlinepubs/9699919799/functions/fstat.html#),
> [*fstatat*](http://pubs.opengroup.org/onlinepubs/9699919799/functions/fstatat.html#),
> [*futimens*](http://pubs.opengroup.org/onlinepubs/9699919799/functions/futimens.html#),
> [*mkdir*](http://pubs.opengroup.org/onlinepubs/9699919799/functions/mkdir.html#tag_16_325),
> [*mkfifo*](http://pubs.opengroup.org/onlinepubs/9699919799/functions/mkfifo.html#tag_16_327),
> [*mknod*](http://pubs.opengroup.org/onlinepubs/9699919799/functions/mknod.html#),
> [*umask*](http://pubs.opengroup.org/onlinepubs/9699919799/functions/umask.html#tag_16_631)

#### CHANGE HISTORY

> First released in Issue 1. Derived from Issue 1 of the SVID.

#### Issue 5

> The DESCRIPTION is updated for alignment with the POSIX Realtime
> Extension.
>
> The type of *st\_blksize* is changed from **long** to **blksize\_t**;
> the type of *st\_blocks* is changed from **long** to **blkcnt\_t**.

#### Issue 6

> The S\_TYPEISMQ(), S\_TYPEISSEM(), and S\_TYPEISSHM() macros are
> unconditionally mandated.
>
> The Open Group Corrigendum U035/4 is applied. In the DESCRIPTION, the
> types **blksize\_t** and **blkcnt\_t** have been described.
>
> The following new requirements on POSIX implementations derive from
> alignment with the Single UNIX Specification:
>
> -   The **dev\_t**, **ino\_t**, **mode\_t**, **nlink\_t**, **uid\_t**,
>     **gid\_t**, **off\_t**, and **time\_t** types are mandated.
>
> S\_IFSOCK and S\_ISSOCK are added for sockets.
>
> The description of **stat** structure members is changed to reflect
> contents when file type is a symbolic link.
>
> The test macro S\_TYPEISTMO is added for alignment with
> IEEE Std 1003.1j-2000.
>
> The **restrict** keyword is added to the prototypes for
> [*lstat*()](http://pubs.opengroup.org/onlinepubs/9699919799/functions/lstat.html)
> and
> [*stat*()](http://pubs.opengroup.org/onlinepubs/9699919799/functions/stat.html).
>
> The
> [*lstat*()](http://pubs.opengroup.org/onlinepubs/9699919799/functions/lstat.html)
> function is made mandatory.
>
> IEEE Std 1003.1-2001/Cor 1-2002, item XBD/TC1/D6/17 is applied, adding
> text regarding the *st\_blocks* member of the **stat** structure to
> the RATIONALE.
>
> IEEE Std 1003.1-2001/Cor 2-2004, item XBD/TC2/D6/25 is applied, adding
> to the DESCRIPTION that the **timespec** structure may be defined as
> described in the
> [*\<time.h\>*](http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/time.h.html)
> header.

#### Issue 7

> SD5-XSH-ERN-161 is applied, updating the DESCRIPTION to clarify that
> the descriptions of the interfaces should be consulted in order to
> determine which structure members have meaningful values.
>
> The
> [*fchmodat*()](http://pubs.opengroup.org/onlinepubs/9699919799/functions/fchmodat.html),
> [*fstatat*()](http://pubs.opengroup.org/onlinepubs/9699919799/functions/fstatat.html),
> [*mkdirat*()](http://pubs.opengroup.org/onlinepubs/9699919799/functions/mkdirat.html),
> [*mkfifoat*()](http://pubs.opengroup.org/onlinepubs/9699919799/functions/mkfifoat.html),
> [*mknodat*()](http://pubs.opengroup.org/onlinepubs/9699919799/functions/mknodat.html),
> and
> [*utimensat*()](http://pubs.opengroup.org/onlinepubs/9699919799/functions/utimensat.html)
> functions are added from The Open Group Technical Standard, 2006,
> Extended API Set Part 2.
>
> The
> [*futimens*()](http://pubs.opengroup.org/onlinepubs/9699919799/functions/futimens.html)
> function is added.
>
> This reference page is clarified with respect to macros and symbolic
> constants.
>
> Changes are made related to support for finegrained timestamps and the
> UTIME\_NOW and UTIME\_OMIT symbolic constants are added.
>
> POSIX.1-2008, Technical Corrigendum 1, XBD/TC1-2008/0068 [207] is
> applied.

*End of informative text.*

* * * * *

