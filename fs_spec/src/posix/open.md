# SYNOPSIS

    [OH]  #include <sys/stat.h> 
    
    #include <fcntl.h>
    
    int open(const char *path, int oflag, ...);
    int openat(int fd, const char *path, int oflag, ...);

# DESCRIPTION

> The open() function shall establish the connection between a file and
> a file descriptor. It shall create an open file description that
> refers to a file and a file descriptor that refers to that open file
> description. The file descriptor is used by other I/O functions to
> refer to that file. The path argument points to a pathname naming the
> file.

NB two notions: file descriptor, and open file description; open file
descriptions are not accessible to users

> PARA The open() function shall return a file descriptor for the
> named file that is the lowest file descriptor not currently open for
> that process. The open file description is new, and therefore the
> file descriptor shall not share it with any other process in the
> system. The FD_CLOEXEC file descriptor flag associated with the new
> file descriptor shall be cleared unless the O_CLOEXEC flag is set in
> oflag.

Note "lowest file descriptor..." and "... not currently open"

open file descriptions are for the system as a whole, not per process;
fds are per-process

`FD_CLOEXEC` is a flag associated with fds, default cleared


> 
> PARA The file offset used to mark the current position within the file shall be set to the beginning of the file.



> 
> PARA The file status flags and file access modes of the open file description shall be set according to the value of oflag.

oflag controls file status flags

> 
> PARA Values for oflag are constructed by a bitwise-inclusive OR of flags from the following list, defined in <fcntl.h>. Applications shall specify exactly one of the first five values (file access modes) below in the value of oflag:

requirement for bitwise or of flags, but not for particular bits

note "exactly one" of the file access modes

NB this statement says "shall specify", but below the error EINVAL (for incorrect oflags) is listed under "function may fail". 

Actually, the "shall specify" is a requirement on applications. But
this seems nonsensical - POSIX doesn't specify applications, only the
library interface. The question is: what is the specification is an
application doesn't specify exactly one of the first five values.

The meaning of may is: "Describes a feature or behavior that is optional for an implementation that conforms to POSIX.1-2008. An application should not rely on the existence of the feature or behavior. An application that relies on such a feature or behavior cannot be assured to be portable across conforming implementations."

So the problem is that there is a strict requirement for oflags to
contain exactly one file access mode, but EINVAL is marked as a may,
so it is not clear what conformant implementations might return if
oflags is incorrect. In fact, EINVAL should probably be marked as a
shall fail. And we should treat EINVAL for incorrect oflags as such.

Probable posix spec error. No! The "shall specify" is a requirement on
applications. If they do something else, then the result is probably
"undefined" (or EINVAL, in the may case).

> 
> O_EXEC
> Open for execute only (non-directory files). The result is unspecified if this flag is applied to a directory.

O_EXEC:1
Does execute only mean that read and write are prohibited?

>
> O_RDONLY
> Open for reading only.
>
> O_RDWR
> Open for reading and writing. The result is undefined if this flag is applied to a FIFO.
>
> O_SEARCH
> Open directory for search only. The result is unspecified if this flag is applied to a non-directory file.

O_SEARCH:1
Search on a directory means?

>
> O_WRONLY
> Open for writing only.
> 

--

> PARA Any combination of the following may be used:
> 
> O_APPEND
> If set, the file offset shall be set to the end of the file prior to each write.
> 
> O_CLOEXEC
> If set, the FD_CLOEXEC flag for the new file descriptor shall be set.
> 

> O_CREAT If the file exists, this flag has no effect except as noted
> under O_EXCL below. Otherwise, the file shall be created; the user
> ID of the file shall be set to the effective user ID of the process;
> the group ID of the file shall be set to the group ID of the file's
> parent directory or to the effective group ID of the process; 
> and
> the access permission bits (see <sys/stat.h>) of the file mode shall
> be set to the value of the argument following the oflag argument

there are additional arguments beyond oflag

> taken as type mode_t modified as follows: a bitwise AND is performed
> on the file-mode bits and the corresponding bits in the complement
> of the process' file mode creation mask. 

each process has a file mode creation mask

> Thus, all bits in the file
> mode whose corresponding bit in the file mode creation mask is set
> are cleared. When bits other than the file permission bits are set,
> the effect is unspecified. 

must check for this
Notice: for mkdir it sais other bits are implementation defined. Inconsitency in spec?

> The argument following the oflag argument
> does not affect whether the file is open for reading, writing, or
> for both. 

why this explicit statement? why might the argument affect the file open mode?

> Implementations shall provide a way to initialize the
> file's group ID to the group ID of the parent
> directory. Implementations may, but need not, provide an
> implementation-defined way to initialize the file's group ID to the
> effective group ID of the calling process.

Looks like there is a choice here: effective group id may not be
supported. (Sorted by addition to architecture record)

> O_DIRECTORY
> If path resolves to a non-directory file, fail and set errno to [ENOTDIR].

ENOTDIR:1
 
> O_DSYNC
> [SIO]  Write I/O operations on the file descriptor shall complete as defined by synchronized I/O data integrity completion. 
> 

> O_EXCL 
> If O_CREAT and O_EXCL are set, open() shall fail if the file
> exists. 

O_EXCL:1

> The check for the existence of the file and the creation of
> the file if it does not exist shall be atomic with respect to other
> threads executing open() naming the same filename in the same
> directory with O_EXCL and O_CREAT set. 

O_EXCL:2

> If O_EXCL and O_CREAT are
> set, and path names a symbolic link, open() shall fail and set errno
> to [EEXIST], regardless of the contents of the symbolic link.

O_EXCL:3

> If O_EXCL is set and O_CREAT is not set, the result is undefined.
> 

D:1

> O_NOCTTY
> If set and 
> path identifies a terminal device, open() shall not cause the terminal device to become the controlling terminal for the process. If path does not identify a terminal device, O_NOCTTY shall be ignored.

We don't currently handle terminal devices

> 
> O_NOFOLLOW
> If path names a symbolic link, fail and set errno to [ELOOP].
> 

See ELOOP:1 below

--

> O_NONBLOCK
> When opening a FIFO with O_RDONLY or O_WRONLY set:
> If O_NONBLOCK is set, an open() for reading-only shall return without delay. An open() for writing-only shall return an error if no process currently has the file open for reading.

We don't deal with FIFOs

> 
> If O_NONBLOCK is clear, an open() for reading-only shall block the calling thread until a thread opens the file for writing. An open() for writing-only shall block the calling thread until a thread opens the file for reading.

This is some kind of synchronization between processes. Note that this appears to happen e.g. when reading a file which already exists and contains data.

> 
> When opening a block special or character special file that supports non-blocking opens:
> 
> If O_NONBLOCK is set, the open() function shall return without blocking for the device to be ready or available. Subsequent behavior of the device is device-specific.
> 
> If O_NONBLOCK is clear, the open() function shall block the calling thread until the device is ready or available before returning.
> 
> Otherwise, the O_NONBLOCK flag shall not cause an error, but it is unspecified whether the file status flags will include the O_NONBLOCK flag.

We don't do block special or character special files.

--

> O_RSYNC
> [SIO]  Read I/O operations on the file descriptor shall complete at the same level of integrity as specified by the O_DSYNC and O_SYNC flags. If both O_DSYNC and O_RSYNC are set in oflag, all I/O operations on the file descriptor shall complete as defined by synchronized I/O data integrity completion. If both O_SYNC and O_RSYNC are set in flags, all I/O operations on the file descriptor shall complete as defined by synchronized I/O file integrity completion. 

We don't do synchronized I/O integrity completion (?)

> 
> O_SYNC
> [XSI|SIO]  Write I/O operations on the file descriptor shall complete as defined by synchronized I/O file integrity completion. 
> [XSI]  The O_SYNC flag shall be supported for regular files, even if the Synchronized Input and Output option is not supported. 

> 
> O_TRUNC
> If the file exists and is a regular file, and the file is
> successfully opened O_RDWR or O_WRONLY, its length shall be
> truncated to 0, and the mode and owner shall be unchanged.

O_TRUNC:1

> It shall have no effect on FIFO special files or terminal device files. 

O_TRUNC:2 FIFO special files, terminal devices.

> Its effect on other file types is implementation-defined. 

O_TRUNC:3

> The result
> of using O_TRUNC without either O_RDWR or O_WRONLY is undefined.

O_TRUNC:4 

Opening for readonly, with truncate, is undefined.

> 
> O_TTY_INIT
> If path identifies a terminal device other than a pseudo-terminal, the device is not already open in any process, and either O_TTY_INIT is set in oflag or O_TTY_INIT has the value zero, open() shall set any non-standard termios structure terminal parameters to a state that provides conforming behavior; see XBD Parameters that Can be Set. It is unspecified whether O_TTY_INIT has any effect if the device is already open in any process. If path identifies the slave side of a pseudo-terminal that is not already open in any process, open() shall set any non-standard termios structure terminal parameters to a state that provides conforming behavior, regardless of whether O_TTY_INIT is set. If path does not identify a terminal device, O_TTY_INIT shall be ignored.
> 

--

> 
> PARA [SIO]  If both the O_SYNC and O_DSYNC flags are set, the effect is as if only the O_SYNC flag was set. 

synchronized I/O integrity completion

> 
> PARA [OB XSR]  If path refers to a STREAMS file, oflag may be constructed from O_NONBLOCK OR'ed with either O_RDONLY, O_WRONLY, or O_RDWR. Other flag values are not applicable to STREAMS devices and shall have no effect on them. The value O_NONBLOCK affects the operation of STREAMS drivers and certain functions applied to file descriptors associated with STREAMS files. For STREAMS drivers, the implementation of O_NONBLOCK is device-specific. 

STREAMS?

> 
> PARA The application shall ensure that it specifies the O_TTY_INIT flag on the first open of a terminal device since system boot or since the device was closed by the process that last had it open. The application need not specify the O_TTY_INIT flag when opening pseudo-terminals. [XSI]   If path names the master side of a pseudo-terminal device, then it is unspecified whether open() locks the slave side so that it cannot be opened. Conforming applications shall call unlockpt() before opening the slave side. 

terminals

> 
> PARA The largest value that can be represented correctly in an object of type off_t shall be established as the offset maximum in the open file description.

open file descriptions have a field "offset maximum"

> 
> PARA The openat() function shall be equivalent to the open() function except in the case where path specifies a relative path. In this case the file to be opened is determined relative to the directory associated with the file descriptor fd instead of the current working directory. If the file descriptor was opened without O_SEARCH, the function shall check whether directory searches are permitted using the current permissions of the directory underlying the file descriptor. If the file descriptor was opened with O_SEARCH, the function shall not perform the check.


permissions

> 
> PARA The oflag parameter and the optional fourth parameter correspond exactly to the parameters of open().
> 
> PARA If openat() is passed the special value AT_FDCWD in the fd parameter, the current working directory shall be used and the behavior shall be identical to a call to open().
> 

at-call

# RETURN VALUE

> Upon successful completion, these functions shall open the file and
> return a non-negative integer representing the lowest numbered
> unused file descriptor.

returns an integer

> Otherwise, these functions shall return -1
> and set errno to indicate the error. If -1 is returned, no files
> shall be created or modified.

# TIMESTAMPS

> PARA If O_CREAT is set and the file did not previously exist, upon
> successful completion, open() shall mark for update the last data
> access, last data modification, and last file status change
> timestamps of the file

OPEN_TS:1.1

> and the last data
> modification and last file status change timestamps of the parent
> directory.

OPEN_TS:1.2

> PARA If O_TRUNC is set and the file did previously exist, upon
> successful completion, open() shall mark for update the last data
> modification and last file status change timestamps of the file.

OPEN_TS:2


# ERRORS

> These functions shall fail if:

> [EACCES]
> Search permission is denied on a component of the path prefix, 

EACCES:1

> or the file exists and the permissions specified by oflag are denied, 

EACCES:2

> or the file does not exist and write permission is denied for the parent directory of the file to be created, 

EACCES:3

> or O_TRUNC is specified and write permission is denied.

EACCES:4

> 
> [EEXIST]
> O_CREAT and O_EXCL are set, and the named file exists.

EEXIST:1

> [EINTR]
> A signal was caught during open().

Signals

> 
> [EINVAL]
> [SIO]  The implementation does not support synchronized I/O for this file. 
> [EIO]
> [OB XSR]  The path argument names a STREAMS file and a hangup or error occurred during the open(). 

synchronized I/O; Streams

> 
> [EISDIR]
> The named file is a directory and oflag includes O_WRONLY or O_RDWR.

EISDIR:1
 
> [ELOOP]
> A loop exists in symbolic links encountered during resolution of the path argument, or O_NOFOLLOW was specified and the path argument names a symbolic link.

ELOOP:1

Symlinks, loops

> 
> [EMFILE]
> All file descriptors available to the process are currently open.

Resources/limits

> 
> [ENAMETOOLONG]
> The length of a component of a pathname is longer than {NAME_MAX}.

Limits

> 
> [ENFILE]
> The maximum allowable number of files is currently open in the system.

Limits

> 
> [ENOENT]
> O_CREAT is not set and a component of path does not name an existing file

ENOENT:1

> or O_CREAT is set and a component of the path prefix of path does not name an existing file, or path points to an empty string.

ENOENT:2

> [ENOENT] or [ENOTDIR]
> O_CREAT is set, and the path argument contains at least one non- <slash> character and ends with one or more trailing <slash> characters. If path names an existing file, an [ENOENT] error shall not occur.
>
ENOENT:3
ENOTDIR:2

> [ENOSR]
> [OB XSR]  The path argument names a STREAMS-based file and the system is unable to allocate a STREAM. 

Streams

> 
> [ENOSPC]
> The directory or file system that would contain the new file cannot be expanded, the file does not exist, and O_CREAT is specified.

Resources

> 
> [ENOTDIR]
> A component of the path prefix names an existing file that is neither a directory nor a symbolic link to a directory; or O_CREAT and O_EXCL are not specified, the path argument contains at least one non- <slash> character and ends with one or more trailing <slash> characters, and the last pathname component names an existing file that is neither a directory nor a symbolic link to a directory; or O_DIRECTORY was specified and the path argument resolves to a non-directory file.
> 
> [ENXIO]
> O_NONBLOCK is set, the named file is a FIFO, O_WRONLY is set, and no process has the file open for reading.

FIFOs

> 
> [ENXIO]
> The named file is a character special or block special file, and the device associated with this special file does not exist.

Special files

> 
> [EOVERFLOW]
> The named file is a regular file and the size of the file cannot be represented correctly in an object of type off_t.

Large files

> 
> [EROFS]
> The named file resides on a read-only file system and either O_WRONLY, O_RDWR, O_CREAT (if the file does not exist), or O_TRUNC is set in the oflag argument.

ROFS

> 

--

> The openat() function shall fail if:

> [EACCES]
> fd was not opened with O_SEARCH and the permissions of the directory underlying fd do not permit directory searches.
> 

Permissions

> [EBADF]
> The path argument does not specify an absolute path and the fd argument is neither AT_FDCWD nor a valid file descriptor open for reading or searching.
> 

EBADF:1


> [ENOTDIR]
> The path argument is not an absolute path and fd is a file descriptor associated with a non-directory file.
> 

--

> These functions may fail if:
> 
> [EAGAIN]
> [XSI]  The path argument names the slave side of a pseudo-terminal device that is locked. 

Pseudo-terminal, locked

> 
> [EINVAL]
> The value of the oflag argument is not valid.

Why "may fail"? What error should be returned if oflag is not valid?

> 
> [ELOOP]
> More than {SYMLOOP_MAX} symbolic links were encountered during resolution of the path argument.

Symbolic links

> 
> [ENAMETOOLONG]
> The length of a pathname exceeds {PATH_MAX}, or pathname resolution of a symbolic link produced an intermediate result with a length that exceeds {PATH_MAX}.

Limits

> 
> [ENOMEM]
> [OB XSR]  The path argument names a STREAMS file and the system is unable to allocate resources. 
> 

Resources

> [ETXTBSY]
> The file is a pure procedure (shared text) file that is being executed and oflag is O_WRONLY or O_RDWR.

Shared text file


# RATIONALE

> ...

> In historical implementations the value of O_RDONLY is zero. Because of that, it is not possible to detect the presence of O_RDONLY and another option. Future implementations should encode O_RDONLY and O_WRONLY as bit flags so that:
> 
> O_RDONLY | O_WRONLY == O_RDWR

This implies that the flags do not correspond to individual
bits. Indeed, in fcntl.h we have:

> The <fcntl.h> header shall define the following symbolic constants for use as the file access modes for open(), openat(), and fcntl(). The values shall be unique, except that O_EXEC and O_SEARCH may have equal values. The values shall be suitable for use in #if preprocessing directives.
> 
> O_EXEC
> Open for execute only (non-directory files). The result is unspecified if this flag is applied to a directory.
> O_RDONLY
> Open for reading only.
> O_RDWR
> Open for reading and writing.
> O_SEARCH
> Open directory for search only. The result is unspecified if this flag is applied to a non-directory file.
> O_WRONLY
> Open for writing only.
> 

The key phrase here is "values shall be unique", as opposed to "The
values shall be bitwise-distinct" that is used elsewhere in fcntl.h
