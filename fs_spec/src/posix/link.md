# NAME

> link, linkat - link one file to another file relative to two directory file descriptors

# SYNOPSIS

    #include <unistd.h>
    
    int link(const char *path1, const char *path2);
    int linkat(int fd1, const char *path1, int fd2,
           const char *path2, int flag);

# DESCRIPTION

PARA: The link() function shall create a new link (directory entry) for the
existing file, path1.

PARA: The path1 argument points to a pathname naming an existing file. The
path2 argument points to a pathname naming the new directory entry to
be created. The link() function shall atomically create a new link for
the existing file and the link count of the file shall be incremented
by one.

PARA: If path1 names a directory, link() shall fail unless the process has
appropriate privileges and the implementation supports using link() on
directories.

PARA: If path1 names a symbolic link, it is implementation-defined whether
link() follows the symbolic link, or creates a new link to the
symbolic link itself.

PARA: If link() fails, no link shall be created and the link count of the
file shall remain unchanged.

PARA: The implementation may require that the calling process has permission
to access the existing file.

PARA: The linkat() function shall be equivalent to the link() function
except that symbolic links shall be handled as specified by the value
of flag (see below) and except in the case where either path1 or path2
or both are relative paths. In this case a relative path path1 is
interpreted relative to the directory associated with the file
descriptor fd1 instead of the current working directory and similarly
for path2 and the file descriptor fd2. If the file descriptor was
opened without O_SEARCH, the function shall check whether directory
searches are permitted using the current permissions of the directory
underlying the file descriptor. If the file descriptor was opened with
O_SEARCH, the function shall not perform the check.

PARA: Values for flag are constructed by a bitwise-inclusive OR of flags
from the following list, defined in <fcntl.h>:

PARA: AT_SYMLINK_FOLLOW If path1 names a symbolic link, a new link for the
target of the symbolic link is created.

PARA: If linkat() is passed the special value AT_FDCWD in the fd1 or fd2
parameter, the current working directory shall be used for the
respective path argument. If both fd1 and fd2 have value AT_FDCWD, the
behavior shall be identical to a call to link(), except that symbolic
links shall be handled as specified by the value of flag.

PARA: If the AT_SYMLINK_FOLLOW flag is clear in the flag argument and the
path1 argument names a symbolic link, a new link is created for the
symbolic link path1 and not its target.

# RETURN VALUE

> Upon successful completion, these functions shall return 0. Otherwise,
> these functions shall return -1 and set errno to indicate the error.

# TIMESTAMPS

> Upon successful completion, link() shall mark for update the last file
> status change timestamp of the file.

LINK_TS:1

> Also, the last data modification
> and last file status change timestamps of the directory that contains
> the new entry shall be marked for update.

LINK_TS:2


# ERRORS

> These functions shall fail if:

## [EACCES] perms

> A component of either path prefix denies search permission, 

EACCES:1

> or the requested link requires writing in a directory that denies write
> permission, 

EACCES:2

> or the calling process does not have permission to access
> the existing file and this is required by the implementation.

EACCES:3


## [EEXIST] reconciled

> The path2 argument resolves to an existing directory entry or refers
> to a symbolic link.
> 

EEXIST:1

Note that "existing directory entry" means a file, directory or other.

Also note that Stevens has that link follows symbolic links, in which
case path2 can't refer to a symbolic link. However, at least on the
Mac, it appears that link follows a symlink if given as first path,
but not if given as second path. So Stevens appears to be
not-completely-true here.


## [ELOOP] symlinks

> A loop exists in symbolic links encountered during resolution of the
> path1 or path2 argument.
> 

Symlinks

## [EMLINK] limits

> The number of links to the file named by path1 would exceed
> {LINK_MAX}.
> 

limits

## [ENAMETOOLONG] limits

> The length of a component of a pathname is longer than {NAME_MAX}.

limits

## [ENOENT] reconciled

> A component of either path prefix does not exist; 

ENOENT:1

> the file named by path1 does not exist; 

ENOENT:2

> or path1 or path2 points to an empty string.

ENOENT:3


## [ENOSPC] space

> The directory to contain the link cannot be extended.

## [ENOTDIR] reconciled

> A component of either path prefix names an existing file that is
> neither a directory nor a symbolic link to a directory, 

ENOTDIR:1

> or the path1
> argument contains at least one non- <slash> character and ends with
> one or more trailing <slash> characters and the last pathname
> component names an existing file that is neither a directory nor a
> symbolic link to a directory, 

ENOTDIR:3

> or the path1 argument names an existing
> non-directory file and the path2 argument names a nonexistent file,
> contains at least one non- <slash> character, and ends with one or
> more trailing <slash> characters.

ENOTDIR:4


## [EPERM] reconciled

> The file named by path1 is a directory and either the calling process
> does not have appropriate privileges

EPERM:1
Reading the rationale, I think these appropriate privilegdes are there to
allow superusers on some systems to create hard links, while
prohibiting it for all other users.

> or the implementation prohibits
> using link() on directories.
> 

EPERM:2


## [EROFS] rofs

> The requested link requires writing in a directory on a read-only file
> system.
> 

rofs

## [EXDEV] exdev

> The link named by path2 and the file named by path1 are on different
> file systems and the implementation does not support links between
> file systems.
> 

exdev

## [EXDEV] exdev

> [OB XSR] path1 refers to a named STREAM.

> The linkat() function shall fail if:

## [EBADF] *at

> The path1 or path2 argument does not specify an absolute path and the
> fd1 or fd2 argument, respectively, is neither AT_FDCWD nor a valid
> file descriptor open for reading or searching.
> 

## [ENOTDIR] *at

> The path1 or path2 argument is not an absolute path and fd1 or fd2,
> respectively, is a file descriptor associated with a non-directory
> file.
> 
> These functions may fail if:

## [ELOOP] symlinks

> More than {SYMLOOP_MAX} symbolic links were encountered during
> resolution of the path1 or path2 argument.
> 

## [ENAMETOOLONG] limits

> The length of a pathname exceeds {PATH_MAX}, or pathname resolution of
> a symbolic link produced an intermediate result with a length that
> exceeds {PATH_MAX}.
> 

## Local variables
Local Variables:
mode: hi-lock
mode: outline-minor
outline-regexp: "^ *[#]+ "
End:
