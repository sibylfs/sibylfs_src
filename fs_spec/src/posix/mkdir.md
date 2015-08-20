# NAME

> mkdir, mkdirat - make a directory relative to directory file descriptor

# SYNOPSIS

    #include <sys/stat.h>
    
    int mkdir(const char *path, mode_t mode);
    int mkdirat(int fd, const char *path, mode_t mode);

# DESCRIPTION

> The mkdir() function shall create a new directory with name path. The
> file permission bits of the new directory shall be initialized from
> mode. These file permission bits of the mode argument shall be
> modified by the process' file creation mask.
> 
> When bits in mode other than the file permission bits are set, the
> meaning of these additional bits is implementation-defined.
> 
> The directory's user ID shall be set to the process' effective user
> ID. The directory's group ID shall be set to the group ID of the
> parent directory or to the effective group ID of the
> process. Implementations shall provide a way to initialize the
> directory's group ID to the group ID of the parent
> directory. Implementations may, but need not, provide an
> implementation-defined way to initialize the directory's group ID to
> the effective group ID of the calling process.
> 
> PARA: The newly created directory shall be an empty directory.

> PARA: If path names a symbolic link, mkdir() shall fail and set errno to
> [EEXIST].

> PARA: The mkdirat() function shall be equivalent to the mkdir() function
> except in the case where path specifies a relative path. In this case
> the newly created directory is created relative to the directory
> associated with the file descriptor fd instead of the current working
> directory. If the file descriptor was opened without O_SEARCH, the
> function shall check whether directory searches are permitted using
> the current permissions of the directory underlying the file
> descriptor. If the file descriptor was opened with O_SEARCH, the
> function shall not perform the check.

> PARA: If mkdirat() is passed the special value AT_FDCWD in the fd parameter,
> the current working directory shall be used and the behavior shall be
> identical to a call to mkdir().


# RETURN VALUE

> Upon successful completion, these functions shall return
> 0. Otherwise, these functions shall return -1 and set errno to
> indicate the error. If -1 is returned, no directory shall be
> created.

# TIMESTAMPS

> Upon successful completion, mkdir() shall mark for update the last
> data access, last data modification, and last file status change
> timestamps of the directory.

MKDIR_TS:1

> Also, the last data modification and last
> file status change timestamps of the directory that contains the new
> entry shall be marked for update.

MKDIR_TS:2


# ERRORS

> These functions shall fail if:

## [EACCES] perms

> Search permission is denied on a component of the path prefix, 

EACCES:1

> or write permission is denied on the parent directory of the directory to
> be created.

EACCES:2


## [EEXIST] reconciled

> The named file exists.

EEXIST:1

## [ELOOP] symlinks

> A loop exists in symbolic links encountered during resolution of the
> path argument.
> 

## [EMLINK] link count

> The link count of the parent directory would exceed {LINK_MAX}.

## [ENAMETOOLONG] limits

> The length of a component of a pathname is longer than {NAME_MAX}.

## [ENOENT] reconciled, POSIX spec error

> A component of the path prefix specified by path does not name an
> existing directory 

ENOENT:4

compared to link and rename, this clause matches a component of the
prefix that names a file (link and mkdir both raise ENOTDIR in this
case). This is almost certainly an error in the POSIX spec - ENOTDIR
is error raised in all other cases.


> or path is an empty string.

ENOENT:3


## [ENOSPC] space

> The file system does not contain enough space to hold the contents of
> the new directory or to extend the parent directory of the new
> directory.
> 

## [ENOTDIR] reconciled

> A component of the path prefix names an existing file that is neither
> a directory nor a symbolic link to a directory.




## [EROFS] rofs

> The parent directory resides on a read-only file system.

> In addition, the mkdirat() function shall fail if:

## [EBADF] *at

> The path argument does not specify an absolute path and the fd
> argument is neither AT_FDCWD nor a valid file descriptor open for
> reading or searching.

## [ENOTDIR] *at

> The path argument is not an absolute path and fd is a file descriptor
> associated with a non-directory file.

> These functions may fail if:

## [ELOOP] symlinks

> More than {SYMLOOP_MAX} symbolic links were encountered during
> resolution of the path argument.
> 

## [ENAMETOOLONG] limits

> The length of a pathname exceeds {PATH_MAX}, or pathname resolution of
> a symbolic link produced an intermediate result with a length that
> exceeds {PATH_MAX}.


## Local variables
Local Variables:
mode: hi-lock
mode: outline-minor
outline-regexp: "^ *[#]+ "
End:
