# NAME

rmdir - remove a directory

# SYNOPSIS

    #include <unistd.h>
    
    int rmdir(const char *path);

# DESCRIPTION

> The rmdir() function shall remove a directory whose name is given by
> path. The directory shall be removed only if it is an empty directory.
> 
> If the directory is the root directory or the current working
> directory of any process, it is unspecified whether the function
> succeeds, or whether it shall fail and set errno to [EBUSY].
> 
> If path names a symbolic link, then rmdir() shall fail and set errno
> to [ENOTDIR].
> 
> If the path argument refers to a path whose final component is either
> dot or dot-dot, rmdir() shall fail.
> 
> If the directory's link count becomes 0 and no process has the
> directory open, the space occupied by the directory shall be freed and
> the directory shall no longer be accessible. If one or more processes
> have the directory open when the last link is removed, the dot and
> dot-dot entries, if present, shall be removed before rmdir() returns
> and no new entries may be created in the directory, but the directory
> shall not be removed until all references to the directory are closed.
> 
> If the directory is not an empty directory, rmdir() shall fail and set
> errno to [EEXIST] or [ENOTEMPTY].

# RETURN VALUE

> Upon successful completion, the function rmdir() shall return
> 0. Otherwise, -1 shall be returned, and errno set to indicate the
> error. If -1 is returned, the named directory shall not be changed.

# TIMESTAMPS

> Upon successful completion, rmdir() shall mark for update the last
> data modification and last file status change timestamps of the parent
> directory.

RMDIR_TS

# ERRORS

> The rmdir() function shall fail if:

## [EACCES] 
> 
> Search permission is denied on a component of the path prefix, or

EACCES:1

> write permission is denied on the parent directory of the directory to
> be removed.

EACCES:2

## [EBUSY] FIXME may not be multiprocs; POSIX unclear; reconciled

> The directory to be removed is currently in use by the system or some
> process and the implementation considers this to be an error.

EBUSY:1

Implementation dependent behaviour

May not be multiprocs if the process itself is using the directory
e.g. because path resolution went into the directory (And back out
again via .., say)

## [EEXIST] or [ENOTEMPTY] reconciled


> The path argument names a directory that is not an empty directory, 

EEXIST:1

> or
> there are hard links to the directory other than dot or a single entry
> in dot-dot.

Dir links

 
## [EINVAL] dot paths
 
> The path argument contains a last component that is dot.

 
## [EIO] eio
 
> A physical I/O error has occurred.
 
## [ELOOP] symlinks
 
> A loop exists in symbolic links encountered during resolution of the
> path argument.
 
## [ENAMETOOLONG] limits
 
> The length of a component of a pathname is longer than {NAME_MAX}.
 
## [ENOENT] reconciled
 
> A component of path does not name an existing file, 

ENOENT:2

Note that this doesn't cover the case where a component names a
non-directory file.

> or the path argument names a nonexistent directory 

ENOENT:1

> or points to an empty string.

ENOENT:3

 
## [ENOTDIR] reconciled
 
> A component of path names an existing file that is neither a directory
> nor a symbolic link to a directory.

ENOTDIR:1
 
## [EPERM] or [EACCES] perms
 
EPERM:1 
EACCES:3

> ## [XSI] The S_ISVTX flag is set on the directory containing the file
> referred to by the path argument and the process does not satisfy the
> criteria specified in XBD Directory Protection.

## [EROFS] rofs
 
> The directory entry to be removed resides on a read-only file system.
 
> The rmdir() function may fail if:
 
## [ELOOP] symlinks
 
> More than {SYMLOOP_MAX} symbolic links were encountered during
> resolution of the path argument.
 
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
