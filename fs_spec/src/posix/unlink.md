# Description

> The unlink() function shall remove a link to a file.
> If path names a symbolic link, unlink() shall remove the symbolic link named by path
> and shall not affect any file or directory named by the contents of the symbolic link.
> Otherwise, unlink() shall remove the link named by the pathname pointed to by path
> and shall decrement the link count of the file referenced by the link.
>
> When the file's link count becomes 0 and no process has the file open,
> the space occupied by the file shall be freed and the file shall no longer be accessible.
> If one or more processes have the file open when the last link is removed,
> the link shall be removed before unlink() returns,
> but the removal of the file contents shall be postponed until all references to the file are closed.
>
> The path argument shall not name a directory
> unless the process has appropriate privileges
> and the implementation supports using unlink() on directories.

# TIMESTAMPS

> Upon successful completion, unlink() shall mark for update the last
> data modification and last file status change timestamps of the
> parent directory.

UNLINK_TS:1

> Also, if the file's link count is not 0, the last file status change
> timestamp of the file shall be marked for update.

UNLINK_TS:2

# ERRORS

>   These functions shall fail and shall not unlink the file if:

>     [EACCES]
>         Search permission is denied for a component of the path prefix, or 

EACCES:1

>         write permission is denied on the directory containing the directory entry to be removed.

EACCES:2

>     [EBUSY]
>         The file named by the path argument cannot be unlinked
>         because it is being used by the system or another process
>         and the implementation considers this an error.

>     [ELOOP]
>         A loop exists in symbolic links encountered during resolution of the path argument.

>     [ENAMETOOLONG]
>         The length of a component of a pathname is longer than {NAME_MAX}.

>     [ENOENT]
>         A component of path does not name an existing file or path
>         is an empty string.

ENOENT:1

Note that this allows ENOENT if path is /path/to/existing/file/extra (ie trying to resolve through a normal file)


>     [ENOTDIR]
>         A component of the path prefix names an existing file that
>         is neither a directory nor a symbolic link to a directory,

ENOTDIR:1

>         or the path argument contains at least one non- <slash>
>         character and ends with one or more trailing <slash>
>         characters and the last pathname component names an existing
>         file that is neither a directory nor a symbolic link to a
>         directory.

ENOTDIR:2

>
>     [EPERM] 
>         The file named by path is a directory, and either the
>         calling process does not have appropriate privileges, or the
>         implementation prohibits using unlink() on directories.

EPERM:1

>     [EPERM] or [EACCES]
>         The S_ISVTX flag is set on the directory containing the file
>         referred to by the path argument and the process does not
>         satisfy the criteria specified in XBD Directory Protection.

EPERM:2
EACCES:3

>     [EROFS]
>         The directory entry to be unlinked is part of a read-only file system.

