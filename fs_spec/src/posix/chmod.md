# Name

> chmod, fchmodat - change mode of a file relative to directory file
> descriptor

# SYNOPSIS

> #include <sys/stat.h>
>
> int chmod(const char *path, mode_t mode);
> int fchmodat(int fd, const char *path, mode_t mode, int flag);

# Description

> The chmod() function shall change S_ISUID, S_ISGID, [XSI] [Option
> Start] S_ISVTX, [Option End] and the file permission bits of the
> file named by the pathname pointed to by the path argument to the
> corresponding bits in the mode argument. The application shall
> ensure that the effective user ID of the process matches the owner
> of the file or the process has appropriate privileges in order to do
> this.

> S_ISUID, S_ISGID, [XSI] [Option Start] S_ISVTX, [Option End] and the
> file permission bits are described in <sys/stat.h>.

> If the calling process does not have appropriate privileges, and if
> the group ID of the file does not match the effective group ID or
> one of the supplementary group IDs and if the file is a regular
> file, bit S_ISGID (set-group-ID on execution) in the file's mode
> shall be cleared upon successful return from chmod().

> Additional implementation-defined restrictions may cause the S_ISUID
> and S_ISGID bits in mode to be ignored.

> Upon successful completion, chmod() shall mark for update the last
> file status change timestamp of the file.

CHMOD_TS

> The fchmodat() function shall be equivalent to the chmod() function
> except in the case where path specifies a relative path. In this
> case the file to be changed is determined relative to the directory
> associated with the file descriptor fd instead of the current
> working directory. If the file descriptor was opened without
> O_SEARCH, the function shall check whether directory searches are
> permitted using the current permissions of the directory underlying
> the file descriptor. If the file descriptor was opened with
> O_SEARCH, the function shall not perform the check.

> Values for flag are constructed by a bitwise-inclusive OR of flags
> from the following list, defined in <fcntl.h>:

> AT_SYMLINK_NOFOLLOW If path names a symbolic link, then the mode of
> the symbolic link is changed.

> If fchmodat() is passed the special value AT_FDCWD in the fd
> parameter, the current working directory shall be used. If also flag
> is zero, the behavior shall be identical to a call to chmod().

# RETURN VALUE

> Upon successful completion, these functions shall return
> 0. Otherwise, these functions shall return -1 and set errno to
> indicate the error. If -1 is returned, no change to the file mode
> occurs.

# ERRORS

> These functions shall fail if:

    [EACCES]
        Search permission is denied on a component of the path prefix.

    [ELOOP]
        A loop exists in symbolic links encountered during resolution of the path argument.
    [ENAMETOOLONG]
        The length of a component of a pathname is longer than {NAME_MAX}.

## ENOENT
        A component of path does not name an existing file or path is an empty string.
ENOENT:1

    [ENOTDIR]
        A component of the path prefix names an existing file that is neither a directory nor a symbolic link to a directory, or the path argument contains at least one non- <slash> character and ends with one or more trailing <slash> characters and the last pathname component names an existing file that is neither a directory nor a symbolic link to a directory.
    [EPERM]
        The effective user ID does not match the owner of the file and the process does not have appropriate privileges.
    [EROFS]
        The named file resides on a read-only file system.

    The fchmodat() function shall fail if:

    [EACCES]
        fd was not opened with O_SEARCH and the permissions of the directory underlying fd do not permit directory searches.
    [EBADF]
        The path argument does not specify an absolute path and the fd argument is neither AT_FDCWD nor a valid file descriptor open for reading or searching.
    [ENOTDIR]
        The path argument is not an absolute path and fd is a file descriptor associated with a non-directory file.

    These functions may fail if:

    [EINTR]
        A signal was caught during execution of the function.
    [EINVAL]
        The value of the mode argument is invalid.
    [ELOOP]
        More than {SYMLOOP_MAX} symbolic links were encountered during resolution of the path argument.
    [ENAMETOOLONG]
        The length of a pathname exceeds {PATH_MAX}, or pathname resolution of a symbolic link produced an intermediate result with a length that exceeds {PATH_MAX}.

    The fchmodat() function may fail if:

    [EINVAL]
        The value of the flag argument is invalid.
    [EOPNOTSUPP]
        The AT_SYMLINK_NOFOLLOW bit is set in the flag argument, path names a symbolic link, and the system does not support changing the mode of a symbolic link.

