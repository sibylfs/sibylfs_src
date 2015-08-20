# Name

> chown, fchownat - change owner and group of a file relative to
> directory file descriptor

# SYNOPSIS

> #include <unistd.h>

> int chown(const char *path, uid_t owner, gid_t group);
> int fchownat(int fd, const char *path, uid_t owner, gid_t group,
>      int flag);

# Description

> The chown() function shall change the user and group ownership of a
> file.

> The path argument points to a pathname naming a file. The user ID
> and group ID of the named file shall be set to the numeric values
> contained in owner and group, respectively.

> Only processes with an effective user ID equal to the user ID of the
> file or with appropriate privileges may change the ownership of a
> file. If _POSIX_CHOWN_RESTRICTED is in effect for path:

> Changing the user ID is restricted to processes with appropriate
> privileges.

> Changing the group ID is permitted to a process with an effective
> user ID equal to the user ID of the file, but without appropriate
> privileges, if and only if owner is equal to the file's user ID or
> (uid_t)-1 and group is equal either to the calling process'
> effective group ID or to one of its supplementary group IDs.

> If the specified file is a regular file, one or more of the S_IXUSR,
> S_IXGRP, or S_IXOTH bits of the file mode are set, and the process
> does not have appropriate privileges, the set-user-ID (S_ISUID) and
> set-group-ID (S_ISGID) bits of the file mode shall be cleared upon
> successful return from chown(). If the specified file is a regular
> file, one or more of the S_IXUSR, S_IXGRP, or S_IXOTH bits of the
> file mode are set, and the process has appropriate privileges, it is
> implementation-defined whether the set-user-ID and set-group-ID bits
> are altered. If the chown() function is successfully invoked on a
> file that is not a regular file and one or more of the S_IXUSR,
> S_IXGRP, or S_IXOTH bits of the file mode are set, the set-user-ID
> and set-group-ID bits may be cleared.

> If owner or group is specified as (uid_t)-1 or (gid_t)-1,
> respectively, the corresponding ID of the file shall not be
> changed. If both owner and group are -1, the times need not be
> updated.

> Upon successful completion, chown() shall mark for update the last
> file status change timestamp of the file.

CHOWN_TS

> The fchownat() function shall be equivalent to the chown() and
> lchown() functions except in the case where path specifies a
> relative path. In this case the file to be changed is determined
> relative to the directory associated with the file descriptor fd
> instead of the current working directory. If the file descriptor was
> opened without O_SEARCH, the function shall check whether directory
> searches are permitted using the current permissions of the
> directory underlying the file descriptor. If the file descriptor was
> opened with O_SEARCH, the function shall not perform the check.

> Values for flag are constructed by a bitwise-inclusive OR of flags
> from the following list, defined in <fcntl.h>:

> AT_SYMLINK_NOFOLLOW If path names a symbolic link, ownership of the
> symbolic link is changed.

> If fchownat() is passed the special value AT_FDCWD in the fd
> parameter, the current working directory shall be used and the
> behavior shall be identical to a call to chown() or lchown()
> respectively, depending on whether or not the AT_SYMLINK_NOFOLLOW
> bit is set in the flag argument.

# Return Value

> Upon successful completion, these functions shall return
> 0. Otherwise, these functions shall return -1 and set errno to
> indicate the error. If -1 is returned, no changes are made in the
> user ID and group ID of the file.

# Errors

> These functions shall fail if:

    [EACCES]
        Search permission is denied on a component of the path prefix.
    [ELOOP]
        A loop exists in symbolic links encountered during resolution of the path argument.
    [ENAMETOOLONG]
        The length of a component of a pathname is longer than {NAME_MAX}.

# ENOENT
> A component of path does not name an existing file or path is an empty string.
ENOENT:1

    [ENOTDIR]
        A component of the path prefix names an existing file that is neither a directory nor a symbolic link to a directory, or the path argument contains at least one non- <slash> character and ends with one or more trailing <slash> characters and the last pathname component names an existing file that is neither a directory nor a symbolic link to a directory.
    [EPERM]
        The effective user ID does not match the owner of the file, or the calling process does not have appropriate privileges and _POSIX_CHOWN_RESTRICTED indicates that such privilege is required.
    [EROFS]
        The named file resides on a read-only file system.

    The fchownat() function shall fail if:

    [EACCES]
        fd was not opened with O_SEARCH and the permissions of the directory underlying fd do not permit directory searches.
    [EBADF]
        The path argument does not specify an absolute path and the fd argument is neither AT_FDCWD nor a valid file descriptor open for reading or searching.
    [ENOTDIR]
        The path argument is not an absolute path and fd is a file descriptor associated with a non-directory file.

    These functions may fail if:

    [EIO]
        An I/O error occurred while reading or writing to the file system.
    [EINTR]
        The chown() function was interrupted by a signal which was caught.
    [EINVAL]
        The owner or group ID supplied is not a value supported by the implementation.
    [ELOOP]
        More than {SYMLOOP_MAX} symbolic links were encountered during resolution of the path argument.
    [ENAMETOOLONG]
        The length of a pathname exceeds {PATH_MAX}, or pathname resolution of a symbolic link produced an intermediate result with a length that exceeds {PATH_MAX}.


    The fchownat() function may fail if:

    [EINVAL]
        The value of the flag argument is not valid.


