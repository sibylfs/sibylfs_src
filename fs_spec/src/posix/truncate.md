# NAME

>    truncate - truncate a file to a specified length

# SYNOPSIS

>    #include <unistd.h>
>
>    int truncate(const char *path, off_t length);

# DESCRIPTION

    > The truncate() function shall cause the regular file named by path to have a size which shall be equal to length bytes.

    > If the file previously was larger than length, the extra data is discarded. If the file was previously shorter than length, its size is increased, and the extended area appears as if it were zero-filled.

    > The application shall ensure that the process has write permission for the file.

    > [XSI] [Option Start] If the request would cause the file size to exceed the soft file size limit for the process, the request shall fail and the implementation shall generate the SIGXFSZ signal for the process. [Option End]

    > The truncate() function shall not modify the file offset for any open file descriptions associated with the file. Upon successful completion, if the file size is changed, truncate() shall mark for update the last data modification and last file status change timestamps of the file, and the S_ISUID and S_ISGID bits of the file mode may be cleared.

# RETURN VALUE

    Upon successful completion, truncate() shall return 0. Otherwise, -1 shall be returned, and errno set to indicate the error.

# TIMESTAMPS

> Upon successful completion, if the file size is changed, truncate()
> shall mark for update the last data modification and last file
> status change timestamps of the file, and the S_ISUID and S_ISGID
> bits of the file mode may be cleared.

TRUNCATE_TS


# ERRORS

    > The truncate() function shall fail if:

    > [EINTR]
    >     A signal was caught during execution.
    > [EINVAL]
    >     The length argument was less than 0.

EINVAL:1

    > [EFBIG] or [EINVAL]
    >     The length argument was greater than the maximum file size.
    > [EIO]
    >     An I/O error occurred while reading from or writing to a file system.

    > [EACCES]
    >     A component of the path prefix denies search permission, 

EACCES:1

    > or write permission is denied on the file.

EACCES:2

    > [EISDIR]
    >     The named file is a directory.

EISDIR:1

    > [ELOOP]
    >     A loop exists in symbolic links encountered during resolution of the path argument.
    > [ENAMETOOLONG]
    >     The length of a component of a pathname is longer than {NAME_MAX}.
    > [ENOENT]
    >     A component of path does not name an existing file or path is an empty string.

ENOENT:1

    > [ENOTDIR]
    >     A component of the path prefix names an existing file that is neither a directory nor a symbolic link to a directory, or the path argument contains at least one non- <slash> character and ends with one or more trailing <slash> characters and the last pathname component names an existing file that is neither a directory nor a symbolic link to a directory.
    > [EROFS]
    >     The named file resides on a read-only file system.


    > The truncate() function may fail if:

    > [ELOOP]
    >     More than {SYMLOOP_MAX} symbolic links were encountered during resolution of the path argument.
    > [ENAMETOOLONG]
    >     The length of a pathname exceeds {PATH_MAX}, or pathname resolution of a symbolic link produced an intermediate result with a length that exceeds {PATH_MAX}.

