# NAME

> readlink, readlinkat - read the contents of a symbolic link

# SYNOPSIS

> #include <unistd.h>

> ssize_t readlink(const char *restrict path, char *restrict buf,
>  size_t bufsize);
>
> ssize_t readlinkat(int fd, const char *restrict path,
>  char *restrict buf, size_t bufsize);

# DESCRIPTION

> The readlink() function shall place the contents of the symbolic
> link referred to by path in the buffer buf which has size
> bufsize. If the number of bytes in the symbolic link is less than
> bufsize, the contents of the remainder of buf are unspecified. If
> the buf argument is not large enough to contain the link content,
> the first bufsize bytes shall be placed in buf.

> If the value of bufsize is greater than {SSIZE_MAX}, the result is
> implementation-defined.

> The readlinkat() function shall be equivalent to the readlink()
> function except in the case where path specifies a relative path. In
> this case the symbolic link whose content is read is relative to the
> directory associated with the file descriptor fd instead of the
> current working directory. If the file descriptor was opened without
> O_SEARCH, the function shall check whether directory searches are
> permitted using the current permissions of the directory underlying
> the file descriptor. If the file descriptor was opened with
> O_SEARCH, the function shall not perform the check.

> If readlinkat() is passed the special value AT_FDCWD in the fd
> parameter, the current working directory shall be used and the
> behavior shall be identical to a call to readlink().


# RETURN VALUE

> Upon successful completion, these functions shall return the count
> of bytes placed in the buffer. Otherwise, these functions shall
> return a value of -1, leave the buffer unchanged, and set errno to
> indicate the error.

# TIMESTAMPS

> Upon successful completion, readlink() shall mark for update the
> last data access timestamp of the symbolic link.

READLINK_TS

# ERRORS

> These functions shall fail if:

# [EACCES]

> Search permission is denied for a component of the path prefix of
> path.

## [EINVAL]

> The path argument names a file that is not a symbolic link.

## [EIO]

> An I/O error occurred while reading from the file system.

## [ELOOP]

> A loop exists in symbolic links encountered during resolution of the
> path argument.

## [ENAMETOOLONG]

> The length of a component of a pathname is longer than {NAME_MAX}.

## [ENOENT]

> A component of path does not name an existing file or path is an
empty string.

## [ENOTDIR]

> A component of the path prefix names an existing file that is
> neither a directory nor a symbolic link to a directory, or the path
> argument contains at least one non- <slash> character and ends with
> one or more trailing <slash> characters and the last pathname
> component names an existing file that is neither a directory nor a
> symbolic link to a directory.


> The readlinkat() function shall fail if:

## [EACCES]

> fd was not opened with O_SEARCH and the permissions of the directory
> underlying fd do not permit directory searches.

## [EBADF]

> The path argument does not specify an absolute path and the fd
> argument is neither AT_FDCWD nor a valid file descriptor open for
> reading or searching.

## [ENOTDIR]

> The path argument is not an absolute path and fd is a file
> descriptor associated with a non-directory file.

> These functions may fail if:

## [ELOOP]

> More than {SYMLOOP_MAX} symbolic links were encountered during
         resolution of the path argument.

## [ENAMETOOLONG]

> The length of a pathname exceeds {PATH_MAX}, or pathname resolution
> of a symbolic link produced an intermediate result with a
> length that exceeds {PATH_MAX}.
