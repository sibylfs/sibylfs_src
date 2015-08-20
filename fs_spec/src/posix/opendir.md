# SYNOPSIS

    #include <dirent.h>

    DIR *fdopendir(int fd);
    DIR *opendir(const char *dirname);

# DESCRIPTION

>     The fdopendir() function shall be equivalent to the opendir()
>     function except that the directory is specified by a file
>     descriptor rather than by a name. The file offset associated
>     with the file descriptor at the time of the call determines
>     which entries are returned.

>     Upon successful return from fdopendir(), the file descriptor is
>     under the control of the system, and if any attempt is made to
>     close the file descriptor, or to modify the state of the
>     associated description, other than by means of closedir(),
>     readdir(), readdir_r(), rewinddir(), or [XSI] [Option Start]
>     seekdir(), [Option End] the behavior is undefined. Upon calling
>     closedir() the file descriptor shall be closed.

>     It is unspecified whether the FD_CLOEXEC flag will be set on the
>     file descriptor by a successful call to fdopendir().

>     The opendir() function shall open a directory stream
>     corresponding to the directory named by the dirname
>     argument. The directory stream is positioned at the first
>     entry. If the type DIR is implemented using a file descriptor,
>     applications shall only be able to open up to a total of
>     {OPEN_MAX} files and directories.

>     If the type DIR is implemented using a file descriptor, the
>     descriptor shall be obtained as if the O_DIRECTORY flag was
>     passed to open().


# RETURN VALUE

>    Upon successful completion, these functions shall return a
>    pointer to an object of type DIR. Otherwise, these functions
>    shall return a null pointer and set errno to indicate the error.

# ERRORS

>     The opendir() function shall fail if:

>     [EACCES] 
>         Search permission is denied for the component of the path
>         prefix of dirname or 

EACCES:1

>         read permission is denied for dirname.

EACCES:2

>     [ELOOP] 
>         A loop exists in symbolic links encountered during
>         resolution of the dirname argument.

>     [ENAMETOOLONG]
>         The length of a component of a pathname is longer than {NAME_MAX}.

>     [ENOENT]
>         A component of dirname does not name an existing directory or dirname is an empty string.

ENOENT:1

>     [ENOTDIR]
>         A component of dirname names an existing file that is neither a directory nor a symbolic link to a directory.

ENOTDIR:1

>     The opendir() function may fail if:

>     [ELOOP]
>         More than {SYMLOOP_MAX} symbolic links were encountered during resolution of the dirname argument.
>     [EMFILE]
>         All file descriptors available to the process are currently open.
>     [ENAMETOOLONG]
>         The length of a pathname exceeds {PATH_MAX}, or pathname resolution of a symbolic link produced an intermediate result with a length that exceeds {PATH_MAX}.
>     [ENFILE]
>         Too many files are currently open in the system.

