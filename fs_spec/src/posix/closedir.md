# SYNOPSIS

>     #include <dirent.h>

>     int closedir(DIR *dirp);

# DESCRIPTION

>     The closedir() function shall close the directory stream
>     referred to by the argument dirp. Upon return, the value of dirp
>     may no longer point to an accessible object of the type DIR. If
>     a file descriptor is used to implement type DIR, that file
>     descriptor shall be closed.

# RETURN VALUE

>     Upon successful completion, closedir() shall return 0;
>     otherwise, -1 shall be returned and errno set to indicate the
>     error.

> ERRORS

>     The closedir() function may fail if:

>     [EBADF]
>         The dirp argument does not refer to an open directory stream.
EBADF:1

>     [EINTR]
>         The closedir() function was interrupted by a signal.

