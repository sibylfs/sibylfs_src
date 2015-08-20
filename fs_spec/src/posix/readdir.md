# NAME

<    readdir, readdir_r - read a directory

# SYNOPSIS

>     #include <dirent.h>

>     struct dirent *readdir(DIR *dirp);
>     int readdir_r(DIR *restrict dirp, struct dirent *restrict entry,
>            struct dirent **restrict result);

# DESCRIPTION

>     The type DIR, which is defined in the <dirent.h> header,
>     represents a directory stream, which is an ordered sequence of
>     all the directory entries in a particular directory. Directory
>     entries represent files; files may be removed from a directory
>     or added to a directory asynchronously to the operation of
>     readdir().

>     The readdir() function shall return a pointer to a structure
>     representing the directory entry at the current position in the
>     directory stream specified by the argument dirp, and position
>     the directory stream at the next entry. It shall return a null
>     pointer upon reaching the end of the directory stream. The
>     structure dirent defined in the <dirent.h> header describes a
>     directory entry. The value of the structure's d_ino member shall
>     be set to the file serial number of the file named by the d_name
>     member. If the d_name member names a symbolic link, the value of
>     the d_ino member shall be set to the file serial number of the
>     symbolic link itself.

>     The readdir() function shall not return directory entries
>     containing empty names. If entries for dot or dot-dot exist, one
>     entry shall be returned for dot and one entry shall be returned
>     for dot-dot; otherwise, they shall not be returned.

>     The application shall not modify the structure to which the
>     return value of readdir() points, nor any storage areas pointed
>     to by pointers within the structure. The returned pointer, and
>     pointers within the structure, might be invalidated or the
>     structure or the storage areas might be overwritten by a
>     subsequent call to readdir() on the same directory stream. They
>     shall not be affected by a call to readdir() on a different
>     directory stream.

>     If a file is removed from or added to the directory after the
>     most recent call to opendir() or rewinddir(), whether a
>     subsequent call to readdir() returns an entry for that file is
>     unspecified.

>     The readdir() function may buffer several directory entries per
>     actual read operation; readdir() shall mark for update the last
>     data access timestamp of the directory each time the directory
>     is actually read.

>     After a call to fork(), either the parent or child (but not
>     both) may continue processing the directory stream using
>     readdir(), rewinddir(), [XSI] [Option Start] or
>     seekdir(). [Option End] If both the parent and child processes
>     use these functions, the result is undefined.

>     The readdir() function need not be thread-safe.

>     Applications wishing to check for error situations should set
>     errno to 0 before calling readdir(). If errno is set to non-zero
>     on return, an error occurred.

>     The readdir_r() function shall initialize the dirent structure
>     referenced by entry to represent the directory entry at the
>     current position in the directory stream referred to by dirp,
>     store a pointer to this structure at the location referenced by
>     result, and position the directory stream at the next entry.

>     The storage pointed to by entry shall be large enough for a
>     dirent with an array of char d_name members containing at least
>     {NAME_MAX}+1 elements.

>     Upon successful return, the pointer returned at *result shall
>     have the same value as the argument entry. Upon reaching the end
>     of the directory stream, this pointer shall have the value NULL.

>     The readdir_r() function shall not return directory entries containing empty names.

>     If a file is removed from or added to the directory after the
>     most recent call to opendir() or rewinddir(), whether a
>     subsequent call to readdir_r() returns an entry for that file is
>     unspecified.

>     The readdir_r() function may buffer several directory entries
>     per actual read operation; readdir_r() shall mark for update the
>     last data access timestamp of the directory each time the
>     directory is actually read.


# RETURN VALUE

>     Upon successful completion, readdir() shall return a pointer to
>     an object of type struct dirent. When an error is encountered, a
>     null pointer shall be returned and errno shall be set to
>     indicate the error. When the end of the directory is
>     encountered, a null pointer shall be returned and errno is not
>     changed.

>     If successful, the readdir_r() function shall return zero;
>     otherwise, an error number shall be returned to indicate the
>     error.


# ERRORS

>     These functions shall fail if:

>     [EOVERFLOW]
>         One of the values in the structure to be returned cannot be represented correctly.

>     These functions may fail if:

>     [EBADF]
>         The dirp argument does not refer to an open directory stream.

EBADF:1

>     [ENOENT]
>         The current position of the directory stream is invalid.

