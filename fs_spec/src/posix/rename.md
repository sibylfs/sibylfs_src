In the following, "reconciled" by an error means that error cases have
been checked against the spec, and they agree.

# Description

> For rename(): [CX] The functionality described on this reference page
> is aligned with the ISO C standard. Any conflict between the
> requirements described here and the ISO C standard is
> unintentional. This volume of POSIX.1-2008 defers to the ISO C
> standard.
> 

Note defers

> PARA: The rename() function shall change the name of a file. The old
> argument points to the pathname of the file to be renamed. The new
> argument points to the new pathname of the file. 

> [CX] If the new
> argument does not resolve to an existing directory entry for a file of
> type directory and the new argument contains at least one non- <slash>
> character and ends with one or more trailing <slash> characters after
> all symbolic links have been processed, rename() shall fail.

Particular processing of slashes at the end of the new argument,
providing body contains at least one non-slash character

Fail with what error? See ENOTDIR or ENOENT:2 or tr/7 EINVAL:1

> 
> PARA: If either the old or new argument names a symbolic link, rename()
> shall operate on the symbolic link itself, and shall not resolve the
> last component of the argument. 

RENAME:2 symbolic links

> If the old argument and the new
> argument resolve to either the same existing directory entry or
> different directory entries for the same existing file, rename() shall
> return successfully and perform no other action.

RENAME:3 same entries

Note "no other action"

> 
> PARA: If the old argument points to the pathname of a file that is not a
> directory, the new argument shall not point to the pathname of a
> directory. 

What is this supposed to mean? The user can call rename /a/f1.txt
/a/b. Presumably this means that, *in a successful call*, this doesn't
happen.


> If the link named by the new argument exists, it shall be
> removed and old renamed to new. 

This para is about files

> In this case, a link named new shall
> remain visible to other processes throughout the renaming operation
> and refer either to the file referred to by new or old before the
> operation began. 

Note atomicity

> Write access permission is required for both the
> directory containing old and the directory containing new.

EACCES:2, EACCES:3

> PARA: If the old argument points to the pathname of a directory, the new
> argument shall not point to the pathname of a file that is not a
> directory. 

FIXME what errors are possible? ../tr/7 seems to have a new argument
point to a pathname of a file that is not a directory, and error is
EINVAL; this clause just says that we must have an error


> If the directory named by the new argument exists, it shall
> be removed and old renamed to new. 

> In this case, a link named new
> shall exist throughout the renaming operation and shall refer either
> to the directory referred to by new or old before the operation
> began. 

> If new names an existing directory, it shall be required to be
> an empty directory.

Note that same directory is handled at RENAME:3

FIXME where in this paragraph is the check for same directory?

> 
> PARA: If either pathname argument refers to a path whose final component is
> either dot or dot-dot, rename() shall fail.


> PARA: If the old argument points to a pathname of a symbolic link, the
> symbolic link shall be renamed. 

> If the new argument points to a
> pathname of a symbolic link, the symbolic link shall be removed.


> PARA: The old pathname shall not name an ancestor directory of the new
> pathname. 

What is an ancestor directory of a pathname? Not in base definitions ch 3

> Write access permission is required for the directory
> containing old and the directory containing new. 

EACCES:2, EACCES:3

> If the old argument
> points to the pathname of a directory, write access permission may be
> required for the directory named by old, and, if it exists, the
> directory named by new.

EACCES:4 EACCES:5

> PARA: If the link named by the new argument exists and the file's link count
> becomes 0 when it is removed and no process has the file open, the
> space occupied by the file shall be freed and the file shall no longer
> be accessible. 

Space usage; this seems very hard to ensure

> If one or more processes have the file open when the
> last link is removed, the link shall be removed before rename()
> returns, but the removal of the file contents shall be postponed until
> all references to the file are closed.

> If the rename() function fails for any reason other than [EIO], any
> file named by new shall be unaffected.

Invariant of rename command

> The renameat() function shall be equivalent to the rename() function
> except in the case where either old or new specifies a relative
> path. If old is a relative path, the file to be renamed is located
> relative to the directory associated with the file descriptor oldfd
> instead of the current working directory. If new is a relative path,
> the same happens only relative to the directory associated with
> newfd. If the file descriptor was opened without O_SEARCH, the
> function shall check whether directory searches are permitted using
> the current permissions of the directory underlying the file
> descriptor. If the file descriptor was opened with O_SEARCH, the
> function shall not perform the check.
> 
> If renameat() is passed the special value AT_FDCWD in the oldfd or
> newfd parameter, the current working directory shall be used in the
> determination of the file for the respective path parameter.

# RETURN VALUE

> Upon successful completion, the rename() function shall return
> 0. Otherwise, it shall return -1, [CX] errno shall be set to indicate
> the error, and neither the file named by old nor the file named by new
> shall be changed or created.
> 
> [CX] Upon successful completion, the renameat() function shall return
> 0. Otherwise, it shall return -1 and set errno to indicate the error.
> 

# TIMESTAMPS

> Upon successful completion, rename() shall mark for update the last
> data modification and last file status change timestamps of the
> parent directory of each file.

RENAME_TS

# ERRORS

> The rename() [CX]   and renameat()   functions shall fail if:

##    [EACCES] 

>    [CX] A component of either path prefix denies search permission; 

EACCESS:1

>    or one of the directories containing old 

EACCESS:2

> or new denies write permissions;

EACCESS:3

>    or, write permission is required and is denied for a directory pointed
>    to by the old 

EACCESS:4

> or new arguments.

EACCESS:5

##    [EBUSY] multiple procs

>     [CX] The directory named by old or new is currently in use by the
>     system or another process, and the implementation considers this an
>     error.
> 

What is the notion of "in use"? 

$ ./posix -r -cwd /tmp "rename a/. b"
rename a/. b
[EBUSY]

whereas "rename a b" succeeds

##    [EEXIST] or [ENOTEMPTY]

>     [CX] 
>     The link named by new is a directory that is not an empty directory. 

ENOTEMPTY:1 EEXIST:1

FIXME rename d to d, we should retrun successfully according to
description; otherwise this breaks invariant that either the command
succeeds (with a single result), or some errors are raised; probably
the intention of posix here is that the description overrides these
error conditions; reconciled

FIXME any rename call, with a target directory not empty, can raise EEXIST or ENOTEMPTY. So e.g. even if the src is None2, we might raise these errors? seems possible; e.g. rather than ENOENT we might raise ENOTEMPTY 

##    [EINVAL] reconciled

>    [CX] [Option Start] The old pathname names an ancestor directory of the new pathname, or either pathname argument contains a final component that is dot or dot-dot. [Option End]

EINVAL:1

Do these directories have to exist? What is the notion of ancestor directory of a pathname? We need to fix up possibility for 

FIXME for d to none, we may return ENOENT in preference

NB final component cannot be . or ..

FIXME we need to add . and .. check to spec



##    [EISDIR] reconciled

>   [CX]  The new argument points to a directory and the old argument points to a file that is not a directory. 

EISDIR:1


##    [ENOENT] reconciled

>     [CX]  The link named by old does not name an existing file,

ENOENT:1

---

> a component of the path prefix of new does not exist, 

ENOENT:2 

---

> or either old or new points to an empty string. 

ENOENT:3 


##    [ENOTDIR]

>     [CX] A component of either path prefix names an existing
>     file that is neither a directory nor a symbolic link to a directory;

ENOTDIR:1
    
FIXME question about what a path prefix is; presumably all but the last entry (if we ignore trailing slashes?) (even if /a/b/c/, prefix is /a/b ? yes, seems sensible, since these must be directories and this is talking about either path)

FIXME this needs to happen in resolve presumably; resolve should be able to indicate that a component was expected to be a dir but was in fact a file

compared to ENOTDIR:5, this talks about the path prefix; our current resolve_relative does not raise this error eg /a/b/f1.txt/ (there is a check FIXME)

---

    
>     or the old argument names a directory and the new argument names a
>     non-directory file;

ENOTDIR:2

reconciled

---


>    or the old argument contains at least one non-
>     <slash> character and ends with one or more trailing <slash>
>     characters and the last pathname component names an existing file that
>     is neither a directory nor a symbolic link to a directory;

ENOTDIR:3

Example: /a/b/f1.txt/

Non-example: /a/b/dir_link/

A similar clause for new argument is ENOTDIR:5. This clause differs in
the inclusion of "symbolic link to a directory". ENOTDIR:5 states
"names an existing non-directory file", which does not apply to
symbolic links. So ENOTDIR:5 would also not apply to /a/b/dir_link/
(depending on what "names ..." means).

FIXME we need a check for "contains one non-slash char"

e.g. /a/b/f1.txt// ; one non-slash rules out / as an old argument; can talk about last pathname component

note that rename f1.txt/ f2.txt is falls under this condition 

posix test suite doesn't appear to check this; Linux gives ENOTDIR:

$ ./posix -r -cwd /tmp "rename a.txt/ b.txt"
rename a.txt/ b.txt
[ENOTDIR]


---




>   or the old
>    argument names an existing non-directory file and the new argument
>    names a nonexistent file, contains at least one non- <slash>
>    character, and ends with one or more trailing <slash> characters; 

ENOTDIR:4 

FIXME What does it mean "names a nonexistent file" if the new argument
ends in a slash? Does /a/nonexistdir/nonexistfile/ name a nonexistent
file? In this case, ENOTDIR:1 probably takes precedent.

reconciled; e.g. rename /a/b/f1.txt /a/c/nonexist/ 

Note that this is the only ENOTDIR case where new (or old) names a nonexist file

---



>    or
>    the new argument names an existing non-directory file, contains at
>    least one non- <slash> character, and ends with one or more trailing
>    <slash> characters.

ENOTDIR:5

this happens in our resolve function; rename f2.txt f1.txt/ is covered by this case (but not by ENOTDIR:1)

FIXME what covers rename f2.txt/ f1.txt ? not ENOTDIR:5 or ENOTDIR:1; ENOTDIR:3 !

but posix pathname resolution A.4.12 says:

Pathnames that end with one or more trailing <slash> characters must refer to directory paths. Earlier versions of this standard were not specific about the distinction between trailing <slash> characters on files and directories, and both were permitted.

POSIX.1-2008 requires that a pathname with a trailing <slash> be rejected unless it refers to a file that is a directory or to a file that is to be created as a directory.

The rename() function and the mv utility further specify that a trailing <slash> cannot be used on a pathname naming a file that does not exist when used as the last argument to rename() or renameat(), or as the last operand to mv.

So we probably need to look at the full rename specifiction text to figure out what happens with trailing slashes




> [EPERM] or [EACCES]
>     The S_ISVTX flag is set on the directory containing the file referred to by old and the process does not satisfy the criteria specified in XBD Directory Protection with respect to old; or new refers to an existing file, the S_ISVTX flag is set on the directory containing this file, and the process does not satisfy the criteria specified in XBD Directory Protection with respect to this file. 

EPERM:1,2 EACCES:6,7

> [EROFS]
>     [CX] he requested operation requires writing in a directory on a read-only file system.

> [EXDEV]
>     [CX] The links named by new and old are on different file systems and the implementation does not support links between file systems. 


## Local variables
Local Variables:
mode: hi-lock
mode: outline-minor
outline-regexp: "^ *[#]+ "
End:
