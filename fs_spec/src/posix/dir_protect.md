4.2 Directory Protection
========================

If a directory is writable and the mode bit S_ISVTX is set on the directory, a process may remove or rename files within that directory only if one or more of the following is true:

- The effective user ID of the process is the same as that of the owner ID of the file.

- The effective user ID of the process is the same as that of the owner ID of the directory.

- The process has appropriate privileges.

- Optionally, the file is writable by the process. Whether or not files that are writable by the process can be removed or renamed is implementation-defined.

If the S_ISVTX bit is set on a non-directory file, the behavior is unspecified.
