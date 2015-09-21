
{
  open Printf
  open Lexing
  open Fs_lr_parser

  type lex_context = Token | Perm_symbol

  type lex_state = {
    mutable context : lex_context;
  }

  let fresh_lex_state () = { context = Token }

  let lexing_error lexbuf msg =
    let loc = (lexeme_start_p lexbuf, lexeme_end_p lexbuf) in
    Fs_ast.error (Fs_ast.string_of_loc loc ^ "\n" ^ msg)
}

let digit = ['0'-'9']
let newline = '\r'? '\n'
let blank = [' ''\t']

rule token lex_state = parse
  | "<- " { LARROW }
  | "->"  { RARROW }
  | '('   { LPAREN }
  | ')'   { RPAREN }
  | '['   { LBRACKET }
  | ']'   { RBRACKET }
  | '{'   { LBRACE }
  | '}'   { RBRACE }
  | '<'   { lex_state.context <- Perm_symbol; perm_symbol lex_state lexbuf }
  | '"'   { STRING (string (Buffer.create 200) lexbuf) }
  | '/'   {
    let buf = Buffer.create 200 in
    Buffer.add_char buf '/';
    let nl_term, str = path buf lexbuf in
    if nl_term then STRING_NL str else STRING str
  }
  | '#'   { COMMENT (comment (Buffer.create 200) lexbuf) }
  | '@'   { AT }
  | '!'   { BANG }
  | ':'   { COLON }
  | '|'   { PIPE }
  | ';'   { SEMI }
  | '='   { EQ }
  | '-'   { DASH }
  | eof   { EOF }
  | newline { new_line lexbuf; NL }
  | blank { token lex_state lexbuf }

  | "0o" digit+ {
    try Scanf.sscanf (Lexing.lexeme lexbuf) "0o%o" (fun x -> OCT x)
    with Scanf.Scan_failure msg -> lexing_error lexbuf msg
  }
  | '-'? digit+ {
    try Scanf.sscanf (Lexing.lexeme lexbuf) "%d" (fun x -> DEC x)
    with Scanf.Scan_failure msg -> lexing_error lexbuf msg
  }

  | "type"          { TYPE }
  | "script"        { SCRIPT }
  | "trace"         { TRACE }
  | "null"          { NULL }
  | "Tau"           { TAU }
  | "DH"            { DH }
  | "FD"            { FD }
  | "Pid"           { PID }
  | "Group_id"      { GROUP_ID }
  | "User_id"       { USER_ID }
  | "dump"          { DUMP }
  | "dump-result"   { DUMP_RESULT }
  | "dump-internal" { DUMP_INTERNAL }
  | 'D'             { D }
  | 'F'             { F }
  | 'L'             { L }
  | "end"           { END }

  | "rewinddir" { REWINDDIR }
  | "pwrite" { PWRITE }
  | "write" { WRITE }
  | "stat" { STAT }
  | "lstat" { LSTAT }
  | "unlink" { UNLINK }
  | "read" { READ }
  | "add_user_to_group" { ADD_USER_TO_GROUP }
  | "close" { CLOSE }
  | "umask" { UMASK }
  | "pread" { PREAD }
  | "truncate" { TRUNCATE }
  | "open" { OPEN }
  | "readlink" { READLINK }
  | "rename" { RENAME }
  | "chown" { CHOWN }
  | "open_close" { OPEN_CLOSE }
  | "create" { CREATE }
  | "mkdir" { MKDIR }
  | "readdir" { READDIR }
  | "link" { LINK }
  | "opendir" { OPENDIR }
  | "destroy" { DESTROY }
  | "rmdir" { RMDIR }
  | "lseek" { LSEEK }
  | "chmod" { CHMOD }
  | "chdir" { CHDIR }
  | "closedir" { CLOSEDIR }
  | "symlink" { SYMLINK }

  | "ETOOMANYREFS" { ETOOMANYREFS }
  | "EPROTONOSUPPORT" { EPROTONOSUPPORT }
  | "ECHILD" { ECHILD }
  | "ETIMEDOUT" { ETIMEDOUT }
  | "EWOULDBLOCK" { EWOULDBLOCK }
  | "EPERM" { EPERM }
  | "ENOBUFS" { ENOBUFS }
  | "EMSGSIZE" { EMSGSIZE }
  | "EHOSTUNREACH" { EHOSTUNREACH }
  | "EFAULT" { EFAULT }
  | "EISCONN" { EISCONN }
  | "EPFNOSUPPORT" { EPFNOSUPPORT }
  | "ESOCKTNOSUPPORT" { ESOCKTNOSUPPORT }
  | "EMFILE" { EMFILE }
  | "ENOENT" { ENOENT }
  | "ENOPROTOOPT" { ENOPROTOOPT }
  | "ECONNRESET" { ECONNRESET }
  | "ENOTCONN" { ENOTCONN }
  | "EPIPE" { EPIPE }
  | "E2BIG" { E2BIG }
  | "ENOTDIR" { ENOTDIR }
  | "ENOTEMPTY" { ENOTEMPTY }
  | "ENOSPC" { ENOSPC }
  | "ENETDOWN" { ENETDOWN }
  | "EBUSY" { EBUSY }
  | "ENOMEM" { ENOMEM }
  | "EOPNOTSUPP" { EOPNOTSUPP }
  | "ESPIPE" { ESPIPE }
  | "EIO" { EIO }
  | "EAGAIN" { EAGAIN }
  | "EAFNOSUPPORT" { EAFNOSUPPORT }
  | "EHOSTDOWN" { EHOSTDOWN }
  | "EOVERFLOW" { EOVERFLOW }
  | "EISDIR" { EISDIR }
  | "EXDEV" { EXDEV }
  | "ECONNREFUSED" { ECONNREFUSED }
  | "ESRCH" { ESRCH }
  | "ENOLCK" { ENOLCK }
  | "EINVAL" { EINVAL }
  | "ENOTSOCK" { ENOTSOCK }
  | "EALREADY" { EALREADY }
  | "EDOM" { EDOM }
  | "ELOOP" { ELOOP }
  | "EDEADLK" { EDEADLK }
  | "EADDRINUSE" { EADDRINUSE }
  | "ENOTTY" { ENOTTY }
  | "ECONNABORTED" { ECONNABORTED }
  | "EEXIST" { EEXIST }
  | "ENXIO" { ENXIO }
  | "EACCES" { EACCES }
  | "EINTR" { EINTR }
  | "ENETUNREACH" { ENETUNREACH }
  | "EROFS" { EROFS }
  | "ENAMETOOLONG" { ENAMETOOLONG }
  | "EDESTADDRREQ" { EDESTADDRREQ }
  | "ENOEXEC" { ENOEXEC }
  | "EINPROGRESS" { EINPROGRESS }
  | "EMLINK" { EMLINK }
  | "EBADF" { EBADF }
  | "EADDRNOTAVAIL" { EADDRNOTAVAIL }
  | "EPROTOTYPE" { EPROTOTYPE }
  | "ERANGE" { ERANGE }
  | "ENOSYS" { ENOSYS }
  | "EFBIG" { EFBIG }
  | "ENETRESET" { ENETRESET }
  | "ENODEV" { ENODEV }
  | "ESHUTDOWN" { ESHUTDOWN }
  | "EUNKNOWNERR" { EUNKNOWNERR }
  | "ENFILE" { ENFILE }

  | "O_APPEND" { O_APPEND }
  | "O_DIRECTORY" { O_DIRECTORY }
  | "O_EXCL" { O_EXCL }
  | "O_RSYNC" { O_RSYNC }
  | "O_RDONLY" { O_RDONLY }
  | "O_EXEC" { O_EXEC }
  | "O_CREAT" { O_CREAT }
  | "O_NOCTTY" { O_NOCTTY }
  | "O_NONBLOCK" { O_NONBLOCK }
  | "O_TTY_INIT" { O_TTY_INIT }
  | "O_WRONLY" { O_WRONLY }
  | "O_SEARCH" { O_SEARCH }
  | "O_NOFOLLOW" { O_NOFOLLOW }
  | "O_SYNC" { O_SYNC }
  | "O_DSYNC" { O_DSYNC }
  | "O_RDWR" { O_RDWR }
  | "O_CLOEXEC" { O_CLOEXEC }
  | "O_TRUNC" { O_TRUNC }

  | "SEEK_CUR"  { SEEK_CUR }
  | "SEEK_END"  { SEEK_END }
  | "SEEK_SET"  { SEEK_SET }
  | "SEEK_DATA" { SEEK_DATA }
  | "SEEK_HOLE" { SEEK_HOLE }

  | "RV_file_perm" { RV_FILE_PERM }
  | "RV_bytes"     { RV_BYTES }
  | "RV_num"       { RV_NUM }
  | "RV_none"      { RV_NONE }
  | "RV_stat"      { RV_STAT }

  | "st_kind"  { ST_KIND }
  | "st_size"  { ST_SIZE }
  | "st_uid"   { ST_UID }
  | "st_ino"   { ST_INO }
  | "st_gid"   { ST_GID }
  | "st_rdev"  { ST_RDEV }
  | "st_nlink" { ST_NLINK }
  | "st_dev"   { ST_DEV }
  | "st_perm"  { ST_PERM }
  | "st_atim"  { ST_ATIM }
  | "st_mtim"  { ST_MTIM }
  | "st_ctim"  { ST_CTIM }

  | "tv_sec"   {T_SEC}
  | "tv_nsec"  {T_NSEC}

  | "S_IFBLK"  { S_IFBLK }
  | "S_IFCHR"  { S_IFCHR }
  | "S_IFIFO"  { S_IFIFO }
  | "S_IFREG"  { S_IFREG }
  | "S_IFDIR"  { S_IFDIR }
  | "S_IFLNK"  { S_IFLNK }
  | "S_IFSOCK" { S_IFSOCK }

  | _ as c { lexing_error lexbuf (sprintf "Unexpected character %c" c) }

and perm_symbol lex_state = parse
  | '-' | '_' { NIL }
  | 'r' { R }
  | 'w' { W }
  | 'x' { X }
  | 's' { SMALL_S }
  | 'S' { BIG_S }
  | 't' { SMALL_T }
  | 'T' { BIG_T }
  | '>' { lex_state.context <- Token; token lex_state lexbuf }

(* TODO: newlines? *)
and string buf = parse
  | "\\\"" { Buffer.add_char buf '\\';
             Buffer.add_char buf '\"';
             string buf lexbuf }
  | '"'    {
    try Scanf.unescaped (Buffer.contents buf)
    with Scanf.Scan_failure msg -> lexing_error lexbuf msg
  }
  | _ as c { Buffer.add_char buf c; string buf lexbuf }
  | eof    { lexing_error lexbuf "Unterminated string" }

and path buf = parse
  | blank   { false, Buffer.contents buf }
  | newline { new_line lexbuf; true, Buffer.contents buf }
  | _ as c  { Buffer.add_char buf c; path buf lexbuf }

and comment buf = parse
  | newline { new_line lexbuf; Buffer.contents buf }
  | _ as c  { Buffer.add_char buf c; comment buf lexbuf }
  | eof     { Buffer.contents buf }

{
  let token lex_state lexbuf = match lex_state.context with
    | Token -> token lex_state lexbuf
    | Perm_symbol -> perm_symbol lex_state lexbuf
}
