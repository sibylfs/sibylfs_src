/*
  SybilFS test script and trace parser
*/
%{
 open Fs_ast

 module Fs_types = Fs_interface.Fs_spec_intf.Fs_types

 let string_of_pid = function None -> "none" | Some p -> string_of_int p

 let parsing_error start stop msg =
   error (string_of_loc (start,stop) ^ "\n" ^ msg)

 let sec start stop = function
   | T_sec sec -> sec
   | _ -> parsing_error start stop "bad sec field type"

 let nsec start stop = function
   | T_nsec nsec -> nsec
   | _ -> parsing_error start stop "bad nsec field type"

 let st_dev start stop = function
   | St_dev st_dev -> st_dev
   | _ -> parsing_error start stop "bad st_dev field type"

 let st_ino start stop = function
   | St_ino st_ino -> st_ino
   | _ -> parsing_error start stop "bad st_ino field type"

 let st_kind start stop = function
   | St_kind st_kind -> st_kind
   | _ -> parsing_error start stop "bad st_kind field type"

 let st_perm start stop = function
   | St_perm st_perm -> st_perm
   | _ -> parsing_error start stop "bad st_perm field type"

 let st_nlink start stop = function
   | St_nlink st_nlink -> st_nlink
   | _ -> parsing_error start stop "bad st_nlink field type"

 let st_uid start stop = function
   | St_uid st_uid -> st_uid
   | _ -> parsing_error start stop "bad st_uid field type"

 let st_gid start stop = function
   | St_gid st_gid -> st_gid
   | _ -> parsing_error start stop "bad st_gid field type"

 let st_rdev start stop = function
   | St_rdev st_rdev -> st_rdev
   | _ -> parsing_error start stop "bad st_rdev field type"

 let st_size start stop = function
   | St_size st_size -> st_size
   | _ -> parsing_error start stop "bad st_size field type"

 let st_atim start stop = function
   | St_atim st_atim -> st_atim
   | _ -> parsing_error start stop "bad st_atim field type"

 let st_mtim start stop = function
   | St_mtim st_mtim -> st_mtim
   | _ -> parsing_error start stop "bad st_mtim field type"

 let st_ctim start stop = function
   | St_ctim st_ctim -> st_ctim
   | _ -> parsing_error start stop "bad st_ctim field type"
%}

%token LARROW RARROW LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token AT NL BANG DASH SEMI EQ COLON PIPE EOF

%token TYPE SCRIPT TRACE NULL TAU DH FD PID GROUP_ID USER_ID
%token DUMP DUMP_RESULT DUMP_INTERNAL D F L END

%token REWINDDIR PWRITE WRITE STAT LSTAT UNLINK READ ADD_USER_TO_GROUP CLOSE UMASK
%token PREAD TRUNCATE OPEN READLINK RENAME CHOWN OPEN_CLOSE CREATE MKDIR
%token READDIR LINK OPENDIR DESTROY RMDIR LSEEK CHMOD CHDIR CLOSEDIR SYMLINK

%token ETOOMANYREFS EPROTONOSUPPORT ECHILD ETIMEDOUT EWOULDBLOCK EPERM ENOBUFS
%token EMSGSIZE EHOSTUNREACH EFAULT EISCONN EPFNOSUPPORT ESOCKTNOSUPPORT EMFILE
%token ENOENT ENOPROTOOPT ECONNRESET ENOTCONN EPIPE E2BIG ENOTDIR ENOTEMPTY
%token ENOSPC ENETDOWN EBUSY ENOMEM EOPNOTSUPP ESPIPE EIO EAGAIN EAFNOSUPPORT
%token EHOSTDOWN EOVERFLOW EISDIR EXDEV ECONNREFUSED ESRCH ENOLCK EINVAL
%token ENOTSOCK EALREADY EDOM ELOOP EDEADLK EADDRINUSE ENOTTY ECONNABORTED
%token EEXIST ENXIO EACCES EINTR ENETUNREACH EROFS ENAMETOOLONG EDESTADDRREQ
%token ENOEXEC EINPROGRESS EMLINK EBADF EADDRNOTAVAIL EPROTOTYPE ERANGE ENOSYS
%token EFBIG ENETRESET ENODEV ESHUTDOWN EUNKNOWNERR ENFILE

%token O_APPEND O_DIRECTORY O_EXCL O_RSYNC O_RDONLY O_EXEC O_CREAT O_NOCTTY
%token O_NONBLOCK O_TTY_INIT O_WRONLY O_SEARCH O_NOFOLLOW O_SYNC O_DSYNC O_RDWR
%token O_CLOEXEC O_TRUNC

%token SEEK_CUR SEEK_END SEEK_SET SEEK_DATA SEEK_HOLE

%token RV_FILE_PERM RV_BYTES RV_NUM RV_NONE RV_STAT

%token ST_KIND ST_SIZE ST_UID ST_INO ST_GID ST_RDEV ST_NLINK ST_DEV ST_PERM ST_ATIM ST_MTIM ST_CTIM

%token S_IFBLK S_IFCHR S_IFIFO S_IFREG S_IFDIR S_IFLNK S_IFSOCK

%token T_SEC T_NSEC

%token NIL R W X SMALL_S BIG_S SMALL_T BIG_T

%token < string > STRING STRING_NL
%token < string > COMMENT

%token < int > DEC
%token < int > OCT

%start script_or_trace
%type < Fs_ast.script_or_trace > script_or_trace
%%

script_or_trace:
 | NL sot=script_or_trace { add_newline sot }
 | c=COMMENT sot=script_or_trace { add_comment c sot }
 | AT TYPE SCRIPT NL s=script { Script (Type_script :: s) }
 | AT TYPE TRACE NL t=trace { Trace (Type_trace :: t) }
 | error {
   let msg = "Declaring a file type with @type [script|trace] is required" in
   parsing_error $startpos $endpos msg
 }
;

lineno:
 | lineno=DEC COLON { lineno }
;

(* Ignore line numbers for now. *)
script:
 | EOF { [] }
 | NL s=script { Nl_script :: s }
 | l=script_action s=script { (Action l) :: s }
 | lineno=lineno? DUMP p=string_nl s=script { (Dump p) :: s }
 | lineno=lineno? DUMP NL s=script { (Dump "/") :: s }
 | c=COMMENT s=script { (Comment_script c) :: s }
 | lineno? DUMP_INTERNAL  error {
   let msg = "dump-internal not allowed in script" in
   parsing_error $startpos $endpos msg
 }
 | lineno? DUMP_RESULT error {
   let msg = "dump-result not allowed in script" in
   parsing_error $startpos $endpos msg
 }
 | error {
   let msg = "Script syntax error" in
   parsing_error $startpos $endpos msg
 }
;

(* TODO: this shouldn't be special see
   <https://bitbucket.org/tomridge/fs/issue/45> *)
(* Ignore line numbers for now. *)
process_action:
 | lineno=lineno? c=create  { c }
 | lineno=lineno? d=destroy { d }
;

(* Ignore line numbers for now. *)
call_action:
 | lineno=lineno? c=call { c }
;

script_action:
 | c=call_action       { c }
 | a=process_action NL { a }
;

(* Ignore line numbers for now. *)
trace:
 | EOF { [] }
 | NL t=trace { Nl_trace :: t }
 | trans=trace_transition t=trace { trans :: t }
 | lineno? DUMP_INTERNAL  t=trace { Dump_internal :: t }
 | lineno? DUMP_RESULT p=string_nl dump=dump_result DUMP_RESULT NL t=trace {
   (Dump_result (p, dump)) :: t
 }
 | lineno? DUMP_RESULT p=string_nl dump=dump_result DUMP_RESULT NL t=trace {
   (Dump_result (p, dump)) :: t
 }
 | c=COMMENT t=trace { (Comment_trace c) :: t }
 | lineno? DUMP error {
   let msg = "dump not allowed in trace" in
   parsing_error $startpos $endpos msg
 }
 | error {
   let msg = "Trace syntax error" in
   parsing_error $startpos $endpos msg
 }
;

dump_result:
 | END { [] }
 | p=STRING PIPE F PIPE ino=DEC PIPE sz=DEC PIPE sha=STRING PIPE atim=STRING PIPE mtim=STRING PIPE ctim=STRING  NL d=dump_result {
   Dump.(DE_file {file_path=p; file_node=ino; file_size=sz; file_sha=sha; file_atim="";file_mtim="";file_ctim=""}) :: d
 }
 | STRING PIPE F PIPE DEC PIPE DEC PIPE error {
   let msg = "File dump entry SHA must be quoted string" in
   parsing_error $startpos($9) $endpos($9) msg
 }
 | dir_path=STRING PIPE D PIPE dir_node=DEC PIPE atim=STRING PIPE mtim=STRING PIPE ctim=STRING NL des=dump_result {
   Dump.(DE_dir { dir_path; dir_node; dir_atim="";dir_mtim="";dir_ctim=""; }) :: des
 }
 | link_path=STRING PIPE L PIPE link_val=STRING PIPE atim=STRING PIPE mtim=STRING PIPE ctim=STRING NL des=dump_result {
   Dump.(DE_symlink { link_path; link_val; link_atim=""; link_mtim=""; link_ctim="";}) :: des
 }
 | err_path=STRING PIPE BANG PIPE call=STRING PIPE e=errno NL d=dump_result {
   Dump.(DE_error (e,call,err_path)) :: d
 }
 | STRING PIPE L PIPE error {
   let msg = "Link dump entry contents must be quoted string" in
   parsing_error $startpos($5) $endpos($5) msg
 }
 | STRING error {
   let msg = "Dump result entry parse error" in
   parsing_error $startpos $endpos msg
 }
 | error {
   let msg = "Dump entry path must be a quoted string" in
   parsing_error $startpos $endpos msg
 }
;

trace_transition:
 | action=script_action { Action_trace action }
 | TAU NL { Tau }
 | return=trace_return NL { Return return }
;

error_or_value:
 | e=errno { Fs_types.Error e }
 | v=value { Fs_types.Value v }
;

(* Ignore line numbers for now. *)
return_form(RV):
 | lineno=lineno? rv=RV { None, rv }
 | lineno=lineno? p=pid LARROW rv=RV { Some p, rv }
;

multi_return:
 | LBRACE eovs=separated_list(SEMI,error_or_value) RBRACE { eovs }
;

trace_return:
 | eov=return_form(error_or_value) { fst eov, Simple_return (snd eov) }
 | ner=return_form(DASH)           { fst ner, No_error_return }
 | mrv=return_form(multi_return)   { fst mrv, Multiple_return (snd mrv) }
;

gid:
 | GROUP_ID gid=DEC { gid }
 | LPAREN GROUP_ID gid=DEC RPAREN { gid }
;

pid:
 | PID pid=DEC { pid }
;

uid:
 | USER_ID uid=DEC { uid }
 | LPAREN USER_ID uid=DEC RPAREN { uid }
;

create:
 | p=pid RARROW CREATE { Some p, Create }
 | p=pid RARROW CREATE u=uid g=gid { Some p, Create_uid (u,g) }
;

destroy:
 | p=pid RARROW DESTROY { Some p, Destroy }
;

call:
 | p=pid RARROW o=op { Some p, Call o }
 | o=op { None, Call o }
;

op:
 | c=close { c }
 | l=link  { l }
 | m=mkdir { m }
 | o=open_ { o }
 | o=open_close { o }
 | p=pread   { p }
 | p=pwrite  { p }
 | r=read    { r }
 | r=readdir { r }
 | r=rewinddir { r }
 | o=opendir { o }
 | c=closedir  { c }
 | r=readlink { r }
 | r=rename { r }
 | r=rmdir { r }
 | s=stat  { s }
 | s=lstat  { s }
 | s=symlink { s }
 | t=truncate { t }
 | u=unlink { u }
 | w=write { w }
 | a=add_user_to_group { a }
 | c=chown { c }
 | c=chmod { c }
 | c=chdir { c }
 | l=lseek { l }
 | u=umask { u }
;

fd:
 | LPAREN FD fd=DEC RPAREN { fd }
;

close:
 | CLOSE f=fd NL { Close f }
;

string_nl:
 | s=STRING NL { s }
 | s=STRING_NL { s }
;

cstring:
 | s=STRING { CS_Some s }
 | NULL     { CS_Null }
;

cstring_nl:
 | s=cstring NL { s }
 | s=STRING_NL  { CS_Some s }
;

link:
 | LINK s0=cstring s1=cstring_nl { Link (s0,s1) }
;

perm_set(BIG, SMALL):
 | NIL NIL NIL { 0, 0 }
 | NIL NIL BIG   { 0, 1 }
 | NIL NIL SMALL { 1, 1 }
 | NIL NIL X   { 1, 0 }
 | NIL W   NIL { 2, 0 }
 | NIL W   BIG   { 2, 1 }
 | NIL W   SMALL { 3, 1 }
 | NIL W   X   { 3, 0 }
 | R   NIL NIL { 4, 0 }
 | R   NIL BIG   { 4, 1 }
 | R   NIL SMALL { 5, 1 }
 | R   NIL X   { 5, 0 }
 | R   W   NIL { 6, 0 }
 | R   W   BIG   { 6, 1 }
 | R   W   SMALL { 7, 1 }
 | R   W   X   { 7, 0 }
;

perm:
 | p=DEC { Fs_types.File_perm (Int32.of_int p) }
 | p=OCT { Fs_types.File_perm (Int32.of_int p) }
 | user =perm_set(BIG_S,SMALL_S)
   group=perm_set(BIG_S,SMALL_S)
   other=perm_set(BIG_T,SMALL_T)
   {
   let user,  uspecial = user in
   let group, gspecial = group in
   let other, ospecial = other in
   Fs_types.File_perm (Int32.of_int (
     (user  lsl 6) + (uspecial lsl 11)
   + (group lsl 3) + (gspecial lsl 10)
   + other         + (ospecial lsl  9)
   ))
 }
;

timestamp_field:
 | T_SEC EQ sec=DEC SEMI { `T_sec, T_sec sec }
 | T_NSEC EQ nsec=DEC SEMI { `T_nsec, T_nsec (Int64.of_int nsec) }
;


timestamp:
 | LBRACE fields=list(timestamp_field) RBRACE {Fs_types.(Os_timestamp {
    tv_sec  = sec   $startpos $endpos (List.assoc `T_sec   fields);
    tv_nsec = nsec  $startpos $endpos (List.assoc `T_nsec  fields);
    })}
;

mkdir:
 | MKDIR s=cstring p=perm NL { Mkdir (s,p) }
;

open_flag:
 | O_EXEC { Fs_types.O_EXEC }
 | O_RDONLY { Fs_types.O_RDONLY }
 | O_RDWR { Fs_types.O_RDWR }
 | O_SEARCH { Fs_types.O_SEARCH }
 | O_WRONLY { Fs_types.O_WRONLY }
 | O_APPEND { Fs_types.O_APPEND }
 | O_CLOEXEC { Fs_types.O_CLOEXEC }
 | O_CREAT { Fs_types.O_CREAT }
 | O_DIRECTORY { Fs_types.O_DIRECTORY }
 | O_DSYNC { Fs_types.O_DSYNC }
 | O_EXCL { Fs_types.O_EXCL }
 | O_NOCTTY { Fs_types.O_NOCTTY }
 | O_NOFOLLOW { Fs_types.O_NOFOLLOW }
 | O_NONBLOCK { Fs_types.O_NONBLOCK }
 | O_RSYNC { Fs_types.O_RSYNC }
 | O_SYNC { Fs_types.O_SYNC }
 | O_TRUNC { Fs_types.O_TRUNC }
 | O_TTY_INIT { Fs_types.O_TTY_INIT }
;

open_flags:
 | LBRACKET fs=separated_list(SEMI,open_flag) RBRACKET { fs }
;

open_:
 | OPEN s=cstring o=open_flags po=perm? NL { Open (s, o, po) }
;

open_close:
 | OPEN_CLOSE s=cstring o=open_flags po=perm? NL { Open_close (s, o, po) }
;

off:
 | off=DEC { off }
;

size:
 | sz=DEC { sz }
;

pread:
 | PREAD f=fd s=size o=off NL { Pread (f,s,o) }
 | PREAD BANG f=fd s=size o=off NL { Pread_det (f,s,o) }
;

pwrite:
 | PWRITE f=fd s=cstring sz=size o=off NL { Pwrite (f,s,sz,o) }
 | PWRITE BANG f=fd s=cstring sz=size o=off NL { Pwrite_det (f,s,sz,o) }
;

read:
 | READ f=fd s=size NL { Read (f,s) }
 | READ BANG f=fd s=size NL { Read_det (f,s) }
;

dir_handle:
 | LPAREN DH d=DEC RPAREN { d }
;

readdir:
 | READDIR d=dir_handle NL { Readdir d }
;

rewinddir:
 | REWINDDIR d=dir_handle NL { Rewinddir d }
;

opendir:
 | OPENDIR s=cstring_nl { Opendir s }
;

closedir:
 | CLOSEDIR d=dir_handle NL { Closedir d }
;

readlink:
 | READLINK s=cstring_nl { Readlink s }
;

rename:
 | RENAME s0=cstring s1=cstring_nl { Rename (s0, s1) }
;

rmdir:
 | RMDIR s=cstring_nl { Rmdir s }
;

stat:
 | STAT s=cstring_nl { Stat s }
;

lstat:
 | LSTAT s=cstring_nl { Lstat s }
;


symlink:
 | SYMLINK s0=cstring s1=cstring_nl { Symlink (s0,s1) }
;

truncate:
 | TRUNCATE s=cstring sz=size NL { Truncate (s,sz) }
;

unlink:
 | UNLINK s=cstring_nl { Unlink s }
;

write:
 | WRITE f=fd s=cstring sz=size NL { Write (f,s,sz) }
 | WRITE BANG f=fd s=cstring sz=size NL { Write_det (f,s,sz) }
;

add_user_to_group:
 | ADD_USER_TO_GROUP u=uid g=gid NL { Add_user_to_group (u,g) }
;

chown:
 | CHOWN s=cstring u=uid g=gid NL { Chown (s,u,g) }
;

chmod:
 | CHMOD s=cstring p=perm NL { Chmod (s, p) }
;

chdir:
 | CHDIR s=cstring_nl { Chdir s }
;

lseek_command:
 | SEEK_SET { Seek_set }
 | SEEK_CUR { Seek_cur }
 | SEEK_END { Seek_end }
 | SEEK_DATA { Seek_data }
 | SEEK_HOLE { Seek_hole }
;

lseek:
 | LSEEK f=fd o=off c=lseek_command NL { Lseek (f,o,c) }
 | LSEEK f=fd o=off c=DEC NL { Lseek (f,o,Seek_whence c) }
;

umask:
 | UMASK p=perm NL { Umask p }
;

errno:
 | E2BIG { Fs_types.E2BIG }
 | EACCES { Fs_types.EACCES }
 | EAGAIN { Fs_types.EAGAIN }
 | EBADF { Fs_types.EBADF }
 | EBUSY { Fs_types.EBUSY }
 | ECHILD { Fs_types.ECHILD }
 | EDEADLK { Fs_types.EDEADLK }
 | EDOM { Fs_types.EDOM }
 | EEXIST { Fs_types.EEXIST }
 | EFAULT { Fs_types.EFAULT }
 | EFBIG { Fs_types.EFBIG }
 | EINTR { Fs_types.EINTR }
 | EINVAL { Fs_types.EINVAL }
 | EIO { Fs_types.EIO }
 | EISDIR { Fs_types.EISDIR }
 | EMFILE { Fs_types.EMFILE }
 | EMLINK { Fs_types.EMLINK }
 | ENAMETOOLONG { Fs_types.ENAMETOOLONG }
 | ENFILE { Fs_types.ENFILE }
 | ENODEV { Fs_types.ENODEV }
 | ENOENT { Fs_types.ENOENT }
 | ENOEXEC { Fs_types.ENOEXEC }
 | ENOLCK { Fs_types.ENOLCK }
 | ENOMEM { Fs_types.ENOMEM }
 | ENOSPC { Fs_types.ENOSPC }
 | ENOSYS { Fs_types.ENOSYS }
 | ENOTDIR { Fs_types.ENOTDIR }
 | ENOTEMPTY { Fs_types.ENOTEMPTY }
 | ENOTTY { Fs_types.ENOTTY }
 | ENXIO { Fs_types.ENXIO }
 | EPERM { Fs_types.EPERM }
 | EPIPE { Fs_types.EPIPE }
 | ERANGE { Fs_types.ERANGE }
 | EROFS { Fs_types.EROFS }
 | ESPIPE { Fs_types.ESPIPE }
 | ESRCH { Fs_types.ESRCH }
 | EXDEV { Fs_types.EXDEV }
 | EWOULDBLOCK { Fs_types.EWOULDBLOCK }
 | EINPROGRESS { Fs_types.EINPROGRESS }
 | EALREADY { Fs_types.EALREADY }
 | ENOTSOCK { Fs_types.ENOTSOCK }
 | EDESTADDRREQ { Fs_types.EDESTADDRREQ }
 | EMSGSIZE { Fs_types.EMSGSIZE }
 | EPROTOTYPE { Fs_types.EPROTOTYPE }
 | ENOPROTOOPT { Fs_types.ENOPROTOOPT }
 | EPROTONOSUPPORT { Fs_types.EPROTONOSUPPORT }
 | ESOCKTNOSUPPORT { Fs_types.ESOCKTNOSUPPORT }
 | EOPNOTSUPP { Fs_types.EOPNOTSUPP }
 | EPFNOSUPPORT { Fs_types.EPFNOSUPPORT }
 | EAFNOSUPPORT { Fs_types.EAFNOSUPPORT }
 | EADDRINUSE { Fs_types.EADDRINUSE }
 | EADDRNOTAVAIL { Fs_types.EADDRNOTAVAIL }
 | ENETDOWN { Fs_types.ENETDOWN }
 | ENETUNREACH { Fs_types.ENETUNREACH }
 | ENETRESET { Fs_types.ENETRESET }
 | ECONNABORTED { Fs_types.ECONNABORTED }
 | ECONNRESET { Fs_types.ECONNRESET }
 | ENOBUFS { Fs_types.ENOBUFS }
 | EISCONN { Fs_types.EISCONN }
 | ENOTCONN { Fs_types.ENOTCONN }
 | ESHUTDOWN { Fs_types.ESHUTDOWN }
 | ETOOMANYREFS { Fs_types.ETOOMANYREFS }
 | ETIMEDOUT { Fs_types.ETIMEDOUT }
 | ECONNREFUSED { Fs_types.ECONNREFUSED }
 | EHOSTDOWN { Fs_types.EHOSTDOWN }
 | EHOSTUNREACH { Fs_types.EHOSTUNREACH }
 | ELOOP { Fs_types.ELOOP }
 | EOVERFLOW { Fs_types.EOVERFLOW }
 | EUNKNOWNERR LPAREN code=DEC RPAREN { Fs_types.EUNKNOWNERR code }
;

file_kind:
 | S_IFBLK { Fs_types.S_IFBLK }
 | S_IFCHR { Fs_types.S_IFCHR }
 | S_IFIFO { Fs_types.S_IFIFO }
 | S_IFREG { Fs_types.S_IFREG }
 | S_IFDIR { Fs_types.S_IFDIR }
 | S_IFLNK { Fs_types.S_IFLNK }
 | S_IFSOCK { Fs_types.S_IFSOCK }
;

stat_field:
 | ST_DEV EQ dev=DEC SEMI { `St_dev, St_dev dev }
 | ST_INO EQ ino=DEC SEMI { `St_ino, St_ino (Fs_types.Inode ino) }
 | ST_KIND EQ kind=file_kind SEMI { `St_kind, St_kind kind }
 | ST_PERM EQ perm=perm SEMI { `St_perm, St_perm perm }
 | ST_ATIM EQ atim=timestamp SEMI {`St_atim, St_atim atim}
 | ST_MTIM EQ mtim=timestamp SEMI {`St_mtim, St_mtim mtim}
 | ST_CTIM EQ ctim=timestamp SEMI {`St_ctim, St_ctim ctim}
 | ST_NLINK EQ nlink=DEC SEMI { `St_nlink, St_nlink nlink }
 | ST_UID EQ uid=DEC SEMI { `St_uid, St_uid (Fs_types.User_id uid) }
 | ST_GID EQ gid=DEC SEMI { `St_gid, St_gid (Fs_types.Group_id gid) }
 | ST_RDEV EQ rdev=DEC SEMI { `St_rdev, St_rdev rdev }
 | ST_SIZE EQ size=DEC SEMI { `St_size, St_size (Int64.of_int size) }
;

value:
 | RV_NONE { Fs_types.RV_none }
 | RV_NUM LPAREN num=DEC RPAREN { Fs_types.RV_num num }
 | RV_BYTES LPAREN bytes=STRING RPAREN {
   Fs_types.RV_bytes (Abstract_string.of_string bytes)
 }
 | RV_FILE_PERM LPAREN p=perm RPAREN { Fs_types.RV_file_perm p }
 | LBRACKET names=separated_list(SEMI,STRING) RBRACKET {
   Fs_types.RV_names names
 }
 | RV_STAT LBRACE fields=list(stat_field) RBRACE { Fs_types.(RV_os_stats {
   os_st_dev   = st_dev   $startpos $endpos (List.assoc `St_dev   fields);
   os_st_ino   = st_ino   $startpos $endpos (List.assoc `St_ino   fields);
   os_st_kind  = st_kind  $startpos $endpos (List.assoc `St_kind  fields);
   os_st_perm  = st_perm  $startpos $endpos (List.assoc `St_perm  fields);
   os_st_nlink = st_nlink $startpos $endpos (List.assoc `St_nlink fields);
   os_st_uid   = st_uid   $startpos $endpos (List.assoc `St_uid   fields);
   os_st_gid   = st_gid   $startpos $endpos (List.assoc `St_gid   fields);
   os_st_rdev  = st_rdev  $startpos $endpos (List.assoc `St_rdev  fields);
   os_st_size  = st_size  $startpos $endpos (List.assoc `St_size  fields);
   os_st_atime = st_atim  $startpos $endpos (List.assoc `St_atim  fields);
   os_st_mtime = st_mtim  $startpos $endpos (List.assoc `St_mtim  fields);
   os_st_ctime = st_ctim  $startpos $endpos (List.assoc `St_ctim  fields);
 })
 }
;
