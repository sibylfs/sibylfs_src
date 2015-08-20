<link href="http://kevinburke.bitbucket.org/markdowncss/markdown.css" rel="stylesheet"></link>

# Traces

Traces are sequences of commands that are used for testing the file-system 
specification. There are 2 different types of traces:

- CHECK-traces (file-suffix `-check.trace`)
- INTERP-traces (file-suffix `-int.trace`)

All traces contain sequences of os-level commands.

## check traces

CHECK-traces contain commands as well as `Tau` transitions and the
expected return value. A simple example is the trace
`example_traces/trace1-check.trace`:

    Pid 1 -> mkdir /tmp_dir_1490/ 0o777
    Tau
    Pid 1 <- RV_none
    
    # Pid 1 can be omitted 
    mkdir /tmp_dir_1490/d1 0o777
    Tau
    RV_none
    
    # Other processes have to be always mentioned 
    create Pid 2 User_id 0 Group_id 0
    Pid 2 -> mkdir /tmp_dir_1490/d1/empty_dir1 0o777
    Tau
    Pid 2 <- RV_none
    
    mkdir /tmp_dir_1490/d1/empty_dir2 0o777
    Tau
    RV_none
    
    Pid 1 -> mkdir /tmp_dir_1490/d1/nonempty_dir1 0o777
    Tau
    Pid 1 <- RV_none
    
    open /tmp_dir_1490/d1/nonempty_dir1/f1.txt [O_CREAT;O_RDWR] 0o666
    Tau
    RV_num(3)
    
    open /tmp_dir_1490/d1/nonempty_dir1/f1.txt [O_RDWR]
    Tau
    RV_num(4)
    
	write (FD 4) "01234" 5
	Tau
	{RV_num(5) ; RV_num(3); RV_num(1);RV_num(2)}

	Pid 1 -> stat /tmp_dir_1490/d1/nonempty_dir1/f1.txt
    Tau
    Pid 1 <- -
    
    # stat a non-existing file
    Pid 1 -> stat /tmp_dir_1490/d1/nosuchdir/non.txt
    Tau
    Pid 1 <- ENOENT


There are a few interesting things to notice about this example trace

- trace-files can contain empty lines and comments
- `Pid 1` is the default process-id and can be ommited
- return values can be both values and errors
- if one just cares that a command does not fail, one can write `Pid x <- -`
- multiple results can be provided in a return statement, if one wants
  to state that all these must be allowed by the spec
  

## INTERP traces

INTERP traces contain only the commands. Tau-transitions and expected
results are omitted.  Since no expected results are stored, INTERP
traces are interpreted, not checked. The commands are executed one after
another and after each command the result is printed.

The INTERP-trace corresponding to our running example is:

    Pid 1 -> mkdir /tmp_dir_1490/ 0o777
    
    # Pid 1 can be omitted 
    mkdir /tmp_dir_1490/d1 0o777
    
    # Other processes have to be always mentioned 
    create Pid 2 (User_id 0) (Group_id 0)
    Pid 2 -> mkdir /tmp_dir_1490/d1/empty_dir1 0o777
    
    mkdir /tmp_dir_1490/d1/empty_dir2 0o777
    
    Pid 1 -> mkdir /tmp_dir_1490/d1/nonempty_dir1 0o777
    
    open /tmp_dir_1490/d1/nonempty_dir1/f1.txt [O_CREAT;O_RDWR] 0o666
    
    open /tmp_dir_1490/d1/nonempty_dir1/f1.txt [O_RDWR]
    
    write (FD 4) "01234" 5
    
    Pid 1 -> stat /tmp_dir_1490/d1/nonempty_dir1/f1.txt
    
    # stat a non-existing file
    Pid 1 -> stat /tmp_dir_1490/d1/nosuchdir/non.txt/



# Interpreting and checking traces

Traces are used for testing. CHECK-traces are checked against the
specification. INTERP traces interpreted.

## Checking traces

CHECK-traces contain OS-level commands as well as `Tau` transitions
and observed results. They are intended to record one concrete,
observed execution. Since some OS-level commands are
non-deterministic, for the same sequence of commands there may be many
observed return values and therefore many traces.

CHECK-traces are checked against the specification, i.e. it is
checked whether the observed behaviour is allowed by the
specification. One starts with a singleton set of states, containing
just the initial state. Then each label is executed in the
specification. A label is executed for all states in the current state
set and the union of of all possible result states is considered for
the next label. If during this checking the state-set becomes empty or
a special state is encountered, the trace is rejected by the
specification; otherwise it is accepted.

While checking traces, state explosion might cause trouble. `Tau`
transitions do the real work of executing some os-level command. For
non-deterministic commands, they lead to an increase in the number of
states. The command *write* can for example write either the full
amount of data or just a smaller junk of arbitrary size. If one is
unlucky, such non-deterministic commands can lead to a state explosion
that makes checking a trace very slow or even infeasible. However,
this is usually not a problem, because observed results, i.e. return
labels collapse the set of states.  In the example of the *write*, the
execution of the `Tau` transition results in many possible states, but
*write* also returns the number of bytes written. Therefore, when
interpreting a return label for *write*, only one of these many states
remains.

There are however, two problems. OS-traces can execute commands in multiple
threads. Therefore, it is possible to get a state explosion by executing
multiple non-deterministic commands in multiple threads, before
executing the corresponding return statements. Another possibility are
return-statements that allow multiple return values. If a return label
accepts e.g. all non-error values, it does not clean the state set any more.
Both these possible causes of state-explosion are demonstrated by the trace
``adhoc_tests/adhoc_state_explosion-os.trace``.


## Interpreting traces

INTERP traces contain no `Tau` transitions and no observed return
values. They only contain OS-commands. The intention is to execute
these commands and observe the results.  This is completely different
than checking a trace with respect to non-determinism.  While checking
traces, we consider *all* allowed executions. While interpreting, we
pick only one.

For interpreting, we again start with a single initial state. We then
execute a label and get after an implicit `Tau` transition an set of
possible result values. Now, one of these possible results is
randomly picked to continue execution with. So, in contrast to
checking traces, no state explosion is possible.

Due to non-deterministic commands, a sequence of commands has a tree of
possible results. Non-deterministic results limit possible results in the future.
Consider for example the following sequence

    open file.txt [O_RDWR;O_CREAT;O_TRUNC] 0o666
	write (FD 3) "0123456789" 10
	pread (FD 3) 10 0
	close (FD 3)

On there own, the `write` might nonderministically write between 0 and 10
bytes and the `pread` might read between 0 and 10 bytes. In sequence,
at most as many bytes as were written can be read though.
A trace is unable to capture this tree-structure. It can only encode
a path through this tree. Therefore, we really need to non-deterministically
choose a state to continue with after each command.

The intention of interpreting traces is making observation of
real-world behaviour. An INTERP-trace represents an interesting
sequence of commands. It is executed against some implementation one
wants to test.  The result can be stored in form of of a FS-trace,
which is checked against the specification. Since multiple
interpretation runs can produce different results, it might even make
sense to convert one INTERP-trace to multiple FS-traces.


# Tools

There are mainly 2 tools: `check` and `posix`. They are both
implemented using the same code-base (``checkLib``). The can both
parse trace-files and process them in various ways. The difference is
that `check` uses our POSIX-specification, whereas `posix` uses the
file-system implementation of the underlying operating system. Both
tools can be used with OS, FS and INTERP traces. Moreover, they can
be used to just check the syntax of trace-files and to convert between
OS- and FS-traces and store the result of interpreting an INTERP trace
as an FS-trace. Additionally, ``posix`` is able to
interpret a trace using the real file-system and then check the
resulting trace against the specification.

The most common use case is probably

	posix2.native -c example_traces/interp_trace1-int.trace

This interprets the trace ``interp_trace1-int.trace`` using the real
file-system and checks the result against the specification.

However, let's have a look at simpler uses:

	check.native example_traces/trace1-check.trace
	posix2.native example_traces/trace1-check.trace
	
These calls run check the trace against the specification and the real
file-system. The tools automatically figure out, which type of trace
is stored in the given file and behaves accordingly.

In general, `check` and `posix` try to provide the same command line
options. However, there are some differences. `check` is able to
simulate different architectures and therefore supports an optional
`-arch` command line argument.  `posix` uses a root-directory on the
real file-system to work in. One can either specify an existing one
via the command line argument `-r` or let `posix` create a temporary
on in the temp-directory. By default this temp-directory is set to the
system default (e.g. `/tmp` on Linux).  It can be changed via the
command line argument `-tmp`. Moreover, ``posix`` supports the
argument ``-c`` to interpret a trace and then check it against the
specification. Otherwise the tools have very similar. Notice however,
that if using multiple processes and multiple users, `posix` requires
superuser privileges. Moreover, it is not only going to write into
it's root dir, but might also create new system users and
groups. Therefore, it is highly recommended to run OS-traces that use
these commands only in a virtual environment.

Both tools are usually pretty quiet. If you want more run-time output, use
the command line option `-v`. To get a list of all available command line
options `--help` can be used.

## Converting traces
The command line option `-o` is used to convert trace files into different formats.
The input and output format are automatically determined by the suffix of the given
file-names. A simple example call is:

	./check.native -o dummy-int.trace example_traces/trace1-check.trace

This is in particular interesting for storing the results of interpreting a trace:

	./posix.native -o result-check.trace example_traces/interp_trace1-int.trace

## Dry-runs
If you want to make sure, no model is executed, no files written to disk no users created etc, there
is the option `--dry-run` or short `-n`. It's main purpose is to just check, whether 
a trace file is syntactically correct.


# Usage scenario
I imagine that one usually starts with a sequence of commands that is interesting to test.
This can easily be written in a INTERP-trace file. Let's call this file `example-int.trace`.
One can then run `check` on this file to see whether the syntax is correct.

	./check.native -n example-int.trace

After one got the syntax right, one might want to see whether the trace behaves as expected.

	./check.native example-int.trace
	./posix.native example-int.trace

After checking the results manually and fixing the trace to behave as excepted, automatic checks
might become desirable for the future. For this purpose, one can convert the INTERP trace into 
a FS trace using posix. This essentially stores the computed results in a format that `posix` and `check` 
can understand.

	./posix.native example-int.trace -o example-check.trace
	
Now, the expected results are stored in a file `check` can understand and we can test against it.

	./check.native -v example-check.trace
	
However, one might also want to skip storing the fs-trace. One can use

	./posix.native -c example-int.trace
	
to interpret the trace and check it against the specification in one go. 
	


