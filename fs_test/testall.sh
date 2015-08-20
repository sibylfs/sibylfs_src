#!/bin/sh -x

# a simple test of the various executables we have

########################################################################
# unit tests - array_update, test_parse_print and test_path

export OCAMLRUNPARAM=b

EXIT=0

TMP1=`mktemp`
EXP=example_traces/os_trace1-check.trace.check.expected
TR=example_traces/os_trace1-check.trace
./check.native --root -v $TR >$TMP1
diff -B $TMP1 $EXP
if [ $? -ne 0 ]; then
    echo "check.native failed on check-trace, see $TMP1 and $EXP"
    echo
    EXIT=1
fi


TMP1=`mktemp`
EXP=example_traces/os_trace1-int.trace.interp.expected-check.trace
TR=example_traces/os_trace1-int.trace
./check.native --root -v $TR >$TMP1
diff -B $TMP1 $EXP
if [ $? -ne 0 ]; then
    echo "check failed on interp-trace, see $TMP1 and $EXP"
    echo
    EXIT=1
fi

TMP2=`mktemp`
EXP2=example_traces/os_trace1-int.trace.check.expected
./check.native --root -v $EXP >$TMP2
diff -B $TMP2 $EXP2
if [ $? -ne 0 ]; then
    echo "check failed on check-interp-trace, see $EXP, $TMP2, and $EXP2"
    echo
    EXIT=1
fi

TMP1=`mktemp`
EXP=example_traces/simple_dump-int.trace.interp.expected-check.trace
TR=example_traces/simple_dump-int.trace
./check.native --root -v $TR >$TMP1
diff -B $TMP1 $EXP
if [ $? -ne 0 ]; then
    echo "check failed on $EXP, see $TMP1 and $TR"
    echo
    EXIT=1
fi

TMP1=`mktemp`
TR=$EXP
EXP=example_traces/simple_dump-int.trace.check.expected
./check.native --root -v $TR >$TMP1
diff -B $TMP1 $EXP
if [ $? -ne 0 ]; then
    echo "check failed on $EXP, see $TMP1 and $TR"
    echo
    EXIT=1
fi

TMP1=`mktemp`
TMP2=`mktemp`
EXP1=example_traces/interp_label_fail_dump-result-int.trace.expected
EXP2=example_traces/interp_label_fail_dump-result-int.trace.expected.stderr
TR=example_traces/interp_label_fail_dump-result-int.trace
./check.native --root -v $TR >$TMP1 2>$TMP2
diff -B $TMP1 $EXP1
if [ $? -ne 0 ]; then
    echo "check failed on $EXP1, see $TMP1 and $TR"
    echo
    EXIT=1
fi
diff -B $TMP2 $EXP2
if [ $? -ne 0 ]; then
    echo "check failed on $EXP2, see $TMP2 and $TR"
    echo
    EXIT=1
fi

TMP1=`mktemp`
TMP2=`mktemp`
EXP1=example_traces/interp_state_fail_special-int.trace.expected
EXP2=example_traces/interp_state_fail_special-int.trace.expected.stderr
TR=example_traces/interp_state_fail_special-int.trace
./check.native --root -v $TR >$TMP1 2>$TMP2
diff -B $TMP1 $EXP1
if [ $? -ne 0 ]; then
    echo "check failed on $EXP1, see $TMP1 and $TR"
    echo
    EXIT=1
fi
diff -B $TMP2 $EXP2
if [ $? -ne 0 ]; then
    echo "check failed on $EXP2, see $TMP2 and $TR"
    echo
    EXIT=1
fi

TMP1=`mktemp`
TMP2=`mktemp`
EXP1=example_traces/interp_warn_state_mixed-int.trace.expected
EXP2=example_traces/interp_warn_state_mixed-int.trace.expected.stderr
TR=example_traces/interp_warn_state_mixed-int.trace
./check.native --root -v -arch posix $TR >$TMP1 2>$TMP2
diff -B $TMP1 $EXP1
if [ $? -ne 0 ]; then
    echo "check failed on $EXP1, see $TMP1 and $TR"
    echo
    EXIT=1
fi
diff -B $TMP2 $EXP2
if [ $? -ne 0 ]; then
    echo "check failed on $EXP2, see $TMP2 and $TR"
    echo
    EXIT=1
fi

TMP1=`mktemp`
TMP2=`mktemp`
EXP1=example_traces/interp_warn_dump_failed-int.trace.expected
EXP2=example_traces/interp_warn_dump_failed-int.trace.expected.stderr
TR=example_traces/interp_warn_dump_failed-int.trace
./check.native --root -v -arch posix $TR >$TMP1 2>$TMP2
diff -B $TMP1 $EXP1
if [ $? -ne 0 ]; then
    echo "check failed on $EXP1, see $TMP1 and $TR"
    echo
    EXIT=1
fi
diff -B $TMP2 $EXP2
if [ $? -ne 0 ]; then
    echo "check failed on $EXP2, see $TMP2 and $TR"
    echo
    EXIT=1
fi

TMP1=`mktemp`
TMP2=`mktemp`
EXP1=example_traces/interp_warn_multi_return-int.trace.expected
EXP2=example_traces/interp_warn_multi_return-int.trace.expected.stderr
TR=example_traces/interp_warn_multi_return-int.trace
./check.native --root -v -arch posix --seed 0 $TR >$TMP1 2>$TMP2
diff -B $TMP1 $EXP1
if [ $? -ne 0 ]; then
    echo "check failed on $EXP1, see $TMP1 and $TR"
    echo
    EXIT=1
fi
diff -B $TMP2 $EXP2
if [ $? -ne 0 ]; then
    echo "check failed on $EXP2, see $TMP2 and $TR"
    echo
    EXIT=1
fi

TMP1=`mktemp`
TMP2=`mktemp`
EXP1=example_traces/simple_dump_missing-check.trace.expected
EXP2=example_traces/simple_dump_missing-check.trace.expected.stderr
TR=example_traces/simple_dump_missing-check.trace
./check.native --root -v -arch posix --seed 0 $TR >$TMP1 2>$TMP2
diff -B $TMP1 $EXP1
if [ $? -ne 0 ]; then
    echo "check failed on $EXP1, see $TMP1 and $TR"
    echo
    EXIT=1
fi
diff -B $TMP2 $EXP2
if [ $? -ne 0 ]; then
    echo "check failed on $EXP2, see $TMP2 and $TR"
    echo
    EXIT=1
fi

TMP1=`mktemp`
TMP2=`mktemp`
EXP1=example_traces/simple_dump_unexpected-check.trace.expected
EXP2=example_traces/simple_dump_unexpected-check.trace.expected.stderr
TR=example_traces/simple_dump_unexpected-check.trace
./check.native --root -v -arch posix --seed 0 $TR >$TMP1 2>$TMP2
diff -B $TMP1 $EXP1
if [ $? -ne 0 ]; then
    echo "check failed on $EXP1, see $TMP1 and $TR"
    echo
    EXIT=1
fi
diff -B $TMP2 $EXP2
if [ $? -ne 0 ]; then
    echo "check failed on $EXP2, see $TMP2 and $TR"
    echo
    EXIT=1
fi

TMP1=`mktemp`
TMP2=`mktemp`
EXP1=example_traces/simple_dump_deviant-check.trace.expected
EXP2=example_traces/simple_dump_deviant-check.trace.expected.stderr
TR=example_traces/simple_dump_deviant-check.trace
./check.native --root -v -arch posix --seed 0 $TR >$TMP1 2>$TMP2
diff -B $TMP1 $EXP1
if [ $? -ne 0 ]; then
    echo "check failed on $EXP1, see $TMP1 and $TR"
    echo
    EXIT=1
fi
diff -B $TMP2 $EXP2
if [ $? -ne 0 ]; then
    echo "check failed on $EXP2, see $TMP2 and $TR"
    echo
    EXIT=1
fi

TMP1=`mktemp`
TMP2=`mktemp`
EXP1=example_traces/simple_dump_type_error-check.trace.expected
EXP2=example_traces/simple_dump_type_error-check.trace.expected.stderr
TR=example_traces/simple_dump_type_error-check.trace
./check.native --root -v -arch posix --seed 0 $TR >$TMP1 2>$TMP2
diff -B $TMP1 $EXP1
if [ $? -ne 0 ]; then
    echo "check failed on $EXP1, see $TMP1 and $TR"
    echo
    EXIT=1
fi
diff -B $TMP2 $EXP2
if [ $? -ne 0 ]; then
    echo "check failed on $EXP2, see $TMP2 and $TR"
    echo
    EXIT=1
fi

TMP1=`mktemp`
TMP2=`mktemp`
EXP1=example_traces/interp_warn_dump_failed-check.trace.expected
EXP2=example_traces/interp_warn_dump_failed-check.trace.expected.stderr
TR=example_traces/interp_warn_dump_failed-check.trace
cp example_traces/interp_warn_dump_failed-int.trace.expected $TR
./check.native --root -v -arch posix --seed 0 $TR >$TMP1 2>$TMP2
diff -B $TMP1 $EXP1
if [ $? -ne 0 ]; then
    echo "check failed on $EXP1, see $TMP1 and $TR"
    echo
    EXIT=1
fi
diff -B $TMP2 $EXP2
if [ $? -ne 0 ]; then
    echo "check failed on $EXP2, see $TMP2 and $TR"
    echo
    EXIT=1
fi

TMP1=`mktemp`
TMP2=`mktemp`
EXP1=example_traces/interp_dump_failed-check.trace.expected
EXP2=example_traces/interp_dump_failed-check.trace.expected.stderr
TR=example_traces/interp_dump_failed-check.trace
./check.native --root -v -arch posix --seed 0 $TR >$TMP1 2>$TMP2
diff -B $TMP1 $EXP1
if [ $? -ne 0 ]; then
    echo "check failed on $EXP1, see $TMP1 and $TR"
    echo
    EXIT=1
fi
diff -B $TMP2 $EXP2
if [ $? -ne 0 ]; then
    echo "check failed on $EXP2, see $TMP2 and $TR"
    echo
    EXIT=1
fi

TMP1=`mktemp`
TMP2=`mktemp`
EXP1=example_traces/interp_dump_impossible-check.trace.expected
EXP2=example_traces/interp_dump_impossible-check.trace.expected.stderr
TR=example_traces/interp_dump_impossible-check.trace
./check.native --root -v -arch posix --seed 0 $TR >$TMP1 2>$TMP2
diff -B $TMP1 $EXP1
if [ $? -ne 0 ]; then
    echo "check failed on $EXP1, see $TMP1 and $TR"
    echo
    EXIT=1
fi
diff -B $TMP2 $EXP2
if [ $? -ne 0 ]; then
    echo "check failed on $EXP2, see $TMP2 and $TR"
    echo
    EXIT=1
fi

TMP1=`mktemp`
TMP2=`mktemp`
EXP1=example_traces/unexpected-check.trace.expected
EXP2=example_traces/unexpected-check.trace.expected.stderr
TR=example_traces/unexpected-check.trace
./check.native --root -v -arch posix --seed 0 $TR >$TMP1 2>$TMP2
diff -B $TMP1 $EXP1
if [ $? -ne 0 ]; then
    echo "check failed on $EXP1, see $TMP1 and $TR"
    echo
    EXIT=1
fi
diff -B $TMP2 $EXP2
if [ $? -ne 0 ]; then
    echo "check failed on $EXP2, see $TMP2 and $TR"
    echo
    EXIT=1
fi

TMP1=`mktemp`
TMP2=`mktemp`
EXP1=example_traces/no_errors-check.trace.expected
EXP2=example_traces/no_errors-check.trace.expected.stderr
TR=example_traces/no_errors-check.trace
./check.native --root -v -arch posix --seed 0 $TR >$TMP1 2>$TMP2
diff -B $TMP1 $EXP1
if [ $? -ne 0 ]; then
    echo "check failed on $EXP1, see $TMP1 and $TR"
    echo
    EXIT=1
fi
diff -B $TMP2 $EXP2
if [ $? -ne 0 ]; then
    echo "check failed on $EXP2, see $TMP2 and $TR"
    echo
    EXIT=1
fi

TMP1=`mktemp`
TMP2=`mktemp`
EXP1=example_traces/check_state_fail_special-check.trace.expected
EXP2=example_traces/check_state_fail_special-check.trace.expected.stderr
TR=example_traces/check_state_fail_special-check.trace
./check.native --root -v -arch posix --seed 0 $TR >$TMP1 2>$TMP2
diff -B $TMP1 $EXP1
if [ $? -ne 0 ]; then
    echo "check failed on $EXP1, see $TMP1 and $TR"
    echo
    EXIT=1
fi
diff -B $TMP2 $EXP2
if [ $? -ne 0 ]; then
    echo "check failed on $EXP2, see $TMP2 and $TR"
    echo
    EXIT=1
fi

TMP1=`mktemp`
TMP2=`mktemp`
EXP1=example_traces/interp_state_fail_none-int.trace.expected
EXP2=example_traces/interp_state_fail_none-int.trace.expected.stderr
TR=example_traces/interp_state_fail_none-int.trace
./check.native --root -v -arch posix --seed 0 $TR >$TMP1 2>$TMP2
diff -B $TMP1 $EXP1
if [ $? -ne 0 ]; then
    echo "check failed on $EXP1, see $TMP1 and $TR"
    echo
    EXIT=1
fi
diff -B $TMP2 $EXP2
if [ $? -ne 0 ]; then
    echo "check failed on $EXP2, see $TMP2 and $TR"
    echo
    EXIT=1
fi

TMP1=`mktemp`
TMP2=`mktemp`
EXP1=example_traces/interleave-check.trace.expected
EXP2=example_traces/interleave-check.trace.expected.stderr
TR=example_traces/interleave-check.trace
./check.native --root -v -arch posix --seed 0 $TR >$TMP1 2>$TMP2
diff -B $TMP1 $EXP1
if [ $? -ne 0 ]; then
    echo "check failed on $EXP1, see $TMP1 and $TR"
    echo
    EXIT=1
fi
diff -B $TMP2 $EXP2
if [ $? -ne 0 ]; then
    echo "check failed on $EXP2, see $TMP2 and $TR"
    echo
    EXIT=1
fi

### testpath ###

TMP1=`mktemp`
./testpath.byte >$TMP1
EXP=paths/testpath.output.expected
diff $TMP1 $EXP
if [ $? -ne 0 ]; then
    echo "testpath.byte failed, see $TMP1 and $EXP"
    echo
    EXIT=1
fi

exit $EXIT

