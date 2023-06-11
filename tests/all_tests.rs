mod infra;

// Your tests go here!
success_tests! {
    {
        name: fact,
        file: "fact.snek",
        input: "10",
        expected: "3628800",
    },
    {
        name: even_odd_1,
        file: "even_odd.snek",
        input: "10",
        expected: "10\ntrue\ntrue",
    },
    {
        name: even_odd_2,
        file: "even_odd.snek",
        input: "9",
        expected: "9\nfalse\nfalse",
    },
    {
        name: false_val,
        file: "false_val.snek",
        expected: "false",
    },
    {
        name: input_compare_1,
        file: "input_compare.snek",
        input: "2",
        expected: "false",
    },
    {
        name: input_compare_2,
        file: "input_compare.snek",
        input: "10",
        expected: "true",
    },
    {
        name: input_num,
        file: "input.snek",
        input: "4611686018427387903",
        expected: "4611686018427387903",
    },
    {
        name: input_true,
        file: "input.snek",
        input: "true",
        expected: "true",
    },
    {
        name: input_false,
        file: "input.snek",
        input: "false",
        expected: "false",
    },
    {
        name: number,
        file: "number.snek",
        expected: "233",
    },
    {
        name: equal,
        file: "equal.snek",
        expected: "true",
    },
    {
        name: equal1,
        file: "equal1.snek",
        expected: "false",
    },
    {
        name: equal2,
        file: "equal2.snek",
        expected: "true",
    },
    {
        name: equal3,
        file: "equal3.snek",
        expected: "false",
    },
    {
        name: isnum_true,
        file: "isnum_true.snek",
        expected: "true",
    },
    {
        name: isnum_false,
        file: "isnum_false.snek",
        expected: "false",
    },
    {
        name: isbool_true,
        file: "isbool_true.snek",
        expected: "true",
    },
    {
        name: isbool_false,
        file: "isbool_false.snek",
        expected: "false",
    },
    {
        name: greater_true,
        file: "greater_true.snek",
        expected: "true",
    },
    {
        name: greater_false,
        file: "greater_false.snek",
        expected: "false",
    },
    {
        name: times,
        file: "times.snek",
        expected: "60",
    },
    {
        name: times_negative,
        file: "times_negative.snek",
        expected: "58596",
    },
    {
        name: multiple_print,
        file: "multiple_print.snek",
        input: "3",
        expected: "0\n1\n2\n3",
    },
    {
        name: call_10,
        file: "call.snek",
        input: "10",
        expected: "10\n22\n72\n20",
    },
    {
        name: recursive_call_3,
        file: "recursive_call.snek",
        input: "3",
        expected: "3\n2\n1\n0",
    },
    {
        name: recursive_fact_10,
        file: "recursive_fact.snek",
        input: "10",
        expected: "3628800",
    },
    {
        name: many_args,
        file: "many_args.snek",
        expected: "10",
    },
    {
        name: fun_no_arg,
        file: "fun_no_arg.snek",
        expected: "10086",
    },
    {
        name: tuple_simple,
        file: "tuple_simple.snek",
        expected: "(tuple 1 (tuple 2 3) 4 (tuple 5 6 7 8))",
    },
    {
        name: tuple_binding,
        file: "tuple_binding.snek",
        expected: "(tuple (tuple (tuple 1 2 3) (tuple 1 2 3)) (tuple (tuple 1 2 3) (tuple 1 2 3)) (tuple (tuple 1 2 3) (tuple 1 2 3)))",
    },
    {
        name: tuple_range_3,
        file: "tuple_range.snek",
        input: "3",
        expected: "(tuple 3 (tuple 2 (tuple 1 0)))",
    },
    {
        name: index_print,
        file: "index_print.snek",
        expected: "(tuple 0 1 2 3)\n0\n1\n2\n3\n(tuple 0 1 2 3)",
    },
    {
        name: tuple_points,
        file: "tuple_points.snek",
        expected: "(tuple 1 2)\n(tuple 3 4)\n(tuple 4 6)\n(tuple 4 6)",
    },
    {
        name: tuple_bst,
        file: "tuple_bst.snek",
        expected: "true\ntrue\nfalse\nfalse\n(tuple (tuple nil -2 (tuple nil -1 nil)) 0 (tuple nil 1 (tuple nil 2 nil)))",
    },
    {
        name: ext_tuple_set,
        file: "ext_tuple_set.snek",
        expected: "(tuple 1 2 3)\n(tuple 1 (tuple 4 5) 3)\n(tuple 1 (tuple (tuple 1 2) 5) 3)",
    },
    {
        name: ext_cycle_print1,
        file: "ext_cycle_print1.snek",
        expected: "(tuple (...) 2)",
    },
    {
        name: ext_cycle_print2,
        file: "ext_cycle_print2.snek",
        expected: "(tuple (tuple (...) 2) (tuple (...) 2))",
    },
    {
        name: ext_cycle_print3,
        file: "ext_cycle_print3.snek",
        expected: "(tuple 0 (tuple 1 2 3 4) (tuple -1 -2 -3))\n(tuple (...) (tuple (...) 2 3 4) (tuple -1 -2 -3))\n(tuple true (tuple false 2 3 4) (tuple -1 -2 -3))",
    },
    {
        name: ext_equal,
        file: "ext_equal.snek",
        expected: "true\ntrue\nfalse\ntrue\nfalse\nfalse",
    },
    {
        name: ext_cycle_equal1,
        file: "ext_cycle_equal1.snek",
        expected: "(tuple (...) 2)\n(tuple (...) 2)\ntrue",
    },
    {
        name: ext_cycle_equal2,
        file: "ext_cycle_equal2.snek",
        expected: "(tuple (...) 2)\n(tuple (tuple (...) 2) 2)\ntrue",
    },
    {
        name: ext_cycle_equal3,
        file: "ext_cycle_equal3.snek",
        expected: "(tuple (tuple (...) 1) 2)\n(tuple (tuple (...) 1) 2)\nfalse",
    },
}

runtime_error_tests! {
    {
        name: invalid_argument,
        file: "invalid_argument.snek",
        expected: "invalid argument",
    },
    {
        name: input_compare_3,
        file: "input_compare.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: equal_invalid,
        file: "equal_invalid.snek",
        expected: "invalid argument",
    },
    {
        name: add1_invalid,
        file: "add1_invalid.snek",
        expected: "invalid argument",
    },
    {
        name: input_invalid,
        file: "input.snek",
        input: "4611686018427387904",
        expected: "Invalid",
    },
    {
        name: input_invalid1,
        file: "input.snek",
        input: "-4611686018427387905",
        expected: "Invalid",
    },
    {
        name: add1_of,
        file: "add1_of.snek",
        expected: "overflow",
    },
    {
        name: sub1_of,
        file: "sub1_of.snek",
        expected: "overflow",
    },
    {
        name: plus_of,
        file: "plus_of.snek",
        expected: "overflow",
    },
    {
        name: plus_of1,
        file: "plus_of1.snek",
        expected: "overflow",
    },
    {
        name: minus_of,
        file: "minus_of.snek",
        expected: "overflow",
    },
    {
        name: times_of,
        file: "times_of.snek",
        expected: "overflow",
    },
    {
        name: index_not_tuple_bool,
        file: "index_not_tuple.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: index_not_tuple_num,
        file: "index_not_tuple.snek",
        input: "10",
        expected: "invalid argument",
    },
    {
        name: index_not_num,
        file: "index_not_num.snek",
        expected: "invalid argument",
    },
    {
        name: index_out_of_bound_3,
        file: "index_out_of_bound.snek",
        input: "3",
        expected: "index out of bound",
    },
    {
        name: index_out_of_bound_negative,
        file: "index_out_of_bound.snek",
        input: "-1",
        expected: "index out of bound",
    },
}

static_error_tests! {
    {
        name: duplicate_params,
        file: "duplicate_params.snek",
        expected: "",
    },
    {
        name: duplicate_params1,
        file: "duplicate_params1.snek",
        expected: "",
    },
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    },
    {
        name: outside_break,
        file: "outside_break.snek",
        expected: "break",
    },
    {
        name: call_wrong_args,
        file: "call_wrong_args.snek",
        expected: "Invalid",
    },
    {
        name: fun_undefined,
        file: "fun_undefined.snek",
        expected: "Invalid",
    },
    {
        name: duplicate_func,
        file: "duplicate_func.snek",
        expected: "Invalid",
    },
    {
        name: duplicate_func1,
        file: "duplicate_func1.snek",
        expected: "Invalid",
    },
    {
        name: fun_reserved_arg,
        file: "fun_reserved_arg.snek",
        expected: "Invalid",
    },
    {
        name: fun_input_fail,
        file: "fun_input_fail.snek",
        expected: "Invalid",
    },
    {
        name: extra_def,
        file: "extra_def.snek",
        expected: "Invalid",
    },
    {
        name: index_wrong_args,
        file: "index_wrong_args.snek",
        expected: "Invalid",
    },
}
