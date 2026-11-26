let () =
  Test_vars_arith.test_valid_declarations_and_parity ();
  Test_vars_arith.test_arithmetic_operations_and_parity ();
  Test_ops_matrix.test_all_operations_csv_matrix ();
  Test_control_flow.test_assignment_and_incdec_parity ();
  Test_control_flow.test_print_and_println_parity ();
  Test_control_flow.test_if_else_and_blocks_parity ();
  Test_features.test_const_declarations_parity ();
  Test_features.test_tuples_and_arrays_parity ();
  Test_features.test_loops_parity ();
  Test_io_funs.test_input_and_format_parity ();
  Test_io_funs.test_anonymous_functions_parity ();
  Test_io_funs.test_emit_c_source ();
  Test_io_funs.test_vm_realtime_stdout_before_input ();
  Test_features.test_assert_and_test_runner_parity ();
  Test_features.test_pkg_manifest_and_scaffold ();
  Test_ops_matrix.test_linter_unused_variable ();
  Test_ops_matrix.test_debugger_trace ();
  Test_errors.test_syntax_and_semantic_errors ();
  Test_url_import.test_url_import_parity ();
  Test_lib_pkg.test_lib_project_and_pack_parity ();
  print_endline "All Sigma variable & arithmetic tests passed!"
