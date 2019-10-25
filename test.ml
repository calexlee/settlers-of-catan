open OUnit2

let board_tests = 
  [

  ]

let suite =
  "test suite for Final Project"  >::: List.flatten [
    board_tests;
  ]

let _ = run_test_tt_main suite