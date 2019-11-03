open OUnit2
open Board

let remove_index_test
    test start index lst expected_output : test = test >:: (fun _ -> 
    assert_equal expected_output (Board.remove_index start index lst)) 

let get_index_test
    test start index lst expected_output : test = test >:: (fun _ -> 
    assert_equal expected_output (Board.get_index start index lst))

let get_index_test
    test start index lst expected_output : test = test >:: (fun _ -> 
    assert_equal expected_output (Board.get_index start index lst))


let board_tests = 
  [
    remove_index_test "remove index test 1" 0 0 [1;2;3;4] [2;3;4];
    remove_index_test "remove index test 2" 0 2 [1;2;3;4] [1;2;4];
    remove_index_test "remove index test 3" 0 3 [1;2;3;4] [1;2;3];

    get_index_test "remove index test 1" 0 0 [1;2;3;4] 1;
    get_index_test "remove index test 2" 0 2 [1;2;3;4] 3;
    get_index_test "remove index test 3" 0 3 [1;2;3;4] 4;

    (*Unit testing can not be used well to test random boards
      We used Utop and extensivly printed boards and ensured
      all the conditions were met for the board being correct manually*)

  ]

let suite =
  "test suite for Final Project"  >::: List.flatten [
    board_tests;

  ]

let _ = run_test_tt_main suite