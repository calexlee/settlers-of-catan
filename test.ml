open OUnit2
open Board
open Player

let remove_index_test
    test start index lst expected_output : test = test >:: (fun _ -> 
    assert_equal expected_output (Board.remove_index start index lst)) 

let get_index_test
    test start index lst expected_output : test = test >:: (fun _ -> 
    assert_equal expected_output (Board.get_index start index lst))

let get_index_test
    test start index lst expected_output : test = test >:: (fun _ -> 
    assert_equal expected_output (Board.get_index start index lst))

let subset_test
    test lst1 lst2 expected_output : test = test>:: (fun _-> 
    assert_equal expected_output (Player.subset lst1 lst2))

let board_tests = 
  [
    remove_index_test "remove index test 1" 0 0 [1;2;3;4] [2;3;4];
    remove_index_test "remove index test 2" 0 2 [1;2;3;4] [1;2;4];
    remove_index_test "remove index test 3" 0 3 [1;2;3;4] [1;2;3];

    get_index_test "remove index test 1" 0 0 [1;2;3;4] 1;
    get_index_test "remove index test 2" 0 2 [1;2;3;4] 3;
    get_index_test "remove index test 3" 0 3 [1;2;3;4] 4;
  ]

let node1 = Node.make_node [] 2 [] true ""
let player1 = Player.make_player "green"

let node_tests = 
  [
    "test add_settlement1">::(fun _ -> assert_equal "city" 
                                 ((Node.add_settlement "city" player1 node1);
                                  (Node.get_settlement node1)));
    "test add_settlement2">::(fun _ -> assert_equal "settlement" 
                                 ((Node.add_settlement "settlement" player1 
                                     node1);
                                  (Node.get_settlement node1)));
  ]
(* 
-------------------------------------------------------------------------------
========================PLAY TESTING DOCUMENTATION==========================
Most of our testing consisted of play testing. For every function and seperate 
piece of functionality we created the implementer extensively used glass box
testing in order to test all the edge cases and corner cases of their 
implementation to make sure the game functioned correctly. We used this type of
testing because we wanted to make sure the game actually behaved in the terminal
as it should, and test it in the enviroment the user would play the game in

After The implementer of the functions glass-box tested them- the rest of the 
used black box play testing by testing the game without knowing how it was 
implemented this provided great feedback and checking to make sure everything 
actually worked as expected

================================Board Testing===================================
Board Testing-

==================================Help Tab======================================


=================================Setup Phase====================================


============================Resource Distribution===============================


==================================Die Rolling===================================


==============================Giving Players Ports==============================


=================================Bank Trading===================================


=================================Player Trading=================================


====================================Robbing=====================================











*)


let suite =
  "test suite for Final Project"  >::: List.flatten [
    board_tests;
    node_tests;
  ]

let _ = run_test_tt_main suite