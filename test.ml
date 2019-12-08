open OUnit2
open Board
open Player

let remove_index_test
    test start index lst expected_output : test = test >:: (fun _ -> 
    assert_equal expected_output (Board.remove_index start index lst)) 

let subset_test
    test lst1 lst2 expected_output : test = test>:: (fun _-> 
    assert_equal expected_output (Player.subset lst1 lst2))

let get_resources_test
    test player expected_output : test = test>:: (fun _-> 
    assert_equal expected_output (Player.resources_to_string player))

let give_sheep 
    test player: test = test>:: (fun _-> 
    Player.give_sheep player)

let give_wheat
    test player: test = test>:: (fun _-> 
    Player.give_wheat player)

let give_rock
    test player: test = test>:: (fun _-> 
    Player.give_rock player)

let give_wood
    test player: test = test>:: (fun _-> 
    Player.give_wood player)

let give_brick
    test player: test = test>:: (fun _-> 
    Player.give_wood player)

let give_port
    test player bool str: test = test>:: (fun _-> 
    Player.give_port player bool str)

let has_three_to_one_test 
    test player expected_output : test = test>:: (fun _-> 
    assert_equal expected_output (Player.has_three_to_one player))

let has_two_to_one_test 
    test player string expected_output : test = test>:: (fun _-> 
    assert_equal expected_output (Player.has_two_to_one player string))

let board = Board.rand_board

let board_tests = 
  [
    remove_index_test "remove index test 1" 0 0 [1;2;3;4] [2;3;4];
    remove_index_test "remove index test 2" 0 1 [1;2;3;4] [1;3;4];
    remove_index_test "remove index test 3" 0 2 [1;2;3;4] [1;2;4];
    remove_index_test "remove index test 4" 0 3 [1;2;3;4] [1;2;3];
  ]

let node1 = Node.make_node [] 2 [] true ""
let player1 = Player.make_player "green"

let player2 = Player.make_player "blue"



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

let player_test = [
  (*get_resources_test "tests if player initialized to no resources" player1 [];
    give_sheep "gives player 1 a sheep" player1;

    get_resources_test "tests if player has a sheep" player1 ["Sheep"];
    give_wood "gives player 1 a wood" player1;
    get_resources_test "tests if player has a sheep" player1 ["Wood";"Sheep"];
    give_rock "gives player 1 a rock" player1;
    get_resources_test "tests if player has a sheep" player1 ["Rock";"Wood";"Sheep"];
    give_brick "gives player 1 a brick" player1;
    get_resources_test "tests if player has a brick" player1 ["Brick";"Rock";"Wood";"Sheep"];
    give_wheat "gives player 1 a wheat" player1;
    get_resources_test "tests if player has a wheat" player1 ["Wheat";"Brick";"Rock";"Wood";"Sheep"];
    has_three_to_one_test "tests if players start with no ports" player1 false;
    has_three_to_one_test "tests if players start with no ports" player2 false;

    give_port "gives player 2 a three to one port" player1 true "";
    has_three_to_one_test "tests if players start with no ports" player1 false;
    give_port "gives player 2 a three to one port" player1 false "sheep";
    has_two_to_one_test "tests if player gets a two to one port" player1 "sheep" true;
    give_port "gives player 2 a three to one port" player1 false "wheat";
    has_two_to_one_test "tests if player gets a two to one port" player1 "wheat" true;
    give_port "gives player 2 a three to one port" player1 false "rock";
    has_two_to_one_test "tests if player gets a two to one port" player1 "rock" true;
    give_port "gives player 2 a three to one port" player1 false "brick";
    has_two_to_one_test "tests if player gets a two to one port" player1 "brick" true;
    give_port "gives player 2 a three to one port" player1 false "wood";
    has_two_to_one_test "tests if player gets a two to one port" player1 "wood" true;*)
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
    player_test;
  ]

let _ = run_test_tt_main suite