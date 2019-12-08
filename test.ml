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

let node_index_test
    test num list expected_output: test = test >:: (fun _ -> 
    assert_equal expected_output (Node.get_index (List.nth list num)))

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
let nodelist = Node.generate_nodes (rand_board ())

let node_tests = 
  [
    "test add_settlement1">::(fun _ -> assert_equal "city" 
                                 ((Node.add_settlement "city" player1 node1);
                                  (Node.get_settlement node1)));
    "test add_settlement2">::(fun _ -> assert_equal "settlement" 
                                 ((Node.add_settlement "settlement" player1 
                                     node1);
                                  (Node.get_settlement node1)));

    node_index_test "testing index	0	"	0	nodelist	0;
    node_index_test "testing index	1	"	1	nodelist	1;
    node_index_test "testing index	2	"	2	nodelist	2;
    node_index_test "testing index	3	"	3	nodelist	3;
    node_index_test "testing index	4	"	4	nodelist	4;
    node_index_test "testing index	5	"	5	nodelist	5;
    node_index_test "testing index	6	"	6	nodelist	6;
    node_index_test "testing index	7	"	7	nodelist	7;
    node_index_test "testing index	8	"	8	nodelist	8;
    node_index_test "testing index	9	"	9	nodelist	9;
    node_index_test "testing index	10	"	10	nodelist	10;
    node_index_test "testing index	11	"	11	nodelist	11;
    node_index_test "testing index	12	"	12	nodelist	12;
    node_index_test "testing index	13	"	13	nodelist	13;
    node_index_test "testing index	14	"	14	nodelist	14;
    node_index_test "testing index	15	"	15	nodelist	15;
    node_index_test "testing index	16	"	16	nodelist	16;
    node_index_test "testing index	17	"	17	nodelist	17;
    node_index_test "testing index	18	"	18	nodelist	18;
    node_index_test "testing index	19	"	19	nodelist	19;
    node_index_test "testing index	20	"	20	nodelist	20;
    node_index_test "testing index	21	"	21	nodelist	21;
    node_index_test "testing index	22	"	22	nodelist	22;
    node_index_test "testing index	23	"	23	nodelist	23;
    node_index_test "testing index	24	"	24	nodelist	24;
    node_index_test "testing index	25	"	25	nodelist	25;
    node_index_test "testing index	26	"	26	nodelist	26;
    node_index_test "testing index	27	"	27	nodelist	27;
    node_index_test "testing index	28	"	28	nodelist	28;
    node_index_test "testing index	29	"	29	nodelist	29;
    node_index_test "testing index	30	"	30	nodelist	30;
    node_index_test "testing index	31	"	31	nodelist	31;
    node_index_test "testing index	32	"	32	nodelist	32;
    node_index_test "testing index	33	"	33	nodelist	33;
    node_index_test "testing index	34	"	34	nodelist	34;
    node_index_test "testing index	35	"	35	nodelist	35;
    node_index_test "testing index	36	"	36	nodelist	36;
    node_index_test "testing index	37	"	37	nodelist	37;
    node_index_test "testing index	38	"	38	nodelist	38;
    node_index_test "testing index	39	"	39	nodelist	39;
    node_index_test "testing index	40	"	40	nodelist	40;
    node_index_test "testing index	41	"	41	nodelist	41;
    node_index_test "testing index	42	"	42	nodelist	42;
    node_index_test "testing index	43	"	43	nodelist	43;
    node_index_test "testing index	44	"	44	nodelist	44;
    node_index_test "testing index	45	"	45	nodelist	45;
    node_index_test "testing index	46	"	46	nodelist	46;
    node_index_test "testing index	47	"	47	nodelist	47;
    node_index_test "testing index	48	"	48	nodelist	48;
    node_index_test "testing index	49	"	49	nodelist	49;
    node_index_test "testing index	50	"	50	nodelist	50;
    node_index_test "testing index	51	"	51	nodelist	51;
    node_index_test "testing index	52	"	52	nodelist	52;
    node_index_test "testing index	53	"	53	nodelist	53;
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