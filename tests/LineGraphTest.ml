open OUnit2
open Directed_linegraph
open Graphlib.Std
module SimpleGraph = Graphlib.Labeled(Core_kernel.Int)(Core_kernel.Int)(Core_kernel.Int)
module SimpleLGraphIso = LinegraphMapper.DLineGraphIsomorphism(SimpleGraph)

let ex_graph = Graphlib.create (module SimpleGraph) ~edges:[({node=1;node_label=10},{node=2;node_label=11},1);
          ({node=1;node_label=10},{node=3;node_label=12},2);
          ({node=2;node_label=11},{node=4;node_label=13},3);
          ({node=3;node_label=12},{node=4;node_label=13},4);] ()

let forward_backward_of_g g = SimpleLGraphIso.backward (SimpleLGraphIso.forward g)

let test_forward_backward g = assert_bool "not equal" (SimpleGraph.equal g (forward_backward_of_g g))

let test_empty_eq _ = test_forward_backward SimpleGraph.empty
let test_simple_eq _ = test_forward_backward ex_graph

let suite = "isomorphism props" >:::
  ["test empty prop" >:: test_empty_eq;
  "test simple prop" >:: test_simple_eq]


let () = run_test_tt_main suite