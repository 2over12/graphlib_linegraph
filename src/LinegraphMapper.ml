open Graphlib.Std
open Core_kernel
module DLineGraph(G: Graph) = struct 
  include Graphlib.Labeled(G.Edge)(struct type t=G.Edge.label end)(Unit)
end


module DLineGraphIsomorphism(G: Graph) =  struct 
  module LineGraph = DLineGraph(G)


  type s = G.t
  type t = LineGraph.t

  let node_to_edge_prods (grph:s) (nd: G.node) = let ins = G.Node.inputs  nd grph in 
    let outs = G.Node.outputs nd grph in 
     Sequence.cartesian_product ins outs

  let forward (norm_graph:s)  = let graph_nodes =  G.nodes norm_graph in 
    let new_edges =  Sequence.map ~f:(function (e1,e2) -> ({node=e1;node_label=G.Edge.label e1},{node=e2;node_label=G.Edge.label e2}, ())) 
      (Sequence.concat (Sequence.map  graph_nodes ~f:(node_to_edge_prods norm_graph)))
      in Graphlib.create (module LineGraph) ~edges:(Sequence.to_list new_edges) ()
  let backward (line_graph:t) = 
      let nodes = LineGraph.nodes line_graph in 
      let labeled_edge_to_tuple (lab_edge: (G.edge, G.Edge.label) labeled) =
        let src_node = G.Edge.src lab_edge.node in
        let dst_node = G.Edge.dst lab_edge.node in 
         (G.Node.label src_node, G.Node.label dst_node, lab_edge.node_label)in

      let orig_edges = Sequence.map ~f:(labeled_edge_to_tuple) nodes in 
    
      Graphlib.create (module G) ~edges:(Sequence.to_list orig_edges) ()



end