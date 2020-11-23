open Graphlib.Std
open Core_kernel

module DLineGraph(G: Graph): sig 
  include module type of Graphlib.Labeled(G.Edge)(struct type t=G.Edge.label end)(Unit)
end

module DLineGraphIsomorphism(G: Graph): sig 
  module LineGraph: module type of DLineGraph(G)

  include Isomorphism with type s = G.t and type t=LineGraph.t

  
end