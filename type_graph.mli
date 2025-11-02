open Ast

(** Global Type Graphs *)

(** Node labels for global type graphs *)
type global_node_label =
  | GEnd                          (** End of protocol *)
  | GMsgNode of role * role       (** Message: sender and receiver *)
  | GBraNode of role * role       (** Branch: sender and receiver *)
  | GParNode                      (** Parallel composition *)

(** Edge labels for global type graphs *)
type global_edge_label =
  | GBase of base                 (** Base type for message nodes *)
  | GLabel of label               (** Label for branch nodes *)
  | GEpsilon                      (** Unlabeled edge for parallel composition *)

(** A global graph node with its ID, label, and bound variables *)
type global_node = {
  gnode_id: int;
  gnode_label: global_node_label;
  gnode_vars: string list;        (** Variables bound at this node *)
}

(** Global type graph module *)
module GlobalGraph : sig
  (** The graph type *)
  type t
  
  (** Create a new empty graph *)
  val create : ?size:int -> unit -> t
  
  (** Add a vertex to the graph *)
  val add_vertex : t -> global_node -> unit
  
  (** Remove a vertex from the graph *)
  val remove_vertex : t -> global_node -> unit
  
  (** Edge type *)
  module E : sig
    type t
    type label = global_edge_label
    
    val create : global_node -> label -> global_node -> t
    val src : t -> global_node
    val dst : t -> global_node
    val label : t -> label
  end
  
  (** Add a labeled edge *)
  val add_edge_e : t -> E.t -> unit
  
  (** Get successor edges *)
  val succ_e : t -> global_node -> E.t list
  
  (** Iterate over vertices *)
  val iter_vertex : (global_node -> unit) -> t -> unit
  
  (** Iterate over edges *)
  val iter_edges_e : (E.t -> unit) -> t -> unit
  
  (** Create a new node *)
  val make_node : int -> global_node_label -> string list -> global_node
  
  (** Pretty print a node *)
  val string_of_node : global_node -> string
  
  (** Pretty print an edge label *)
  val string_of_edge_label : global_edge_label -> string
end

(** Local Type Graphs *)

(** Node labels for local type graphs *)
type local_node_label =
  | LEnd                          (** End of protocol *)
  | LSendNode of role             (** Send to role *)
  | LRecvNode of role             (** Receive from role *)
  | LIntNode of role              (** Internal choice with role *)
  | LExtNode of role              (** External choice with role *)

(** Edge labels for local type graphs *)
type local_edge_label =
  | LBase of base                 (** Base type for send/recv nodes *)
  | LLabel of label               (** Label for choice nodes *)

(** A local graph node with its ID, label, and bound variables *)
type local_node = {
  lnode_id: int;
  lnode_label: local_node_label;
  lnode_vars: string list;        (** Variables bound at this node *)
}

(** Local type graph module *)
module LocalGraph : sig
  (** The graph type *)
  type t
  
  (** Create a new empty graph *)
  val create : ?size:int -> unit -> t
  
  (** Add a vertex to the graph *)
  val add_vertex : t -> local_node -> unit
  
  (** Remove a vertex from the graph *)
  val remove_vertex : t -> local_node -> unit
  
  (** Edge type *)
  module E : sig
    type t
    type label = local_edge_label
    
    val create : local_node -> label -> local_node -> t
    val src : t -> local_node
    val dst : t -> local_node
    val label : t -> label
  end
  
  (** Add a labeled edge *)
  val add_edge_e : t -> E.t -> unit
  
  (** Get successor edges *)
  val succ_e : t -> local_node -> E.t list
  
  (** Iterate over vertices *)
  val iter_vertex : (local_node -> unit) -> t -> unit
  
  (** Iterate over edges *)
  val iter_edges_e : (E.t -> unit) -> t -> unit
  
  (** Create a new node *)
  val make_node : int -> local_node_label -> string list -> local_node
  
  (** Pretty print a node *)
  val string_of_node : local_node -> string
  
  (** Pretty print an edge label *)
  val string_of_edge_label : local_edge_label -> string
end

(** Conversion Functions *)

(** Convert a global type to a graph
    
    @param g The global type to convert
    @return (graph, entry_node) where entry_node is the starting node
*)
val global_to_graph : string global -> GlobalGraph.t * global_node

(** Convert a local type to a graph
    
    @param t The local type to convert
    @return (graph, entry_node) where entry_node is the starting node
*)
val local_to_graph : string local -> LocalGraph.t * local_node

(** Convert a global graph back to AST
    
    Uses the stored variable names to reconstruct recursion binders.
    
    @param graph The graph to convert
    @param entry The entry node
    @return The reconstructed global type
*)
val graph_to_global : GlobalGraph.t -> global_node -> string global

(** Convert a local graph back to AST
    
    Uses the stored variable names to reconstruct recursion binders.
    
    @param graph The graph to convert
    @param entry The entry node
    @return The reconstructed local type
*)
val graph_to_local : LocalGraph.t -> local_node -> string local

(** {1 Visualization} *)

(** Generate DOT format for a global graph
    
    @param graph The graph to visualize
    @param entry The entry node
    @return DOT format string that can be rendered with Graphviz
*)
val global_to_dot : GlobalGraph.t -> global_node -> string

(** Generate DOT format for a local graph
    
    @param graph The graph to visualize
    @param entry The entry node
    @return DOT format string that can be rendered with Graphviz
*)
val local_to_dot : LocalGraph.t -> local_node -> string

