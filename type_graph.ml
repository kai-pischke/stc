open Ast

(* String map for environment *)
module StringMap = Map.Make(String)

(** Global Type Graphs *)

(** Node labels for global type graphs *)
type global_node_label =
  | GEnd
  | GMsgNode of role * role        (* sender, receiver *)
  | GBraNode of role * role        (* sender, receiver *)
  | GParNode                       (* parallel composition *)

(** Edge labels for global type graphs *)
type global_edge_label =
  | GBase of base                  (* for message nodes *)
  | GLabel of label                (* for branch nodes *)
  | GEpsilon                       (* for parallel composition *)

(** A global graph node with its ID, label, and bound variables *)
type global_node = {
  gnode_id: int;
  gnode_label: global_node_label;
  gnode_vars: string list;         (* variables bound at this node *)
}

(** Global type graph using OCamlGraph *)
module GlobalGraph = struct
  module G = Graph.Imperative.Digraph.ConcreteLabeled(struct
    type t = global_node
    let compare n1 n2 = compare n1.gnode_id n2.gnode_id
    let hash n = Hashtbl.hash n.gnode_id
    let equal n1 n2 = n1.gnode_id = n2.gnode_id
  end)(struct
    type t = global_edge_label
    let compare = compare
    let default = GEpsilon
  end)
  
  include G
  
  (** Create a new node *)
  let make_node id label vars = {
    gnode_id = id;
    gnode_label = label;
    gnode_vars = vars;
  }
  
  (** Pretty print a global node *)
  let string_of_node n =
    let vars_str = if n.gnode_vars = [] then ""
                   else " [" ^ String.concat ", " n.gnode_vars ^ "]" in
    match n.gnode_label with
    | GEnd -> Printf.sprintf "n%d: end%s" n.gnode_id vars_str
    | GMsgNode (p, q) -> Printf.sprintf "n%d: %s->%s:Msg%s" n.gnode_id p q vars_str
    | GBraNode (p, q) -> Printf.sprintf "n%d: %s->%s:Bra%s" n.gnode_id p q vars_str
    | GParNode -> Printf.sprintf "n%d: Par%s" n.gnode_id vars_str
  
  (** Pretty print an edge label *)
  let string_of_edge_label = function
    | GBase b -> b
    | GLabel l -> l
    | GEpsilon -> "ε"
end

(** Local Type Graphs *)

(** Node labels for local type graphs *)
type local_node_label =
  | LEnd
  | LSendNode of role              (* receiver *)
  | LRecvNode of role              (* sender *)
  | LIntNode of role               (* internal choice with role *)
  | LExtNode of role               (* external choice with role *)

(** Edge labels for local type graphs *)
type local_edge_label =
  | LBase of base                  (* for send/recv nodes *)
  | LLabel of label                (* for choice nodes *)

(** A local graph node with its ID, label, and bound variables *)
type local_node = {
  lnode_id: int;
  lnode_label: local_node_label;
  lnode_vars: string list;         (* variables bound at this node *)
}

(** Local type graph using OCamlGraph *)
module LocalGraph = struct
  module G = Graph.Imperative.Digraph.ConcreteLabeled(struct
    type t = local_node
    let compare n1 n2 = compare n1.lnode_id n2.lnode_id
    let hash n = Hashtbl.hash n.lnode_id
    let equal n1 n2 = n1.lnode_id = n2.lnode_id
  end)(struct
    type t = local_edge_label
    let compare = compare
    let default = LLabel ""
  end)
  
  include G
  
  (** Create a new node *)
  let make_node id label vars = {
    lnode_id = id;
    lnode_label = label;
    lnode_vars = vars;
  }
  
  (** Pretty print a local node *)
  let string_of_node n =
    let vars_str = if n.lnode_vars = [] then ""
                   else " [" ^ String.concat ", " n.lnode_vars ^ "]" in
    match n.lnode_label with
    | LEnd -> Printf.sprintf "n%d: end%s" n.lnode_id vars_str
    | LSendNode r -> Printf.sprintf "n%d: !%s%s" n.lnode_id r vars_str
    | LRecvNode r -> Printf.sprintf "n%d: ?%s%s" n.lnode_id r vars_str
    | LIntNode r -> Printf.sprintf "n%d: ⊕%s%s" n.lnode_id r vars_str
    | LExtNode r -> Printf.sprintf "n%d: &%s%s" n.lnode_id r vars_str
  
  (** Pretty print an edge label *)
  let string_of_edge_label = function
    | LBase b -> b
    | LLabel l -> l
end

(** Conversion from AST to Graph *)

(** Convert a global type to a graph *)
let global_to_graph (g : string global) : GlobalGraph.t * global_node =
  let graph = GlobalGraph.create () in
  let node_counter = ref 0 in
  
  let new_node_id () =
    let id = !node_counter in
    incr node_counter;
    id
  in
  
  (* Recursive conversion with environment and variable queue
     - env: Maps variable names to their binding nodes
     - queue: Variables from rec binders waiting to be bound
     - g: The global type to convert
     Returns the node representing this type *)
  let rec convert (env : global_node StringMap.t) (queue : string list) (g : string global) 
      : global_node =
    match g with
    | GRec (x, body, _) ->
        (* Add variable to queue and continue - rec doesn't create a node *)
        convert env (x :: queue) body
    
    | GVar (x, _) ->
        (* Look up the binding site in the environment *)
        (match StringMap.find_opt x env with
         | Some node -> node
         | None -> 
             (* This shouldn't happen in well-formed types *)
             failwith (Printf.sprintf "Unbound variable %s in global type" x))
    
    | GEnd _ ->
        (* Create end node with current queue *)
        let node = GlobalGraph.make_node (new_node_id ()) GEnd queue in
        GlobalGraph.add_vertex graph node;
        (* Bind all queued variables to this node *)
        let _env' = List.fold_left (fun e var -> StringMap.add var node e) env queue in
        node
    
    | GMsg (p, q, base, cont, _) ->
        (* Create message node with current queue *)
        let node = GlobalGraph.make_node (new_node_id ()) (GMsgNode (p, q)) queue in
        GlobalGraph.add_vertex graph node;
        (* Bind all queued variables to this node *)
        let env' = List.fold_left (fun e var -> StringMap.add var node e) env queue in
        (* Continue with empty queue and updated environment *)
        let cont_node = convert env' [] cont in
        GlobalGraph.add_edge_e graph (GlobalGraph.E.create node (GBase base) cont_node);
        node
    
    | GBra (p, q, branches, _) ->
        (* Create branch node with current queue *)
        let node = GlobalGraph.make_node (new_node_id ()) (GBraNode (p, q)) queue in
        GlobalGraph.add_vertex graph node;
        (* Bind all queued variables to this node *)
        let env' = List.fold_left (fun e var -> StringMap.add var node e) env queue in
        (* Process each branch with empty queue and updated environment *)
        List.iter (fun (label, branch_g) ->
          let branch_node = convert env' [] branch_g in
          GlobalGraph.add_edge_e graph (GlobalGraph.E.create node (GLabel label) branch_node)
        ) branches;
        node
    
    | GPar (g1, g2, _) ->
        (* Create parallel node with current queue *)
        let node = GlobalGraph.make_node (new_node_id ()) GParNode queue in
        GlobalGraph.add_vertex graph node;
        (* Bind all queued variables to this node *)
        let env' = List.fold_left (fun e var -> StringMap.add var node e) env queue in
        (* Process both branches with empty queue and updated environment *)
        let n1 = convert env' [] g1 in
        let n2 = convert env' [] g2 in
        GlobalGraph.add_edge_e graph (GlobalGraph.E.create node GEpsilon n1);
        GlobalGraph.add_edge_e graph (GlobalGraph.E.create node GEpsilon n2);
        node
  in
  
  let entry = convert StringMap.empty [] g in
  (graph, entry)

(** Convert a local type to a graph
    
    @param t The local type to convert
    @return (graph, entry_node) where entry_node is the starting node
*)
let local_to_graph (t : string local) : LocalGraph.t * local_node =
  let graph = LocalGraph.create () in
  let node_counter = ref 0 in
  
  let new_node_id () =
    let id = !node_counter in
    incr node_counter;
    id
  in
  
  (* Recursive conversion with environment and variable queue
     - env: Maps variable names to their binding nodes
     - queue: Variables from rec binders waiting to be bound
     - t: The local type to convert
     Returns the node representing this type *)
  let rec convert (env : local_node StringMap.t) (queue : string list) (t : string local) 
      : local_node =
    match t with
    | LRec (x, body, _) ->
        (* Add variable to queue and continue - rec doesn't create a node *)
        convert env (x :: queue) body
    
    | LVar (x, _) ->
        (* Look up the binding site in the environment *)
        (match StringMap.find_opt x env with
         | Some node -> node
         | None -> 
             (* This shouldn't happen in well-formed types *)
             failwith (Printf.sprintf "Unbound variable %s in local type" x))
    
    | LEnd _ ->
        (* Create end node with current queue *)
        let node = LocalGraph.make_node (new_node_id ()) LEnd queue in
        LocalGraph.add_vertex graph node;
        (* Bind all queued variables to this node *)
        let _env' = List.fold_left (fun e var -> StringMap.add var node e) env queue in
        node
    
    | LSend (role, base, cont, _) ->
        (* Create send node with current queue *)
        let node = LocalGraph.make_node (new_node_id ()) (LSendNode role) queue in
        LocalGraph.add_vertex graph node;
        (* Bind all queued variables to this node *)
        let env' = List.fold_left (fun e var -> StringMap.add var node e) env queue in
        (* Continue with empty queue and updated environment *)
        let cont_node = convert env' [] cont in
        LocalGraph.add_edge_e graph (LocalGraph.E.create node (LBase base) cont_node);
        node
    
    | LRecv (role, base, cont, _) ->
        (* Create receive node with current queue *)
        let node = LocalGraph.make_node (new_node_id ()) (LRecvNode role) queue in
        LocalGraph.add_vertex graph node;
        (* Bind all queued variables to this node *)
        let env' = List.fold_left (fun e var -> StringMap.add var node e) env queue in
        (* Continue with empty queue and updated environment *)
        let cont_node = convert env' [] cont in
        LocalGraph.add_edge_e graph (LocalGraph.E.create node (LBase base) cont_node);
        node
    
    | LInt (role, branches, _) ->
        (* Create internal choice node with current queue *)
        let node = LocalGraph.make_node (new_node_id ()) (LIntNode role) queue in
        LocalGraph.add_vertex graph node;
        (* Bind all queued variables to this node *)
        let env' = List.fold_left (fun e var -> StringMap.add var node e) env queue in
        (* Process each branch with empty queue and updated environment *)
        List.iter (fun (label, branch_t) ->
          let branch_node = convert env' [] branch_t in
          LocalGraph.add_edge_e graph (LocalGraph.E.create node (LLabel label) branch_node)
        ) branches;
        node
    
    | LExt (role, branches, _) ->
        (* Create external choice node with current queue *)
        let node = LocalGraph.make_node (new_node_id ()) (LExtNode role) queue in
        LocalGraph.add_vertex graph node;
        (* Bind all queued variables to this node *)
        let env' = List.fold_left (fun e var -> StringMap.add var node e) env queue in
        (* Process each branch with empty queue and updated environment *)
        List.iter (fun (label, branch_t) ->
          let branch_node = convert env' [] branch_t in
          LocalGraph.add_edge_e graph (LocalGraph.E.create node (LLabel label) branch_node)
        ) branches;
        node
  in
  
  let entry = convert StringMap.empty [] t in
  (graph, entry)

(** {1 Conversion from Graph to AST} *)

(** Convert a global graph back to AST
    
    Uses the stored variable names to reconstruct recursion binders.
    
    @param graph The graph to convert
    @param entry The entry node
    @return The reconstructed global type
*)
let graph_to_global (graph : GlobalGraph.t) (entry : global_node) : string global =
  let loc = Loc.dummy in
  let visited = Hashtbl.create 10 in
  
  let rec convert (node : global_node) : string global =
    (* Check if we've seen this node (for handling cycles) *)
    if Hashtbl.mem visited node.gnode_id then
      (* This is a back-edge - should be represented as a variable *)
      match node.gnode_vars with
      | x :: _ -> Ast.GVar (x, loc)
      | [] -> Ast.GEnd loc  (* Shouldn't happen in well-formed graphs *)
    else begin
      Hashtbl.add visited node.gnode_id ();
      
      let result = match node.gnode_label with
        | GEnd -> Ast.GEnd loc
        
        | GMsgNode (p, q) ->
            let out_edges = GlobalGraph.succ_e graph node in
            (match out_edges with
             | [] -> Ast.GEnd loc  (* No continuation *)
             | edge :: _ ->
                 let label = GlobalGraph.E.label edge in
                 let target = GlobalGraph.E.dst edge in
                 let base = match label with
                   | GBase b -> b
                   | _ -> "unknown"
                 in
                 let cont = convert target in
                 Ast.GMsg (p, q, base, cont, loc))
        
        | GBraNode (p, q) ->
            let out_edges = GlobalGraph.succ_e graph node in
            let branches = List.map (fun edge ->
              let label = GlobalGraph.E.label edge in
              let target = GlobalGraph.E.dst edge in
              let lbl = match label with
                | GLabel l -> l
                | _ -> "unknown"
              in
              (lbl, convert target)
            ) out_edges in
            Ast.GBra (p, q, branches, loc)
        
        | GParNode ->
            let out_edges = GlobalGraph.succ_e graph node in
            (match out_edges with
             | [] -> Ast.GEnd loc
             | [edge] -> convert (GlobalGraph.E.dst edge)
             | edge1 :: edge2 :: _ ->
                 let g1 = convert (GlobalGraph.E.dst edge1) in
                 let g2 = convert (GlobalGraph.E.dst edge2) in
                 Ast.GPar (g1, g2, loc))
      in
      
      (* Wrap with recursion binders if this node has bound variables *)
      List.fold_left (fun acc var ->
        Ast.GRec (var, acc, loc)
      ) result node.gnode_vars
    end
  in
  
  convert entry

(** Convert a local graph back to AST
    
    Uses the stored variable names to reconstruct recursion binders.
    
    @param graph The graph to convert
    @param entry The entry node
    @return The reconstructed local type
*)
let graph_to_local (graph : LocalGraph.t) (entry : local_node) : string local =
  let loc = Loc.dummy in
  let visited = Hashtbl.create 10 in
  
  let rec convert (node : local_node) : string local =
    if Hashtbl.mem visited node.lnode_id then
      match node.lnode_vars with
      | x :: _ -> Ast.LVar (x, loc)
      | [] -> Ast.LEnd loc
    else begin
      Hashtbl.add visited node.lnode_id ();
      
      let result = match node.lnode_label with
        | LEnd -> Ast.LEnd loc
        
        | LSendNode role ->
            let out_edges = LocalGraph.succ_e graph node in
            (match out_edges with
             | [] -> Ast.LEnd loc
             | edge :: _ ->
                 let label = LocalGraph.E.label edge in
                 let target = LocalGraph.E.dst edge in
                 let base = match label with
                   | LBase b -> b
                   | _ -> "unknown"
                 in
                 let cont = convert target in
                 Ast.LSend (role, base, cont, loc))
        
        | LRecvNode role ->
            let out_edges = LocalGraph.succ_e graph node in
            (match out_edges with
             | [] -> Ast.LEnd loc
             | edge :: _ ->
                 let label = LocalGraph.E.label edge in
                 let target = LocalGraph.E.dst edge in
                 let base = match label with
                   | LBase b -> b
                   | _ -> "unknown"
                 in
                 let cont = convert target in
                 Ast.LRecv (role, base, cont, loc))
        
        | LIntNode role ->
            let out_edges = LocalGraph.succ_e graph node in
            let branches = List.map (fun edge ->
              let label = LocalGraph.E.label edge in
              let target = LocalGraph.E.dst edge in
              let lbl = match label with
                | LLabel l -> l
                | _ -> "unknown"
              in
              (lbl, convert target)
            ) out_edges in
            Ast.LInt (role, branches, loc)
        
        | LExtNode role ->
            let out_edges = LocalGraph.succ_e graph node in
            let branches = List.map (fun edge ->
              let label = LocalGraph.E.label edge in
              let target = LocalGraph.E.dst edge in
              let lbl = match label with
                | LLabel l -> l
                | _ -> "unknown"
              in
              (lbl, convert target)
            ) out_edges in
            Ast.LExt (role, branches, loc)
      in
      
      List.fold_left (fun acc var ->
        Ast.LRec (var, acc, loc)
      ) result node.lnode_vars
    end
  in
  
  convert entry

(** Visualisation Helpers *)

(** Generate DOT format for a global graph *)
let global_to_dot (graph : GlobalGraph.t) (_entry : global_node) : string =
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer "digraph G {\n";
  Buffer.add_string buffer "  rankdir=LR;\n";
  Buffer.add_string buffer "  node [shape=box];\n";
  
  (* Add all vertices *)
  GlobalGraph.iter_vertex (fun node ->
    let shape = match node.gnode_label with
      | GEnd -> "doublecircle"
      | GParNode -> "diamond"
      | _ -> "box"
    in
    let label = GlobalGraph.string_of_node node in
    Buffer.add_string buffer (Printf.sprintf "  n%d [label=\"%s\", shape=%s];\n"
      node.gnode_id label shape)
  ) graph;
  
  (* Add all edges *)
  GlobalGraph.iter_edges_e (fun edge ->
    let src = GlobalGraph.E.src edge in
    let dst = GlobalGraph.E.dst edge in
    let label = GlobalGraph.string_of_edge_label (GlobalGraph.E.label edge) in
    Buffer.add_string buffer (Printf.sprintf "  n%d -> n%d [label=\"%s\"];\n"
      src.gnode_id dst.gnode_id label)
  ) graph;
  
  Buffer.add_string buffer "}\n";
  Buffer.contents buffer

(** Generate DOT format for a local graph *)
let local_to_dot (graph : LocalGraph.t) (_entry : local_node) : string =
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer "digraph L {\n";
  Buffer.add_string buffer "  rankdir=LR;\n";
  Buffer.add_string buffer "  node [shape=box];\n";
  
  (* Add all vertices *)
  LocalGraph.iter_vertex (fun node ->
    let shape = match node.lnode_label with
      | LEnd -> "doublecircle"
      | _ -> "box"
    in
    let label = LocalGraph.string_of_node node in
    Buffer.add_string buffer (Printf.sprintf "  n%d [label=\"%s\", shape=%s];\n"
      node.lnode_id label shape)
  ) graph;
  
  (* Add all edges *)
  LocalGraph.iter_edges_e (fun edge ->
    let src = LocalGraph.E.src edge in
    let dst = LocalGraph.E.dst edge in
    let label = LocalGraph.string_of_edge_label (LocalGraph.E.label edge) in
    Buffer.add_string buffer (Printf.sprintf "  n%d -> n%d [label=\"%s\"];\n"
      src.lnode_id dst.lnode_id label)
  ) graph;
  
  Buffer.add_string buffer "}\n";
  Buffer.contents buffer

