(* loc.ml: Location information for AST nodes *)

type t = {
  start_line : int;
  start_col : int;
  end_line : int;
  end_col : int;
}

(* A dummy location for when we don't have position information *)
let dummy = {
  start_line = 0;
  start_col = 0;
  end_line = 0;
  end_col = 0;
}

let pp fmt loc =
  Format.fprintf fmt "%d:%d-%d:%d"
    loc.start_line loc.start_col
    loc.end_line loc.end_col

