(* Interface for pretty-printing modules *)
module type PPRINTER = sig
  val print: out_channel -> Nix.Ast.expr * Nix.Comments.t Queue.t -> unit
end
