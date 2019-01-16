(* Interface for pretty-printing modules *)
module type PPRINTER = sig
  val print: out_channel -> Nix.Ast.expr -> unit
end
