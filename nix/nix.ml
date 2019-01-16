module Nix = struct
  module Parser = Parser
  module Lexer = Lexer
  module Ast = Types
end

include Nix
