(lang dune 3.0)
(name nixformat)
(using menhir 2.0)
(generate_opam_files true)
(license MIT)
(authors "Denis Korzunov <disya2@gmail.com>")
(source (github d2km/nixformat))

(package
  (name nixformat)
  (depends menhir pprint)
  (synopsis "Nix code formatter")
  (description "A code formatter written in ocaml using menhir and ocamllex"))
