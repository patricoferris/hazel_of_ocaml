(lang dune 3.16)
(using mdx 0.4)
(name hazel_of_ocaml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name hazel_of_ocaml)
 (synopsis "Hazel code from OCaml code")
 (description "A best-effort transpiler of OCaml code to Hazel code")
 (depends 
   ocaml
   dune
   ; Our deps
   (ppxlib (<= 0.35.0))
   merlin-lib
   ppx_expect
   (mdx :with-test)
   ; Hazel deps which are vendored
   re
   sexplib
   unionFind
   reason
   menhir
   ppx_let
   ppx_deriving_qcheck
   qcheck-alcotest
   ppx_sexp_conv
 )
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
