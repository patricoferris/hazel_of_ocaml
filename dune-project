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
   (ocaml (= 5.2.0))
   dune
   ; Our deps
   fmt
   (ppxlib (<= 0.35.0))
   (merlin-lib (= 5.3-502))
   ppx_expect
   (mdx :with-test)
 )
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
