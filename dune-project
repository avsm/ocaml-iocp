(lang dune 3.10)
(name iocp)
(generate_opam_files true)
(source (github ocaml-multicore/ocaml-iocp))
(license "MIT")
(authors "Patrick Ferris" "Anil Madhavapeddy <anil@recoil.org>")
(maintainers "patrick@sirref.org")
(package
 (name iocp)
 (synopsis "OCaml bindings for Windows IOCP")
 (description "Bindings to the Windows IOCP API. See https://github.com/ocaml-multicore/eio for a higher-level API using this.")
 (depends
   (ocaml (>= 5.2))
   cstruct
   (alcotest :with-test)))
