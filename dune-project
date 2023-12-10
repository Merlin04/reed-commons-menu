(lang dune 3.12)

(name menu)

(generate_opam_files true)

(source
 (github merlin04/reed-menu))

(authors "B Smith")

(license ISC)

(package
 (name menu)
 (synopsis "Reed College commons menu")
 (description "Reed College commons menu")
 (depends
  ocaml
  dune
  ppx_deriving
  ppx_deriving_yojson
  cohttp-lwt-unix
  re
  tls-lwt
  logs
  dream
  ppx_fields_conv
  crunch
  calendar))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
