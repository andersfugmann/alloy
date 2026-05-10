(lang dune 3.0)

(name alloy)

(generate_opam_files true)

(package
 (name alloy)
 (allow_empty)
 (synopsis "URL routing across browser profiles")
 (depends
  (ocaml
   (>= 5.0))
  (base
   (>= v0.17))
  (stdio
   (>= v0.17))
  (yojson
   (>= 2.0))
  (ppx_deriving_yojson
   (>= 3.0))
  (ppx_jane
   (>= v0.17))
  (eio
   (>= 1.0))
  (eio_main
   (>= 1.0))
  (re
   (>= 1.0))
  (js_of_ocaml
   (>= 5.0))
  (js_of_ocaml-lwt
   (>= 5.0))
  (js_of_ocaml-ppx
   (>= 5.0))
  (lwt
   (>= 5.0))
  (lwt_ppx
   (>= 5.0))
  (cmdliner
   (>= 1.0))
  (sqlite3
   (>= 5.0))
  (ipaddr
   (>= 5.0))
  (ppx_expect
   (and
    :with-test
    (>= v0.17)))
  (ppx_inline_test
   (and
    :with-test
    (>= v0.17)))))
