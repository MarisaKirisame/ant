(lang dune 3.16)
(using menhir 3.0)
(using ctypes 0.3)

(name ant)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name ant)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune pprint cmdliner batteries core)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
