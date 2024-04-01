(lang dune 3.14)


(name tincan)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name tincan)
 (synopsis "Socket Chat CLI app")
 (depends 
   (ocaml (= 5.1.1)) 
   dune 
   (riot (= "0.0.8")) 
   (minttea (= "0.0.2")) 
   (spices (= "0.0.2")) 
   (leaves (= "0.0.2"))
   containers)
 (tags
  (topics "TCP" "Sockets" "CLI")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
