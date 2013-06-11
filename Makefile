SOURCES = trie.mli trie.ml \
					prefix.mli prefix.ml \
					relation.mli relation.ml \
					intern.mli intern.ml \
					env.mli env.ml \
					term.ml \
					grammar.mly token.mll \
					main.ml
RESULT  = term

all: byte-code-library native-code-library

include OCamlMakefile
