SOURCES = trie.mli trie.ml \
					prefix.mli prefix.ml \
					relation.mli relation.ml \
					intern.mli intern.ml \
					aterm.ml \
					parser.mly token.mll \
					main.ml
RESULT  = aterm

all: native-code-library byte-code-library

include OCamlMakefile
