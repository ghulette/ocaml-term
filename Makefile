SOURCES = aterm.ml parser.mly token.mll main.ml
RESULT  = aterm

all: native-code-library byte-code-library

include OCamlMakefile