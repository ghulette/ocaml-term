SOURCES = util.mli util.ml \
	  option.mli option.ml \
          monad.mli monad.ml \
          relation.mli relation.ml \
          intern.mli intern.ml \
          env.mli env.ml \
          term.mli term.ml \
          grammar.mly token.mll \
          main.ml
RESULT = term

all: byte-code-library native-code-library

include OCamlMakefile
