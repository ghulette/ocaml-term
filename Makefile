SOURCES = util.mli util.ml \
	  option.mli option.ml \
          monad.mli monad.ml \
          relation.mli relation.ml \
	  value.mli value.ml \
          intern.mli intern.ml \
          term_base.ml unification.ml antiunification.ml \
	  grammar.mly token.mll \
          term.mli term.ml \
	  rewrite.mli rewrite.ml \
	  strategy.mli strategy.ml
RESULT = term

all: byte-code-library native-code-library

include OCamlMakefile
