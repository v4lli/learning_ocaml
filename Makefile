OUT := ./cut.native

$(OUT): cut.ml
	ocamlbuild -use-ocamlfind $@

.PHONY: test
test: $(OUT)
	test "$$(echo '1,2,3,4' | $(OUT) -d ',' -f 2)" = "2"
	test "$$(echo '1,2,3,4' | $(OUT) -d ',' -f 2-4)" = "2,3,4"
