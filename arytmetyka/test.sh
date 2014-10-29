ocamlc -c arytmetyka.mli
ocamlc -c arytmetyka.ml
ocamlc -c arytest.ml
ocamlc -o script_test arytmetyka.cmo arytest.cmo
chmod +x script_test
./script_test
rm ./script_test *.cmo *.cmi
