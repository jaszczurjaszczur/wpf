ocamlc -c *.mli *.ml
ocamlc -o script_test arytmetyka.cmo arytest.cmo
chmod +x script_test
./script_test
rm ./script_test *.cmo *.cmi
