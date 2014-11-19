ocamlc -c *.mli *.ml
ocamlc -o script_test leftist.cmo leftest.cmo
chmod +x script_test
./script_test
rm ./script_test *.cmo *.cmi
