

for f in examples/*.cl; do
	echo "File -> $f"
	./lexer $f > my_output.txt
	/afs/ir.stanford.edu/class/cs143/bin/lexer $f > ref_output.txt
	diff my_output.txt ref_output.txt
done
