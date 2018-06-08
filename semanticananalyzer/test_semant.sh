
for f in /afs/ir.stanford.edu/class/cs143/examples/*.cl; do
        echo "File -> $f"
        ./mysemant -s $f > my_output.txt
        ./theirsemant $f > ref_output.txt
        diff my_output.txt ref_output.txt
done
