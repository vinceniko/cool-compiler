# `make`.

test_file=my_test.cl
if [ $# -ne 0 ]
  then
    test_file=$1
fi

../../refimpl/lexer $test_file > refimpl_out.txt
./lexer $test_file > my_out.txt
diff -c refimpl_out.txt my_out.txt