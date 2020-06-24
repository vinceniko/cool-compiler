# `make`.

test_file=good.cl


../refimpl/lexer $test_file | ../refimpl/parser > refimpl_out_good.txt
../refimpl/lexer $test_file | ./parser > my_out_good.txt

diff -c refimpl_out_good.txt my_out_good.txt | tee diff_good.txt

###

test_file=bad.cl

# set -o pipefail  # doesnt work

../refimpl/lexer $test_file | ../refimpl/parser 2>&1 > refimpl_out_bad.txt
../refimpl/lexer $test_file | ./parser 2>&1 > my_out_bad.txt

diff -c refimpl_out_bad.txt my_out_bad.txt 2>&1 | tee diff_bad.txt

###

