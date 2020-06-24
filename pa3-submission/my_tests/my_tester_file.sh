# `make`.

test_file=$1

name=$(basename -- "$test_file")

refimpl="refimpl_out_${name}.txt"
mine="my_out_${name}.txt"

../refimpl/lexer $test_file | ../refimpl/parser > $refimpl
../refimpl/lexer $test_file | ./parser > $mine

diff -c refimpl mine | tee "diff_${refimpl}.txt"
