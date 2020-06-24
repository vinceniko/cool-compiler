import subprocess
import sys
import os


def diff_parser_test(test_file):
    fname = os.path.basename(test_file)
    refimpl_out = f"refimpl_{fname}.txt"
    mine_out = f"mine_{fname}.txt"

    kwargs = {
        "shell": True,
        "stderr": subprocess.STDOUT,
    }
    subprocess.check_output(f"./lexer {test_file} | ./parser | ./semant > {refimpl_out}", **kwargs)
    subprocess.check_output(f"./lexer {test_file} | ./parser | ../refimpl/semant > {mine_out}", **kwargs)

    print(f"differences in {fname}:", subprocess.check_output(f"diff -y --suppress-common-lines {refimpl_out} {mine_out} | grep '^' | wc -l", **kwargs).decode("utf-8").strip())
    subprocess.check_output(f"diff -c {refimpl_out} {mine_out} | tee diff_{fname}.txt", **kwargs)


if __name__ == "__main__":
    test_file = None
    try:
        d = sys.argv[1]
        if os.path.isfile(d):
            test_file = d
        else:
            raise ValueError("Not File")
    except IndexError:
        pass

    diff_parser_test(test_file)
