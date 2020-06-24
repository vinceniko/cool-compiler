from my_tester import diff_parser_test
import sys
import os

if __name__ == "__main__":
    dir_path = None
    
    try:
        dir_path = sys.argv[1]
        if not os.path.isdir(dir_path):
            raise ValueError("Not Dir")
    except IndexError:
        pass

    for fname in os.listdir(dir_path):
        if os.path.splitext(fname)[1] == ".cl":
            full_path = os.path.join(dir_path, fname)
            diff_parser_test(full_path)