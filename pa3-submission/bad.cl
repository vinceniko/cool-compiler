(*
 *  execute "coolc bad.cl" to see the error messages that the coolc parser
 *  generates
 *
 *  execute "myparser bad.cl" to see the error messages that your parser
 *  generates
 *)

(* no error *)
class A {
};

(* error:  b is not a type identifier *)
Class b inherits A {
};

(* error:  a is not a type identifier *)
Class C inherits a {
};

(* error:  keyword inherits is misspelled *)
Class D inherts A {
};

(* error:  closing brace is missing *)
Class E inherits A {
;

Class F inherits A {
    -- formal comma errors
    extra_comma(,) : Int {
        0
    };
    extra_two_commas(,,) : Int {
        0
    };
    extra_comma_front(, two : Arg2) : Int {
        0
    };
    extra_comma_back(two : Arg2 ,) : Int {
        0
    };
};

class G {
    empty_block() : Int {
        {

        }
    };
    double_semi_colon() : Int {  -- problem
        {
            c <- 0;;
        }
    };
};


class H {
    method_dispatch() : Int {
        c.method()
    };
    method_dispatch_w_args() : Int {
        {
            c.method(arg1, );
            c.method(, arg1);
            c.method(,);
        }
    };
    method_static_dispatch() : Int {
        {
            c@D.method();
            c@.method();
        }
    };
    method_static_dispatch_w_args() : Int {
        {
            c@D.method(arg1, );
            c@D.method(, arg1);
            c@D.method(,);
        }
    };
};

-- let expressions
class I {
    -- no type
    a: Int <- let b: <- 0, c: Int <- 1 in 2;
    -- no ':'
    d: Int <- let e Int, f: Int <- 1 in 2;
    -- extra comma
    d: Int <- let e Int, in 2;
    (*
    -- the below do not work with refimpl
    
    -- no expression
    h: Int <- let i: Int in;
    -- no in.
    f: Int <- let g: Int;
    method() : Int {
        f: Int <- let g: Int;
        h: Int <- let i: Int in;
    };*)
};

-- case expressions
class J {
    -- no branches
    b : Int <- case b * 1 of 
                esac;
    -- missing ; at end of branch
    c : Int <- case b * 1 of 
                    new_b : B => 1
                esac;
    -- no expression at end of branch
    d : Int <- case b * 1 of 
                    new_b : B => 1;
                    new_c;
                esac;
};

-- bad parenthesis
class K {
    a : Int <- (1)(1);
    b : Int <- (1(1);
    c : Int <- (1(1;
    d : (Int <- ());
};

-- extraneous semi-colon
class L {
    ;
};

-- two operators in a row
class M {
    (*
    -- below doesnt work with refimpl
    a : Int <- 1 * * 2;
    *)
}

-- conditionals mixed with arithmetic
class N. {
    a : Int <- 1 * 3 <= 2;
}