-- no features
class A {
};

-- empty features
Class BB__ inherits A {
    -- empty features
};

-- single feature
Class C {
    single_feature() : Int {
        0
    };
};

-- formals and multiple features and attribute assignment
Class D inherits A {
    -- attribute
    c : Int;
    -- attribute assign
    d : Int <- 0;

    -- method with no formals
    convert() : Int {
        c <- 1
    };
    -- one formal
    one_formal(one : Arg1) : Returned {
        1
    };
    -- more than one formal
    two_formals(one : Arg1, two : Arg2) : IntObject {
        1
    };
};

-- arithmetic expressions
Class E inherits A {
    c : Int <- 1 * 2;

    mul_add() : Int {
        c <- 1 + 1 * 2
    };
    add_mul() : Int {
        c <- (1 + 1) * 2 * (3 + (4 + 4))
    };
    add_sub() : Int {
        c <- 1 + 1 - 2
    };
    mul_div(): Int {
        c <- 1 * 2 / 2
    };
};

-- conditional expressions
Class F inherits A {
    conditional_lq() : Int {
        c <- 1 <= 1
    };
    conditional_le() : Int {
        c <- 1 < 1
    };
    conditional_eq() : Int {
        c <- 1 = 1
    };
    conditional_not() : Int {
        c <- not not 1
    };
};

-- block expression
Class G inherits A {
    block_one() : Int {
        {
            c <- 0;
        }
    };
    block_two() : Int {
        {
            c <- 0;
            c <- 1;
        }
    };
};

class H {
    method_dispatch() : Int {
        c.method()
    };
    method_dispatch_w_args() : Int {
        {
            c.method(arg1);
            c.method(arg1, arg2);
        }
    };
    method_static_dispatch() : Int {
        c@D.method()
    };
    method_static_dispatch_w_args() : Int {
        {
            c@D.method(arg1);
            c@D.method(arg1, arg2);
        }
    };
    self_dispatch() : Int {
        dispatch()
    };
};

-- let expressions
class I {
    a : Int <- let b : Int <- 0, c : Int <- 1 in 2;
    a : Int <- let b : Int <- c <- d * 1, c : Int <- 1 in 2;
    d : Int <- let e : Int, f : Int <- 1 in 2;
    g : Int <- let h : Int in 0;
};

-- case expressions
class J {
    a : Int <- case b * 1 of 
                    new_b : B => {
                        test;
                        2;
                    };
                esac;
    b : Int <- case b * 1 of 
                    new_b : B => {
                        test;
                        2;
                    };
                    new_c : C => 1;
                esac;
    c : Int <- case b * 1 of 
                    new_b : B => 1;
                esac;
};

-- conditionals mixed with arithmetic
class K {
    a : Int <- 1 * 3 <= 2;
}