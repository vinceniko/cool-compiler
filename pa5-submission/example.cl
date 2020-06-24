class Z inherits IO {
    b : Int <- 500;
    hello_world() : Int {
        {
            out_string("z");
            0;
        }
    };
};

class D inherits IO {
    hello_world() : Int {
        {
            out_string(self.type_name());
            out_string("D");
            0;
        }
    };
};

class A inherits D {
    b : Int <- 10;
    f: Object <- out_string("A init\n");

    -- doesnt work
    new_me() : SELF_TYPE {
        new SELF_TYPE
    };
    hello_world() : Int {
        {
            out_string(self.type_name());
            self@D.hello_world();
            out_string("hello");
            0;
        }
    };
    new_Z() : Z {
        new Z
    };
    get_b() : Int {
        b
    };
};

class Main inherits A {
    a : Int <- 10 + 1;
    d : Int <- 4;
    e : Int;
    c: SELF_TYPE;
    x: Object <- out_string("main init\n");
    test() : Object {
        out_int(2*(10+20)/10) -- 6
    };
    hello_world() : Int {
        {
            out_string("world");
            1;
        }
    };
    formal(x : Int) : Object {
        out_int(x)
    };
    new_test() : A {
        new A
    };
    get_c() : SELF_TYPE {
        c
    };
    get_d() : Int {
        d
    };
    set_d(num : Int) : Int {
        d <- num
    };
    name : String <- "vinced";
    -- main is what is executed so just fill this with test cases as necessary, comments show expected output and behavior
    main() : Object {
        -- out_string("hello world\n")
        -- let new_main : Main <- new Main in {
        --     out_int(new_main.get_d()); -- 4
        --     new_main.set_d(5);
        --     out_int(new_main.get_d()); -- 5
        -- }
        -- let i : Int <- a in { a <- a + 2; out_int(i); i; };
        -- {
        --     -- not fine
        --     case new Main of
        --         x: Main => case new Main of
        --             x: Main => 0;
        --             -- z: Object => 0;
        --         esac;
        --         z: Object => 0;
        --     esac;
        -- }
        -- {
        --     -- fine
        --     case new Main of
        --         x: Main => case new Main of
        --             x: Main => 0;
        --             -- z: Object => 0;
        --         esac;
        --         -- z: Object => 0;
        --     esac;
        -- }
        {
            formal(999);
            
            out_int(formal_test(888));
            out_int(~1);

            self@A.hello_world(); -- hello
            out_int(self@Main.get_d());
            (new_Z()).hello_world(); -- z
            -- (new_me()).hello_world(); -- world
            {
                out_int(hello_world()); -- world, 1
            };
            self@A.hello_world(); -- hello
            (new_Z()).hello_world(); -- z
            out_int(add(3, 2)); -- 5
            out_int(mul(4, 5)); -- 20
            out_int(add_mul(6,7)); -- 42
            out_int(attr_mul(3)); -- 11*3 = 33
            out_int(a); -- 11
            out_string("\n");
            {
                b <- b * 2;
                out_int(b); --20
                out_int(new_me().get_b()); --10
            };
            let x : Int <- 44, y : Int <- 2 in out_int(x*y); -- 88
            {
                if true then out_int(2) else out_int(4) fi; -- 2
                if false then out_int(2) else out_int(4) fi; -- 4
                if not false then out_int(2) else out_int(4) fi; -- 2
                out_int(if true then 3 else 5 fi); -- 3
                out_int(a <- let x : Int <-3, y : Int <- 5 in y); -- 5
                let x : Int <-3, y : Int <- 5 in {
                    out_int(y);
                    out_int(x);
                }; -- 5
                out_int(let x : Int <-3, y : Int <- 5 in y); -- 5
            };
            {
                if 1 <= 1 then out_string("le") else out_string("nle") fi; -- le
                if 1 <= 2 then out_string("le") else out_string("nle") fi; -- le
                if 2 <= 1 then out_string("le") else out_string("nle") fi; -- nle
                if 1 < 1 then out_string("l") else out_string("nl") fi; -- nl
                if 1 < 2 then out_string("l") else out_string("nl") fi; -- l
                out_int(if 1 <= 1 then 6 else 7 fi); -- 6
            };
            {
                if (1 = 1) 
                then if (1 = 1)
                    then out_string("eq2")
                    else out_string("neq2") fi
                else out_string("neq") fi; -- nested if
                if (1 = 2) then out_string("eq") else out_string("neq") fi;
                if ("vincd" = "vince") then out_string("eq") else out_string("neq") fi; -- neq
                if ("vince" = "vince") then out_string("eq") else out_string("neq") fi; -- eq
                let name : String <- "vinced" in {
                    out_string(name.substr(0, name.length()-1));
                    out_string(name);
                };
                out_string(name.substr(0, name.length()-1));
                out_string(name);
                name <- name.substr(0, name.length()-1);
                out_string(name);
                out_string("\n");
                -- let name : String <- "vinced" in
                --     if (name.substr(0, name.length()-1) = "vince") then out_string("eq") else out_string("neq") fi; -- eq
                if (1 = 1) then out_string("eq") else out_string("neq") fi; -- eq
                if (2 = 1) then out_string("eq") else out_string("neq") fi; -- eq
                if (new A = new A) then out_string("eq") else out_string("neq") fi; -- eq
                if (new A = new Z) then out_string("eq") else out_string("neq") fi; -- neq
            };

            let x: Int <- 0 in 
            while x < 10 loop {
                out_int(x);
                out_string(", ");
                x <- x + 1;
            } pool;

            
            {
                case new Main of
                    z: Z => 0;
                    new_main: Main => {
                        out_int(new_main.get_d()); -- 4
                        new_main.set_d(5);
                        out_int(new_main.get_d()); -- 5
                    };
                esac;

            };
            {
                self@A.hello_world(); -- hello
                case new Main of
                    z: Z => 0;
                    m : Main => new SELF_TYPE;
                    o : Object => new A;
                esac;
            };

            {
               -- easy
            case new Main of
                x: Main => 0;
                z: Object => 0;
            esac;
            };
            {
                -- nested case
                case new Main of
                    x: Main => case new Main of
                        z: Object => out_string("z\n");
                        x: Main => out_string("x\n");
                    esac;
                    z: Object => 0;
                esac;
            };
            {
                -- return case
                out_int(case new Main of
                    x: Main => case new Main of
                        x: Main => 1;
                    esac;
                    -- z: Object => 0;
                esac);
            };

            -- keep this last
            -- 3 errors 
            -- {
            --     -- case on void
            --     case c of
            --         z: Z => 0;
            --         new_c: Main => 0;
            --     esac;
            -- };
            -- {
            --     -- missing branch
            --     case new Main of
            --         z: Z => 0;
            --     esac;
            -- };
            {
                if (isvoid a) then out_string("void") else out_string("not_void") fi;
                if (isvoid c) then out_string("void") else out_string("not_void") fi;
                out_int(d);
                out_int(e);
                let voider : SELF_TYPE in if (isvoid voider) then out_string("void") else out_string("not_void") fi ;
                let voider : Int in out_int(voider); -- 0
                let voider : SELF_TYPE in voider.test(); -- dispatch to void
            };
            {
                if ("vince".substr(0, 1) = "vince".substr(0,1)) then out_string("true") else out_string("false") fi;
            };
        }
    };
    formal_test(x: Int) : Int {
        x
    };
    add(x: Int, y: Int) : Int {
        x + y
    };
    mul(x: Int, y: Int) : Int {
        x * y
    };
    add_mul(x: Int, y: Int) : Int {
        mul(add(x, 0), y)
    };
    attr() : Int {
        a
    };
    attr_mul(x: Int) : Int {
        a * x
    };
};


-- recursion test
-- class Main inherits IO {
--     recurse(n : Int) : Object {
--         {
--             if (0 < n) then
--             recurse(n-1)
--             else new Object fi;
--             out_int(n);
--             out_string(", ");
--         }
--     };
--     main() : Object {
--         recurse(10)
--     };
-- };

-- assignment with attribute inheritance
-- class A inherits IO {
--    a : Int <- 10;
-- };
-- class Main inherits A {
--    b : Int <- a;
--    main() : Object {
--       {
--          out_int(b);
--       }
--    };
-- };
