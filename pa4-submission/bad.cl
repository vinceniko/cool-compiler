class C {
	a : Int;
	b : Bool;
	-- multiply defined
	a: bool;
	-- inferred type does not match init of attr
	c: Int <- true;
	init(x : Int, y : Bool) : C {
		{
			a <- x;
			b <- y;
			-- assignment to undeclared var
			z <- 1;
			self;
		}
	};
	-- redefinition of method
	init() : C {
		-- undeclared identifier
		z
	};
	-- new with undefined class
	d: Int <- new D;
};

-- redefinition
class C {};
class C {};

class H {
	-- cant return the type of Self in place of SELF_TYPE
	-- self cannot be the name of a formal
	init(x : Int, y : Bool, self: Int) : SELF_TYPE {
		new H
	};
};


-- cant inherit from self_type
class G inherits SELF_TYPE {};

-- inheritance cycle
class A inherits B {};
class B inherits A {};

-- inheritance cycle
class D inherits F {};
class E inherits D {};
class F inherits E {};

-- inherit from undefined class
class Y inherits Z {};  -- NOTE: triggered after the class C redefinition, and then exits. removing this still triggers class C errors which do not go onto inheritance cycle errors

-- inherit from builtin
class Int1 inherits Int {};
class Bool1 inherits Bool {};
class String1 inherits String {};

Class Main inherits C {
	-- redefinition of inherited attribute
	a : Bool;
	main():C {
	 {
		-- dispatch error
		-- dispatch with incorrect formals	
		(new C).init(1,1);
		(new C).init(1,true,3);
		-- same with static dispatch
		(new C)@C.init(1,1);
		(new C)@C.init(1,true,3);
		-- undefined method
		(new C).iinit(1,true);
		(new C);
		}
	};
	-- redefining inherited method with different number of formals and different formal types and different return types
	init(x : Bool) : Main { 
		new Main
	};
	-- sdispatch to undefined class
	sdispatch_undefined_test: Int <- (1)@Z.z();
	-- undefined return type
	foo() : Z {};
};

-- Main with no main method defined
class Main {};

class Main {
	-- main should not have args
	main(x : Int) : Int { 
		0 
	};
};

-- surpisingly this is fine, returning an instance of Main
class Main {
	main() : Main {
		new Main
	};
};

class X {
	-- below are expr with operands that dont conform
	not_test: Bool <- not 1;
	neg_test: Int <- ~not_test;
	plus_test: Int <- 1 + not_test;
	sub_test: Int <- 1 - not_test;
	mul_test: Int <- 1 * not_test;
	div_test: Int <- 1 / not_test;
	lt_test: Bool <- 1 < not_test;
	leq_test: Bool <- 1 <= not_test;
	-- non bool type in if
	if_test: Bool <- if 1 then true else false;
	case_test: Bool <- case a of
		-- dupe branch
		num: Int => 0;
		num: Int => num;
	esac;
	-- undeclared type in let init
	let_test: Int let x: Z <- 1 in { x };
};