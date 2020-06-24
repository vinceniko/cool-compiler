-- inheritance
class D inherits C {};
class F inherits D {};

class A inherits C {};
class B inherits C {};

Class Main {
	main():C { new C };
	dispatch():C {
		-- dispatch
		(new C).init(1,true)
	};
	sdispatch() : C {
		(new C)@C.init(1, true)
	};
};

class C {
	a : Int <- 2;
	b : Bool;
	init(x : Int, y : Bool) : C {
		{
			a <- x;
			b <- y;
			self;
		}
	};
	self_type_return() : SELF_TYPE {
		self
	};
	self_return() : C {
		self
	};
	self() : C {
		new SELF_TYPE
	};
	if_else_LCA() : C {
		-- type check as LCA which is C
		if 0 = 0 then new A else new B fi
	};
	let_block(x: Int) : Int {
		let x : Int <- 1 in {
			x;
		}
	};
	arith() : Int {
		{
			1 * 0;
			1 + 0;
			1 / 0;
			1 - 0;
			~1;
		}
	};
	case_check() : Int {
		case a of
			num: Bool => 0;
			num: Int => num;
		esac
	};
};
