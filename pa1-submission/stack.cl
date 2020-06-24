(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)

(*
   author: Vincent Nikolayev, vn500
*)

class Stack inherits IO {
   top : StackNode;

   push(entity : StackObject) : Object {
      -- push builds a StackNode from the StackObject and pushes it to the stack
      (let node : StackNode <- (new StackNode).init(entity) in {
         node.set_prev(top);
         top <- node;
      })
   };
   pop() : StackObject {
      -- pop returns the underlying StackObject and not the StackNode container
      let old_top : StackObject <- top.get_data() in {
         top <- top.get_prev();
         
         old_top;
      }
   };
   print() : Object {
      let curr : StackNode <- top in {
         while not (isvoid curr) loop {
            curr.get_data().print();
            out_string("\n");
            curr <- curr.get_prev();
         }
         pool;
      }
   };
   
   -- getters, setters
   get_top() : StackNode {
      top
   };
};

Class StackNode {
   -- StackNode is the container that is placed on the stack and holds Stack-Entities (objects and commands)
   data : StackObject;
   prev : StackNode;

   init(cmd : StackObject) : StackNode {
      { data <- cmd; self; }
   };

   -- getters, setters
   get_data() : StackObject {
      data
   };
   get_prev() : StackNode {
      prev
   };
   set_prev(new_prev : StackNode) : Object {
      prev <- new_prev
   };
};

class StackEntity inherits IO {
   -- StackEntity is the base class for Stack-Objects and Stack-Commands
   -- should be treated as an abstract class
   symbol : String;

   from(symbol : String) : StackEntity {
      -- from takes a symbol, creates the specific StackEntity corresponding to the symbol, and returns it cast as StackEntity
      if symbol = "" then (new NullCmd) else  -- fix: empty strings were saved as ints; fail silently
      if symbol = "+" then (new PlusCmd).set_symbol(symbol) else -- set the symbol here to avoid specifying an attribute for each subclass
      if symbol = "s" then (new SwapCmd).set_symbol(symbol) else
      if symbol = "e" then (new EvalCmd) else
      if symbol = "d" then (new DisplayCmd) else
      if symbol = "x" then (new NullCmd)
         else (new IntObject).set_symbol(symbol)  -- assumes all other inputs are ints
      fi fi fi fi fi fi
   };
   exec_from_io(stack : Stack) : Object {
      -- exec_from_io is executed when receiving a StackEntity from I/O, i.e. stdin
      -- it has a stack param so that the stack can be operated on
      -- in the base class it is treated as a pure virtual function
      s_abort("exec_from_io")
   };
   s_abort(msg : String) : Object {
      -- aborts with string message argument
      {
         out_string("Called pure virtual function: ");
         out_string(msg);
         abort();  -- do nothing
      }
   };
};

class StackObject inherits StackEntity {
   -- a StackObject is a StackEntity that should be placed onto the stack, i.e. int, "+", "s"
   -- treated as an abstract class
   
   set_symbol(new_symbol : String) : SELF_TYPE {
   -- set_symbol is functionally equivalent to init but named as set_symbol for clarity. uses builder pattern
      { symbol <- new_symbol; self; }
   };
   print() : Object {
      out_string(symbol)
   };
   exec_from_io(stack : Stack) : Object {
      -- exec_from_io is called when receiving from I/O, and pushes onto Stack
      stack.push(self)
   };
};

class IntObject inherits StackObject {
   -- IntObject is an int

   convert() : Int {
      (new A2I).a2i(symbol)
   };
   add(right : IntObject) : IntObject {
      (new IntObject).set_symbol(
         (new A2I).i2a(convert() + right.convert())
      )
   };
};

class StackCmd inherits StackObject {
   -- a StackCmd is a Base class for StackObjects which can be executed from inside of the stack when an "e" is input
   -- treated as abstract class

   exec_from_stack(stack : Stack) : Object {
      -- exec_from_stack is treated as a pure virtual function
      s_abort("exec_from_stack")  -- do nothing
   };
};

class PlusCmd inherits StackCmd {
   -- PlusCmd is a "+"

   exec_from_stack(stack : Stack) : Object {
      {
         -- exec_from_stack adds the two proceeding ints after the PlusCmd (self)
         stack.pop(); -- exec_from_stack called only when self is on top of stack, so pop self off

         -- generates runtime error if any of two IntEntities expected underneath are missing
         -- assumes that the two StackObjects underneath are ints, otherwise aborts due to void dispatch runtime error
         -- (I believe the assignment said that no error checking was needed on this)
         case stack.get_top().get_data() of 
            i1: IntObject => 
               case stack.get_top().get_prev().get_data() of
                  i2: IntObject => {
                     stack.pop();
                     stack.pop();

                     stack.push(i1.add(i2));
                  };
         esac;
         esac;
      }
   };
};

class SwapCmd inherits StackCmd {
   -- SwapCmd is "s"
   
   exec_from_stack(stack : Stack) : Object {
      {
         -- swap two proceeding StackObjects
         stack.pop();  -- exec_from_stack called only when self is on top of stack, so pop self off

         -- assumes that there are at least two items in the stack, if not, fails with void dispatch error
         -- (I believe the assignment said that no error checking was needed on this)
         let first : StackObject <- stack.pop() in  -- first from top
         let sec : StackObject <- stack.pop() in {
            stack.push(first);
            stack.push(sec);
         };
      }
   };
};

class DisplayCmd inherits StackEntity {
   --DisplayCmd is "d"

   exec_from_io(stack : Stack) : Object {
      -- print out stack
      stack.print()
   };
};

class EvalCmd inherits StackEntity {
   -- EvalCmd is "e"

   exec_from_io(stack : Stack) : Object {
      -- exec_from_io calls exec_from_stack on the top StackEntity in the stack (if it is a stack command and if the stack is not empty); fails silently (continues to run) otherwise, i.e. "stack is left unchanged"
      if not (isvoid stack.get_top()) then
         case stack.get_top().get_data() of 
            cmd : StackCmd => cmd.exec_from_stack(stack);
            foo : Object => new Object;  -- default case: do nothing; silently fail
         esac
      else
         new Object  -- default case: do nothing; silently fail
      fi
   };
};

class NullCmd inherits StackEntity {
   -- NullCmd is used for "x"

   exec_from_io(stack : Stack) : Object { 
      -- could've modified to abort, but the assignment specified exiting gracefully, so I opted for the ExitCmd to do nothing and for main to check for the "x"
      new Object  -- do nothing
   };
};

class Main inherits IO {
   stack : Stack;

   main() : Object {
      {
         stack <- new Stack;
         let in_str : String in {
            while not (in_str = "x") loop {  -- to exit gracefully
               out_string(">");
               in_str <- in_string();
               (new StackEntity).from(in_str).exec_from_io(stack);
               -- DEBUG: out_string("\n");
            }
            pool;
         };
      }
   };
};
