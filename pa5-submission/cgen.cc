
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;
int curr_lineno;

CgenClassTable* classtable;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       copy,
       Int,
       in_int,
       in_string,
       IO,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       Object,
       out_int,
       out_string,
       prim_slot,
       self,
       SELF_TYPE,
       Str,
       str_field,
       substr,
       type_name,
       val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any 
//   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  // os << "# start of generated code\n";

  initialize_constants();
  classtable = new CgenClassTable(classes,os);
  classtable->code();
  classtable->exitscope();

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

// takes into account wordsize
static void emit_load_wordsize(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

// takes into account wordsize
static void emit_store_wordsize(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& s)
{
  emit_store_wordsize(reg,0,SP,s);
  emit_addiu(SP,SP,-4,s);
}

// discards val from stack
static void emit_pop(ostream& s)
{
  emit_addiu(SP,SP,4,s);
}

static void emit_pop(char *reg, ostream& s)
{
  emit_load_wordsize(reg,1,SP,s);
  emit_pop(s);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load_wordsize(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store_wordsize(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load_wordsize(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  /*if (source != A1)*/ emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl; // size

// DONE:
 /***** Add dispatch information for class String ******/
  s << WORD << "String" << DISPTAB_SUFFIX << endl; // dispTab

      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl;  // object size

// DONE:
 /***** Add dispatch information for class Int ******/
  s << WORD << "Int" << DISPTAB_SUFFIX << endl; // dispTab


      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl;   // object size

// DONE:
 /***** Add dispatch information for class Bool ******/
  s << WORD << "Bool" << DISPTAB_SUFFIX << endl; // dispTab


      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  // VIN: these are also constants defined above so i don't think that looking them up is necessary
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{

   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();

  // VIN: put over here to set tag based on inheritance heirarchy
   stringclasstag = this->get_class_tag(Str); /* Change to your String class tag here */;
   intclasstag =    this->get_class_tag(Int); /* Change to your Int class tag here */;
   boolclasstag =   this->get_class_tag(Bool); /* Change to your Bool class tag here */;
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
     class_(IO, 
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
      class_(Str, 
	     Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat, 
				   single_Formals(formal(arg, Str)),
				   Str, 
				   no_expr()))),
	    single_Features(method(substr, 
				   append_Formals(single_Formals(formal(arg, Int)), 
						  single_Formals(formal(arg2, Int))),
				   Str, 
				   no_expr()))),
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

CgenNodeP CgenClassTable::get_class(Symbol _find) {
  if (_find == SELF_TYPE) {
    _find = this->_class;
  }
    
  CgenNodeP out = nullptr;
  nodes_iter(
      if (_find == _class->name) {
        out = _class;
        early_break = true;
      }
  );
  return out;
}

int CgenClassTable::get_class_tag(Symbol _find) {
  int out = -1;
  nodes_iter(
      if (_find == _class->name) {
        out = i;
        early_break = true;
      }
  );
  return out;
}


void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

template<class Entry>
ostream& code_entry(Entry entry, ostream& o) {
  o << WORD;
  entry->code_ref(o); // prints index of entry
  o << endl;
  return o;
}

template<class Elem>
ostream& code_lookup_entry(StringTable<Elem> table, char *s, ostream& o) {
  return code_entry(table.lookup_string(s), o);
}

// i.e.
// class_nameTab:
// 	.word	str_const2
// 	.word	str_const3
void CgenClassTable::class_nameTab() {
  this->str << CLASSNAMETAB << LABEL;
  nodes_iter(
    classtable->_class = _class->get_name();
    Symbol class_sym = _class->get_name();
    // StringEntry* entry = stringtable.lookup_string(class_sym->get_string());
    // this->str << WORD;
    // entry->code_ref(this->str); // prints index of entry
    // this->str << endl;
    code_lookup_entry(stringtable, class_sym->get_string(), this->str);
  );
}

void CgenClassTable::class_objTab() {
  this->str << CLASSOBJTAB << LABEL;
  nodes_iter(
    classtable->_class = _class->get_name();
    this->str << WORD << _class->get_name() << PROTOBJ_SUFFIX << endl;
    this->str << WORD << _class->get_name() << CLASSINIT_SUFFIX << endl;
  );
}

FeatureCol<attr_class> CgenNode::get_this_attrs() {
  FeatureCol<attr_class> attrs;
  features_iter(
    auto attr = dynamic_cast<attr_class *>(feature);
    if (attr != NULL) {
      attrs.push_back({this, attr});
    }
  );
  return attrs;
}

FeatureCol<attr_class> CgenNode::get_all_attrs() {
  FeatureCol<attr_class> this_attrs = this->get_this_attrs();
  if (this->get_parentnd() != nullptr) {
    this_attrs.splice(this_attrs.begin(), this->get_parentnd()->get_all_attrs()); // recurse
  }
  return this_attrs;
}

FeatureCol<method_class> CgenNode::get_this_methods() {
  FeatureCol<method_class> methods;
  features_iter(
    auto attr = dynamic_cast<method_class *>(feature);
    if (attr != NULL) {
      methods.push_back({this, attr});
    }
  );
  return methods;
}

FeatureCol<method_class> CgenNode::get_all_methods() {
  FeatureCol<method_class> this_methods = this->get_this_methods();
  if (this->get_parentnd() != nullptr) {
    // std::vector<FeatureElem<method_class>> out;
    auto parent_methods = this->get_parentnd()->get_all_methods();
    // for (auto p_method : parent_methods) {
    //   out.push_back(p_method);
    // }
    for (auto t_method : this_methods) {
      bool found = false;
      for (auto p_method = parent_methods.begin(); p_method != parent_methods.end(); p_method++) {
        if (p_method->feature->name == t_method.feature->name) {
          p_method->feature = t_method.feature; // replace
          p_method->owner = t_method.owner; // replace
          found = true;
          break;
        }
      }
      if (!found) {
        parent_methods.push_back(t_method);
      }
    }
    return parent_methods;
  }
  return this_methods;
}

void CgenClassTable::protObjs_attr(attr_class *attr_) {
  // literals before the "else" branch
  if (attr_->type_decl == Int) {
    code_lookup_entry(inttable, "0", this->str);
  } else if (attr_->type_decl == Bool) {
    code_entry(&falsebool, this->str);
  } else if (attr_->type_decl == Str) {
    code_lookup_entry(stringtable, "", this->str);
  } else { // some object, so give void
    this->str << WORD << 0 << endl;
  }
}

// reverse iter towards ancestors
template <typename FeatureType>
int FeatureCol<FeatureType>::_get_offset(Symbol s) {
  int i = this->size()-1;
  for (auto it = --this->end(); it != this->begin(); --it, --i) {
    FeatureType *_feature = it->feature;
    if (s == _feature->name) {
      break;
    }
  }
  return i;
}

template <typename FeatureType>
FeatureType* FeatureCol<FeatureType>::get(Symbol s) {
  for (auto it = --this->end(); it != this->begin(); --it) {
    FeatureType *_feature = it->feature;
    if (s == _feature->name) {
      return _feature;
    }
  }
  return nullptr;
}


// used for method offset in protOb
template <>
int FeatureCol<method_class>::get_offset(Symbol s) {
  return this->_get_offset(s);
}

// used for attribute offset in protOb
template <>
int FeatureCol<attr_class>::get_offset(Symbol s) {
  return 3 + this->_get_offset(s);
}

std::map<Symbol, int> CgenClassTable::get_class_tags() {
  std::map<Symbol, int> out;
  nodes_iter (
    out.insert( std::pair<Symbol, int>(_class->get_name(), i) );
  );
  return out;
}

InheritanceRange CgenClassTable::get_inheritance_range(Symbol _class) {
  return this->get_class(_class)->get_inheritance_range(this->get_class_tags());
}

void CgenClassTable::dispTabs() {
  nodes_iter(
    classtable->_class = _class->get_name();

    this->str << _class->get_name() << DISPTAB_SUFFIX << LABEL;
    std::list<FeatureElem<method_class>> methods = _class->get_all_methods();
    for (auto method_col: methods) {
      this->str << WORD << method_col.owner->get_name() << '.' << method_col.feature->name << endl;
    }
  );
}

void CgenClassTable::protObjs() {
  nodes_iter(
    classtable->_class = _class->get_name();

    FeatureCol<attr_class> attrs = _class->get_all_attrs();
    this->str << WORD << -1 << endl;  // gc
    this->str << _class->get_name() << PROTOBJ_SUFFIX << LABEL;  // title
    this->str << WORD << i << endl; // class tag
    this->str << WORD <<  3 + attrs.size() << endl; // obj size; 3 + counts the other object header fields
    this->str << WORD << _class->get_name() << DISPTAB_SUFFIX << endl; // dispTab

    // attrs
    // primitive types covered in starter code; install_basic_classes
    for (auto attr_col: attrs) {
      this->protObjs_attr(attr_col.feature);
    }
  );
}

static void emit_addiu_wordsize(char *dest, char *src1, int imm, ostream& s) {
  emit_addiu(dest, src1, imm * WORD_SIZE, s);
}

void emit_prologue(ostream& str) {
  // str << "#prologue" << endl;

  // addiu	$sp $sp -12
	// sw	$fp 12($sp)
	// sw	$s0 8($sp)
	// sw	$ra 4($sp)
	// addiu	$fp $sp 4
	// move	$s0 $a0
  
  emit_addiu_wordsize(SP, SP, -3, str);
  emit_store_wordsize(FP, 3, SP, str);
  emit_store_wordsize(SELF, 2, SP, str);
  emit_store_wordsize(RA, 1, SP, str);
  emit_addiu_wordsize(FP, SP, 0, str);
  emit_move(SELF, ACC, str);  // init self which is passed in a0
}

void emit_epilogue(ostream& str) {
  // str << "#epilogue" << endl;

  // move	$a0 $s0
	// lw	$fp 12($sp)
	// lw	$s0 8($sp)
	// lw	$ra 4($sp)
	// addiu	$sp $sp 12
	// jr	$ra
  
  emit_move(ACC, SELF, str);  // return self
  emit_load_wordsize(FP, 3, SP, str);
  emit_load_wordsize(SELF, 2, SP, str);
  emit_load_wordsize(RA, 1, SP, str);
  emit_addiu_wordsize(SP, SP, 3, str);
  emit_return(str); 
}

void emit_method_prologue(ostream& str) {
  // str << "#method_prologue" << endl;

  // addiu	$sp $sp -12
	// sw	$fp 12($sp)
	// sw	$s0 8($sp)
	// sw	$ra 4($sp)
	// addiu	$fp $sp 4
  
  emit_addiu_wordsize(SP, SP, -3, str);
  emit_store_wordsize(FP, 3, SP, str);
  emit_store_wordsize(SELF, 2, SP, str);
  emit_store_wordsize(RA, 1, SP, str);
  emit_addiu_wordsize(FP, SP, 0, str);  // point behind return
  emit_move(SELF, ACC, str); // TODO: delete?
}

void emit_method_epilogue(ostream& str, int len_formals) {
  // str << "#method_epilogue" << endl;
	
  // lw	$fp 12($sp)
	// lw	$s0 8($sp)
	// lw	$ra 4($sp)
	// addiu	$sp $sp 12
	// jr	$ra
  
  // emit_move(ACC, SELF, str); // delete this since ACC will hold the result of the method
  emit_load_wordsize(FP, 3, SP, str);
  emit_load_wordsize(SELF, 2, SP, str);
  emit_load_wordsize(RA, 1, SP, str);
  emit_addiu_wordsize(SP, SP, 3 + len_formals, str);
  emit_return(str); 
}

void emit_class_init(CgenNodeP _class, ostream& s) {
  std::string class_init_label = _class->get_name()->get_string();
  class_init_label += CLASSINIT_SUFFIX;
  emit_jal(const_cast<char *>(class_init_label.c_str()), s);
}

void CgenClassTable::inits() {
  // addiu	$sp $sp -12
	// sw	$fp 12($sp)
	// sw	$s0 8($sp)
	// sw	$ra 4($sp)
	// addiu	$fp $sp 4
	// move	$s0 $a0
  // 
	// jal	Object_init
  // 
	// move	$a0 $s0
	// lw	$fp 12($sp)
	// lw	$s0 8($sp)
	// lw	$ra 4($sp)
	// addiu	$sp $sp 12
	// jr	$ra	

  
  nodes_iter(
    classtable->fn_ctx.enterscope();
    classtable->_class = _class->get_name();

    this->str << _class->get_name() << CLASSINIT_SUFFIX << LABEL;
    emit_prologue(this->str);

    // init ancestor
    if (_class->get_name() != Object) {
      emit_class_init(_class->get_parentnd(), this->str);
    }
    // code gen for attributes init exprs
    // this could be cleaner i.e. attributes are loaded again into the symbol table when methods() is run which may be wasteful
    // but to generate code to assign to the attribute necessitated first entering the attribute into the symbol table here so that during initialization all the attributes defined in the class can be referenced in any attribute initilization
    // i.e. class A { a <- b; b <- a; };
    FeatureCol<attr_class> attrs = _class->get_all_attrs();
    // add attributes to scope
    for (auto attr_col: attrs) {
      attr_class* _attr = attr_col.feature;
      classtable->fn_ctx.add(_attr->name, AttrBindType);
    }

    attrs = _class->get_this_attrs();
    // gen init code for attribute
    for (auto attr_col: attrs) {
      attr_class* _attr = attr_col.feature;
      
      // if init expr is not no_expr then some result is in a0, so store to s0 (otherwise keep default init)
      bool has_expr = dynamic_cast<no_expr_class*>(_attr->init) == nullptr;  // 0 is returned when dynamic cast fails
      if (has_expr) {
        _attr->init->code(this->str); // result is in a0
        classtable->fn_ctx.lookup(_attr->name)->emit_store_into(this->str); // store result of a0 in s0
      }
    }

    emit_epilogue(this->str);

    classtable->fn_ctx.exitscope(AttrBindType);
  );
}

void CgenClassTable::methods() {
  nodes_iter(
    classtable->fn_ctx.enterscope();
    classtable->_class = _class->get_name();

    if (_class->get_name() == Str || _class->get_name() == Object || _class->get_name() == IO) {
      return; // equivalent of break in the node_iter macro
    };
    
    this->_class = _class->get_name();

    // populate symbol table with attributes (including ancestors)
    FeatureCol<attr_class> attrs = _class->get_all_attrs();
    for (auto attr_col: attrs) {
      attr_class* _attr = attr_col.feature;
      classtable->fn_ctx.add(_attr->name, AttrBindType);
    }

    // generate code for this class' methods
    FeatureCol<method_class> methods = _class->get_this_methods();
    for (auto method_col : methods) {
      classtable->fn_ctx.enterscope();

      method_class* _method = method_col.feature;

      this->str << _class->get_name() << "." << _method->name << LABEL;
      emit_method_prologue(this->str);

      // int len_formals = _method->formals->len();
      int len_formals = 0;
      provided_list_iter(_method->formals) {
        len_formals++;
      }
      for (int curr_len_formals = len_formals-1; curr_len_formals >= 0; --curr_len_formals) {
        Formal _formal= _method->formals->nth(curr_len_formals);
        classtable->fn_ctx.add(_formal->get_name(), FormalBindType); // store symbol
      }
      
      _method->expr->code(this->str);

      // clean up params
      emit_method_epilogue(this->str, len_formals);

      classtable->fn_ctx.exitscope(FormalBindType);
    }
    classtable->fn_ctx.exitscope(AttrBindType);
  );
}

void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

// DONE:
//                 Add your code to emit
//                   - prototype objects DONE
//                   - class_nameTab DONE
//                   - dispatch tables DONE
//
  this->class_nameTab();
  this->class_objTab();
  this->dispTabs();
  this->protObjs();

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

// DONE:
//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...

  this->inits();
  this->methods();
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

// load into a reg. default is into acc
void VarBinding::emit_load_from(ostream &s, char* into_reg) {
  emit_load_wordsize(into_reg, get_offset(), reg, s);
  // reg = _reg; // ACC
}

// store into the reg. default is from acc
void VarBinding::emit_store_into(ostream &s, char* from_reg) {
  emit_store_wordsize(from_reg, get_offset(), reg, s);
}

void emit_object_copy(ostream& s) {
  emit_jal("Object.copy", s);
}

void assign_class::code(ostream &s) {
  this->expr->code(s); // result is in ACC
  classtable->fn_ctx.lookup(this->name)->emit_store_into(s);
}

void emit_void_err(char *abort, int label, tree_node *node, ostream &s) {
  emit_bne(ACC, ZERO, label, s); // Abort if self (stored in $s0) is null
  emit_load_address(ACC, "str_const0", s);
  emit_load_imm(T1, node->get_line_number(), s);
  emit_jal(abort, s);
}

template <typename DispatchType>
void emit_dispatch(DispatchType dispatch, Symbol callee_class, ostream& s) {
  Expressions arg_exprs = dispatch->actual;
  provided_list_iter(arg_exprs) {
    Expression arg_expr = arg_exprs->nth(i);
    arg_expr->code(s); // result saved in a0
    emit_push(ACC, s); // store arg on stack
  }
  // emit_move(SELF, ACC, s); // called by caller not this (callee)
  dispatch->expr->code(s);

  int curr_label = classtable->label_no++;
  // void check on calling object
  emit_void_err("_dispatch_abort", curr_label, dispatch, s);

  // not void
  emit_label_def(curr_label, s);
  if (dynamic_cast<dispatch_class *>(dispatch) != nullptr) {
    emit_load_wordsize(T1, 2, ACC, s); // load address of callee's vmt from a0
  } else { // static dispatch
    auto disptab = std::string(callee_class->get_string()) + DISPTAB_SUFFIX; // load the disptab
    emit_load_address(T1, const_cast<char *>(disptab.c_str()), s); // load address of callee's vmt from a0
  }
  
  CgenNodeP _class = classtable->get_class(callee_class); // takes care of SELF_TYPE
  if (_class == nullptr) { // DEBUG, shouldn't execute
    cout << "get_class returned nullptr for: " << callee_class << endl;
    exit(1);
  }
  // load the method from the vmt below
  FeatureCol<method_class> methods = _class->get_all_methods();
  int offset = methods.get_offset(dispatch->name);
  emit_load_wordsize(T1, offset, T1, s); // load address of method
  emit_jalr(T1, s);  // dispatch
}

void static_dispatch_class::code(ostream &s) {
  emit_dispatch(this, this->type_name, s);
}

void dispatch_class::code(ostream &s) {
  emit_dispatch(this, this->expr->type, s);
}

void cond_class::code(ostream &s) {
  // 	lw	$t1 12($a0)
  // 	beqz	$t1 label4
  // 	la	$a0 int_const4
  // 	b	label5
  // label4:
  // 	la	$a0 int_const6
  // label5:
  // 	lw	$fp 12($sp)
  // 	lw	$s0 8($sp)
  // 	lw	$ra 4($sp)
  this->pred->code(s); // condition in a0

  emit_load_wordsize(T1, 3, ACC, s); // load bool val
  int f_label = classtable->label_no++;
  emit_beqz(T1, f_label, s);
  // true content
  this->then_exp->code(s);
  int t_label = classtable->label_no++;
  emit_branch(t_label, s); // to jump over false content
  // false content
  emit_label_def(f_label, s);
  this->else_exp->code(s);
  // ends
  emit_label_def(t_label, s);
}

void loop_class::code(ostream &s) {
  // label4:
  //   la	$a0 bool_const0
  //   lw	$t1 12($a0)
  //   beq	$t1 $zero label5
  //   la	$a0 int_const3
  //   b	label4
  // label5:
  //   move	$a0 $zero

  int loop_label = classtable->label_no++;
  int end_label = classtable->label_no++;
  emit_label_def(loop_label, s);

  this->pred->code(s);
  emit_load_wordsize(T1, 3, ACC, s); // load bool val
  emit_beqz(T1, end_label, s);
  this->body->code(s);
  emit_branch(loop_label, s);

  emit_label_def(end_label, s);
  emit_move(ACC, ZERO, s);
}

struct TaggedBranch {
  branch_class* branch;
  int tag;
  InheritanceRange tag_range;
};

bool operator<(const TaggedBranch& l, const TaggedBranch& r) {
  return l.tag_range.max - l.tag_range.min < r.tag_range.max - r.tag_range.min;
}

void typcase_class::code(ostream &s) {
  // case c of
  //   new_c: Main => 0;
  // esac
  // 
  //   sw	$s1 0($fp)
  //   lw	$a0 28($s0) # load c attr
  //   bne	$a0 $zero label5 # check if c is void
  //   la	$a0 str_const0 # void stuff
  //   li	$t1 56 # void stuff
  //   jal	_case_abort2 # void stuff
  // label5: # not void
  //   lw	$t2 0($a0) # load c's class tag
  //      # NOTE: 4 and 4 is actually a range that refers to the class min and max descendant tag, so object is the 0..(number of classes).
    //   blt	$t2 4 label6 # branch if c tag less than 4 which is the class tag for branch
    //   bgt	$t2 4 label6 # this and the above line are equivalent of neq; then move to next branch or abort if matching branch not found
  //   move	$s1 $a0 # store c
  //   la	$a0 int_const1 # return 0
  //   b	label4
  // label6:
  //   jal	_case_abort
  // label4:
  //   lw	$s1 0($fp)
  //   lw	$fp 16($sp)
  //   lw	$s0 12($sp)
  //   lw	$ra 8($sp)
  //   addiu	$sp $sp 16
  //   jr	$ra	

  this->expr->code(s);
  emit_push(ACC, s); // store for var binding
  // void check
  int matched_label = classtable->label_no++;
  emit_void_err("_case_abort2", classtable->label_no, this, s);
  emit_label_def(classtable->label_no++, s);

  // the below sorts the branches on class tag in descending order such that the most specific classes (those with the least descendants) come first
  // this is so the least common ancestor branch is chosen
  std::vector<TaggedBranch> sorted_branches;
  provided_list_iter(this->cases) {
    branch_class* branch = dynamic_cast<branch_class *>(this->cases->nth(i));

    sorted_branches.push_back(
      TaggedBranch{
        branch, 
        classtable->get_class_tag(branch->type_decl), 
        classtable->get_inheritance_range(branch->type_decl)
      }
    );
  }
  std::sort(sorted_branches.begin(), sorted_branches.end(), operator<);

  int j = 0;
  for (auto tagged_branch: sorted_branches) {
    classtable->fn_ctx.enterscope();

    if (j++ == 0) emit_load_wordsize(T2, 0, ACC, s); // load obj class tag, TODO: can T2 get overwritten in future branch cgen? see below push/pop comments

    // compare tags
    int next_label = classtable->label_no++;
    emit_blti(T2, tagged_branch.tag_range.min, next_label, s);
    emit_bgti(T2, tagged_branch.tag_range.max, next_label, s);
    // emit_push(T2, s); // doesnt fix hairscary and not used by refimpl

    classtable->fn_ctx.add(tagged_branch.branch->name, LocalBindType);
    tagged_branch.branch->expr->code(s);
    // emit_pop(T2, s);

    emit_branch(matched_label, s);

    classtable->fn_ctx.exitscope(LocalBindType);

    emit_label_def(next_label, s);
  }

  emit_jal("_case_abort", s);
  emit_label_def(matched_label, s);
  emit_pop(s); // remove expr
}

void block_class::code(ostream &s) {
  provided_list_iter(this->body) {
    auto expression = this->body->nth(i);
    expression->code(s);
  }
}

void emit_prim_init(Symbol type, ostream& s) {
  if (type == Int) {
    emit_load_int(ACC, inttable.lookup_string("0"), s);
  }
  else if(type == Bool) {
    emit_load_bool(ACC, falsebool, s);
  }
  else if (type == Str){
    emit_load_string(ACC, stringtable.lookup_string(""), s);
  }
  else {
    emit_load_imm(ACC, 0, s);
  }
}

void let_class::code(ostream &s) {
  classtable->fn_ctx.enterscope();

  bool is_no_expr = dynamic_cast<no_expr_class *>(this->init) != nullptr; // 0 is returned when dynamic cast fails
  if (is_no_expr) {
    emit_prim_init(this->type_decl, s);
  } else {
    this->init->code(s); // result in a0
  }

  emit_push(ACC, s);
  classtable->fn_ctx.add(this->identifier, LocalBindType);
  
  this->body->code(s);

  emit_pop(s);
  
  classtable->fn_ctx.exitscope(LocalBindType);
}

template <typename Func, typename arith_node>
void emit_arith(Func emit_arith_func, arith_node node, ostream &s) {
  node->e1->code(s); // result of e1 in a0
  emit_push(ACC, s); // push e1 to stack
  
  node->e2->code(s);  // result of e2 in a0
	emit_object_copy(s); // copy e2, store in acc

  emit_pop(T1, s); // load e1 from stack to T1 and pop

  // ints are objects so load the attr
  emit_load_wordsize(T1, 3, T1, s); // load e1 copy attr
  emit_load_wordsize(T2, 3, ACC, s); // dont overwrite ACC since its an object that needs to get the result so e2 attr store to t2
  emit_arith_func(T1, T1, T2, s); // add e1 and e2 attr
  emit_store_wordsize(T1, 3, ACC, s);  // store back into e1 copy (ACC) to create result
}

void plus_class::code(ostream &s) {
  emit_arith(emit_add, this, s);
}

void sub_class::code(ostream &s) {
  emit_arith(emit_sub, this, s);
}

void mul_class::code(ostream &s) {
  emit_arith(emit_mul, this, s);
}

void divide_class::code(ostream &s) {
  emit_arith(emit_div, this, s);  // div by 0 covered by runtime or os
}

void neg_class::code(ostream &s) {
  this->e1->code(s); // result in a0

  emit_jal("Object.copy", s); // copy
	emit_load_wordsize(T1, 3, ACC, s); // load the copy
  emit_neg(T1, T1, s); // negate
	emit_store_wordsize(T1, 3, ACC, s); // store back the value into e1 at a0
}

enum CompType {
  PrimCompType, // prim types compare attributes
  ObjectCompType, // object types compare pointer addresses
};

// compares both the int attr values of prim types (use T1 and T2) and addresses of objects and strings (use T3 and T4)
template<typename EmitFunc>
void emit_comp(Expression e1, Expression e2, EmitFunc emit_func, CompType comp_type, ostream &s) {
  e1->code(s);
  if (comp_type != ObjectCompType) {
    emit_load_wordsize(ACC, 3, ACC, s); // get int val
  }
  emit_push(ACC, s);

  e2->code(s);
  if (comp_type == ObjectCompType) {
    emit_move(T2, ACC, s); // get object
  } else {
    emit_load_wordsize(T2, 3, ACC, s); // get int val
  }
  emit_pop(T1, s); // get pushed e1

  emit_load_bool(ACC, BoolConst(1), s);
  emit_func(T1, T2, classtable->label_no, s);
  emit_load_bool(ACC, BoolConst(0), s);
  emit_label_def(classtable->label_no++, s);
}

void lt_class::code(ostream &s) {
  // auto fn = [&]() { emit_blt(T1, T2, classtable->label_no, s); };
  emit_comp(this->e1, this->e2, emit_blt, PrimCompType, s);
}

void eq_class::code(ostream &s) {
  // prims
  if (this->e1->get_type() == Bool || this->e1->get_type() == Int || this->e1->get_type() == Str) {
    e1->code(s);
    emit_push(ACC, s);
    e2->code(s);
    emit_move(T2, ACC, s); // get object

    emit_pop(T1, s); // get object
    
    // covers all prim
    emit_load_bool(ACC, BoolConst(1), s);
    emit_load_bool(A1, BoolConst(0), s);
		emit_jal("equality_test",s);

    return;
  }

  // objects
  emit_comp(this->e1, this->e2, emit_beq, ObjectCompType, s);
}

void leq_class::code(ostream &s) {
  // auto fn = [&]() { emit_bleq(T1, T2, classtable->label_no, s); };
  emit_comp(this->e1, this->e2, emit_bleq, PrimCompType, s);
}

void comp_class::code(ostream &s) {
  // lw	$t1 12($a0)
	// la	$a0 bool_const1
	// beqz	$t1 label4
	// la	$a0 bool_const0

  this->e1->code(s); // result in a0

  emit_load_wordsize(T1, 3, ACC, s);  // load int value of bool
  emit_load_bool(ACC, BoolConst(1), s);
  emit_beqz(T1, classtable->label_no, s);
  emit_load_bool(ACC, BoolConst(0), s); // loaded e1 is true i.e. 1
  emit_label_def(classtable->label_no++, s);
}

void int_const_class::code(ostream& s)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s) {  
  if (this->type_name == SELF_TYPE) { // dynamic self
    emit_move(T1, SELF, s); // move self to T1
		emit_load_wordsize(T1, 0, T1, s); //  get class id
		emit_sll(T1, T1, 3, s);  // class_id * 8, i.e. every other address
		emit_load_address(T2, "class_objTab", s);
		emit_addu(T1, T1, T2, s);  // T1 = T2 + T1  entry in class_objTab
		emit_load_wordsize(ACC, 0, T1, s);// a0 is the class_protObj
		emit_load_wordsize(T2, 1, T1, s); // t2 stores name_init, offset one from class_objTab to get init 
		emit_jal("Object.copy", s);
		emit_jalr(T2, s);       // call name_init method
    return;
  }
  // to copy, store protObj in a0 and then call class_Init
  std::string _type = this->type_name->get_string();
  auto class_protObj = _type + std::string(PROTOBJ_SUFFIX);
  auto class_init = _type + std::string(CLASSINIT_SUFFIX);
  emit_load_address(ACC, const_cast<char *>(class_protObj.c_str()), s);
  emit_object_copy(s);
  emit_jal(const_cast<char *>(class_init.c_str()), s);
}

void isvoid_class::code(ostream &s) {
  // 	move	$t1 $a0
  // 	la	$a0 bool_const1
  // 	beqz	$t1 label4
  // 	la	$a0 bool_const0
  // label4:
  // 	lw	$fp 12($sp)
  // 	lw	$s0 8($sp)
  // 	lw	$ra 4($sp)
  // 	addiu	$sp $sp 12
  // 	jr	$ra	

  this->e1->code(s); // result is in a0

  emit_move(T1, ACC, s);  // load result of expr
  emit_load_bool(ACC, BoolConst(1), s);
  emit_beqz(T1, classtable->label_no, s);
  emit_load_bool(ACC, BoolConst(0), s); // not execed in asm if not void (branches over)
  emit_label_def(classtable->label_no++, s);
}

void no_expr_class::code(ostream &s) {
  // emit_move(ACC, ZERO, s); // NOTE: chose not to go this route and instead check whether init expressions are of type no_expr because different contexts required different behavior on no_expr, search instances for details
}

// goal is to load object into ACC
void object_class::code(ostream &s) {
  // self goes here
  if (this->name == self) {
		emit_move(ACC, SELF, s);  // return self
		return;
	}

  // look up in symbol table
  classtable->fn_ctx.lookup(this->name)->emit_load_from(s);
}


