#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

// Vin
#include <algorithm>
#include <list>
#include <string>
#include <map>
#include <vector>
#include <functional>
#include <typeinfo>

// for debugging
#define MIPS_COMMENT "# "

#define provided_list_iter(list) \
for (int i = list->first(); list->more(i); i = list->next(i))

// reverse iter
#define provided_list_r_iter(list) \
for (int i = list->len()-1; i >= 0; --i)

// must be called within CgenClassTable
// iterates through the nodes in the correct order starting with Object
// this is implemented recursively in postorder operation since the nodes are in reverse order and there is no efficient way to iterate in reverse without first storing them in another ds
#define nodes_iter(stmts) \
  std::function<void(List<CgenNode> *)> recurse; \
  int i = 0; \
  bool early_break = false; \
  recurse = [&](auto node) { \
      if (node != nullptr) { \
         CgenNode* _class = node->hd(); \
         if (!early_break) \
            recurse(node->tl()); \
         stmts \
         i++; \
      } \
  }; \
  recurse(this->nds)

// must be called within class that has a features member, i.e. attr or method
#define features_iter(stmts) \
   provided_list_iter(this->features) { \
      Feature feature = this->features->nth(i); \
      stmts \
   }

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

template <typename FeatureType>
struct FeatureElem {
  CgenNode *owner; // which class owns the feature
  FeatureType *feature;
};

// FeatureCol is a collection of features which have a relevant offset used by cgen
template <typename FeatureType>
class FeatureCol: public std::list<FeatureElem<FeatureType>> {
   int _get_offset(Symbol);
public:
   int get_offset(Symbol);
   FeatureType* get(Symbol);
};

// VarBinding is a variable that is bound at runtime to some location represented by a register and an offset from the address stored at that register
class VarBinding {
protected:
   char *reg;
   int offset; // in wordsize
public:
   VarBinding() : reg(ACC), offset(0) {}
   VarBinding(char *_reg, int _offset) : reg(_reg), offset(_offset) {}
   char *get_reg() { return reg; }
   virtual int get_offset() = 0; // virtual for +/- offset
   
   // in the function name: the proposition refers to "this" instance
   // ACC is the default since the stack approach was used
   void emit_load_from(ostream &s, char* = ACC);
   // in the function name: the proposition refers to "this" instance
   // ACC is the default since the stack approach was used
   void emit_store_into(ostream &s, char* = ACC);
};

class AttrBinding: public VarBinding {
public:
   AttrBinding(int _offset) : VarBinding(SELF, _offset) {}
   int get_offset() { return 3 + offset; }  // attributes are at the third offset of the protObj
};

class FormalBinding: public VarBinding {
public:
   // FP is used since the var is stored on the stack
   FormalBinding(int _offset) : VarBinding(FP, _offset) {}
   int get_offset() { return (1 + 3 + offset); } // 1 so thats behind the variable itself, 3 because of fp, self, return
};

class LocalBinding: public VarBinding {
public:
   // FP is used since the var is stored on the stack
   LocalBinding(int _offset) : VarBinding(FP, _offset) {}
   int get_offset() { return -offset; }
};

enum BindingType {
   AttrBindType,
   FormalBindType,
   LocalBindType,
};

// FunctionContext tracks the current offset for variable bindings (VarBinding), and instantiates them
class FunctionContext : public SymbolTable<Symbol, VarBinding> {
public:
   int attr_count;
   int formal_count;
   int local_count;

   FunctionContext() : attr_count(0), formal_count(0), local_count(0) {}
   
   void add(Symbol _var, BindingType _type) {
      VarBinding* binding; 
      switch (_type) {
      case AttrBindType:
         binding = new AttrBinding(attr_count);
         attr_count++;
         break;
      case FormalBindType:
         binding = new FormalBinding(formal_count);
         formal_count++;
         break;
      case LocalBindType:
         binding = new LocalBinding(local_count);
         local_count++;
         break;
      }
      this->addid(_var, binding);
   }

   // redefinition of parent method which takes a BindingType and exits the scope and resets the offset for the BindingType
   void exitscope(BindingType _type) {
      switch (_type) {
      case AttrBindType:
         this->attr_count = 0;
         break;
      case FormalBindType:
         this->formal_count = 0;
         break;
      case LocalBindType:
         this->local_count = 0;
         break;
      }
      SymbolTable::exitscope();
   }
};

struct InheritanceRange {
   int min;
   int max;
};

// CgenClassTable is a global table holding all the classes as well as other globals necessary during code generation. it is accessible everywhere in the cgen code and is a singleton
class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;


// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
   
   void protObjs_attr(attr_class *);
public:
   // below are globals essentially

   // the current label_no
   int label_no;
   // the current class
   Symbol _class;
   // the current function context
   FunctionContext fn_ctx;

   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();

   // all relevant cgen occurs in these methods:
   void class_nameTab();
   void class_objTab();
   void protObjs();
   void dispTabs();
   void inits();
   void methods();

   CgenNodeP get_class(Symbol);
   int get_class_tag(Symbol);
   // get the tags for all the classes in the inheritance hierarchy
   std::map<Symbol, int> get_class_tags();
   // get the range of tags (descendant classes) covered by a class
   InheritanceRange get_inheritance_range(Symbol _class);
};

class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }

   // get only this class' attrs
   FeatureCol<attr_class> get_this_attrs();
   // get all attrs including inherited
   FeatureCol<attr_class> get_all_attrs();

   // get only this class' methods
   FeatureCol<method_class> get_this_methods();
   // get all methods including inherited
   FeatureCol<method_class> get_all_methods();

   // recursive function that gets the inheritance range covered by this class
   // the tags_cache argument maps symbols to their tags and is passed along recursively to prevent recomputing
   InheritanceRange get_inheritance_range(const std::map<Symbol, int> &tags_cache) {
      int curr_tag = tags_cache.at(this->get_name());

      auto range = InheritanceRange{curr_tag, curr_tag};
      if (this->children != nullptr) {
         std::vector<InheritanceRange> tags;
         for (auto curr = this->children; curr != nullptr; curr = curr->tl()) {
            CgenNode *_class = curr->hd();
            tags.push_back(curr->hd()->get_inheritance_range(tags_cache));
         }

         for (auto r : tags) {
            if (r.max > range.max) {
               range.max = r.max;
            }
            if (r.min < range.min) {
               range.min = r.min;
            }
         }
      }
      
      return range;
   }
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

