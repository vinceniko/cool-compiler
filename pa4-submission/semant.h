#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#include <map>
#include <vector>
#include <list>
#include <set>

#define TRUE 1
#define FALSE 0

class ClassNode;
class InheritanceTree;
// class AncestorIter;
class ClassTable;
typedef ClassTable *ClassTableP;

typedef method_class* Method;
typedef attr_class* Attr;

Symbol self_type_convert(Symbol expr, Class_ class_);

class ClassNode {
private:
  Class_ class_;
  std::vector<ClassNode*> children;
  uint tag;
  uint max_subclass;
  void compute_max_subclass();
  uint compute_tags(uint&);
  void compute_tags();
public:
  bool errored;
  ClassNode(Class_ new_class) : class_(new_class) {}
  void insert_child(ClassNode *class_node);
  void compute();
  ClassNode* find(Class_);
  ostream& print(ostream& o, int spaces);
  void error() { errored = true; }
  Class_ get_class() { return this->class_; }
  std::list<ClassNode*> level_order();

  friend class InheritanceTree;
};

// Inheritance Tree maps Symbol (i.e. class name) to ClassNodes inside of the tree. Object is the root
class InheritanceTree {
public:
  std::map<Symbol, ClassNode*> tree;  // TODO: privatize and create getter
  InheritanceTree() {}

  InheritanceTree(Class_ object_, Class_ int_, Class_ io_, Class_ bool_, Class_ str_);
  void init_class(Class_);
  void create_tree();
  void compute();
  ClassNode* find_from_tree(Class_);
  // panicking find
  Class_ at(Symbol class_) {
    return this->tree.at(class_)->class_;
  }
  void check_inheritance_cycles(Classes);
  std::list<Class_> level_order();
  // AncestorIter get_ancestors(Class_ class_);
  std::list<Class_> get_ancestors(Class_);
  std::list<Class_> get_ancestors(Symbol);
  std::list<Class_> get_inheritance(Class_);
  std::list<Class_> get_inheritance(Symbol);
  Class_ LCA(Class_ A, Class_ B);
  Class_ LCA(Symbol A, Symbol B);
  std::list<Attr> get_attributes(Class_);
  std::list<Method> get_methods(Class_);
  bool descends_from(Symbol descendant, Symbol ancestor);
  bool descends_from(Class_ descendant, Class_ ancestor);
  Class_ get_parent(Class_ class_);
  Class_ get_class(Symbol class_);
  Class_ get_class(Class_ class_);
  ClassNode* get_classnode(Symbol class_);
  ostream& print(ostream& o);
  ostream& print_map(ostream& o);
};


// class AncestorIter {
//   InheritanceTree* inheritance_tree;
//   Class_ curr; 
// public:
//   AncestorIter(InheritanceTree* tree, Class_ class_) : inheritance_tree(tree), curr(class_) {}
//   Class_ next() {
//     Class_* parent = inheritance_tree->get_class(this->curr->get_parent());
//     this->curr = *parent;
//     return this->curr;
//   }
//   bool more() {
//     return (inheritance_tree->get_parent(this->curr) != nullptr);
//   }
// };

// AncestorIter InheritanceTree::get_ancestors(Class_ class_) {
//   return AncestorIter(new InheritanceTree(*this), class_);
// }

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  ostream& error_stream;

public:
  ClassTable();

  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
  ostream &semant_error(Class_ c, tree_node *t);
  
  void install_basic_classes();
  void semant(Classes);
  
  InheritanceTree inheritance_tree;

  void install_classes(Classes);
  void classes_check(Classes);
  void main_check();
  // check to make sure that features are correct
  // attributes cannot redefine inherited attributes
  // methods need same signature if redefining
  // includes formals: does the type exist?
  void add_ancestor_attrs(Class_);
  void add_methods(Class_);
  void features_check(Classes);

  SymbolTable<Symbol, Symbol> symboltable;  // name to type

  Class_ curr_class;  // for error printing, accessing the file name
};

#endif

