#include "semant.h"
#include "utilities.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

extern int semant_debug;
extern char *curr_filename;
int curr_lineno;

ClassTable *classtable;
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol
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
static void initialize_constants(void) {
    arg = idtable.add_string("arg");
    arg2 = idtable.add_string("arg2");
    Bool = idtable.add_string("Bool");
    concat = idtable.add_string("concat");
    cool_abort = idtable.add_string("abort");
    copy = idtable.add_string("copy");
    Int = idtable.add_string("Int");
    in_int = idtable.add_string("in_int");
    in_string = idtable.add_string("in_string");
    IO = idtable.add_string("IO");
    length = idtable.add_string("length");
    Main = idtable.add_string("Main");
    main_meth = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any
    //   user-defined class.
    No_class = idtable.add_string("_no_class");
    No_type = idtable.add_string("_no_type");
    Object = idtable.add_string("Object");
    out_int = idtable.add_string("out_int");
    out_string = idtable.add_string("out_string");
    prim_slot = idtable.add_string("_prim_slot");
    self = idtable.add_string("self");
    SELF_TYPE = idtable.add_string("SELF_TYPE");
    Str = idtable.add_string("String");
    str_field = idtable.add_string("_str_field");
    substr = idtable.add_string("substr");
    type_name = idtable.add_string("type_name");
    val = idtable.add_string("_val");
}

Symbol self_type_convert(Symbol expr, Class_ class_) {
    return expr != SELF_TYPE ? expr : class_->get_name();
}

void ClassNode::insert_child(ClassNode *class_node) {
    this->children.push_back(class_node);
}

// computes tags and max_subclass
void ClassNode::compute() {
    this->compute_tags();
    this->compute_max_subclass();
}

void ClassNode::compute_max_subclass() {
    if (children.size() == 0) {
        return;
    }
    // else there are children
    for (std::vector<ClassNode *>::iterator node = children.begin(); node != children.end(); ++node) {
        (*node)->compute_max_subclass(); // recurse
        if ((*node)->max_subclass > this->max_subclass) {
            this->max_subclass = (*node)->max_subclass;
        }
    }
}

std::list<ClassNode *> ClassNode::level_order() {
    std::list<ClassNode *> out;

    std::list<ClassNode *> q;

    q.push_front(this);

    int i = 0;
    while (!q.empty()) {
        ClassNode *curr = q.front();
        out.push_back(curr);

        for (int j = 0; j < static_cast<int>(curr->children.size()); j++) {
            q.push_back(curr->children[j]);
        }

        q.pop_front();
    }

    return out;
}

// convenient way to get classes such that descendants follow ancestors
std::list<Class_> InheritanceTree::level_order() {
    std::list<Class_> out;

    auto q = this->tree[Object]->level_order();
    for (auto it = q.begin(); it != q.end(); ++it) {
        out.push_back((*it)->class_);
    }

    return out;
}

// dfs finishing times saved as tags
uint ClassNode::compute_tags(uint &time) {
    this->tag = this->max_subclass = time;
    for (uint i = 0; i < this->children.size(); ++i) {
        this->children[i]->compute_tags(++time);
    }
    return time;
}

void ClassNode::compute_tags() {
    uint time = 0;
    this->compute_tags(time);
}

// used for debugging
ostream &ClassNode::print(ostream &o, int spaces) {
    int size = static_cast<int>(this->children.size());
    // print right nodes
    if (size > 0) {
        cout << endl;
        for (int j = size - 1; j >= size / 2; --j) {
            this->children[j]->print(o, spaces + 9);
        }
    }

    // print this node
    for (int i = 0; i < spaces; i++) {
        o << ' ';
    }
    o << this->class_->get_name() << ": " << this->tag << ", " << this->max_subclass << endl;

    // print left nodes
    if (size > 0) {
        for (int j = size / 2 - 1; j >= 0; --j) {
            this->children[j]->print(o, spaces + 9);
        }
    }
    return o;
}

InheritanceTree::InheritanceTree(Class_ object_, Class_ int_, Class_ io_, Class_ bool_, Class_ str_) {
    ClassNode *root = new ClassNode(object_);
    this->tree[object_->get_name()] = root;

    this->init_class(int_);
    this->init_class(io_);
    this->init_class(bool_);
    this->init_class(str_);
}

// adds class to map, should be prior to adding to tree with create_tree()
void InheritanceTree::init_class(Class_ class_) {
    if (
        class_->get_parent() == Bool ||
        class_->get_parent() == Int ||
        class_->get_parent() == Str) {
        classtable->semant_error(class_) << "Class " << class_->get_name() << " cannot inherit class " << class_->get_parent() << endl;
    } else if (class_->get_name() == SELF_TYPE) {
        classtable->semant_error(class_) << "Redefinition of basic class SELF_TYPE." << endl;
    } else if (this->tree.find(class_->get_name()) == this->tree.end()) { // not in map
        this->tree[class_->get_name()] = new ClassNode(class_);  // desired outcome
    } else {
        classtable->semant_error(class_) << "Class " << class_->get_name() << " was previously defined" << endl;
        this->tree[class_->get_name()]->error();
    }
}

// iterates over all Classes and populates the inheritance tree
void InheritanceTree::create_tree() {
    for (auto pair = this->tree.begin(); pair != this->tree.end(); ++pair) {
        ClassNode *curr = pair->second;
        // found parent
        if (this->tree.find(curr->class_->get_parent()) != this->tree.end()) { // in map
            ClassNode *parent_node = this->tree.at(curr->class_->get_parent());
            parent_node->insert_child(curr);
            this->tree[curr->class_->get_name()] = curr;
        } else if (curr->class_->get_parent() != No_class) { // Object inherits from this special type
            classtable->semant_error(curr->class_) << "Class " << curr->class_->get_name() << " inherits from an undefined base class " << curr->class_->get_parent() << endl;
            // this->tree.erase(curr->class_->get_name());
            this->tree[curr->class_->get_name()]->error();
        }
    }
}

ClassNode *ClassNode::find(Class_ find_class_) {
    if (this->class_ == find_class_) {
        return this;
    }
    if (static_cast<int>(children.size()) > 0) {  // static cast to silence warnings
        for (auto curr = this->children.begin(); curr != this->children.end(); ++curr) {
            ClassNode *found = (*curr)->find(find_class_);
            if (found != nullptr) {
                return found;
            }
        }
    }
    return nullptr;
}

void InheritanceTree::compute() {
    this->tree[Object]->compute();
}

ostream &InheritanceTree::print(ostream &o) {
    this->tree[Object]->print(o, 0);
    return o;
}

ostream &InheritanceTree::print_map(ostream &o) {
    for (auto pair = this->tree.begin(); pair != this->tree.end(); ++pair) {
        o << pair->first->get_string() << ' ' << pair->second->tag << ", " << pair->second->max_subclass << endl;
    }
    return o;
}

ClassNode *InheritanceTree::find_from_tree(Class_ class_) {
    return this->tree[Object]->find(class_);
}

// inheritance cycles are checked by finding what classes didn't trigger previous errors and are cannot be reached by walking down from Object in the tree. Classes involved in inheritance cycles cannot be reached from object because they mutually descend from each other, meaning they are part of a disconnected tree 
void InheritanceTree::check_inheritance_cycles(Classes classes) {
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) { // TODO? reverse iter for matching ordering with refimpl, UPDATE: outputs dont have to match
        Class_ class_ = classes->nth(i);

        bool not_in_tree = this->find_from_tree(class_) == nullptr;
        auto pair = this->tree.find(class_->get_name());
        bool cleared = pair != this->tree.end() && !pair->second->errored; // didn't trigger other errors
        if (not_in_tree && cleared && class_->get_name() != Main) {
            classtable->semant_error(class_) << "Class " << class_->get_name() << ", or an ancestor of " << class_->get_name() << ", is involved in an inheritance cycle " << endl;
            // this->tree.erase(class_->get_name());
            this->tree[class_->get_name()]->error();
        }
    }
}

std::list<Method> InheritanceTree::get_methods(Class_ class_) {
    std::list<Method> methods;
    Features features = class_->get_features();

    for (int i = features->first(); features->more(i); ++i) {
        Feature feature = features->nth(i);
        if (feature->feature_type() == "method") {
            methods.push_back(static_cast<Method>(feature));
        }
    }

    return methods;
}

std::list<Attr> InheritanceTree::get_attributes(Class_ class_) {
    std::list<Attr> attributes;
    Features features = class_->get_features();

    for (int i = features->first(); features->more(i); ++i) {
        Feature feature = features->nth(i);
        if (feature->feature_type() == "attribute") {
            attributes.push_back(static_cast<Attr>(feature));
        }
    }

    return attributes;
}

// returns ancestors ordered from beginning to end, i.e. closest ancestor at beginning. should only be called on an existing class
std::list<Class_> InheritanceTree::get_ancestors(Class_ descendant) {
    std::list<Class_> ancestors;
    Class_ ancestor = descendant;
    while (ancestor != nullptr) {
        ancestor = this->get_parent(ancestor);
        if (ancestor != nullptr) {
            ancestors.push_back(ancestor);
        }
    }
    // cout << "get_ancestors finished" << endl;
    return ancestors;
}

// returns ancestors ordered from beginning to end, i.e. closest ancestor at beginning. should only be called on an existing class
std::list<Class_> InheritanceTree::get_ancestors(Symbol descendant) {
    return this->get_ancestors(this->at(descendant));
}

bool InheritanceTree::descends_from(Symbol descendant, Symbol ancestor) {
    ClassNode *ancestor_class = this->get_classnode(ancestor);
    ClassNode *class_ = this->get_classnode(descendant);
    if (ancestor_class != nullptr && class_ != nullptr) {
        return ancestor_class->tag <= class_->tag && class_->tag <= ancestor_class->max_subclass;
    }
    return false;
}

bool InheritanceTree::descends_from(Class_ descendant, Class_ ancestor) {
    return this->descends_from(descendant->get_name(), ancestor->get_name());
}

Class_ InheritanceTree::get_parent(Class_ class_) {
    auto parent = tree.find(class_->get_parent());
    if (parent != tree.end()) {
        return parent->second->class_;
    }
    return nullptr;
}
Class_ InheritanceTree::get_class(Symbol class_) {
    auto curr = tree.find(class_);
    if (curr != tree.end()) {
        return curr->second->class_;
    }
    return nullptr;
}
Class_ InheritanceTree::get_class(Class_ class_) {
    return this->get_class(class_->get_name());
}
ClassNode *InheritanceTree::get_classnode(Symbol class_) {
    auto curr = tree.find(class_);
    if (curr != tree.end()) {
        return curr->second;
    }
    return nullptr;
}

// gets ancestors with the descendant at the front, represents the entire inheritance
std::list<Class_> InheritanceTree::get_inheritance(Class_ descendant) {
    auto ancestors = this->get_ancestors(descendant);
    ancestors.push_front(descendant);
    return ancestors;
}

// gets ancestors with the descendant at the front, represents the entire inheritance
std::list<Class_> InheritanceTree::get_inheritance(Symbol descendant) {
    return this->get_inheritance(this->at(descendant));
}

Class_ InheritanceTree::LCA(Class_ a, Class_ b) {
    auto ancestors = this->get_inheritance(a);
    for (auto it = ancestors.begin(); it != ancestors.end(); ++it) {
        Class_ ancestor = *it;
        if (descends_from(b, ancestor)) {
            return ancestor;
        }
    }
    return nullptr;
}

Class_ InheritanceTree::LCA(Symbol a, Symbol b) {
    return this->LCA(this->at(a), this->at(b));
}

// void InheritanceTree::check_inheritance_cycles() {
//     for (auto pair = tree.begin(); pair != tree.end(); ++pair) {
//         Class_ class_ = pair->second->class_;

//         if (this->find_from_tree(class_) == nullptr) {
//             classtable->semant_error(class_) << "Class " << class_->get_name() << ", or an ancestor of " << class_->get_name() << ", is involved in an inheritance cycle " << endl;
//             this->tree.erase(class_->get_name());
//         }
//     }
// }

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
    curr_lineno = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.

    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    //
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
        class_(Object,
               No_class,
               append_Features(
                   append_Features(
                       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                   single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
               filename);

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class =
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
               filename);

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class =
        class_(Int,
               Object,
               single_Features(attr(val, prim_slot, no_expr())),
               filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
        class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //
    Class_ Str_class =
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
               filename);

    this->inheritance_tree = InheritanceTree(Object_class, IO_class, Int_class, Bool_class, Str_class);
}

void ClassTable::install_classes(Classes classes) {
    // install classes here
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        this->inheritance_tree.init_class(classes->nth(i));
    }
}

void ClassTable::classes_check(Classes classes) {
    this->inheritance_tree.create_tree();
    this->inheritance_tree.compute();

    // this->inheritance_tree.print_map(cout); // DEBUG
    // this->inheritance_tree.print(cout);     // DEBUG
    // cout << endl;

    // this->inheritance_tree.tree[Int]->print(cout, 0); // DEBUG

    // delete all errored classes before checking cycle
    for (auto pair = this->inheritance_tree.tree.begin(); pair != this->inheritance_tree.tree.end(); ++pair) {
        if (pair->second->errored) {
            this->inheritance_tree.tree.erase(pair->first);
        }
    }

    this->inheritance_tree.check_inheritance_cycles(classes);

    // this->inheritance_tree.print_map(cout); // DEBUG
}

void ClassTable::main_check() {
    if (this->inheritance_tree.get_class(Main) == nullptr) {
        this->semant_error() << "Class Main is not defined" << endl;
        return;
    }

    Class_ main_class = this->inheritance_tree.tree[Main]->get_class();
    auto main_methods = this->inheritance_tree.get_methods(main_class);
    for (auto it = main_methods.begin(); it != main_methods.end(); ++it) {
        Method main_method = *it;
        if (main_method->get_name() == main_meth) {
            Formals main_formals = main_method->get_formals();
            if (main_formals->more(0)) {
                this->semant_error(main_class, main_method) << "'main' method should have no arguments." << endl;
            }
            return;
        }
    }
    // reaches here if no main method
    this->semant_error(main_class) << "No 'main' method in class Main." << endl;
}

void quit_early(int semant_errors) {
    if (semant_errors > 0) {
        classtable->semant_error() << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}

bool match_eval_type(Symbol assigned, Symbol expr_type, Class_ class_) {
    if (assigned == expr_type || expr_type == No_type) { // self_type == self_type or x==x or No_type built in
        return true;
        // function cannot return a non self_type i.e. object of the class if return type is self_type
    } else if (expr_type != SELF_TYPE && assigned == SELF_TYPE) {
        return false;
    } else if (expr_type == SELF_TYPE) {
        expr_type = class_->get_name(); // assign actual type
    }

    // DEBUG
    // cout << classtable->inheritance_tree.descends_from(Int, Object) << endl;
    // cout << classtable->inheritance_tree.descends_from(Object, Int) << endl;
    // cout << expr_type << " " << assigned << endl;
    return classtable->inheritance_tree.descends_from(expr_type, assigned);
}

Symbol Feature_class::type_check(Class_) {
    cout << "type checking unimplemented feature" << endl;
    return No_type;
}
Symbol Expression_class::type_check(Class_) {
    cout << "type checking unimplemented expression" << endl;
    return No_type;
}

void ClassTable::add_ancestor_attrs(Class_ class_) {
    // adds ancestor attributes to symboltable (won't execute for top of heirarchy)
    std::list<Class_> ancestors = classtable->inheritance_tree.get_ancestors(class_);
    for (auto it = ancestors.begin(); it != ancestors.end(); ++it) {
        Class_ ances = *it;

        std::list<Attr> ances_attributes = this->inheritance_tree.get_attributes(ances);
        // iter over ancestors
        for (auto it = ances_attributes.begin(); it != ances_attributes.end(); ++it) {
            Attr ances_attr = *it;
            // check for redefined feature
            bool ances_attr_redefined = false;

            // iter over current class attributes
            std::list<Attr> class_attributes = this->inheritance_tree.get_attributes(class_);
            for (auto it = class_attributes.begin(); it != class_attributes.end(); ++it) {
                attr_class *attr = *it;
                // cout << "ATTR: " << attr->get_name() << endl;
                if (attr->get_name() == ances_attr->get_name()) {
                    // redefined err
                    ances_attr_redefined = true;
                    classtable->semant_error(class_, attr) << "Attribute " << attr->get_name() << " is an attribute of an inherited class." << endl;
                    break;
                }
            }
            if (!ances_attr_redefined) {
                classtable->symboltable.addid(ances_attr->get_name(), new Symbol(ances_attr->get_type())); // ancestor attribute okay so add to symbol table
            }
        }
    }
}

void ClassTable::features_check(Classes classes) {
    std::list<Class_> classes_level = classtable->inheritance_tree.level_order();
    for (auto it = classes_level.begin(); it != classes_level.end(); ++it) {
        classtable->symboltable.enterscope(); // scope for inherited attr

        Class_ class_ = *it;
        // cout << "parent: " << (*inheritance_tree.get_parent(class_))->get_name() << endl;  // DEBUG
        this->curr_class = class_;
        if (class_->get_name() == Object ||
            class_->get_name() == IO ||
            class_->get_name() == Int ||
            class_->get_name() == Bool ||
            class_->get_name() == Str) {
            continue;
        }

        this->add_ancestor_attrs(class_);
        this->add_methods(class_);

        // must do attributes first
        classtable->symboltable.enterscope(); // scope for attr
        auto attributes = this->inheritance_tree.get_attributes(class_);
        for (auto it = attributes.begin(); it != attributes.end(); ++it) {
            Attr attr_ = *it;
            attr_->type_check(class_);
        }
        auto methods = this->inheritance_tree.get_methods(class_);
        for (auto it = methods.begin(); it != methods.end(); ++it) {
            Method method_ = *it;
            method_->type_check(class_);
        }
        classtable->symboltable.exitscope();
        classtable->symboltable.exitscope();
    }
}

Symbol loop_class::type_check(Class_ class_) {
    if (this->pred->type_check(class_) != Bool) {
        classtable->semant_error(class_, this) << "Loop condition does not have type Bool." << endl;
    }
    this->body->type_check(class_);
    this->type = Object;
    return this->type;
}

Symbol isvoid_class::type_check(Class_ class_) {
    this->e1->type_check(class_);
    this->type = Bool;
    return this->type;
}

Symbol comp_class::type_check(Class_ class_) {
    this->type = this->e1->type_check(class_);
    if (this->type != Bool) {
        classtable->semant_error(class_, this) << "Argument of 'not' has type " << this->type << " instead of Bool." << endl;
    }
    this->type = Bool;
    return this->type;
}

Symbol neg_class::type_check(Class_ class_) {
    this->type = this->e1->type_check(class_);
    if (this->type != Int) {
        classtable->semant_error(class_, this) << "Argument of '~' has type " << this->type << " instead of Int." << endl;
    }
    this->type = Int;
    return this->type;
}

Symbol plus_class::type_check(Class_ class_) {
    Symbol lhs = this->e1->type_check(class_);
    Symbol rhs = this->e2->type_check(class_);
    if (lhs != Int || rhs != Int) {
        classtable->semant_error(class_, this) << "non-Int arguments: " << lhs << " + " << rhs << endl;
    }
    this->type = Int;
    return this->type;
}

Symbol sub_class::type_check(Class_ class_) {
    Symbol lhs = this->e1->type_check(class_);
    Symbol rhs = this->e2->type_check(class_);
    if (lhs != Int || rhs != Int) {
        classtable->semant_error(class_, this) << "non-Int arguments: " << lhs << " - " << rhs << endl;
    }
    this->type = Int;
    return this->type;
}

Symbol mul_class::type_check(Class_ class_) {
    Symbol lhs = this->e1->type_check(class_);
    Symbol rhs = this->e2->type_check(class_);
    if (lhs != Int || rhs != Int) {
        classtable->semant_error(class_, this) << "non-Int arguments: " << lhs << " * " << rhs << endl;
    }
    this->type = Int;
    return this->type;
}

Symbol divide_class::type_check(Class_ class_) {
    Symbol lhs = this->e1->type_check(class_);
    Symbol rhs = this->e2->type_check(class_);
    if (lhs != Int || rhs != Int) {
        classtable->semant_error(class_, this) << "non-Int arguments: " << lhs << " / " << rhs << endl;
    }
    this->type = Int;
    return this->type;
}

Symbol lt_class::type_check(Class_ class_) {
    Symbol lhs = this->e1->type_check(class_);
    Symbol rhs = this->e2->type_check(class_);
    if (lhs != Int || rhs != Int) {
        classtable->semant_error(class_, this) << "non-Int arguments: " << lhs << " < " << rhs << endl;
    }
    this->type = Bool;
    return this->type;
}

Symbol eq_class::type_check(Class_ class_) {
    Symbol lhs = this->e1->type_check(class_);
    Symbol rhs = this->e2->type_check(class_);
    if ((lhs == Int || lhs == Bool || lhs == Str || rhs == Int || rhs == Bool || rhs == Str) && lhs != rhs) {
        classtable->semant_error(class_, this) << "Illegal comparison with a basic type." << endl;
    }
    this->type = Bool;
    return this->type;
}

Symbol leq_class::type_check(Class_ class_) {
    Symbol lhs = this->e1->type_check(class_);
    Symbol rhs = this->e2->type_check(class_);
    if (lhs != Int || rhs != Int) {
        classtable->semant_error(class_, this) << "non-Int arguments: " << lhs << " <= " << rhs << endl;
    }
    this->type = Bool;
    return this->type;
}

Symbol attr_class::type_check(Class_ class_) {
    Symbol expr_type = this->get_init()->type_check(class_);
    if (classtable->inheritance_tree.get_class(this->get_type()) == nullptr) { // not defined
        classtable->semant_error(class_, this) << "Class " << this->get_type() << " of attribute " << this->get_name() << " is undefined." << endl;
    } else if (classtable->symboltable.probe(this->get_name()) != NULL) {
        classtable->semant_error(class_, this) << "Attribute " << this->get_name() << " is multiply defined in class." << endl;
    } else if (!match_eval_type(this->get_type(), expr_type, class_)) { // defined but type doesnt match
        classtable->semant_error(class_, this) << "Inferred type " << expr_type << " of initialization of attribute " << this->get_name() << " does not conform to declared type " << this->get_type() << endl;
    } else {
        classtable->symboltable.addid(this->get_name(), new Symbol(this->get_type())); // attribute okay so add to symbol table
    }
    return expr_type;
}

Symbol no_expr_class::type_check(Class_ class_) {
    this->type = No_type;
    return this->type;
}

Symbol block_class::type_check(Class_ class_) {
    for (int i = this->body->first(); this->body->more(i); i = this->body->next(i)) {
        this->type = this->body->nth(i)->type_check(class_); // last type after iteration is returned
    }
    return this->type;
}

// x + 3 ie x, or returning x
Symbol object_class::type_check(Class_ class_) {
    if (this->name == self) {
        this->type = SELF_TYPE;
    } else if (classtable->symboltable.lookup(this->name)) {
        this->type = *classtable->symboltable.lookup(this->name);
    } else {
        classtable->semant_error(class_, this) << "Undeclared identifier " << this->name << endl;
        this->type = Object;
    }
    return this->type;
}

Symbol new__class::type_check(Class_ class_) {
    if (this->type_name != SELF_TYPE && classtable->inheritance_tree.get_class(this->type_name) == nullptr) {
        classtable->semant_error(class_, this) << "'new' used with undefined class " << this->type_name << endl;
        // this->type_name = Object;
    }
    this->type = this->type_name;
    return this->type;
}

Symbol bool_const_class::type_check(Class_) {
    this->type = Bool;
    return this->type;
}

Symbol int_const_class::type_check(Class_) {
    this->type = Int;
    return this->type;
}

Symbol string_const_class::type_check(Class_) {
    this->type = Str;
    return this->type;
}

Symbol assign_class::type_check(Class_ class_) {
    Symbol expr_type = expr->type_check(class_);

    if (classtable->symboltable.lookup(name) == NULL) {
        classtable->semant_error(class_, this) << "Assignment to undeclared variable " << name << endl;
        this->type = expr_type;
        return this->type;
    }

    Symbol assigned = *classtable->symboltable.lookup(name);

    if (!match_eval_type(assigned, expr_type, class_)) {
        classtable->semant_error(class_, this) << "Type " << expr_type << " of assigned expression does not conform to declared type " << assigned << " of identifier " << name << endl;
        this->type = assigned;
        return this->type;
    }

    this->type = expr_type;
    return this->type;
}

Symbol let_class::type_check(Class_ class_) {
    classtable->symboltable.enterscope();

    classtable->symboltable.addid(this->identifier, new Symbol(this->type_decl));

    Symbol init_type = this->init->type_check(class_);
    if (classtable->inheritance_tree.get_class(this->type_decl) == nullptr) {
        classtable->semant_error(class_, this) << "Class " << this->type_decl << " of let-bound identifier " << this->identifier << " is undefined." << endl;
    } else if (!match_eval_type(this->type_decl, init_type, class_)) {
        classtable->semant_error(class_, this) << "Inferred type " << init_type << " of initialization of " << this->identifier << " does not conform to identifier's declared type " << this->type_decl << endl;
    }

    this->type = this->body->type_check(class_);

    classtable->symboltable.exitscope();
    return type;
}

Symbol cond_class::type_check(Class_ class_) {
    if (this->pred->type_check(class_) != Bool) {
        classtable->semant_error(class_, this) << "Predicate of 'if' does not have type Bool." << endl;
    }

    Symbol then_type = this->then_exp->type_check(class_);
    Symbol else_type = this->else_exp->type_check(class_);

    if (then_type == SELF_TYPE && else_type == SELF_TYPE) {
        this->type = SELF_TYPE;
    } else {
        this->type = classtable->inheritance_tree.LCA(self_type_convert(then_type, class_), self_type_convert(else_type, class_))->get_name();
    }
    return this->type;
}

Symbol typcase_class::type_check(Class_ class_) {
    Symbol expr_type = expr->type_check(class_);

    // type check cases
    Cases cases_ = this->cases;
    std::set<Symbol> branch_types;
    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        branch_class *branch = static_cast<branch_class *>(cases->nth(i));

        if (branch_types.find(branch->get_type()) != branch_types.end()) {
            classtable->semant_error(class_, branch) << "Duplicate branch " << branch->get_type() << " in case statement." << endl;
        } 
        branch_types.insert(branch->get_type());

        Symbol branch_type = branch->type_check(class_);
        if (i == cases->first()) {
            this->type = branch_type;
        } else if (this->type != SELF_TYPE || branch_type != SELF_TYPE) {
            this->type = classtable->inheritance_tree.LCA(self_type_convert(this->type, class_), self_type_convert(branch_type, class_))->get_name();  // take the running LCA or "union" as described in manual
        }
    }

    return this->type;
}

Symbol branch_class::type_check(Class_ class_) {
    classtable->symboltable.enterscope();

    classtable->symboltable.addid(name, new Symbol(type_decl));
    Symbol expr_type = expr->type_check(class_);

    classtable->symboltable.exitscope();
    return expr_type;
}

Symbol static_dispatch_class::type_check(Class_ class_) {
    Symbol calling_expr = this->expr->type_check(class_);

    if (this->type_name != SELF_TYPE && classtable->inheritance_tree.get_class(this->type_name) == nullptr) {
        classtable->semant_error(class_, this) << "Static dispatch to undefined class " << this->type_name << endl;
        this->type = Object;
        return this->type;
    } else if (calling_expr != SELF_TYPE && classtable->inheritance_tree.get_class(calling_expr) == nullptr) {
        classtable->semant_error(class_, this) << "Static dispatch on undefined class " << calling_expr << endl;
        this->type = Object;
        return this->type;
    } else if (!match_eval_type(this->type_name, calling_expr, class_)) {
        classtable->semant_error(class_, this) << "Expression type " << calling_expr << " does not conform to declared static dispatch type " << this->type_name << endl;
        this->type = Object;
        return this->type;
    }

    // find declaration
    Method method_ = nullptr;
    auto ancestors = classtable->inheritance_tree.get_inheritance(self_type_convert(calling_expr, class_));
    for (auto it = ancestors.begin(); it != ancestors.end(); ++it) {
        Class_ ances = *it;
        auto methods = classtable->inheritance_tree.get_methods(ances);
        for (auto it = methods.begin(); it != methods.end(); ++it) {
            Method ances_method = *it;
            if (ances_method->get_name() == this->name) {
                method_ = ances_method;

                // type check args and match to formals
                Formals formals = method_->get_formals();
                int k1 = this->actual->first(), k2 = formals->first();
                while (this->actual->more(k1) && formals->more(k2)) {
                    Symbol actual_type = this->actual->nth(k1)->type_check(class_);
                    Symbol formal_type = formals->nth(k2)->get_type();
                    if (!match_eval_type(formal_type, actual_type, class_)) {
                        classtable->semant_error(class_, this) << "In call of method " << this->name << ", type " << actual_type << " of parameter " << formals->nth(k2)->get_name() << " does not conform to declared type " << formal_type << endl;
                    }
                    k1 = this->actual->next(k1);
                    k2 = formals->next(k2);
                    if (this->actual->more(k1) xor formals->more(k2)) {
                        classtable->semant_error(class_, this) << "Method " << this->name << " called with wrong number of arguments." << endl;
                    }
                }
                break;
            }
        }
    }

    if (method_ == nullptr) { // no declaration found
        classtable->semant_error(class_, this) << "Dispatch to undefined method " << this->name << endl;
        this->type = Object;
        return this->type;
    } 

    this->type = method_->get_return_type();
    if (this->type == SELF_TYPE) {
        this->type = calling_expr;
    }
    
    return this->type;
}

Symbol dispatch_class::type_check(Class_ class_) {
    Symbol calling_expr = this->expr->type_check(class_);

    if (calling_expr != SELF_TYPE && classtable->inheritance_tree.get_class(calling_expr) == nullptr) {
        classtable->semant_error(class_, this) << "Dispatch on undefined class " << calling_expr << endl;
        this->type = Object;
        return this->type;
    }

    // find declaration
    Method method_ = nullptr;
    auto ancestors = classtable->inheritance_tree.get_inheritance(self_type_convert(calling_expr, class_));
    for (auto it = ancestors.begin(); it != ancestors.end(); ++it) {
        Class_ ances = *it;
        auto methods = classtable->inheritance_tree.get_methods(ances);
        for (auto it = methods.begin(); it != methods.end(); ++it) {
            Method ances_method = *it;
            if (ances_method->get_name() == this->name) {
                method_ = ances_method;

                // type check args and match to formals
                Formals formals = method_->get_formals();
                int k1 = this->actual->first(), k2 = formals->first();
                while (this->actual->more(k1) && formals->more(k2)) {
                    Symbol actual_type = this->actual->nth(k1)->type_check(class_);
                    Symbol formal_type = formals->nth(k2)->get_type();
                    if (!match_eval_type(formal_type, actual_type, class_)) {
                        classtable->semant_error(class_, this) << "In call of method " << this->name << ", type " << actual_type << " of parameter " << formals->nth(k2)->get_name() << " does not conform to declared type " << formal_type << endl;
                    }
                    k1 = this->actual->next(k1);
                    k2 = formals->next(k2);
                    if (this->actual->more(k1) xor formals->more(k2)) {
                        classtable->semant_error(class_, this) << "Method " << this->name << " called with wrong number of arguments." << endl;
                    }
                }
                break;
            }
        }
    }

    if (method_ == nullptr) { // no declaration found
        classtable->semant_error(class_, this) << "Dispatch to undefined method " << this->name << endl;
        this->type = Object;
        return this->type;
    } 
    this->type = method_->get_return_type();
    if (this->type == SELF_TYPE) {
        this->type = calling_expr;
    }

    return this->type;
}

void ClassTable::add_methods(Class_ class_) {
    // adds ancestor attributes to symboltable
    std::list<Class_> ancestors = classtable->inheritance_tree.get_ancestors(class_);
    for (auto it = ancestors.begin(); it != ancestors.end(); ++it) {
        Class_ ances = *it;

        std::set<Method> set_methods; 

        std::list<Method> ances_methods = this->inheritance_tree.get_methods(ances);
        std::list<Method> methods = this->inheritance_tree.get_methods(class_);
        for (auto it = methods.begin(); it != --methods.end(); ++it) {
            Method method_ = *it;
            if (set_methods.find(method_) != set_methods.end()) {
                classtable->semant_error(class_, method_) << "Method " << method_->get_name() << " is multiply defined." << endl;
            }
            set_methods.insert(method_);

            // check to see if redefines ancestor
            for (auto it = ancestors.begin(); it != ancestors.end(); ++it) {
                Class_ ances = *it;
                std::list<Method> ances_methods = this->inheritance_tree.get_methods(ances);
                for (auto it = ances_methods.begin(); it != ances_methods.end(); ++it) {
                    Method ancestor_method = *it;

                    if (ancestor_method->get_name() == method_->get_name()) {
                        if (method_->get_return_type() != ancestor_method->get_return_type()) {
                            classtable->semant_error(class_, method_) << "In redefined method " << method_->get_name() << ", return type " << method_->get_return_type() << " is different from original return type " << ancestor_method->get_return_type() << endl;
                        }
                        Formals ancestor_formals = ancestor_method->get_formals();
                        Formals curr_formals = method_->get_formals();

                        int k1 = curr_formals->first(), k2 = ancestor_formals->first();
                        while (curr_formals->more(k1) && ancestor_formals->more(k2)) {
                            if (curr_formals->nth(k1)->get_type() != ancestor_formals->nth(k2)->get_type()) {
                                classtable->semant_error(class_, method_) << "In redefined method " << method_->get_name() << ", parameter type " << curr_formals->nth(k1)->get_type() << " is different from original type " << ancestor_formals->nth(k2)->get_type() << endl;
                            }
                            k1 = curr_formals->next(k1);
                            k2 = ancestor_formals->next(k2);
                            if (curr_formals->more(k1) xor ancestor_formals->more(k2)) {
                                classtable->semant_error(class_, method_) << "Incompatible number of formal parameters in redefined method " << method_->get_name() << endl;
                            }
                        }
                    }
                }
            }
        }
    }
}

Symbol method_class::type_check(Class_ class_) {
    // todo: method redefinition
    classtable->symboltable.enterscope();
    // below doesnt work because each method has its own scope and an attribute and method can have the same name 
    // if (classtable->symboltable.probe(this->get_name()) != NULL) {
    //     classtable->semant_error(class_, this) << "Method " << this->get_name() << " is multiply defined" << endl;
    // }
    // check formals and return types
    Formals formals = this->get_formals();
    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        Formal formal_ = formals->nth(i);
        if (formal_->get_name() == self) {
            classtable->semant_error(class_, formal_) << "'self' cannot be the name of a formal parameter." << endl;
        } else if (classtable->symboltable.probe(formal_->get_name())) { // already in this scope in symbol table
            classtable->semant_error(class_, formal_) << "Formal parameter " << formal_->get_name() << " is multiply defined." << endl;
        } else if (classtable->inheritance_tree.get_class(formal_->get_type()) == nullptr) {
            classtable->semant_error(class_, formal_) << "Class " << formal_->get_type() << " of formal parameter " << formal_->get_name() << " is undefined." << endl;
        } else { // add to symbol table
            classtable->symboltable.addid(formal_->get_name(), new Symbol(formal_->get_type()));
        }
    }
    Symbol expr_type = this->expr->type_check(class_);
    if (this->return_type != SELF_TYPE && classtable->inheritance_tree.get_class(return_type) == nullptr) {  // need to check for new SELF_TYPE
        classtable->semant_error(class_, this->expr) << "Undefined return type " << return_type << " in method " << name << "." << endl;
    } else if (!match_eval_type(this->return_type, expr_type, class_)) {
        classtable->semant_error(class_, this->expr) << "Inferred return type " << expr_type << " of method " << name << " does not conform to declared return type " << return_type << endl;
    }
    // cout << "passed method check" << endl; // DEBUG

    classtable->symboltable.exitscope();

    return No_type;
}

ClassTable::ClassTable() : semant_errors(0), error_stream(cerr) {}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
//    All versions return an output stream to which you should write
//    an appropriate human-readable error message.
//
///////////////////////////////////////////////////////////////////

ostream &ClassTable::semant_error(Class_ c) {
    return semant_error(c->get_filename(), c);
}

ostream &ClassTable::semant_error(Class_ c, tree_node *t) {
    return semant_error(c->get_filename(), t);
}

ostream &ClassTable::semant_error(Symbol filename, tree_node *t) {
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream &ClassTable::semant_error() {
    semant_errors++;
    return error_stream;
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant() {
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    classtable = new ClassTable(); // NOTE: had to create this constructor so that InheritanceTree has access to global classtable instance and the error stream (to simplify rather than passing in error stream to InheritanceTree via functions or member)
    /* some semantic analysis code may go here */
    classtable->install_basic_classes();
    // cout << "Passed install_basic_classes" << endl; //DEBUG
    classtable->install_classes(classes);
    quit_early(classtable->errors());
    // cout << "Passed install_classes" << endl; // DEBUG

    classtable->classes_check(classes);

    quit_early(classtable->errors());

    classtable->main_check();

    // all type checking and scope checking
    classtable->features_check(classes);

    quit_early(classtable->errors());
}
