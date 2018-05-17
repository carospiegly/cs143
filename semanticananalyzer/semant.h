#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"
#include <map>

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  bool check_inheritance_graph_for_cycles();
  bool check_inheritance_graph_for_cycles(int num_classes, 
					std::map<Symbol,int> & symbol_to_class_index_map, 
					std::map<Symbol,Symbol> & child_to_parent_classmap);
  std::set<Symbol> gather_valid_classes();
  void verify_parent_classes_defined(std::set<Symbol> & valid_classes);
  void add_class_methods_to_method_table(Class__class *curr_class,
                                        SymbolTable<std::pair(Symbol,Symbol),
                                                std::vector<Symbol>> & method_table);
  void populate_child_parent_and_unique_ID_maps(std::map<Symbol,Symbol> & child_to_parent_classmap,
                                                std::map<Symbol,int> & symbol_to_class_index_map);
  ostream& error_stream;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};


#endif

