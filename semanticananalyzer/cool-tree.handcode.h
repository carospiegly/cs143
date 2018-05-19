//
// The following include files must come first.

#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H

#include <iostream>
#include "tree.h"
#include "cool.h"
#include "stringtab.h"
#include <map>
#include <utility>


#define yylineno curr_lineno;
extern int yylineno;

inline Boolean copy_Boolean(Boolean b) {return b; }
inline void assert_Boolean(Boolean) {}
inline void dump_Boolean(ostream& stream, int padding, Boolean b)
	{ stream << pad(padding) << (int) b << "\n"; }

void dump_Symbol(ostream& stream, int padding, Symbol b);
void assert_Symbol(Symbol b);
Symbol copy_Symbol(Symbol b);

class Program_class;
typedef Program_class *Program;
class Class__class;
typedef Class__class *Class_;
class Feature_class;
typedef Feature_class *Feature;
class Formal_class;
typedef Formal_class *Formal;
class Expression_class;
typedef Expression_class *Expression;
class Case_class;
typedef Case_class *Case;

typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;
typedef list_node<Feature> Features_class;
typedef Features_class *Features;
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;

#define Program_EXTRAS                          \
virtual void semant() = 0;			\
virtual void dump_with_types(ostream&, int) = 0; \
virtual void add_own_attributes_to_scope(Symbol,std::map<Symbol,Class_>&,SymbolTable<Symbol,Symbol> *) = 0;	\
virtual void add_parent_attributes_to_scope(std::map<Symbol,Symbol>&,std::map<Symbol,Class_>&,Symbol) = 0;	\
virtual void verify_type_of_all_class_features(SymbolTable<Symbol,Symbol> *,std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > &,ostream&,Symbol,std::map<Symbol,Symbol>&,std::map<Symbol,Class_>&) = 0;



#define program_EXTRAS                          \
void semant();     				\
void dump_with_types(ostream&,int);            \
void add_own_attributes_to_scope(Symbol,std::map<Symbol,Class_>&,SymbolTable<Symbol,Symbol> *); 	\
void add_parent_attributes_to_scope(std::map<Symbol,Symbol>&,std::map<Symbol,Class_>&,Symbol);	\
void verify_type_of_all_class_features(  SymbolTable<Symbol,Symbol> *,std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > &,ostream&,Symbol,std::map<Symbol,Symbol>&,std::map<Symbol,Class_> &);



#define Class__EXTRAS  \
virtual Symbol get_filename() = 0;      \
virtual void dump_with_types(ostream&,int) = 0; \
virtual Symbol get_name() = 0; \
virtual Symbol get_parent() = 0; \


#define class__EXTRAS \
Symbol get_filename() { return filename; }             \
void dump_with_types(ostream&,int);   \
Symbol get_name() { return name; } \
Symbol get_parent() { return parent; } \

#define Feature_EXTRAS                                        \
virtual void dump_with_types(ostream&,int) = 0; \


#define Feature_SHARED_EXTRAS                                       \
void dump_with_types(ostream&,int);    



#define Formal_EXTRAS                              \
virtual void dump_with_types(ostream&,int) = 0;


#define formal_EXTRAS                           \
void dump_with_types(ostream&,int);


#define Case_EXTRAS                             \
virtual void dump_with_types(ostream& ,int) = 0; \
Symbol type_check(SymbolTable<Symbol,Symbol> *symtab, std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map, ostream& error_stream, Symbol class_symbol, std::map<Symbol,Symbol> _child_to_parent_classmap);



#define branch_EXTRAS                                   \
void dump_with_types(ostream& ,int); \
Symbol type_check(SymbolTable<Symbol,Symbol> *symtab, std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map, ostream& error_stream, Symbol class_symbol, std::map<Symbol,Symbol> _child_to_parent_classmap);



#define Expression_EXTRAS                    \
Symbol type;                                 \
Symbol get_type() { return type; }           \
Expression set_type(Symbol s) { type = s; return this; } \
virtual void dump_with_types(ostream&,int) = 0;  \
void dump_type(ostream&, int);               \
Expression_class() { type = (Symbol) NULL; }

#define Expression_SHARED_EXTRAS           \
void dump_with_types(ostream&,int);       \
Symbol type_check(SymbolTable<Symbol,Symbol> *symtab, std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map, ostream& error_stream, Symbol class_symbol, std::map<Symbol,Symbol> _child_to_parent_classmap);

#endif
