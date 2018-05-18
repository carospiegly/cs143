

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"
#include <map>
#include <list>
#include <vector>
#include <set>
#include <utility>
#include "cool-tree.h"
#include <iostream>

extern int semant_debug;
extern char *curr_filename;


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



/* We perform some semantic analysis here.
  
   This function accepts "Classes", which is a typedef for typedef Classes_class *Classes
   "Classes_class" is in turn defined as "typedef list_node<Class_> Classes_class"
   
   We use the list_node interface, which is an implementation of a list.

   "Class_" is a type def for :typedef class Class__class *Class_"

   _declared_classes_map is a map that maps
    _std::map Type -> Class_ that keeps track of all the declared classes, 
    so Bool, Int, etc will have an entry there.
*/
ClassTable::ClassTable(Classes classes) : 	_classes(classes), 
                                            _valid_classes(),
                                            _symbol_to_class_index_map(),
                                            _child_to_parent_classmap(),
                                            _method_map(),
                                            _declared_classes_map(),   
                                            semant_errors(0) , 
                                            error_stream(cerr)
{
	// walk through each of the classes in the class list of the program
	//list_node<Class__class *> class_deep_copy = classes->copy_list(); // make a deep copy. We might need to modify it as we go???	

    // method map is indexed by Class Symbol, Method Symbol

	// PASS 1 -- make a set with all of the class names (not including the parent each is inherited from)
	gather_valid_classes();

	// PASS 2 -- MAKE SURE THAT EACH CLASS THAT WAS INHERITED FROM WAS REAL
	verify_parent_classes_are_defined();

	// PASS 3 over the program
	// keep global counter of how many classes we have seen so far, and this is the unique ID for each class
	populate_child_parent_and_unique_ID_maps(); 
	
	// unique_class_idx holds the total number of classes
	bool is_cyclic = check_inheritance_graph_for_cycles();
	if (is_cyclic)
	{
		error_stream << "THROW ERROR! Graph is cyclic";
	}
	// RETURN SOME VALUE return is_cyclic;

}

/*
   In PASS 1, we make a set with all of the class names 
   (not including the parent each is inherited from).
   The size of the set is how many classes we have seen so far.
*/
void ClassTable::gather_valid_classes() 
{
    // in what cases would this not be equal to classes.len()?
    for(int i = _classes->first(); _classes->more(i); i = _classes->next(i))
    {
            Class__class *curr_class = _classes->nth(i);
            Symbol curr_class_name = curr_class->get_name();
            if( _valid_classes.find(curr_class_name) == _valid_classes.end() )
            {
                    // does not exist in the map yet
                    _valid_classes.insert( curr_class_name );
            } else {
                    error_stream << "THROW ERROR! CLASS DEFINED TWICE\n" ;
            }
    }

}

/*
        // PASS 2 MAKE SURE THAT EACH CLASS THAT WAS INHERITED FROM WAS REAL
    // decremenet the size of the set (total number of classes) if any of them were fake
*/
void ClassTable::verify_parent_classes_are_defined()
{
	for(int i = _classes->first(); _classes->more(i); i = _classes->next(i))
	{
		Class__class *curr_class = _classes->nth(i);
		Symbol child_class_name = curr_class->get_name();
		Symbol parent_class_name = curr_class->get_parent();
		// If it has no parent, parent is type Object
		if( (parent_class_name!= Object) && ( _valid_classes.find(parent_class_name) == _valid_classes.end() ) )
		{
			_valid_classes.erase(child_class_name);
			error_stream << "THROW ERROR! child inherits from an undefined class\n";
		}
	}
}


void ClassTable::populate_child_parent_and_unique_ID_maps()
{
    int unique_class_idx = 0;
	for(int i = _classes->first(); _classes->more(i); i = _classes->next(i))
    {
        Class__class *curr_class = _classes->nth(i);
        Symbol child_class_name = curr_class->get_name();
        Symbol parent_class_name = curr_class->get_parent();
        if( _valid_classes.find(child_class_name) != _valid_classes.end() )
        {
	    _declared_classes_map.insert(std::make_pair(child_class_name, curr_class));
            // besides acyclicity, we can say this class is legit bc it persisted in the valid classes set
            // SOME WILL NOT INHERIT FROM ANY CLASS -- HAVE NOT ACCOUNTED FOR THIS CASE YET
            if (parent_class_name != NULL)
            {
                    _child_to_parent_classmap.insert(std::make_pair(child_class_name, parent_class_name ));
            }
            _symbol_to_class_index_map.insert(std::make_pair(child_class_name,unique_class_idx));
            unique_class_idx++;

            // add the class to the symbol table
            // add the methods of this class to the methodtable
            add_class_methods_to_method_map(curr_class);
        }
    }
	// assert size of set is the same size as the number of unique classes
	assert( unique_class_idx == (int)_valid_classes.size() );
}


/*
        Add the methods of this class to the methodtable
*/
void ClassTable::add_class_methods_to_method_map(Class__class *curr_class)
{
    Symbol curr_class_name = curr_class->get_name();
    list_node<Feature> *curr_features = curr_class->get_features();
    for(int j = curr_features->first(); curr_features->more(j); j = curr_features->next(j))
    {
        Feature_class *curr_feat = curr_features->nth(j);
        // every feature is either a method, or an attribute
        if (curr_feat->feat_is_method() ) // I am a method! ;
        {
            // key is (curr class, name of the method)
            std::pair<Symbol,Symbol> key = std::make_pair( curr_class_name, curr_feat->get_name() );

            // the value is the std::vector<Symbols>, all parameters and then return type
            std::vector<Symbol> params_and_rt = std::vector<Symbol>( curr_feat->get_params_and_rt() );

            // if its a method, then we need to add it to the method table
            _method_map.insert(std::make_pair( key, params_and_rt ));
        }
    }
}


std::map<std::pair<Symbol,Symbol>, std::vector<Symbol> >  ClassTable::get_method_map()
{
    return _method_map;
}


void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
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
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

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


	_declared_classes_map.insert(std::make_pair( Object , Object_class));
	_declared_classes_map.insert(std::make_pair( IO , IO_class));
	_declared_classes_map.insert(std::make_pair( Int , Int_class));
	_declared_classes_map.insert(std::make_pair( Bool , Bool_class));
	_declared_classes_map.insert(std::make_pair( Str , Str_class));



}

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
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
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
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > method_map = 
        classtable->get_method_map();
    SymbolTable<Symbol,Symbol> *id_to_type_symtab = new SymbolTable<Symbol,Symbol>();

    // WE LOOP THROUGH IN TERMS OF CLASS HIERARCHY! NOT IN TERMS OF PROGRAM ORDER

    // Perform all type checking
    for(int i = classes->first(); classes->more(i); i = classes->next(i))
    {
        id_to_type_symtab->enterscope();

        Class__class *curr_class = classes->nth(i);
/*        
        //for each class, add attributes of inherited classes 
        Class_class *parent = (classtable->get_child_map()).find(curr_class);

        list_node<Feature> *curr_features = parent->get_features();
    
    for(int j = curr_features->first(); curr_features->more(j); j = curr_features->next(j))
    {
        Feature_class *curr_feat = curr_features->nth(j);
 
        if (!curr_feat->feat_is_method() )
        {
              id_to_type_symtab->addid( curr_feat->get_name(), get_type_decl() );
         } 

    }
	Class_class *new_parent = (classtable->get_child_map()).find(parent);

	while(new_parent!=NULL){
    
        list_node<Feature> *curr_features = new_parent->get_features();
   
   	 for(int j = curr_features->first(); curr_features->more(j); j = curr_features->next(j))
    {
        Feature_class *curr_feat = curr_features->nth(j);
 
        if (!curr_feat->feat_is_method() )
        {
              id_to_type_symtab->addid( curr_feat->get_name(), get_type_decl() );
         } 
     }
 }
  */      //TYPE CHECK HERE
        // get down to the first expression of class
        //curr_class->type_check(id_to_type_symtab, method_map, classtable->semant_error(curr_class) );  
        id_to_type_symtab->exitscope(); 
    }


    /* some semantic analysis code may go here */
    
    if (classtable->errors()) {
	   cerr << "Compilation halted due to static semantic errors." << endl;
	   exit(1);
    }

    // free the memory
    delete id_to_type_symtab;
    delete classtable; // automatically frees the method table

}



// /*  This will be our function that recursively descends the AST.
// */
// void program_class::process_standing_node(ostream& stream, int n)
// {
//    // dump_line(stream,n,this);
//    // stream << pad(n) << "_object\n";
//    // dump_Symbol(stream, n+2, name);
//    // dump_type(stream,n);
// }




 /* USE TO DETECT A CYCLE IN A GRAPH */
class ClassGraph
{
    int V;    // No. of vertices
    std::list<int> *adj;    // Pointer to an array containing adjacency lists
    bool isCyclicUtil(int v, bool visited[], bool *rs);  // used by isCyclic()
public:
    ClassGraph(int V);   // Constructor
    void addEdge(int v, int w);   // to add an edge to graph
    bool isCyclic();    // returns true if there is a cycle in this graph
};

ClassGraph::ClassGraph(int V)
{
    this->V = V;
    adj = new std::list<int>[V];
}



void ClassGraph::addEdge(int v, int w)
{
    adj[v].push_back(w); // Add w to v’s list.
}

// This function is a variation of DFSUytil() in https://www.geeksforgeeks.org/archives/18212
bool ClassGraph::isCyclicUtil(int v, bool visited[], bool *recStack)
{
    if(visited[v] == false)
    {
        // Mark the current node as visited and part of recursion stack
        visited[v] = true;
        recStack[v] = true;

        // Recur for all the vertices adjacent to this vertex
        std::list<int>::iterator i;
        for(i = adj[v].begin(); i != adj[v].end(); ++i)
        {
            if ( !visited[*i] && isCyclicUtil(*i, visited, recStack) )
                return true;
            else if (recStack[*i])
                return true;
        }

    }
    recStack[v] = false;  // remove the vertex from recursion stack
    return false;
}



// Returns true if the graph contains a cycle, else false.
// This function is a variation of DFS() in https://www.geeksforgeeks.org/archives/18212
bool ClassGraph::isCyclic()
{
    // Mark all the vertices as not visited and not part of recursion
    // stack
    bool *visited = new bool[V];
    bool *recStack = new bool[V];
    for(int i = 0; i < V; i++)
    {
        visited[i] = false;
        recStack[i] = false;
    }

    // Call the recursive helper function to detect cycle in different
    // DFS trees
    for(int i = 0; i < V; i++)
        if (isCyclicUtil(i, visited, recStack))
            return true;

    return false;
}




/*
Code taken from
https://www.geeksforgeeks.org/detect-cycle-in-a-graph/

PASS THE MAPS IN BY REFERENCE
*/
bool ClassTable::check_inheritance_graph_for_cycles()
{
    int num_classes = _valid_classes.size();
    // Create a graph given in the above diagram
    ClassGraph g(num_classes);

    for (std::map<Symbol, Symbol>::iterator it=_child_to_parent_classmap.begin(); it!=_child_to_parent_classmap.end(); ++it){
	
	// SOME WILL NOT INHERIT FROM ANY CLASS -- HAVE NOT ACCOUNTED FOR THIS CASE YET
	Symbol parent_class_name = it->second;
	Symbol child_class_name = it->first;
	int parent_idx = _symbol_to_class_index_map.find( parent_class_name )->second;
	int child_idx = _symbol_to_class_index_map.find( child_class_name )->second;
        std::cout << "Adding edge from " << parent_idx << " to " << child_idx << std::endl;
	g.addEdge(parent_idx, child_idx);
   }

    if(g.isCyclic())
    {
        std::cout << "Graph contains cycle" << std::endl;
        return true;
    } else {
        std::cout << "Graph doesn't contain cycle" << std::endl;
        return false;
    }
}


   Symbol static_dispatch_class::type_check(     SymbolTable<Symbol,Symbol> *symtab,
                        std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map,
                        ostream& error_stream)
   {
      // typename, name
      expr->type_check(symtab, method_map, error_stream);

      for(int i = actual->first(); actual->more(i); i = actual->next(i))
        actual->nth(i)->type_check(symtab, method_map, error_stream);

      return type_name; // CHANGE THIS TO RETURN THE LAST ARG OF FORMALS
   }


Symbol dispatch_class::type_check(SymbolTable<Symbol,Symbol> *symtab,
                        std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map,
                        ostream& error_stream)
        {
                expr->type_check(symtab, method_map, error_stream);
                for(int i = actual->first(); actual->more(i); i = actual->next(i))
                {
                        actual->nth(i)->type_check(symtab, method_map, error_stream);
                }
        return name; // we should instead be returning last Symbol in "actual"
        }


   Symbol loop_class::type_check(     SymbolTable<Symbol,Symbol> *symtab,
                        std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map,
                        ostream& error_stream)
   {
      if ( pred->type_check(symtab, method_map, error_stream) != Bool )
      {
         error_stream << "You did not use a boolean predicate for the while loop";
      }
      // check if T2 is in the table!!
      // body->type_check(symtab, method_map, error_stream);

      type = Object;
      return type;
   }



   Symbol plus_class::type_check(     SymbolTable<Symbol,Symbol> *symtab,
                        std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map,
                        ostream& error_stream)
{
      if(! (	(e1->type_check(symtab,method_map,error_stream) == Int) 
		&& (e2 ->type_check(symtab,method_map,error_stream) == Int))	 ){
         //error
        error_stream << "Attempted to add two non-integers";
      }

      type = Int;
      return Int;
   }


 Symbol sub_class::type_check(SymbolTable<Symbol,Symbol> *symtab,
                        std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map,
                        ostream& error_stream)
   {


      if(! (    (e1->type_check(symtab,method_map,error_stream) == Int)
                && (e2 ->type_check(symtab,method_map,error_stream) == Int))     ){
         //error
        error_stream << "Attempted to add two non-integers";
      }
      type = Int;
      return type;
   }



  Symbol isvoid_class::type_check(SymbolTable<Symbol,Symbol> *symtab,
                        std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map,
                        ostream& error_stream)
   {
      e1->type_check(symtab, method_map, error_stream);
      return Object; // ????
   }


   Symbol no_expr_class::type_check(SymbolTable<Symbol,Symbol> *symtab,
                        std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map,
                        ostream& error_stream)
   {
      // can use "this" if needed
      return Object; // ????
   }




 Symbol mul_class::type_check(SymbolTable<Symbol,Symbol> *symtab,
                        std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map,
                        ostream& error_stream)
   {

      if(! (    (e1->type_check(symtab,method_map,error_stream) == Int)
                && (e2 ->type_check(symtab,method_map,error_stream) == Int))     ){
         //error
        error_stream << "Attempted to add two non-integers";
      }

      type = Int;
      return Int;
   }


 Symbol divide_class::type_check(SymbolTable<Symbol,Symbol> *symtab,
                        std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map,
                        ostream& error_stream)
   {

      if(! (    (e1->type_check(symtab,method_map,error_stream) == Int)
                && (e2 ->type_check(symtab,method_map,error_stream) == Int))     ){
         //error
        error_stream << "Attempted to add two non-integers";
      }

      type = Int;
      return Int;
   }


 Symbol neg_class::type_check(SymbolTable<Symbol,Symbol> *symtab,
                        std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map,
                        ostream& error_stream)
   {

      if(! (e1->type_check(symtab,method_map,error_stream) != Int) ){
         //error
         error_stream << "You tried to negate a non-integer";
      }

      type = Int;
      return Int;
   }


Symbol lt_class::type_check(   SymbolTable<Symbol,Symbol> *symtab,
                        std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map,
                        ostream& error_stream)
   {
      // this

      if(! (    (e1->type_check(symtab,method_map,error_stream) == Int)
                && (e2 ->type_check(symtab,method_map,error_stream) == Int))     ){
         //error
        error_stream << "Attempted to add two non-integers";
      }

      type = Bool;
      return type;
   }



Symbol eq_class::type_check(     SymbolTable<Symbol,Symbol> *symtab,
                        std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map,
                        ostream& stream)
   {

      Symbol T1 = e1->type_check( symtab, method_map, stream);
      Symbol T2 = e2->type_check( symtab, method_map, stream);
      if ( ((T1 == Bool) && (T2 != Bool)) || ((T2 == Bool) && (T1 != Bool)) ){
         stream << "You tried to check different types for equality v bad";
      }
      if ( ((T1 == Int) && (T2 != Int)) || ((T2 == Int) && (T1 != Int)) )
      {
         stream << "You tried to check different types for equality v bad";
      }
      if ( ((T1 == Str) && (T2 != Str)) || ((T2 == Str) && (T1 != Str)) )
      {
         stream << "You tried to check different types for equality v bad";
      }
      type = Bool;
      return Bool;
  }



   Symbol leq_class::type_check(    SymbolTable<Symbol,Symbol> *symtab,
                                    std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map,
                                    ostream& error_stream)
   {
      e1->type_check(symtab, method_map, error_stream);
      e2->type_check(symtab, method_map, error_stream);
      return Bool;
   }


   Symbol comp_class::type_check(	SymbolTable<Symbol,Symbol> *symtab,
                                    std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map,
                                    ostream& error_stream)
   {
      e1->type_check(symtab, method_map, error_stream);
      return Bool;
   }


   Symbol string_const_class::type_check(	SymbolTable<Symbol,Symbol> *symtab,
                                            std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map,
                                            ostream& error_stream)
   {
      // access this
      //print_escaped_string(stream,token->get_string());
      return token; // how to return that it is a Symbol?
   }



Symbol new__class::type_check(  SymbolTable<Symbol,Symbol> *symtab,
                                std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map,
                                ostream& error_stream)
   {
      // member variables are:
      // type_name, this
      return Object; // CHANGE THIS TO TYPE
    }



Symbol object_class::type_check(    SymbolTable<Symbol,Symbol> *symtab,
                                    std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map,
                                    ostream& error_stream)
{
    type = Object;
    return type;
}



void bool_const_class::type_check(  SymbolTable<Symbol,Symbol> *symtab,
                                    std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map,
                                    ostream& error_stream)
{
    type = Bool;
    return type;
}



void int_const_class::type_check(   SymbolTable<Symbol,Symbol> *symtab,
                                    std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map,
                                    ostream& error_stream)
{
    type = Int;
    return type;
}


Symbol let_class::type_check( SymbolTable<Symbol,Symbol> *symtab,
                            std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map,
                            ostream& error_stream)
{

   stream << pad(n) << "_let\n";
   // identifier
   // type_decl
   init->dump_with_types(stream, n+2);
   body->dump_with_types(stream, n+2);

   type = Object; // FIX THIS, IT'S WRONG
   return type;
}



Symbol block_class::type_check( SymbolTable<Symbol,Symbol> *symtab,
                                std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map,
                                ostream& error_stream)
{
   for(int i = body->first(); body->more(i); i = body->next(i))
     body->nth(i)->dump_with_types(stream, n+2);

    // type of a block is the value of the last expression
    return Object; // FIX THIS, IT'S WRONG
}



Symbol typcase_class::type_check( SymbolTable<Symbol,Symbol> *symtab,
                                std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map,
                                ostream& error_stream)
{


   expr->dump_with_types(stream, n+2);
   for(int i = cases->first(); cases->more(i); i = cases->next(i))
     cases->nth(i)->dump_with_types(stream, n+2);
    return Object; // FIX THIS, THIS IS WRONG
}



Symbol cond_class::type_check(SymbolTable<Symbol,Symbol> *symtab,
                            std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map,
                            ostream& error_stream)
{

    if ( !(pred->type_check(symtab, method_map, error_stream) == Bool) )
    {
        error_stream << "You use a conditional (if/then/else) without a boolean predicate";
    }
    then_exp->type_check(symtab, method_map, error_stream);
    else_exp->type_check(symtab, method_map, error_stream);
    // Find the LEAST UPPER BOUND OF THESE LAST TWO
    return Object; // CHANGE THIS, IT'S WRONG
}


Symbol assign_class::type_check(  SymbolTable<Symbol,Symbol> *symtab,
                                std::map<std::pair<Symbol,Symbol>,std::vector<Symbol> > & method_map,
                                ostream& error_stream)
{
    // WE SHOULD BE RETURNING SOME SUBTYPE THING
    return expr->type_check(symtab, method_map, error_stream);
}








