module scripting.s7_lib_interface;
import stdlib;
import cstdlib;

// alias is private; we don't want the s7_int type leaking, outside world can
// just use long; I want typechecking in case the size in s7 does actually
// change, but that change can be compartmentalized and (hopefully) not escape
// this file
private alias s7_int = long;
private alias s7_double = float;

// This is incomplete.  This is NOT A BUG.  Functions and data structures will
// be added here _as needed_, and no more frequently

extern (C):

struct s7_scheme {}
struct s7_cell {}
alias s7_pointer = s7_cell*;

s7_scheme *s7_init();
void s7_quit(s7_scheme *sc);
s7_pointer s7_eval_c_string(s7_scheme *sc, in char *str);
s7_pointer s7_eval_c_string_with_environment(s7_scheme *sc, in char *str, s7_pointer e);
s7_pointer s7_car(s7_pointer p);
s7_pointer s7_cdr(s7_pointer p);

s7_pointer s7_load(s7_scheme *sc, in char *file);
s7_pointer s7_load_with_environment(s7_scheme *sc, in char *filename, s7_pointer e);
s7_pointer s7_add_to_load_path(s7_scheme *sc, in char *dir);

s7_pointer s7_symbol_table_find_name(s7_scheme *sc, in char *name);

bool s7_is_boolean(s7_pointer x);
bool s7_is_c_pointer(s7_pointer arg);
bool s7_is_float_vector(s7_pointer p);
bool s7_is_integer(s7_pointer p);
bool s7_is_list(s7_scheme *sc, s7_pointer p);
bool s7_is_null(s7_scheme *sc, s7_pointer p);
bool s7_is_ratio(s7_pointer arg); 
bool s7_is_real(s7_pointer p);
bool s7_is_string(s7_pointer p);

s7_pointer s7_make_boolean(s7_scheme *sc, bool x);
s7_pointer s7_make_c_pointer(s7_scheme *sc, void *ptr);
s7_pointer s7_make_float_vector(s7_scheme *sc, s7_int len, s7_int dims, s7_int *dim_info);
s7_pointer s7_make_integer(s7_scheme *sc, s7_int num);
s7_pointer s7_make_real(s7_scheme *sc, s7_double num);
s7_pointer s7_make_string_with_length(s7_scheme *sc, in char *str, s7_int len);
s7_pointer s7_define_function(s7_scheme *sc, in char *name, s7_pointer function(s7_scheme*, s7_pointer) fnc, s7_int required_args, s7_int optional_args, bool rest_arg, in char *doc);
s7_pointer s7_inlet(s7_scheme *sc, s7_pointer bindings);

bool s7_boolean(s7_scheme *sc, s7_pointer x);
s7_int s7_integer(s7_pointer p);
void *s7_c_pointer(s7_pointer p);
s7_double s7_real(s7_pointer p);
s7_int s7_numerator(s7_pointer x);
s7_int s7_denominator(s7_pointer x);
const(char) *s7_string(s7_pointer p);

char *s7_object_to_c_string(s7_scheme *sc, s7_pointer obj);
s7_pointer s7_name_to_value(s7_scheme *sc, in char *name);

s7_int s7_vector_length(s7_pointer vec);
s7_double *s7_float_vector_elements(s7_pointer vec);

s7_pointer s7_nil(s7_scheme *sc);
s7_pointer s7_cons(s7_scheme *sc, s7_pointer a, s7_pointer b);
s7_pointer s7_call(s7_scheme *sc, s7_pointer func, s7_pointer args);
