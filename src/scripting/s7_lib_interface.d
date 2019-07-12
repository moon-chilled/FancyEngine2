module scripting.s7_lib_interface;
import stdlib;
import cstdlib;

// alias is private; we don't want the s7_int type leaking, outside world can
// just use long; I want typechecking in case the size in s7 does actually
// change, but that change can be compartmentalized and (hopefully) not escape
// this file
private alias s7_int = long;
private alias s7_double = double;

// This is incomplete.  This is NOT A BUG.  Functions and data structures will
// be added here _as needed_, and no more frequently

extern (C):

struct s7_scheme {}
struct s7_cell {}
alias s7_pointer = s7_cell*;

s7_scheme *s7_init();
void s7_quit(s7_scheme *sc);
s7_pointer s7_eval_c_string(s7_scheme *sc, const char *str);

bool s7_is_null(s7_scheme *sc, s7_pointer p);
bool s7_is_boolean(s7_pointer x);
bool s7_is_list(s7_scheme *sc, s7_pointer p);
bool s7_is_string(s7_pointer p);
bool s7_is_integer(s7_pointer p);
bool s7_is_real(s7_pointer p);
bool s7_is_ratio(s7_pointer arg); 

bool s7_boolean(s7_scheme *sc, s7_pointer x);
const(char) *s7_string(s7_pointer p);
s7_int s7_integer(s7_pointer p);
s7_double s7_real(s7_pointer p);
s7_int s7_numerator(s7_pointer x);
s7_int s7_denominator(s7_pointer x);

char *s7_object_to_c_string(s7_scheme *sc, s7_pointer obj);
