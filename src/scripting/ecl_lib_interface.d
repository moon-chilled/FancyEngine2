module scripting.ecl_lib_interface;
import stdlib;

import core.stdc.config;
public import core.stdc.stddef;
import core.stdc.stdarg: va_list;
static import core.simd;
static import std.conv;

import derelict.util.loader;
import derelict.util.system;

version (X86_64) {
	alias fixnum = long;
} else version (X86) {
	alias fixnum = int;
}

public class DerelictECLLoader: SharedLibLoader {
	public this() {
		static if (Derelict_OS_Windows) {
			enum lib_name = "ecl.dll";
		} else static if (Derelict_OS_Mac) {
			enum lib_name = "libecl.dylib";
		} else static if (Derelict_OS_Posix) {
			enum lib_name = "libecl.so";
		} else {
			static assert(0);
		}
		super(lib_name);
	}

	protected override void loadSymbols() {
		bindFunc(cast(void**)&cl_boot, "cl_boot");
		bindFunc(cast(void**)&cl_shutdown, "cl_shutdown");
		bindFunc(cast(void**)&si_safe_eval, "si_safe_eval");
		bindFunc(cast(void**)&cl_eval, "cl_eval");
		bindFunc(cast(void**)&si_string_to_object, "si_string_to_object");
		bindFunc(cast(void**)&ecl_make_constant_base_string, "ecl_make_constant_base_string");
		bindFunc(cast(void**)&ecl_make_simple_base_string, "ecl_make_simple_base_string");
		bindFunc(cast(void**)&ecl_make_double_float, "ecl_make_double_float");
		bindFunc(cast(void**)&ecl_def_c_function_va, "ecl_def_c_function_va");
		bindFunc(cast(void**)&ecl_set_option, "ecl_set_option");
		bindFunc(cast(void**)&si_copy_to_simple_base_string, "si_copy_to_simple_base_string");
		bindFunc(cast(void**)&ecl_process_env, "ecl_process_env");
	}
}


__gshared extern (C) nothrow @nogc {
	int function(int, char**) cl_boot;
	void function() cl_shutdown;
	cl_object function(fixnum, cl_object, cl_lispunion*, ...) si_safe_eval;
	cl_object function(cl_lispunion*) cl_eval;
	cl_object function(fixnum, cl_lispunion*, ...) si_string_to_object;
	cl_object function(const char *s, fixnum i) ecl_make_constant_base_string;
	cl_object function(const char *s, fixnum i) ecl_make_simple_base_string;
	fixnum function(cl_object) fixint;
	float function(cl_object) ecl_to_float;
	double function(cl_object) ecl_to_double;
	cl_object function(double) ecl_make_double_float;
	void function(cl_object, cl_object function(fixnum, ...)) ecl_def_c_function_va;
	void function(int, fixnum) ecl_set_option;
	cl_object function(cl_object) si_copy_to_simple_base_string;
	cl_env_struct *function() ecl_process_env;

	pragma(inline, true) cl_object cl_safe_eval(cl_object form, cl_object env, cl_object value) { return si_safe_eval(3, form, env, value); }
	pragma(inline, true) cl_object lsym(string s) { return si_string_to_object(1, s.lstr); }
	pragma(inline, true) cl_object lstr(string s) { return ecl_make_simple_base_string(cast(char*)(s.ptr), s.length); }
	pragma(inline, true) cl_type ecl_t_of(cl_object o) {
		int i = 3&cast(fixnum)o;
		return i ? cast(cl_type)i : cast(cl_type)(o.d.t);
	}

	struct cl_env_struct {
		int disable_interrupts;
		fixnum nvalues;
		cl_object[64] values;
		cl_object function_;
		fixnum stack_size;
		fixnum stack_limit_size;
		cl_object* stack;
		cl_object* stack_top;
		cl_object* stack_limit;
		fixnum thread_local_bindings_size;
		cl_object* thread_local_bindings;
		cl_object bindings_array;
		fixnum bds_size;
		fixnum bds_limit_size;
		//ecl_bds_frame* bds_org;
		//ecl_bds_frame* bds_top;
		//ecl_bds_frame* bds_limit;
		//ecl_ihs_frame* ihs_top;
		fixnum frs_size;
		fixnum frs_limit_size;
		//ecl_frame* frs_org;
		//ecl_frame* frs_top;
		//ecl_frame* frs_limit;
		//ecl_frame* nlj_fr;
		fixnum frame_id;
		fixnum cs_size;
		fixnum cs_limit_size;
		fixnum cs_max_size;
		char* cs_org;
		char* cs_limit;
		char* cs_barrier;
		cl_object string_pool;
		//cl_compiler_env* c_env;
		//cl_object fmt_aux_stream;
		cl_object[3] big_register;
		cl_object own_process;
		cl_object pending_interrupt;
		cl_object signal_queue;
		cl_object signal_queue_spinlock;
		void* default_sigmask;
		//ecl_cache* method_cache;
		//ecl_cache* slot_cache;
		/*
		c_ulong ffi_args_limit;
		void **ffi_types; //_ffi_type** ffi_types; //idk where to get a struct _ffi_type
		ecl_ffi_values* ffi_values;
		ecl_ffi_values** ffi_values_ptrs;
		*/
		void* altstack;
		c_ulong altstack_size;
		int trap_fpe_bits;
		void* old_exception_filter;
		cl_object packages_to_be_created;
		cl_object packages_to_be_created_p;
		void* fault_address;
		int cleanup;
	}


	enum Nil = cast(cl_object)cl_type.t_list;
	enum cl_type {
		t_start = 0,
		t_list = 1,
		t_character = 2,
		t_fixnum = 3,
		t_bignum = 4,
		t_ratio = 5,
		t_singlefloat = 6,
		t_doublefloat = 7,
		t_longfloat = 8,
		t_complex = 9,
		t_symbol = 10,
		t_package = 11,
		t_hashtable = 12,
		t_array = 13,
		t_vector = 14,
		t_string = 15,
		t_base_string = 16,
		t_bitvector = 17,
		t_stream = 18,
		t_random = 19,
		t_readtable = 20,
		t_pathname = 21,
		t_bytecodes = 22,
		t_bclosure = 23,
		t_cfun = 24,
		t_cfunfixed = 25,
		t_cclosure = 26,
		t_instance = 27,
		t_structure = 27,
		t_process = 28,
		t_lock = 29,
		t_rwlock = 30,
		t_condition_variable = 31,
		t_semaphore = 32,
		t_barrier = 33,
		t_mailbox = 34,
		t_codeblock = 35,
		t_foreign = 36,
		t_frame = 37,
		t_weak_pointer = 38,
		t_end = 39,
		t_other = 40,
		t_contiguous = 41,
		FREE = 127,
	}
	struct __mpf_struct {
		int _mp_prec;
		int _mp_size;
		c_ulong _mp_exp;
		c_ulong *_mp_d;
	}
	struct __mpq_struct {
		__mpz_struct _mp_num;
		__mpz_struct _mp_den;
	}
	struct __mpz_struct {
		int _mp_alloc;
		int _mp_size;
		c_ulong* _mp_d;
	}


	alias cl_object = cl_lispunion*;
	union cl_lispunion {
		ecl_bignum big;
		ecl_ratio ratio;
		ecl_singlefloat SF;
		ecl_doublefloat DF;
		ecl_long_float longfloat;
		ecl_complex complex;
		ecl_symbol symbol;
		ecl_package pack;
		ecl_hashtable hash;
		ecl_array array;
		ecl_vector vector;
		ecl_base_string base_string;
		ecl_string string;
		ecl_stream stream;
		/*
		   ecl_random random;
		   ecl_readtable readtable;
		   ecl_pathname pathname;
		   ecl_bytecodes bytecodes;
		   ecl_bclosure bclosure;
		   ecl_cfun cfun;
		   ecl_cfunfixed cfunfixed;
		   ecl_cclosure cclosure;
		   */
		   ecl_dummy d;
		   /*
		   ecl_instance instance;
		   ecl_process process;
		   ecl_queue queue;
		   ecl_lock lock;
		   ecl_rwlock rwlock;
		   ecl_condition_variable condition_variable;
		   ecl_semaphore semaphore;
		   ecl_barrier barrier;
		   ecl_mailbox mailbox;
		   ecl_codeblock cblock;
		   ecl_foreign foreign;
		   ecl_stack_frame frame;
		   ecl_weak_pointer weak;
		 */
	}
	struct ecl_singlefloat {
		byte t;
		byte m;
		byte padding1;
		byte padding2;
		float SFVAL;
	}
	struct ecl_doublefloat {
		byte t;
		byte m;
		byte padding1;
		byte padding2;
		double DFVAL;
	}
	struct ecl_long_float {
		byte t;
		byte m;
		byte padding1;
		byte padding2;
		real value;
	}
	struct ecl_bignum {
		byte t;
		byte m;
		byte padding1;
		byte padding2;
		__mpz_struct[1] big_num;
	}
	struct ecl_ratio {
		byte t;
		byte m;
		byte padding1;
		byte padding2;
		cl_object den;
		cl_object num;
	}
	struct ecl_complex {
		byte t;
		byte m;
		byte padding1;
		byte padding2;
		cl_object real_;
		cl_object imag;
	}
	enum ecl_stype {
		ecl_stp_ordinary = 0,
		ecl_stp_constant = 1,
		ecl_stp_special = 2,
		ecl_stp_macro = 4,
		ecl_stp_special_form = 8,
	}
	enum ecl_stp_ordinary = ecl_stype.ecl_stp_ordinary;
	enum ecl_stp_constant = ecl_stype.ecl_stp_constant;
	enum ecl_stp_special = ecl_stype.ecl_stp_special;
	enum ecl_stp_macro = ecl_stype.ecl_stp_macro;
	enum ecl_stp_special_form = ecl_stype.ecl_stp_special_form;
	struct ecl_symbol {
		byte t;
		byte m;
		byte stype;
		byte dynamic;
		cl_object value;
		cl_object gfdef;
		cl_object plist;
		cl_object name;
		cl_object hpack;
		c_ulong binding;
	}
	struct ecl_package {
		byte t;
		byte m;
		byte locked;
		byte padding;
		cl_object name;
		cl_object nicknames;
		cl_object shadowings;
		cl_object uses;
		cl_object usedby;
		cl_object internal;
		cl_object external;
	}
	struct ecl_hashtable_entry {
		cl_object key;
		cl_object value;
	}
	struct ecl_hashtable {
		byte t;
		byte m;
		byte test;
		byte weak;
		ecl_hashtable_entry* data;
		c_ulong entries;
		c_ulong size;
		c_ulong limit;
		cl_object rehash_size;
		cl_object threshold;
		double factor;
		cl_object function(cl_lispunion*, cl_lispunion*, cl_lispunion*) get;
		cl_object function(cl_lispunion*, cl_lispunion*, cl_lispunion*) set;
		int function(cl_object, cl_lispunion*) rem;
	}
	enum cl_elttype {
		ecl_aet_object = 0,
		ecl_aet_sf = 1,
		ecl_aet_df = 2,
		ecl_aet_bit = 3,
		ecl_aet_fix = 4,
		ecl_aet_index = 5,
		ecl_aet_b8 = 6,
		ecl_aet_i8 = 7,
		ecl_aet_b16 = 8,
		ecl_aet_i16 = 9,
		ecl_aet_b32 = 10,
		ecl_aet_i32 = 11,
		ecl_aet_b64 = 12,
		ecl_aet_i64 = 13,
		ecl_aet_ch = 14,
		ecl_aet_bc = 15,
		ecl_aet_last_type = 15,
	}
	enum ecl_aet_object = cl_elttype.ecl_aet_object;
	enum ecl_aet_sf = cl_elttype.ecl_aet_sf;
	enum ecl_aet_df = cl_elttype.ecl_aet_df;
	enum ecl_aet_bit = cl_elttype.ecl_aet_bit;
	enum ecl_aet_fix = cl_elttype.ecl_aet_fix;
	enum ecl_aet_index = cl_elttype.ecl_aet_index;
	enum ecl_aet_b8 = cl_elttype.ecl_aet_b8;
	enum ecl_aet_i8 = cl_elttype.ecl_aet_i8;
	enum ecl_aet_b16 = cl_elttype.ecl_aet_b16;
	enum ecl_aet_i16 = cl_elttype.ecl_aet_i16;
	enum ecl_aet_b32 = cl_elttype.ecl_aet_b32;
	enum ecl_aet_i32 = cl_elttype.ecl_aet_i32;
	enum ecl_aet_b64 = cl_elttype.ecl_aet_b64;
	enum ecl_aet_i64 = cl_elttype.ecl_aet_i64;
	enum ecl_aet_ch = cl_elttype.ecl_aet_ch;
	enum ecl_aet_bc = cl_elttype.ecl_aet_bc;
	enum ecl_aet_last_type = cl_elttype.ecl_aet_last_type;
	union ecl_array_data
	{
		cl_object* t;
		ubyte* bc;
		int* c;
		ubyte* b8;
		byte* i8;
		ushort* b16;
		short* i16;
		uint* b32;
		int* i32;
		c_ulong* b64;
		c_long* i64;
		float* sf;
		double* df;
		c_long* fix;
		c_ulong* index;
		ubyte* bit;
	}
	struct ecl_array
	{
		byte t;
		byte m;
		byte elttype;
		byte flags;
		cl_object displaced;
		c_ulong dim;
		c_ulong* dims;
		ecl_array_data self;
		ubyte offset;
		ubyte rank;
	}
	struct ecl_vector
	{
		byte t;
		byte m;
		byte elttype;
		byte flags;
		cl_object displaced;
		c_ulong dim;
		c_ulong fillp;
		ecl_array_data self;
		ubyte offset;
	}
	struct ecl_base_string {
		byte t;
		byte m;
		byte elttype;
		byte flags;
		cl_object displaced;
		fixnum dim;
		fixnum fillp;
		ubyte* self;
	}
	struct ecl_string {
		byte t;
		byte m;
		byte elttype;
		byte flags;
		cl_object displaced;
		fixnum dim;
		fixnum fillp;
		wchar_t* self;
	}
	enum ecl_smmode
	{
		ecl_smm_input = 0,
		ecl_smm_input_file = 1,
		ecl_smm_output = 2,
		ecl_smm_output_file = 3,
		ecl_smm_io = 4,
		ecl_smm_io_file = 5,
		ecl_smm_synonym = 6,
		ecl_smm_broadcast = 7,
		ecl_smm_concatenated = 8,
		ecl_smm_two_way = 9,
		ecl_smm_echo = 10,
		ecl_smm_string_input = 11,
		ecl_smm_string_output = 12,
		ecl_smm_probe = 13,
		ecl_smm_sequence_input = 14,
		ecl_smm_sequence_output = 15,
	}
	enum ecl_smm_input = ecl_smmode.ecl_smm_input;
	enum ecl_smm_input_file = ecl_smmode.ecl_smm_input_file;
	enum ecl_smm_output = ecl_smmode.ecl_smm_output;
	enum ecl_smm_output_file = ecl_smmode.ecl_smm_output_file;
	enum ecl_smm_io = ecl_smmode.ecl_smm_io;
	enum ecl_smm_io_file = ecl_smmode.ecl_smm_io_file;
	enum ecl_smm_synonym = ecl_smmode.ecl_smm_synonym;
	enum ecl_smm_broadcast = ecl_smmode.ecl_smm_broadcast;
	enum ecl_smm_concatenated = ecl_smmode.ecl_smm_concatenated;
	enum ecl_smm_two_way = ecl_smmode.ecl_smm_two_way;
	enum ecl_smm_echo = ecl_smmode.ecl_smm_echo;
	enum ecl_smm_string_input = ecl_smmode.ecl_smm_string_input;
    enum ecl_smm_string_output = ecl_smmode.ecl_smm_string_output;
    enum ecl_smm_probe = ecl_smmode.ecl_smm_probe;
    enum ecl_smm_sequence_input = ecl_smmode.ecl_smm_sequence_input;
    enum ecl_smm_sequence_output = ecl_smmode.ecl_smm_sequence_output;
    struct ecl_file_ops
    {
        c_ulong function(cl_object, ubyte*, c_ulong) write_byte8;
        c_ulong function(cl_object, ubyte*, c_ulong) read_byte8;
        void function(cl_object, cl_lispunion*) write_byte;
        cl_object function(cl_lispunion*) read_byte;
        int function(cl_object) read_char;
        int function(cl_object, int) write_char;
        void function(cl_object, int) unread_char;
        int function(cl_object) peek_char;
        c_ulong function(cl_object, cl_lispunion*, c_ulong, c_ulong) read_vector;
        c_ulong function(cl_object, cl_lispunion*, c_ulong, c_ulong) write_vector;
        int function(cl_object) listen;
        void function(cl_object) clear_input;
        void function(cl_object) clear_output;
        void function(cl_object) finish_output;
        void function(cl_object) force_output;
        int function(cl_object) input_p;
        int function(cl_object) output_p;
        int function(cl_object) interactive_p;
        cl_object function(cl_lispunion*) element_type;
        cl_object function(cl_lispunion*) length;
        cl_object function(cl_lispunion*) get_position;
        cl_object function(cl_lispunion*, cl_lispunion*) set_position;
        int function(cl_object) column;
        cl_object function(cl_lispunion*) close;
    }
    enum Ecl_Stream {
        binary = 0,
        format = 15,
        default_format = 2,
        iso_8859_1 = 1,
        latin_1 = 1,
        utf_8 = 2,
        ucs_2 = 3,
        ucs_2le = 133,
        ucs_2be = 5,
        ucs_4 = 6,
        ucs_4le = 135,
        ucs_4be = 7,
        user_format = 8,
        us_ascii = 10,
        cr = 16,
        lf = 32,
        signed_bytes = 64,
        little_endian = 128,
        c_stream = 256,
        might_seek = 512,
        close_components = 1024,
    }
    alias cl_eformat_decoder = int function(cl_object);
    alias cl_eformat_encoder = int function(cl_object, ubyte*, int);
    alias cl_eformat_read_byte8 = c_ulong function(cl_object, ubyte*, c_ulong);
    struct ecl_stream {
        byte t;
        byte m;
        byte mode;
        byte closed;
        ecl_file_ops* ops;
        static union _Anonymous_9 {
		import core.stdc.stdio: FILE;
            FILE *stream;
            c_long descriptor;
        }
        _Anonymous_9 file;
        cl_object object0;
        cl_object object1;
        cl_object byte_stack;
        c_ulong column;
        c_long last_char;
        c_long[2] last_code;
        c_long int0;
        c_long int1;
        c_ulong byte_size;
        c_long last_op;
        char* buffer;
        cl_object format;
        int function(cl_object, ubyte*, int) encoder;
        int function(cl_object) decoder;
        cl_object format_table;
        int flags;
        int eof_char;
    }

	struct ecl_dummy {
		byte t;
		byte m;
		byte padding1;
		byte padding2;
	}

    enum ecl_option {
        incremental_gc = 0,
        trap_sigsegv = 1,
        trap_sigfpe = 2,
        trap_sigint = 3,
        trap_sigill = 4,
        trap_sigbus = 5,
        trap_sigpipe = 6,
        trap_sigchld = 7,
        trap_interrupt_signal = 8,
        signal_handling_thread = 9,
        signal_queue_size = 10,
        booted = 11,
        bind_stack_size = 12,
        bind_stack_safety_area = 13,
        frame_stack_size = 14,
        frame_stack_safety_area = 15,
        lisp_stack_size = 16,
        lisp_stack_safety_area = 17,
        c_stack_size = 18,
        c_stack_safety_area = 19,
        sigaltstack_size = 20,
        heap_size = 21,
        heap_safety_area = 22,
        thread_interrupt_signal = 23,
        set_gmp_memory_functions = 24,
        use_setmode_on_files = 25,
        limit = 26,
    }
}


/+
extern(C) {
    struct ucontext_t {
        c_ulong uc_flags;
        ucontext_t* uc_link;
        stack_t uc_stack;
        mcontext_t uc_mcontext;
        __sigset_t uc_sigmask;
        _libc_fpstate __fpregs_mem;
        ulong[4] __ssp;
    }
    struct mcontext_t {
        long[23] gregs;
        _libc_fpstate* fpregs;
        ulong[8] __reserved1;
    }
    alias fpregset_t = _libc_fpstate*;
    struct _libc_fpstate {
        ushort cwd;
        ushort swd;
        ushort ftw;
        ushort fop;
        c_ulong rip;
        c_ulong rdp;
        uint mxcsr;
        uint mxcr_mask;
        _libc_fpxreg[8] _st;
        _libc_xmmreg[16] _xmm;
        uint[24] __glibc_reserved1;
    }
    struct _libc_xmmreg {
        uint[4] element;
    }
    struct _libc_fpxreg
    {
        ushort[4] significand;
        ushort exponent;
        ushort[3] __glibc_reserved1;
    }
    alias gregset_t = long[23];
    alias greg_t = long;
    alias fsfilcnt_t = c_ulong;
    alias fsblkcnt_t = c_ulong;
    alias blkcnt_t = c_long;
    alias blksize_t = c_long;
    alias register_t = c_long;
    alias u_int64_t = c_ulong;
    alias u_int32_t = uint;
    alias u_int16_t = ushort;
    alias u_int8_t = ubyte;
    alias key_t = int;
    alias caddr_t = char*;
    alias daddr_t = int;
    alias ssize_t = c_long;
    alias id_t = uint;
    alias pid_t = int;
    alias off_t = c_long;
    alias uid_t = uint;
    alias nlink_t = c_ulong;
    alias mode_t = uint;
    alias gid_t = uint;
    alias dev_t = c_ulong;
    alias ino_t = c_ulong;
    alias loff_t = c_long;
    alias fsid_t = __fsid_t;
    alias u_quad_t = c_ulong;
    alias quad_t = c_long;
    alias u_long = c_ulong;
    alias u_int = uint;
    alias u_short = ushort;
    alias u_char = ubyte;
    int pselect(int, fd_set*, fd_set*, fd_set*, const(timespec)*, const(__sigset_t)*) @nogc nothrow;
    int select(int, fd_set*, fd_set*, fd_set*, timeval*) @nogc nothrow;
    alias fd_mask = c_long;
    struct fd_set
    {
        c_long[16] __fds_bits;
    }
    alias __fd_mask = c_long;
    alias suseconds_t = c_long;
    enum _Anonymous_0
    {
        GMP_ERROR_NONE = 0,
        GMP_ERROR_UNSUPPORTED_ARGUMENT = 1,
        GMP_ERROR_DIVISION_BY_ZERO = 2,
        GMP_ERROR_SQRT_OF_NEGATIVE = 4,
        GMP_ERROR_INVALID_ARGUMENT = 8,
    }
    enum GMP_ERROR_NONE = _Anonymous_0.GMP_ERROR_NONE;
    enum GMP_ERROR_UNSUPPORTED_ARGUMENT = _Anonymous_0.GMP_ERROR_UNSUPPORTED_ARGUMENT;
    enum GMP_ERROR_DIVISION_BY_ZERO = _Anonymous_0.GMP_ERROR_DIVISION_BY_ZERO;
    enum GMP_ERROR_SQRT_OF_NEGATIVE = _Anonymous_0.GMP_ERROR_SQRT_OF_NEGATIVE;
    enum GMP_ERROR_INVALID_ARGUMENT = _Anonymous_0.GMP_ERROR_INVALID_ARGUMENT;
    c_long __gmpn_sec_invert_itch(c_long) @nogc nothrow;
    int __gmpn_sec_invert(c_ulong*, c_ulong*, const(c_ulong)*, c_long, c_ulong, c_ulong*) @nogc nothrow;
    c_long __gmpn_sec_div_r_itch(c_long, c_long) @nogc nothrow;
    void __gmpn_sec_div_r(c_ulong*, c_long, const(c_ulong)*, c_long, c_ulong*) @nogc nothrow;
    c_long __gmpn_sec_div_qr_itch(c_long, c_long) @nogc nothrow;
    c_ulong __gmpn_sec_div_qr(c_ulong*, c_ulong*, c_long, const(c_ulong)*, c_long, c_ulong*) @nogc nothrow;
    void __gmpn_sec_tabselect(c_ulong*, const(c_ulong)*, c_long, c_long, c_long) @nogc nothrow;
    c_long __gmpn_sec_powm_itch(c_long, c_ulong, c_long) @nogc nothrow;
    void __gmpn_sec_powm(c_ulong*, const(c_ulong)*, c_long, const(c_ulong)*, c_ulong, const(c_ulong)*, c_long, c_ulong*) @nogc nothrow;
    c_long __gmpn_sec_sqr_itch(c_long) @nogc nothrow;
    void __gmpn_sec_sqr(c_ulong*, const(c_ulong)*, c_long, c_ulong*) @nogc nothrow;
    c_long __gmpn_sec_mul_itch(c_long, c_long) @nogc nothrow;
    void __gmpn_sec_mul(c_ulong*, const(c_ulong)*, c_long, const(c_ulong)*, c_long, c_ulong*) @nogc nothrow;
    void __gmpn_cnd_swap(c_ulong, c_ulong*, c_ulong*, c_long) @nogc nothrow;
    c_long __gmpn_sec_sub_1_itch(c_long) @nogc nothrow;
    c_ulong __gmpn_sec_sub_1(c_ulong*, const(c_ulong)*, c_long, c_ulong, c_ulong*) @nogc nothrow;
    c_long __gmpn_sec_add_1_itch(c_long) @nogc nothrow;
    c_ulong __gmpn_sec_add_1(c_ulong*, const(c_ulong)*, c_long, c_ulong, c_ulong*) @nogc nothrow;
    struct max_align_t
    {
        long __clang_max_align_nonce1;
        real __clang_max_align_nonce2;
    }
    c_ulong __gmpn_cnd_sub_n(c_ulong, c_ulong*, const(c_ulong)*, const(c_ulong)*, c_long) @nogc nothrow;
    c_ulong __gmpn_cnd_add_n(c_ulong, c_ulong*, const(c_ulong)*, const(c_ulong)*, c_long) @nogc nothrow;
    void __gmpn_zero(c_ulong*, c_long) @nogc nothrow;
    void __gmpn_copyd(c_ulong*, const(c_ulong)*, c_long) @nogc nothrow;
    void __gmpn_copyi(c_ulong*, const(c_ulong)*, c_long) @nogc nothrow;
    void __gmpn_xnor_n(c_ulong*, const(c_ulong)*, const(c_ulong)*, c_long) @nogc nothrow;
    void __gmpn_xor_n(c_ulong*, const(c_ulong)*, const(c_ulong)*, c_long) @nogc nothrow;
    void __gmpn_nior_n(c_ulong*, const(c_ulong)*, const(c_ulong)*, c_long) @nogc nothrow;
    void __gmpn_iorn_n(c_ulong*, const(c_ulong)*, const(c_ulong)*, c_long) @nogc nothrow;
    void __gmpn_ior_n(c_ulong*, const(c_ulong)*, const(c_ulong)*, c_long) @nogc nothrow;
    void __gmpn_nand_n(c_ulong*, const(c_ulong)*, const(c_ulong)*, c_long) @nogc nothrow;
    void __gmpn_andn_n(c_ulong*, const(c_ulong)*, const(c_ulong)*, c_long) @nogc nothrow;
    alias ptrdiff_t = c_long;
    alias size_t = c_ulong;
    alias wchar_t = int;


    void __gmpn_and_n(c_ulong*, const(c_ulong)*, const(c_ulong)*, c_long) @nogc nothrow;
    void __gmpn_tdiv_qr(c_ulong*, c_ulong*, c_long, const(c_ulong)*, c_long, const(c_ulong)*, c_long) @nogc nothrow;
    c_ulong __gmpn_submul_1(c_ulong*, const(c_ulong)*, c_long, c_ulong) @nogc nothrow;
    void* dlopen(const(char)*, int) @nogc nothrow;
    int dlclose(void*) @nogc nothrow;
    void* dlsym(void*, const(char)*) @nogc nothrow;
    char* dlerror() @nogc nothrow;
    c_ulong __gmpn_sub_n(c_ulong*, const(c_ulong)*, const(c_ulong)*, c_long) @nogc nothrow;
    c_ulong __gmpn_sub_1(c_ulong*, const(c_ulong)*, c_long, c_ulong) @nogc nothrow;
    c_ulong __gmpn_sub(c_ulong*, const(c_ulong)*, c_long, const(c_ulong)*, c_long) @nogc nothrow;
    c_long __gmpn_sqrtrem(c_ulong*, c_ulong*, const(c_ulong)*, c_long) @nogc nothrow;
    c_ulong __gmpn_sizeinbase(const(c_ulong)*, c_long, int) @nogc nothrow;
    c_long __gmpn_set_str(c_ulong*, const(ubyte)*, c_ulong, int) @nogc nothrow;
    c_ulong __gmpn_scan1(const(c_ulong)*, c_ulong) @nogc nothrow;
    c_ulong __gmpn_scan0(const(c_ulong)*, c_ulong) @nogc nothrow;
    c_ulong __gmpn_rshift(c_ulong*, const(c_ulong)*, c_long, uint) @nogc nothrow;
    alias cl_fixnum = c_long;
    alias cl_index = c_ulong;
    alias cl_hashkey = c_ulong;
    alias ecl_character = int;
    alias ecl_base_char = ubyte;
    void __gmpn_random2(c_ulong*, c_long) @nogc nothrow;
    void __gmpn_random(c_ulong*, c_long) @nogc nothrow;
    c_ulong __gmpn_preinv_mod_1(const(c_ulong)*, c_long, c_ulong, c_ulong) @nogc nothrow;
    c_long __gmpn_pow_1(c_ulong*, const(c_ulong)*, c_long, c_ulong, c_ulong*) @nogc nothrow;
    c_ulong __gmpn_popcount(const(c_ulong)*, c_long) @nogc nothrow;
    int __gmpn_perfect_power_p(const(c_ulong)*, c_long) @nogc nothrow;
    static cl_object _ecl_car(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_cdr(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_caar(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_cdar(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_cadr(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_cddr(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_caaar(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_cdaar(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_cadar(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_cddar(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_caadr(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_cdadr(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_caddr(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_cdddr(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_caaaar(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_cdaaar(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_cadaar(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_cddaar(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_caadar(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_cdadar(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_caddar(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_cdddar(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_caaadr(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_cdaadr(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_cadadr(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_cddadr(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_caaddr(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_cdaddr(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_cadddr(cl_lispunion*) @nogc nothrow;
    static cl_object _ecl_cddddr(cl_lispunion*) @nogc nothrow;
    cl_object ecl_car(cl_lispunion*) @nogc nothrow;
    cl_object ecl_cdr(cl_lispunion*) @nogc nothrow;
    cl_object ecl_caar(cl_lispunion*) @nogc nothrow;
    cl_object ecl_cdar(cl_lispunion*) @nogc nothrow;
    cl_object ecl_cadr(cl_lispunion*) @nogc nothrow;
    cl_object ecl_cddr(cl_lispunion*) @nogc nothrow;
    cl_object ecl_caaar(cl_lispunion*) @nogc nothrow;
    cl_object ecl_cdaar(cl_lispunion*) @nogc nothrow;
    cl_object ecl_cadar(cl_lispunion*) @nogc nothrow;
    cl_object ecl_cddar(cl_lispunion*) @nogc nothrow;
    cl_object ecl_caadr(cl_lispunion*) @nogc nothrow;
    cl_object ecl_cdadr(cl_lispunion*) @nogc nothrow;
    cl_object ecl_caddr(cl_lispunion*) @nogc nothrow;
    cl_object ecl_cdddr(cl_lispunion*) @nogc nothrow;
    cl_object ecl_caaaar(cl_lispunion*) @nogc nothrow;
    cl_object ecl_cdaaar(cl_lispunion*) @nogc nothrow;
    cl_object ecl_cadaar(cl_lispunion*) @nogc nothrow;
    cl_object ecl_cddaar(cl_lispunion*) @nogc nothrow;
    cl_object ecl_caadar(cl_lispunion*) @nogc nothrow;
    cl_object ecl_cdadar(cl_lispunion*) @nogc nothrow;
    cl_object ecl_caddar(cl_lispunion*) @nogc nothrow;
    cl_object ecl_cdddar(cl_lispunion*) @nogc nothrow;
    cl_object ecl_caaadr(cl_lispunion*) @nogc nothrow;
    cl_object ecl_cdaadr(cl_lispunion*) @nogc nothrow;
    cl_object ecl_cadadr(cl_lispunion*) @nogc nothrow;
    cl_object ecl_cddadr(cl_lispunion*) @nogc nothrow;
    cl_object ecl_caaddr(cl_lispunion*) @nogc nothrow;
    cl_object ecl_cdaddr(cl_lispunion*) @nogc nothrow;
    cl_object ecl_cadddr(cl_lispunion*) @nogc nothrow;
    cl_object ecl_cddddr(cl_lispunion*) @nogc nothrow;
    int __gmpn_perfect_square_p(const(c_ulong)*, c_long) @nogc nothrow;
    void __gmpn_com(c_ulong*, const(c_ulong)*, c_long) @nogc nothrow;
    c_ulong __gmpn_neg(c_ulong*, const(c_ulong)*, c_long) @nogc nothrow;
    void __gmpn_sqr(c_ulong*, const(c_ulong)*, c_long) @nogc nothrow;
    void __gmpn_mul_n(c_ulong*, const(c_ulong)*, const(c_ulong)*, c_long) @nogc nothrow;
    c_ulong __gmpn_mul_1(c_ulong*, const(c_ulong)*, c_long, c_ulong) @nogc nothrow;
    c_ulong __gmpn_mul(c_ulong*, const(c_ulong)*, c_long, const(c_ulong)*, c_long) @nogc nothrow;
    c_ulong __gmpn_mod_1(const(c_ulong)*, c_long, c_ulong) @nogc nothrow;
    c_ulong __gmpn_lshift(c_ulong*, const(c_ulong)*, c_long, uint) @nogc nothrow;
    cl_object cl_car(cl_lispunion*) @nogc nothrow;
    cl_object cl_cdr(cl_lispunion*) @nogc nothrow;
    cl_object cl_caar(cl_lispunion*) @nogc nothrow;
    cl_object cl_cdar(cl_lispunion*) @nogc nothrow;
    cl_object cl_cadr(cl_lispunion*) @nogc nothrow;
    cl_object cl_cddr(cl_lispunion*) @nogc nothrow;
    cl_object cl_caaar(cl_lispunion*) @nogc nothrow;
    cl_object cl_cdaar(cl_lispunion*) @nogc nothrow;
    cl_object cl_cadar(cl_lispunion*) @nogc nothrow;
    cl_object cl_cddar(cl_lispunion*) @nogc nothrow;
    cl_object cl_caadr(cl_lispunion*) @nogc nothrow;
    cl_object cl_cdadr(cl_lispunion*) @nogc nothrow;
    cl_object cl_caddr(cl_lispunion*) @nogc nothrow;
    cl_object cl_cdddr(cl_lispunion*) @nogc nothrow;
    cl_object cl_caaaar(cl_lispunion*) @nogc nothrow;
    cl_object cl_cdaaar(cl_lispunion*) @nogc nothrow;
    cl_object cl_cadaar(cl_lispunion*) @nogc nothrow;
    cl_object cl_cddaar(cl_lispunion*) @nogc nothrow;
    cl_object cl_caadar(cl_lispunion*) @nogc nothrow;
    cl_object cl_cdadar(cl_lispunion*) @nogc nothrow;
    cl_object cl_caddar(cl_lispunion*) @nogc nothrow;
    cl_object cl_cdddar(cl_lispunion*) @nogc nothrow;
    cl_object cl_caaadr(cl_lispunion*) @nogc nothrow;
    cl_object cl_cdaadr(cl_lispunion*) @nogc nothrow;
    cl_object cl_cadadr(cl_lispunion*) @nogc nothrow;
    cl_object cl_cddadr(cl_lispunion*) @nogc nothrow;
    cl_object cl_caaddr(cl_lispunion*) @nogc nothrow;
    cl_object cl_cdaddr(cl_lispunion*) @nogc nothrow;
    cl_object cl_cadddr(cl_lispunion*) @nogc nothrow;
    cl_object cl_cddddr(cl_lispunion*) @nogc nothrow;
    alias ecl_init_function_t = void function(cl_object);
    c_ulong __gmpn_hamdist(const(c_ulong)*, const(c_ulong)*, c_long) @nogc nothrow;
    alias cl_env_ptr = cl_env_struct*;
    struct ecl_bds_frame
    {
        cl_object symbol;
        cl_object value;
    }
    struct ecl_ihs_frame
    {
        ecl_ihs_frame* next;
        cl_object function_;
        cl_object lex_env;
        c_ulong index;
        c_ulong bds;
    }
    struct ecl_frame
    {
        __jmp_buf_tag[1] frs_jmpbuf;
        cl_object frs_val;
        c_ulong frs_bds_top_index;
        ecl_ihs_frame* frs_ihs;
        c_ulong frs_sp;
    }
    cl_env_struct* ecl_process_env() @nogc nothrow;
    struct cl_core_struct
    {
        cl_object packages;
        cl_object lisp_package;
        cl_object user_package;
        cl_object keyword_package;
        cl_object system_package;
        cl_object ext_package;
        cl_object clos_package;
        cl_object gray_package;
        cl_object mp_package;
        cl_object c_package;
        cl_object ffi_package;
        cl_object pathname_translations;
        cl_object library_pathname;
        cl_object terminal_io;
        cl_object null_stream;
        cl_object standard_input;
        cl_object standard_output;
        cl_object error_output;
        cl_object standard_readtable;
        cl_object dispatch_reader;
        cl_object default_dispatch_macro;
        cl_object char_names;
        cl_object null_string;
        cl_object plus_half;
        cl_object minus_half;
        cl_object imag_unit;
        cl_object minus_imag_unit;
        cl_object imag_two;
        cl_object singlefloat_zero;
        cl_object doublefloat_zero;
        cl_object singlefloat_minus_zero;
        cl_object doublefloat_minus_zero;
        cl_object longfloat_zero;
        cl_object longfloat_minus_zero;
        cl_object gensym_prefix;
        cl_object gentemp_prefix;
        cl_object gentemp_counter;
        cl_object Jan1st1970UT;
        cl_object system_properties;
        cl_object setf_definitions;
        cl_object processes;
        cl_object processes_spinlock;
        cl_object global_lock;
        cl_object error_lock;
        cl_object global_env_lock;
        cl_object libraries;
        c_ulong max_heap_size;
        cl_object bytes_consed;
        cl_object gc_counter;
        int gc_stats;
        int path_max;
        char* safety_region;
        void* default_sigmask;
        c_ulong default_sigmask_bytes;
        c_ulong last_var_index;
        cl_object reused_indices;
        cl_object slash;
        cl_object compiler_dispatch;
        cl_object rehash_size;
        cl_object rehash_threshold;
        cl_object external_processes;
        cl_object external_processes_lock;
        cl_object known_signals;
    }
    extern __gshared cl_core_struct cl_core;
    cl_object ecl_alloc_object(cl_type) @nogc nothrow;
    cl_object ecl_alloc_instance(c_ulong) @nogc nothrow;
    pragma(mangle, "ecl_cons") cl_object ecl_cons_(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_list1(cl_lispunion*) @nogc nothrow;
    cl_object si_gc(c_long, ...) @nogc nothrow;
    cl_object si_gc_dump() @nogc nothrow;
    cl_object si_gc_stats(cl_lispunion*) @nogc nothrow;
    void* ecl_alloc_unprotected(c_ulong) @nogc nothrow;
    void* ecl_alloc_atomic_unprotected(c_ulong) @nogc nothrow;
    void* ecl_alloc(c_ulong) @nogc nothrow;
    void* ecl_alloc_atomic(c_ulong) @nogc nothrow;
    void* ecl_alloc_uncollectable(c_ulong) @nogc nothrow;
    void ecl_free_uncollectable(void*) @nogc nothrow;
    void ecl_dealloc(void*) @nogc nothrow;
    c_ulong __gmpn_get_str(ubyte*, int, c_ulong*, c_long) @nogc nothrow;
    cl_object ecl_alloc_compact_object(cl_type, c_ulong) @nogc nothrow;
    cl_object si_make_weak_pointer(cl_lispunion*) @nogc nothrow;
    cl_object si_weak_pointer_value(cl_lispunion*) @nogc nothrow;
    cl_object si_mangle_name(c_long, cl_lispunion*, ...) @nogc nothrow;
    union cl_symbol_initializer
    {
        static struct _Anonymous_1
        {
            const(char)* name;
            int type;
            void* fun;
            short narg;
            cl_object value;
        }
        _Anonymous_1 init;
        ecl_symbol data;
    }
    extern __gshared cl_symbol_initializer[0] cl_symbols;
    extern __gshared c_ulong cl_num_symbols_in_core;
    cl_object APPLY_fixed(c_long, cl_lispunion* function(), cl_lispunion**) @nogc nothrow;
    cl_object APPLY(c_long, cl_lispunion* function(c_long, ...), cl_lispunion**) @nogc nothrow;
    cl_object cl_row_major_aref(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_row_major_aset(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_make_vector(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    void* ecl_row_major_ptr(cl_object, c_ulong, c_ulong) @nogc nothrow;
    cl_object cl_array_element_type(cl_lispunion*) @nogc nothrow;
    cl_object cl_array_rank(cl_lispunion*) @nogc nothrow;
    cl_object cl_array_dimension(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_array_total_size(cl_lispunion*) @nogc nothrow;
    cl_object cl_adjustable_array_p(cl_lispunion*) @nogc nothrow;
    cl_object cl_array_displacement(cl_lispunion*) @nogc nothrow;
    cl_object si_array_raw_data(cl_lispunion*) @nogc nothrow;
    cl_object si_array_element_type_byte_size(cl_lispunion*) @nogc nothrow;
    cl_object cl_svref(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_svset(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_array_has_fill_pointer_p(cl_lispunion*) @nogc nothrow;
    cl_object cl_fill_pointer(cl_lispunion*) @nogc nothrow;
    cl_object si_fill_pointer_set(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_replace_array(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_aref(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_aset(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_make_pure_array(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_copy_subarray(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_fill_array_with_elt(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    void FEwrong_dimensions(cl_object, c_ulong) @nogc nothrow;
    c_ulong ecl_to_index(cl_object) @nogc nothrow;
    c_ulong ecl_array_dimension(cl_object, c_ulong) @nogc nothrow;
    c_ulong ecl_array_rank(cl_object) @nogc nothrow;
    cl_object ecl_aref_unsafe(cl_lispunion*, c_ulong) @nogc nothrow;
    cl_object ecl_aset_unsafe(cl_lispunion*, c_ulong, cl_lispunion*) @nogc nothrow;
    cl_object ecl_aref(cl_lispunion*, c_ulong) @nogc nothrow;
    cl_object ecl_aref1(cl_lispunion*, c_ulong) @nogc nothrow;
    cl_object ecl_aset(cl_lispunion*, c_ulong, cl_lispunion*) @nogc nothrow;
    cl_object ecl_aset1(cl_lispunion*, c_ulong, cl_lispunion*) @nogc nothrow;
    void ecl_array_allocself(cl_object) @nogc nothrow;
    cl_object ecl_alloc_simple_vector(c_ulong, cl_elttype) @nogc nothrow;
    cl_elttype ecl_array_elttype(cl_object) @nogc nothrow;
    cl_elttype ecl_symbol_to_elttype(cl_object) @nogc nothrow;
    cl_object ecl_elttype_to_symbol(cl_elttype) @nogc nothrow;
    void ecl_copy_subarray(cl_object, c_ulong, cl_lispunion*, c_ulong, c_ulong) @nogc nothrow;
    void ecl_reverse_subarray(cl_object, c_ulong, c_ulong) @nogc nothrow;
    cl_object cl_set(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_makunbound(cl_lispunion*) @nogc nothrow;
    cl_object cl_fmakunbound(cl_lispunion*) @nogc nothrow;
    cl_object si_fset(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_get_sysprop(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_put_sysprop(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_rem_sysprop(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_setf_definition(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_setf_definition(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    void ecl_clear_compiler_properties(cl_object) @nogc nothrow;
    c_long __gmpn_gcdext(c_ulong*, c_ulong*, c_long*, c_ulong*, c_long, c_ulong*, c_long) @nogc nothrow;
    cl_object _ecl_fix_times_fix(c_long, c_long) @nogc nothrow;
    cl_object _ecl_big_register_copy(cl_lispunion*) @nogc nothrow;
    cl_object _ecl_big_register_normalize(cl_lispunion*) @nogc nothrow;
    cl_object _ecl_big_times_fix(cl_lispunion*, c_long) @nogc nothrow;
    cl_object _ecl_big_times_big(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object _ecl_big_plus_fix(cl_lispunion*, c_long) @nogc nothrow;
    cl_object _ecl_big_plus_big(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object _ecl_fix_minus_big(c_long, cl_lispunion*) @nogc nothrow;
    cl_object _ecl_big_minus_big(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object _ecl_fix_divided_by_big(c_long, cl_lispunion*) @nogc nothrow;
    cl_object _ecl_big_divided_by_fix(cl_lispunion*, c_long) @nogc nothrow;
    cl_object _ecl_big_divided_by_big(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object _ecl_big_gcd(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object _ecl_big_ceiling(cl_lispunion*, cl_lispunion*, cl_lispunion**) @nogc nothrow;
    cl_object _ecl_big_floor(cl_lispunion*, cl_lispunion*, cl_lispunion**) @nogc nothrow;
    cl_object _ecl_big_negate(cl_lispunion*) @nogc nothrow;
    void _ecl_big_register_free(cl_object) @nogc nothrow;
    cl_object bignum1(c_long) @nogc nothrow;
    cl_object si_compiled_function_name(cl_lispunion*) @nogc nothrow;
    cl_object si_compiled_function_block(cl_lispunion*) @nogc nothrow;
    cl_object cl_function_lambda_expression(cl_lispunion*) @nogc nothrow;
    cl_object si_compiled_function_file(cl_lispunion*) @nogc nothrow;
    cl_object ecl_make_cfun(cl_lispunion* function(), cl_lispunion*, cl_lispunion*, int) @nogc nothrow;
    cl_object ecl_make_cfun_va(cl_lispunion* function(c_long, ...), cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_make_cclosure_va(cl_lispunion* function(c_long, ...), cl_lispunion*, cl_lispunion*) @nogc nothrow;
    void ecl_def_c_function(cl_object, cl_lispunion* function(), int) @nogc nothrow;
    void ecl_def_c_macro(cl_object, cl_lispunion* function(), int) @nogc nothrow;
    void ecl_def_c_macro_va(cl_object, cl_lispunion* function(c_long, ...)) @nogc nothrow;
    void ecl_set_function_source_file_info(cl_object, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    void ecl_cmp_defmacro(cl_object) @nogc nothrow;
    void ecl_cmp_defun(cl_object) @nogc nothrow;
    cl_object cl_digit_char_p(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_charE(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_charNE(c_long, ...) @nogc nothrow;
    cl_object cl_charL(c_long, ...) @nogc nothrow;
    cl_object cl_charG(c_long, ...) @nogc nothrow;
    cl_object cl_charLE(c_long, ...) @nogc nothrow;
    cl_object cl_charGE(c_long, ...) @nogc nothrow;
    cl_object cl_char_equal(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_char_not_equal(c_long, ...) @nogc nothrow;
    cl_object cl_char_lessp(c_long, ...) @nogc nothrow;
    cl_object cl_char_greaterp(c_long, ...) @nogc nothrow;
    cl_object cl_char_not_greaterp(c_long, ...) @nogc nothrow;
    cl_object cl_char_not_lessp(c_long, ...) @nogc nothrow;
    cl_object cl_digit_char(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_alpha_char_p(cl_lispunion*) @nogc nothrow;
    cl_object cl_alphanumericp(cl_lispunion*) @nogc nothrow;
    cl_object cl_both_case_p(cl_lispunion*) @nogc nothrow;
    cl_object cl_char_code(cl_lispunion*) @nogc nothrow;
    cl_object cl_char_downcase(cl_lispunion*) @nogc nothrow;
    cl_object cl_char_int(cl_lispunion*) @nogc nothrow;
    cl_object cl_char_name(cl_lispunion*) @nogc nothrow;
    cl_object cl_char_upcase(cl_lispunion*) @nogc nothrow;
    cl_object cl_character(cl_lispunion*) @nogc nothrow;
    cl_object cl_code_char(cl_lispunion*) @nogc nothrow;
    cl_object cl_graphic_char_p(cl_lispunion*) @nogc nothrow;
    cl_object cl_lower_case_p(cl_lispunion*) @nogc nothrow;
    cl_object cl_name_char(cl_lispunion*) @nogc nothrow;
    cl_object cl_standard_char_p(cl_lispunion*) @nogc nothrow;
    cl_object cl_upper_case_p(cl_lispunion*) @nogc nothrow;
    int ecl_string_case(cl_object) @nogc nothrow;
    int ecl_alpha_char_p(int) @nogc nothrow;
    int ecl_alphanumericp(int) @nogc nothrow;
    int ecl_both_case_p(int) @nogc nothrow;
    int ecl_char_downcase(int) @nogc nothrow;
    int ecl_char_upcase(int) @nogc nothrow;
    int ecl_graphic_char_p(int) @nogc nothrow;
    int ecl_lower_case_p(int) @nogc nothrow;
    int ecl_standard_char_p(int) @nogc nothrow;
    int ecl_base_char_p(int) @nogc nothrow;
    int ecl_upper_case_p(int) @nogc nothrow;
    int ecl_base_string_case(cl_object) @nogc nothrow;
    int ecl_char_code(cl_object) @nogc nothrow;
    ubyte ecl_base_char_code(cl_object) @nogc nothrow;
    int ecl_digitp(int, int) @nogc nothrow;
    int ecl_char_eq(cl_object, cl_lispunion*) @nogc nothrow;
    int ecl_char_cmp(cl_object, cl_lispunion*) @nogc nothrow;
    int ecl_char_equal(cl_object, cl_lispunion*) @nogc nothrow;
    int ecl_char_compare(cl_object, cl_lispunion*) @nogc nothrow;
    short ecl_digit_char(c_long, c_long) @nogc nothrow;
    cl_object cl_find_class(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_class_of(cl_lispunion*) @nogc nothrow;
    cl_object si_specialp(cl_lispunion*) @nogc nothrow;
    c_long ecl_ifloor(c_long, c_long) @nogc nothrow;
    c_long ecl_imod(c_long, c_long) @nogc nothrow;
    char ecl_to_char(cl_object) @nogc nothrow;
    c_ulong ecl_to_unsigned_integer(cl_object) @nogc nothrow;
    int ecl_aref_bv(cl_object, c_ulong) @nogc nothrow;
    int ecl_aset_bv(cl_object, c_ulong, int) @nogc nothrow;
    void cl_throw(cl_object) @nogc nothrow;
    void cl_return_from(cl_object, cl_lispunion*) @nogc nothrow;
    void cl_go(cl_object, cl_lispunion*) @nogc nothrow;
    struct _ecl_va_list {
	    va_list args;
	    cl_object *sp;
	    int narg;
    }
    void cl_parse_key(ecl_va_list*, int, cl_object*, cl_lispunion**, cl_lispunion**, int) @nogc nothrow;
    cl_object cl_grab_rest_args(ecl_va_list*) @nogc nothrow;
    cl_object si_macrolet_function(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_process_lambda_list(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_process_lambda(cl_lispunion*) @nogc nothrow;
    cl_object si_make_lambda(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_function_block_name(cl_lispunion*) @nogc nothrow;
    cl_object si_valid_function_name_p(cl_lispunion*) @nogc nothrow;
    cl_object si_process_declarations(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_eval_with_env(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_interpreter_stack(c_long) @nogc nothrow;
    cl_object ecl_stack_frame_open(cl_env_struct*, cl_lispunion*, c_ulong) @nogc nothrow;
    void ecl_stack_frame_push(cl_object, cl_lispunion*) @nogc nothrow;
    void ecl_stack_frame_push_values(cl_object) @nogc nothrow;
    cl_object ecl_stack_frame_pop_values(cl_lispunion*) @nogc nothrow;
    void ecl_stack_frame_close(cl_object) @nogc nothrow;
    c_ulong __gmpn_gcdext_1(c_long*, c_long*, c_ulong, c_ulong) @nogc nothrow;
    void FEstack_underflow() @nogc nothrow;
    void FEstack_advance() @nogc nothrow;
    cl_object* ecl_stack_grow(cl_env_struct*) @nogc nothrow;
    cl_object* ecl_stack_set_size(cl_env_struct*, c_ulong) @nogc nothrow;
    c_ulong ecl_stack_push_values(cl_env_struct*) @nogc nothrow;
    void ecl_stack_pop_values(cl_env_struct*, c_ulong) @nogc nothrow;
    cl_object ecl_interpret(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object _ecl_bytecodes_dispatch(c_long, ...) @nogc nothrow;
    cl_object _ecl_bclosure_dispatch(c_long, ...) @nogc nothrow;
    cl_object si_bc_disassemble(cl_lispunion*) @nogc nothrow;
    cl_object si_bc_split(cl_lispunion*) @nogc nothrow;
    cl_object si_bc_join(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_error(c_long, cl_lispunion*) @nogc nothrow;
    cl_object cl_cerror(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    void ecl_internal_error(const(char)*) @nogc nothrow;
    void ecl_unrecoverable_error(cl_env_struct*, const(char)*) @nogc nothrow;
    void ecl_cs_overflow() @nogc nothrow;
    void FEprogram_error(const(char)*, int) @nogc nothrow;
    void FEprogram_error_noreturn(const(char)*, int) @nogc nothrow;
    void FEcontrol_error(const(char)*, int) @nogc nothrow;
    void FEreader_error(const(char)*, cl_object, int) @nogc nothrow;
    void FEerror(const(char)*, int) @nogc nothrow;
    void FEcannot_open(cl_object) @nogc nothrow;
    void FEend_of_file(cl_object) @nogc nothrow;
    void FEclosed_stream(cl_object) @nogc nothrow;
    void FEwrong_type_argument(cl_object, cl_lispunion*) @nogc nothrow;
    void FEwrong_type_only_arg(cl_object, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    void FEwrong_type_nth_arg(cl_object, c_long, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    void FEwrong_type_key_arg(cl_object, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    void FEwrong_num_arguments(cl_object) @nogc nothrow;
    void FEwrong_num_arguments_anonym() @nogc nothrow;
    void FEwrong_index(cl_object, cl_lispunion*, int, cl_lispunion*, c_ulong) @nogc nothrow;
    void FEunbound_variable(cl_object) @nogc nothrow;
    void FEinvalid_macro_call(cl_object) @nogc nothrow;
    void FEinvalid_variable(const(char)*, cl_object) @nogc nothrow;
    void FEassignment_to_constant(cl_object) @nogc nothrow;
    void FEundefined_function(cl_object) @nogc nothrow;
    void FEinvalid_function(cl_object) @nogc nothrow;
    void FEinvalid_function_name(cl_object) @nogc nothrow;
    void FEprint_not_readable(cl_object) @nogc nothrow;
    cl_object CEerror(cl_lispunion*, const(char)*, int, ...) @nogc nothrow;
    void FElibc_error(const(char)*, int) @nogc nothrow;
    cl_object si_signal_type_error(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_funcall(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_apply(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object* _ecl_va_sp(c_long) @nogc nothrow;
    cl_object si_unlink_symbol(cl_lispunion*) @nogc nothrow;
    cl_object cl_constantp(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_apply_from_stack(c_ulong, cl_lispunion*) @nogc nothrow;
    cl_object ecl_apply_from_stack_frame(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object function(c_long, ...) ecl_function_dispatch(cl_env_struct*, cl_lispunion*) @nogc nothrow;
    cl_object _ecl_link_call(cl_lispunion*, cl_lispunion* function(c_long, ...)*, cl_lispunion*, int, ecl_va_list*) @nogc nothrow;
    cl_object si_get_cdata(cl_lispunion*) @nogc nothrow;
    cl_object si_add_cdata(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_make_codeblock() @nogc nothrow;
    cl_object ecl_library_open(cl_lispunion*, int) @nogc nothrow;
    void* ecl_library_symbol(cl_object, const(char)*, int) @nogc nothrow;
    cl_object ecl_library_error(cl_lispunion*) @nogc nothrow;
    int ecl_library_close(cl_object) @nogc nothrow;
    void ecl_library_close_all() @nogc nothrow;
    cl_object si_mmap(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_munmap(cl_lispunion*) @nogc nothrow;
    cl_object si_mmap_array(cl_lispunion*) @nogc nothrow;
    cl_object si_dump_c_backtrace(cl_lispunion*) @nogc nothrow;
    cl_object si_backtrace(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_allocate_foreign_data(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_foreign_elt_type_p(cl_lispunion*) @nogc nothrow;
    cl_object si_foreign_data_p(cl_lispunion*) @nogc nothrow;
    cl_object si_foreign_data_address(cl_lispunion*) @nogc nothrow;
    cl_object si_foreign_data_equal(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_foreign_data_pointer(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_foreign_data_ref(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_foreign_data_ref_elt(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_foreign_data_set(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_foreign_data_set_elt(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_foreign_data_tag(cl_lispunion*) @nogc nothrow;
    cl_object si_foreign_data_recast(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_free_foreign_data(cl_lispunion*) @nogc nothrow;
    cl_object si_make_foreign_data_from_array(cl_lispunion*) @nogc nothrow;
    cl_object si_null_pointer_p(cl_lispunion*) @nogc nothrow;
    cl_object si_size_of_foreign_elt_type(cl_lispunion*) @nogc nothrow;
    cl_object si_alignment_of_foreign_elt_type(cl_lispunion*) @nogc nothrow;
    cl_object si_load_foreign_module(cl_lispunion*) @nogc nothrow;
    cl_object si_unload_foreign_module(cl_lispunion*) @nogc nothrow;
    cl_object si_find_foreign_symbol(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_call_cfun(c_long, cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_make_dynamic_callback(c_long, cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_free_ffi_closure(cl_lispunion*) @nogc nothrow;
    c_ulong __gmpn_gcd_1(const(c_ulong)*, c_long, c_ulong) @nogc nothrow;
    cl_object ecl_make_foreign_data(cl_lispunion*, c_ulong, void*) @nogc nothrow;
    cl_object ecl_allocate_foreign_data(cl_lispunion*, c_ulong) @nogc nothrow;
    void* ecl_foreign_data_pointer_safe(cl_object) @nogc nothrow;
    char* ecl_base_string_pointer_safe(cl_object) @nogc nothrow;
    cl_object ecl_null_terminated_base_string(cl_lispunion*) @nogc nothrow;
    cl_object ecl_foreign_data_ref_elt(void*, ecl_ffi_tag) @nogc nothrow;
    void ecl_foreign_data_set_elt(void*, ecl_ffi_tag, cl_object) @nogc nothrow;
    c_long __gmpn_gcd(c_ulong*, c_ulong*, c_long, c_ulong*, c_long) @nogc nothrow;
    cl_object cl_make_synonym_stream(cl_lispunion*) @nogc nothrow;
    cl_object cl_synonym_stream_symbol(cl_lispunion*) @nogc nothrow;
    cl_object cl_make_two_way_stream(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_two_way_stream_input_stream(cl_lispunion*) @nogc nothrow;
    cl_object cl_two_way_stream_output_stream(cl_lispunion*) @nogc nothrow;
    cl_object cl_make_echo_stream(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_echo_stream_input_stream(cl_lispunion*) @nogc nothrow;
    cl_object cl_echo_stream_output_stream(cl_lispunion*) @nogc nothrow;
    cl_object cl_make_string_output_stream(c_long, ...) @nogc nothrow;
    cl_object cl_get_output_stream_string(cl_lispunion*) @nogc nothrow;
    cl_object cl_streamp(cl_lispunion*) @nogc nothrow;
    cl_object cl_input_stream_p(cl_lispunion*) @nogc nothrow;
    cl_object cl_output_stream_p(cl_lispunion*) @nogc nothrow;
    cl_object cl_stream_element_type(cl_lispunion*) @nogc nothrow;
    cl_object cl_stream_external_format(cl_lispunion*) @nogc nothrow;
    cl_object cl_file_length(cl_lispunion*) @nogc nothrow;
    cl_object si_get_string_input_stream_index(cl_lispunion*) @nogc nothrow;
    cl_object si_make_string_output_stream_from_string(cl_lispunion*) @nogc nothrow;
    cl_object si_copy_stream(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_open_stream_p(cl_lispunion*) @nogc nothrow;
    cl_object cl_make_broadcast_stream(c_long, ...) @nogc nothrow;
    cl_object cl_broadcast_stream_streams(cl_lispunion*) @nogc nothrow;
    cl_object cl_make_concatenated_stream(c_long, ...) @nogc nothrow;
    cl_object cl_concatenated_stream_streams(cl_lispunion*) @nogc nothrow;
    cl_object cl_make_string_input_stream(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_make_sequence_input_stream(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_make_sequence_output_stream(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_close(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_open(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_file_position(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_file_string_length(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_do_write_sequence(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_do_read_sequence(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_file_column(cl_lispunion*) @nogc nothrow;
    cl_object cl_interactive_stream_p(cl_lispunion*) @nogc nothrow;
    cl_object si_set_buffering_mode(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_stream_external_format_set(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    int ecl_input_stream_p(cl_object) @nogc nothrow;
    int ecl_output_stream_p(cl_object) @nogc nothrow;
    cl_object ecl_stream_element_type(cl_lispunion*) @nogc nothrow;
    int ecl_interactive_stream_p(cl_object) @nogc nothrow;
    cl_object ecl_open_stream(cl_lispunion*, ecl_smmode, cl_lispunion*, cl_lispunion*, c_long, int, cl_lispunion*) @nogc nothrow;
    cl_object ecl_make_string_input_stream(cl_lispunion*, c_ulong, c_ulong) @nogc nothrow;
    cl_object ecl_make_string_output_stream(c_ulong, int) @nogc nothrow;
    cl_object ecl_read_byte(cl_lispunion*) @nogc nothrow;
    void ecl_write_byte(cl_object, cl_lispunion*) @nogc nothrow;
    int ecl_read_char_noeof(cl_object) @nogc nothrow;
    int ecl_read_char(cl_object) @nogc nothrow;
    void ecl_unread_char(int, cl_object) @nogc nothrow;
    int ecl_peek_char(cl_object) @nogc nothrow;
    int ecl_write_char(int, cl_object) @nogc nothrow;
    void writestr_stream(const(char)*, cl_object) @nogc nothrow;
    void ecl_force_output(cl_object) @nogc nothrow;
    void ecl_finish_output(cl_object) @nogc nothrow;
    void ecl_clear_input(cl_object) @nogc nothrow;
    void ecl_clear_output(cl_object) @nogc nothrow;
    int ecl_listen_stream(cl_object) @nogc nothrow;
    cl_object ecl_file_position(cl_lispunion*) @nogc nothrow;
    cl_object ecl_file_position_set(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_file_length(cl_lispunion*) @nogc nothrow;
    int ecl_file_column(cl_object) @nogc nothrow;
    c_long ecl_normalize_stream_element_type(cl_object) @nogc nothrow;
    cl_object ecl_make_stream_from_fd(cl_lispunion*, int, ecl_smmode, c_long, int, cl_lispunion*) @nogc nothrow;
    cl_object ecl_make_stream_from_FILE(cl_lispunion*, void*, ecl_smmode, c_long, int, cl_lispunion*) @nogc nothrow;
    cl_object si_file_stream_fd(cl_lispunion*) @nogc nothrow;
    int ecl_stream_to_handle(cl_object, int) @nogc nothrow;
    void ecl_set_finalizer_unprotected(cl_object, cl_lispunion*) @nogc nothrow;
    cl_object si_get_finalizer(cl_lispunion*) @nogc nothrow;
    cl_object si_set_finalizer(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_format(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    c_ulong __gmpn_div_qr_2(c_ulong*, c_ulong*, const(c_ulong)*, c_long, const(c_ulong)*) @nogc nothrow;
    void ecl_register_root(cl_object*) @nogc nothrow;
    void _ecl_set_method_hash_size(cl_env_struct*, c_ulong) @nogc nothrow;
    cl_object si_clear_gfun_hash(cl_lispunion*) @nogc nothrow;
    cl_object clos_set_funcallable_instance_function(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_generic_function_p(cl_lispunion*) @nogc nothrow;
    cl_object _ecl_standard_dispatch(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl__make_hash_table(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_hash_table_p(cl_lispunion*) @nogc nothrow;
    cl_object si_hash_set(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_remhash(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_clrhash(cl_lispunion*) @nogc nothrow;
    cl_object cl_hash_table_count(cl_lispunion*) @nogc nothrow;
    cl_object cl_sxhash(cl_lispunion*) @nogc nothrow;
    cl_object cl_maphash(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_hash_table_rehash_size(cl_lispunion*) @nogc nothrow;
    cl_object cl_hash_table_rehash_threshold(cl_lispunion*) @nogc nothrow;
    cl_object cl_hash_table_size(cl_lispunion*) @nogc nothrow;
    cl_object cl_hash_table_test(cl_lispunion*) @nogc nothrow;
    cl_object si_hash_table_iterator(cl_lispunion*) @nogc nothrow;
    cl_object cl_make_hash_table(c_long, ...) @nogc nothrow;
    cl_object cl_gethash(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_copy_hash_table(cl_lispunion*) @nogc nothrow;
    cl_object si_hash_eql(c_long, ...) @nogc nothrow;
    cl_object si_hash_equal(c_long, ...) @nogc nothrow;
    cl_object si_hash_equalp(c_long, ...) @nogc nothrow;
    cl_object si_hash_table_content(cl_lispunion*) @nogc nothrow;
    cl_object si_hash_table_fill(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_hash_table_weakness(cl_lispunion*) @nogc nothrow;
    cl_object ecl_sethash(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_gethash(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_gethash_safe(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    int ecl_remhash(cl_object, cl_lispunion*) @nogc nothrow;
    cl_object _ecl_sethash(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    c_ulong ecl_hash_table_count(cl_object) @nogc nothrow;
    cl_object si_allocate_raw_instance(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_instance_class(cl_lispunion*) @nogc nothrow;
    cl_object si_instance_class_set(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_instance_ref(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object clos_safe_instance_ref(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_instance_set(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_instancep(cl_lispunion*) @nogc nothrow;
    cl_object si_unbound() @nogc nothrow;
    cl_object si_sl_boundp(cl_lispunion*) @nogc nothrow;
    cl_object si_sl_makunbound(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_instance_sig(cl_lispunion*) @nogc nothrow;
    cl_object si_instance_sig_set(cl_lispunion*) @nogc nothrow;
    cl_object ecl_allocate_instance(cl_lispunion*, c_ulong) @nogc nothrow;
    cl_object ecl_instance_ref(cl_lispunion*, c_long) @nogc nothrow;
    cl_object ecl_instance_set(cl_lispunion*, c_long, cl_lispunion*) @nogc nothrow;
    cl_object si_copy_instance(cl_lispunion*) @nogc nothrow;
    cl_object ecl_slot_value(cl_lispunion*, const(char)*) @nogc nothrow;
    cl_object ecl_slot_value_set(cl_lispunion*, const(char)*, cl_lispunion*) @nogc nothrow;
    c_ulong __gmpn_div_qr_1(c_ulong*, c_ulong*, const(c_ulong)*, c_long, c_ulong) @nogc nothrow;
    c_ulong __gmpn_divrem_2(c_ulong*, c_long, c_ulong*, c_long, const(c_ulong)*) @nogc nothrow;
    cl_object cl_fifth(cl_lispunion*) @nogc nothrow;
    cl_object cl_sixth(cl_lispunion*) @nogc nothrow;
    cl_object cl_seventh(cl_lispunion*) @nogc nothrow;
    cl_object cl_eighth(cl_lispunion*) @nogc nothrow;
    cl_object cl_ninth(cl_lispunion*) @nogc nothrow;
    cl_object cl_tenth(cl_lispunion*) @nogc nothrow;
    cl_object cl_endp(cl_lispunion*) @nogc nothrow;
    cl_object cl_list_length(cl_lispunion*) @nogc nothrow;
    cl_object cl_nth(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_nthcdr(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_copy_list(cl_lispunion*) @nogc nothrow;
    cl_object cl_copy_alist(cl_lispunion*) @nogc nothrow;
    cl_object cl_copy_tree(cl_lispunion*) @nogc nothrow;
    cl_object cl_revappend(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_ldiff(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_rplaca(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_rplacd(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_tailp(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_memq(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_nreconc(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_cons(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_acons(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_list(c_long, ...) @nogc nothrow;
    cl_object cl_listX(c_long, ...) @nogc nothrow;
    cl_object cl_append(c_long, ...) @nogc nothrow;
    cl_object cl_tree_equal(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_last(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_make_list(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_nconc(c_long, ...) @nogc nothrow;
    cl_object cl_butlast(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_nbutlast(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_subst(c_long, cl_lispunion*, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_nsubst(c_long, cl_lispunion*, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_sublis(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_nsublis(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_member(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_member1(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_adjoin(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_pairlis(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_rassoc(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_assoc(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_proper_list_p(cl_lispunion*) @nogc nothrow;
    cl_object ecl_last(cl_lispunion*, c_ulong) @nogc nothrow;
    cl_object ecl_butlast(cl_lispunion*, c_ulong) @nogc nothrow;
    cl_object ecl_nbutlast(cl_lispunion*, c_ulong) @nogc nothrow;
    cl_object ecl_append(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    int ecl_endp(cl_object) @nogc nothrow;
    cl_object ecl_nth(c_long, cl_lispunion*) @nogc nothrow;
    cl_object ecl_nthcdr(c_long, cl_lispunion*) @nogc nothrow;
    cl_object ecl_nconc(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    int ecl_member_eq(cl_object, cl_lispunion*) @nogc nothrow;
    cl_object ecl_memql(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_member(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_assq(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_assql(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_assoc(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_assqlp(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_remove_eq(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_delete_eq(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_load_bytecodes(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_load_source(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_load_binary(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_load(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_macroexpand(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_macroexpand_1(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_macro_function(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_argc() @nogc nothrow;
    cl_object si_argv(cl_lispunion*) @nogc nothrow;
    cl_object si_getenv(cl_lispunion*) @nogc nothrow;
    cl_object si_setenv(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_environ() @nogc nothrow;
    cl_object si_pointer(cl_lispunion*) @nogc nothrow;
    cl_object si_quit(c_long, ...) @nogc nothrow;
    cl_object si_exit(c_long) @nogc nothrow;

        extern __gshared const(char)* ecl_self;
    c_long ecl_get_option(int) @nogc nothrow;
    cl_object cl_mapcar(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_maplist(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_mapc(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_mapl(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_mapcan(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_mapcon(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_values_list(cl_lispunion*) @nogc nothrow;
    cl_object cl_values(c_long, ...) @nogc nothrow;
    cl_object cl_conjugate(cl_lispunion*) @nogc nothrow;
    cl_object cl_1P(cl_lispunion*) @nogc nothrow;
    cl_object cl_1M(cl_lispunion*) @nogc nothrow;
    cl_object cl_X(c_long, ...) @nogc nothrow;
    cl_object cl_P(c_long, ...) @nogc nothrow;
    cl_object cl_M(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_N(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_gcd(c_long, ...) @nogc nothrow;
    cl_object cl_lcm(c_long, ...) @nogc nothrow;
    cl_object ecl_times(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object number_to_complex(cl_lispunion*) @nogc nothrow;
    cl_object ecl_plus(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_minus(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_negate(cl_lispunion*) @nogc nothrow;
    cl_object ecl_divide(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_integer_divide(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_gcd(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_one_plus(cl_lispunion*) @nogc nothrow;
    cl_object ecl_one_minus(cl_lispunion*) @nogc nothrow;
    cl_object ecl_conjugate(cl_lispunion*) @nogc nothrow;
    c_ulong fixnnint(cl_object) @nogc nothrow;
    cl_object ecl_make_integer(c_long) @nogc nothrow;
    cl_object ecl_make_unsigned_integer(c_ulong) @nogc nothrow;
    int ecl_to_bit(cl_object) @nogc nothrow;
    ubyte ecl_to_uint8_t(cl_object) @nogc nothrow;
    byte ecl_to_int8_t(cl_object) @nogc nothrow;
    c_ulong __gmpn_divrem_1(c_ulong*, c_long, const(c_ulong)*, c_long, c_ulong) @nogc nothrow;
    ushort ecl_to_uint16_t(cl_object) @nogc nothrow;
    short ecl_to_int16_t(cl_object) @nogc nothrow;
    c_ulong __gmpn_divrem(c_ulong*, c_long, c_ulong*, c_long, const(c_ulong)*, c_long) @nogc nothrow;
    ushort ecl_to_ushort(cl_object) @nogc nothrow;
    short ecl_to_short(cl_object) @nogc nothrow;
    c_ulong __gmpn_divexact_by3c(c_ulong*, const(c_ulong)*, c_long, c_ulong) @nogc nothrow;
    uint ecl_to_uint32_t(cl_object) @nogc nothrow;
    int ecl_to_int32_t(cl_object) @nogc nothrow;
    void __gmpn_divexact_1(c_ulong*, const(c_ulong)*, c_long, c_ulong) @nogc nothrow;
    int __gmpn_zero_p(const(c_ulong)*, c_long) @nogc nothrow;
    ulong ecl_to_ulong_long(cl_object) @nogc nothrow;
    long ecl_to_long_long(cl_object) @nogc nothrow;
    cl_object ecl_make_ulong_long(ulong) @nogc nothrow;
    cl_object ecl_make_long_long(long) @nogc nothrow;
    cl_object ecl_make_ratio(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_make_single_float(float) @nogc nothrow;
    cl_object ecl_make_complex(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_rational(cl_lispunion*) @nogc nothrow;
    int __gmpn_cmp(const(c_ulong)*, const(c_ulong)*, c_long) @nogc nothrow;
    real ecl_to_long_double(cl_object) @nogc nothrow;
    cl_object ecl_make_long_float(real) @nogc nothrow;
    cl_object cl_numerator(cl_lispunion*) @nogc nothrow;
    cl_object cl_denominator(cl_lispunion*) @nogc nothrow;
    cl_object cl_mod(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_rem(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_decode_float(cl_lispunion*) @nogc nothrow;
    cl_object cl_scale_float(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_float_radix(cl_lispunion*) @nogc nothrow;
    cl_object cl_float_digits(cl_lispunion*) @nogc nothrow;
    cl_object cl_float_precision(cl_lispunion*) @nogc nothrow;
    cl_object cl_integer_decode_float(cl_lispunion*) @nogc nothrow;
    cl_object cl_realpart(cl_lispunion*) @nogc nothrow;
    cl_object cl_imagpart(cl_lispunion*) @nogc nothrow;
    cl_object cl_float(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_floor(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_ceiling(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_truncate(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_round(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_float_sign(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_complex(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object ecl_floor1(cl_lispunion*) @nogc nothrow;
    cl_object ecl_ceiling1(cl_lispunion*) @nogc nothrow;
    cl_object ecl_truncate1(cl_lispunion*) @nogc nothrow;
    cl_object ecl_round1(cl_lispunion*) @nogc nothrow;
    int ecl_signbit(cl_object) @nogc nothrow;
    cl_object ecl_floor2(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_ceiling2(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_truncate2(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_round2(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_E(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_NE(c_long, ...) @nogc nothrow;
    cl_object cl_L(c_long, ...) @nogc nothrow;
    cl_object cl_G(c_long, ...) @nogc nothrow;
    cl_object cl_GE(c_long, ...) @nogc nothrow;
    cl_object cl_LE(c_long, ...) @nogc nothrow;
    cl_object cl_max(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_min(c_long, cl_lispunion*, ...) @nogc nothrow;
    int ecl_number_equalp(cl_object, cl_lispunion*) @nogc nothrow;
    int ecl_number_compare(cl_object, cl_lispunion*) @nogc nothrow;
    c_ulong __gmpn_addmul_1(c_ulong*, const(c_ulong)*, c_long, c_ulong) @nogc nothrow;
    c_ulong __gmpn_add_n(c_ulong*, const(c_ulong)*, const(c_ulong)*, c_long) @nogc nothrow;
    c_ulong __gmpn_add_1(c_ulong*, const(c_ulong)*, c_long, c_ulong) @nogc nothrow;
    c_ulong __gmpn_add(c_ulong*, const(c_ulong)*, c_long, const(c_ulong)*, c_long) @nogc nothrow;
    void __gmpf_urandomb(__mpf_struct*, __gmp_randstate_struct*, c_ulong) @nogc nothrow;
    cl_object cl_lognand(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_lognor(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_logandc1(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_logandc2(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_logorc1(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_logorc2(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_lognot(cl_lispunion*) @nogc nothrow;
    cl_object cl_boole(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_logbitp(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_ash(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_logcount(cl_lispunion*) @nogc nothrow;
    cl_object cl_integer_length(cl_lispunion*) @nogc nothrow;
    cl_object si_bit_array_op(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_logior(c_long, ...) @nogc nothrow;
    cl_object cl_logxor(c_long, ...) @nogc nothrow;
    cl_object cl_logand(c_long, ...) @nogc nothrow;
    cl_object cl_logeqv(c_long, ...) @nogc nothrow;
    c_long ecl_logand_index(cl_object, c_ulong) @nogc nothrow;
    cl_object ecl_boole(int, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_ash(cl_lispunion*, c_long) @nogc nothrow;
    int ecl_fixnum_bit_length(c_long) @nogc nothrow;
    c_ulong ecl_integer_length(cl_object) @nogc nothrow;
    cl_object cl_zerop(cl_lispunion*) @nogc nothrow;
    cl_object cl_plusp(cl_lispunion*) @nogc nothrow;
    cl_object cl_minusp(cl_lispunion*) @nogc nothrow;
    cl_object cl_oddp(cl_lispunion*) @nogc nothrow;
    cl_object cl_evenp(cl_lispunion*) @nogc nothrow;
    cl_object si_float_nan_p(cl_lispunion*) @nogc nothrow;
    cl_object si_float_infinity_p(cl_lispunion*) @nogc nothrow;
    int ecl_zerop(cl_object) @nogc nothrow;
    int ecl_plusp(cl_object) @nogc nothrow;
    int ecl_minusp(cl_object) @nogc nothrow;
    int ecl_oddp(cl_object) @nogc nothrow;
    int ecl_evenp(cl_object) @nogc nothrow;
    int ecl_float_nan_p(cl_object) @nogc nothrow;
    int ecl_float_infinity_p(cl_object) @nogc nothrow;
    cl_object cl_random_state_p(cl_lispunion*) @nogc nothrow;
    cl_object cl_random(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_make_random_state(c_long, ...) @nogc nothrow;
    cl_object ecl_make_random_state(cl_lispunion*) @nogc nothrow;
    c_long ecl_fixnum_expt(c_long, c_long) @nogc nothrow;
    cl_object cl_abs(cl_lispunion*) @nogc nothrow;
    cl_object cl_exp(cl_lispunion*) @nogc nothrow;
    cl_object cl_expt(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_sqrt(cl_lispunion*) @nogc nothrow;
    cl_object cl_sin(cl_lispunion*) @nogc nothrow;
    cl_object cl_cos(cl_lispunion*) @nogc nothrow;
    cl_object cl_tan(cl_lispunion*) @nogc nothrow;
    cl_object cl_sinh(cl_lispunion*) @nogc nothrow;
    cl_object cl_cosh(cl_lispunion*) @nogc nothrow;
    cl_object cl_tanh(cl_lispunion*) @nogc nothrow;
    cl_object cl_atan(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_log(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_log1p(cl_lispunion*) @nogc nothrow;
    cl_object ecl_log1p(cl_lispunion*) @nogc nothrow;
    cl_object ecl_log1(cl_lispunion*) @nogc nothrow;
    cl_object ecl_log2(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_atan2(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_atan1(cl_lispunion*) @nogc nothrow;
    cl_object ecl_abs(cl_lispunion*) @nogc nothrow;
    cl_object ecl_exp(cl_lispunion*) @nogc nothrow;
    cl_object ecl_expt(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_sqrt(cl_lispunion*) @nogc nothrow;
    cl_object ecl_sin(cl_lispunion*) @nogc nothrow;
    cl_object ecl_cos(cl_lispunion*) @nogc nothrow;
    cl_object ecl_tan(cl_lispunion*) @nogc nothrow;
    cl_object ecl_sinh(cl_lispunion*) @nogc nothrow;
    cl_object ecl_cosh(cl_lispunion*) @nogc nothrow;
    cl_object ecl_tanh(cl_lispunion*) @nogc nothrow;
    void CEpackage_error(const(char)*, const(char)*, cl_object, int, ...) @nogc nothrow;
    cl_object si_select_package(cl_lispunion*) @nogc nothrow;
    cl_object cl_find_package(cl_lispunion*) @nogc nothrow;
    cl_object cl_package_name(cl_lispunion*) @nogc nothrow;
    cl_object cl_package_nicknames(cl_lispunion*) @nogc nothrow;
    cl_object cl_package_use_list(cl_lispunion*) @nogc nothrow;
    cl_object cl_package_used_by_list(cl_lispunion*) @nogc nothrow;
    cl_object cl_package_shadowing_symbols(cl_lispunion*) @nogc nothrow;
    cl_object cl_list_all_packages() @nogc nothrow;
    cl_object si_package_hash_tables(cl_lispunion*) @nogc nothrow;
    cl_object si_package_lock(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_delete_package(cl_lispunion*) @nogc nothrow;
    cl_object cl_make_package(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_intern(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_find_symbol(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_unintern(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_export(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_unexport(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_import(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_rename_package(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_shadowing_import(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_shadow(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_use_package(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_unuse_package(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object ecl_make_package(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_rename_package(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_find_package_nolock(cl_lispunion*) @nogc nothrow;
    cl_object ecl_find_package(const(char)*) @nogc nothrow;
    cl_object si_coerce_to_package(cl_lispunion*) @nogc nothrow;
    cl_object ecl_current_package() @nogc nothrow;
    cl_object ecl_find_symbol(cl_lispunion*, cl_lispunion*, int*) @nogc nothrow;
    cl_object ecl_intern(cl_lispunion*, cl_lispunion*, int*) @nogc nothrow;
    cl_object _ecl_intern(const(char)*, cl_lispunion*) @nogc nothrow;
    int ecl_unintern(cl_object, cl_lispunion*) @nogc nothrow;
    void cl_export2(cl_object, cl_lispunion*) @nogc nothrow;
    void cl_unexport2(cl_object, cl_lispunion*) @nogc nothrow;
    void cl_import2(cl_object, cl_lispunion*) @nogc nothrow;
    void ecl_shadowing_import(cl_object, cl_lispunion*) @nogc nothrow;
    void ecl_shadow(cl_object, cl_lispunion*) @nogc nothrow;
    void ecl_use_package(cl_object, cl_lispunion*) @nogc nothrow;
    void ecl_unuse_package(cl_object, cl_lispunion*) @nogc nothrow;
    int ecl_string_match(cl_object, c_ulong, c_ulong, cl_lispunion*, c_ulong, c_ulong) @nogc nothrow;
    cl_object cl_pathname(cl_lispunion*) @nogc nothrow;
    cl_object cl_logical_pathname(cl_lispunion*) @nogc nothrow;
    cl_object cl_pathnamep(cl_lispunion*) @nogc nothrow;
    cl_object cl_pathname_host(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_pathname_device(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_pathname_directory(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_pathname_name(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_pathname_type(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_pathname_version(cl_lispunion*) @nogc nothrow;
    cl_object cl_namestring(cl_lispunion*) @nogc nothrow;
    cl_object cl_file_namestring(cl_lispunion*) @nogc nothrow;
    cl_object cl_directory_namestring(cl_lispunion*) @nogc nothrow;
    cl_object cl_host_namestring(cl_lispunion*) @nogc nothrow;
    cl_object si_logical_pathname_p(cl_lispunion*) @nogc nothrow;
    cl_object cl_pathname_match_p(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_translate_pathname(c_long, cl_lispunion*, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_translate_logical_pathname(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_parse_namestring(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_parse_logical_namestring(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_merge_pathnames(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_make_pathname(c_long, ...) @nogc nothrow;
    cl_object cl_enough_namestring(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_pathname_translations(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_default_pathname_defaults() @nogc nothrow;
    cl_object cl_wild_pathname_p(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object ecl_make_pathname(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_parse_namestring(cl_lispunion*, c_ulong, c_ulong, c_ulong*, cl_lispunion*) @nogc nothrow;
    cl_object coerce_to_physical_pathname(cl_lispunion*) @nogc nothrow;
    cl_object coerce_to_file_pathname(cl_lispunion*) @nogc nothrow;
    void __gmpf_ui_sub(__mpf_struct*, c_ulong, const(__mpf_struct)*) @nogc nothrow;
    cl_object ecl_namestring(cl_lispunion*, int) @nogc nothrow;
    cl_object si_coerce_to_filename(cl_lispunion*) @nogc nothrow;
    cl_object ecl_merge_pathnames(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    int ecl_logical_hostname_p(cl_object) @nogc nothrow;
    cl_object cl_identity(cl_lispunion*) @nogc nothrow;
    cl_object cl_null(cl_lispunion*) @nogc nothrow;
    cl_object cl_symbolp(cl_lispunion*) @nogc nothrow;
    cl_object cl_atom(cl_lispunion*) @nogc nothrow;
    cl_object cl_consp(cl_lispunion*) @nogc nothrow;
    cl_object cl_listp(cl_lispunion*) @nogc nothrow;
    cl_object cl_numberp(cl_lispunion*) @nogc nothrow;
    cl_object cl_integerp(cl_lispunion*) @nogc nothrow;
    cl_object cl_rationalp(cl_lispunion*) @nogc nothrow;
    cl_object cl_floatp(cl_lispunion*) @nogc nothrow;
    cl_object cl_realp(cl_lispunion*) @nogc nothrow;
    cl_object cl_complexp(cl_lispunion*) @nogc nothrow;
    cl_object cl_characterp(cl_lispunion*) @nogc nothrow;
    cl_object cl_stringp(cl_lispunion*) @nogc nothrow;
    cl_object cl_bit_vector_p(cl_lispunion*) @nogc nothrow;
    cl_object cl_vectorp(cl_lispunion*) @nogc nothrow;
    cl_object cl_simple_string_p(cl_lispunion*) @nogc nothrow;
    cl_object cl_simple_bit_vector_p(cl_lispunion*) @nogc nothrow;
    cl_object cl_simple_vector_p(cl_lispunion*) @nogc nothrow;
    cl_object cl_arrayp(cl_lispunion*) @nogc nothrow;
    cl_object cl_packagep(cl_lispunion*) @nogc nothrow;
    cl_object cl_functionp(cl_lispunion*) @nogc nothrow;
    cl_object cl_compiled_function_p(cl_lispunion*) @nogc nothrow;
    cl_object cl_eq(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_eql(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_equal(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_equalp(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_fixnump(cl_lispunion*) @nogc nothrow;
    int floatp(cl_object) @nogc nothrow;
    int ecl_numberp(cl_object) @nogc nothrow;
    int ecl_realp(cl_object) @nogc nothrow;
    int ecl_eql(cl_object, cl_lispunion*) @nogc nothrow;
    int ecl_equal(cl_object, cl_lispunion*) @nogc nothrow;
    int ecl_equalp(cl_object, cl_lispunion*) @nogc nothrow;
    int ecl_stringp(cl_object) @nogc nothrow;
    cl_object cl_write_byte(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_write_sequence(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_write(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_prin1(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_print(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_pprint(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_princ(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_write_char(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_write_string(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_write_line(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_terpri(c_long, ...) @nogc nothrow;
    cl_object cl_finish_output(c_long, ...) @nogc nothrow;
    cl_object cl_fresh_line(c_long, ...) @nogc nothrow;
    cl_object cl_force_output(c_long, ...) @nogc nothrow;
    cl_object cl_clear_output(c_long, ...) @nogc nothrow;
    cl_object si_write_object(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_write_ugly_object(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_princ(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_prin1(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_print(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_terpri(cl_lispunion*) @nogc nothrow;
    void ecl_write_string(cl_object, cl_lispunion*) @nogc nothrow;
    void ecl_princ_str(const(char)*, cl_object) @nogc nothrow;
    int ecl_princ_char(int, cl_object) @nogc nothrow;
    c_long ecl_print_level() @nogc nothrow;
    c_long ecl_print_length() @nogc nothrow;
    int ecl_print_base() @nogc nothrow;
    int ecl_print_radix() @nogc nothrow;
    cl_object ecl_print_case() @nogc nothrow;
    int ecl_print_gensym() @nogc nothrow;
    int ecl_print_array() @nogc nothrow;
    int ecl_print_readably() @nogc nothrow;
    int ecl_print_escape() @nogc nothrow;
    int ecl_print_circle() @nogc nothrow;
    cl_object si_integer_to_string(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_float_string(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_float_to_digits(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_float_to_string_free(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_print_unreadable_object_function(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_get_buffer_string() @nogc nothrow;
    cl_object si_put_buffer_string(cl_lispunion*) @nogc nothrow;
    cl_object cl_read_sequence(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_readtablep(cl_lispunion*) @nogc nothrow;
    cl_object si_standard_readtable() @nogc nothrow;
    cl_object cl_read(c_long, ...) @nogc nothrow;
    cl_object cl_read_preserving_whitespace(c_long, ...) @nogc nothrow;
    cl_object cl_read_delimited_list(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_read_line(c_long, ...) @nogc nothrow;
    cl_object cl_read_char(c_long, ...) @nogc nothrow;
    cl_object cl_unread_char(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_peek_char(c_long, ...) @nogc nothrow;
    cl_object cl_listen(c_long, ...) @nogc nothrow;
    cl_object cl_read_char_no_hang(c_long, ...) @nogc nothrow;
    cl_object cl_clear_input(c_long, ...) @nogc nothrow;
    cl_object cl_parse_integer(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_read_byte(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_copy_readtable(c_long, ...) @nogc nothrow;
    cl_object cl_readtable_case(cl_lispunion*) @nogc nothrow;
    cl_object si_readtable_case_set(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_set_syntax_from_char(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_set_macro_character(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_get_macro_character(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_make_dispatch_macro_character(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_set_dispatch_macro_character(c_long, cl_lispunion*, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_get_dispatch_macro_character(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_read_object_or_ignore(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_readtable_lock(c_long, cl_lispunion*, ...) @nogc nothrow;
    int ecl_readtable_get(cl_object, int, cl_lispunion**) @nogc nothrow;
    void ecl_readtable_set(cl_object, int, ecl_chattrib, cl_lispunion*) @nogc nothrow;
    cl_object ecl_read_object_non_recursive(cl_lispunion*) @nogc nothrow;
    cl_object ecl_read_object(cl_lispunion*) @nogc nothrow;
    cl_object ecl_parse_number(cl_lispunion*, c_ulong, c_ulong, c_ulong*, uint) @nogc nothrow;
    cl_object ecl_parse_integer(cl_lispunion*, c_ulong, c_ulong, c_ulong*, uint) @nogc nothrow;
    int ecl_invalid_character_p(int) @nogc nothrow;
    cl_object ecl_copy_readtable(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_current_readtable() @nogc nothrow;
    int ecl_current_read_base() @nogc nothrow;
    char ecl_current_read_default_float_format() @nogc nothrow;
    void __gmpf_ui_div(__mpf_struct*, c_ulong, const(__mpf_struct)*) @nogc nothrow;
    cl_object ecl_init_module(cl_lispunion*, void function(cl_lispunion*)) @nogc nothrow;
    cl_object cl_fboundp(cl_lispunion*) @nogc nothrow;
    cl_object cl_symbol_function(cl_lispunion*) @nogc nothrow;
    cl_object cl_fdefinition(cl_lispunion*) @nogc nothrow;
    cl_object si_coerce_to_function(cl_lispunion*) @nogc nothrow;
    cl_object cl_symbol_value(cl_lispunion*) @nogc nothrow;
    cl_object cl_boundp(cl_lispunion*) @nogc nothrow;
    cl_object cl_special_operator_p(cl_lispunion*) @nogc nothrow;
    cl_object ecl_fdefinition(cl_lispunion*) @nogc nothrow;
    int ecl_boundp(cl_env_struct*, cl_object) @nogc nothrow;
    cl_object si_sequence_start_end(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_elt(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_elt_set(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_copy_seq(cl_lispunion*) @nogc nothrow;
    cl_object cl_length(cl_lispunion*) @nogc nothrow;
    cl_object cl_reverse(cl_lispunion*) @nogc nothrow;
    cl_object cl_nreverse(cl_lispunion*) @nogc nothrow;
    cl_object cl_subseq(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object ecl_elt(cl_lispunion*, c_long) @nogc nothrow;
    cl_object ecl_elt_set(cl_lispunion*, c_long, cl_lispunion*) @nogc nothrow;
    c_long ecl_length(cl_object) @nogc nothrow;
    cl_object ecl_subseq(cl_lispunion*, c_ulong, c_ulong) @nogc nothrow;
    cl_object ecl_copy_seq(cl_lispunion*) @nogc nothrow;
    cl_object si_ihs_top() @nogc nothrow;
    cl_object si_ihs_fun(cl_lispunion*) @nogc nothrow;
    cl_object si_ihs_env(cl_lispunion*) @nogc nothrow;
    cl_object si_ihs_bds(cl_lispunion*) @nogc nothrow;
    cl_object si_ihs_next(cl_lispunion*) @nogc nothrow;
    cl_object si_ihs_prev(cl_lispunion*) @nogc nothrow;
    cl_object si_frs_top() @nogc nothrow;
    cl_object si_frs_bds(cl_lispunion*) @nogc nothrow;
    cl_object si_frs_tag(cl_lispunion*) @nogc nothrow;
    cl_object si_frs_ihs(cl_lispunion*) @nogc nothrow;
    cl_object si_bds_top() @nogc nothrow;
    cl_object si_bds_var(cl_lispunion*) @nogc nothrow;
    cl_object si_bds_val(cl_lispunion*) @nogc nothrow;
    cl_object si_sch_frs_base(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_reset_stack_limits() @nogc nothrow;
    cl_object si_reset_margin(cl_lispunion*) @nogc nothrow;
    cl_object si_set_limit(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_get_limit(cl_lispunion*) @nogc nothrow;
    c_ulong ecl_progv(cl_env_struct*, cl_object, cl_lispunion*) @nogc nothrow;
    void ecl_bds_unwind(cl_env_struct*, c_ulong) @nogc nothrow;
    void ecl_unwind(cl_env_struct*, ecl_frame*) @nogc nothrow;
    ecl_frame* frs_sch(cl_object) @nogc nothrow;
    cl_object cl_char(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_char_set(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_string_trim(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_string_left_trim(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_string_right_trim(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_string(cl_lispunion*) @nogc nothrow;
    cl_object cl_make_string(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_stringE(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_string_equal(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_stringL(c_long, ...) @nogc nothrow;
    cl_object cl_stringG(c_long, ...) @nogc nothrow;
    cl_object cl_stringLE(c_long, ...) @nogc nothrow;
    cl_object cl_stringGE(c_long, ...) @nogc nothrow;
    cl_object cl_stringNE(c_long, ...) @nogc nothrow;
    cl_object cl_string_lessp(c_long, ...) @nogc nothrow;
    cl_object cl_string_greaterp(c_long, ...) @nogc nothrow;
    cl_object cl_string_not_greaterp(c_long, ...) @nogc nothrow;
    cl_object cl_string_not_lessp(c_long, ...) @nogc nothrow;
    cl_object cl_string_not_equal(c_long, ...) @nogc nothrow;
    cl_object cl_string_upcase(c_long, ...) @nogc nothrow;
    cl_object cl_string_downcase(c_long, ...) @nogc nothrow;
    cl_object cl_string_capitalize(c_long, ...) @nogc nothrow;
    cl_object cl_nstring_upcase(c_long, ...) @nogc nothrow;
    cl_object cl_nstring_downcase(c_long, ...) @nogc nothrow;
    cl_object cl_nstring_capitalize(c_long, ...) @nogc nothrow;
    cl_object si_base_string_concatenate(c_long, ...) @nogc nothrow;
    cl_object ecl_alloc_adjustable_base_string(c_ulong) @nogc nothrow;
    void __gmpf_trunc(__mpf_struct*, const(__mpf_struct)*) @nogc nothrow;
    cl_object make_base_string_copy(const(char)*) @nogc nothrow;
    cl_object ecl_cstring_to_base_string_or_nil(const(char)*) @nogc nothrow;
    int ecl_string_eq(cl_object, cl_lispunion*) @nogc nothrow;
    int ecl_member_char(int, cl_object) @nogc nothrow;
    int ecl_fits_in_base_string(cl_object) @nogc nothrow;
    int ecl_char(cl_object, c_ulong) @nogc nothrow;
    int ecl_char_set(cl_object, c_ulong, int) @nogc nothrow;
    cl_object si_structure_subtype_p(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_copy_structure(cl_lispunion*) @nogc nothrow;
    cl_object si_structure_name(cl_lispunion*) @nogc nothrow;
    cl_object si_structure_ref(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_structure_set(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_structurep(cl_lispunion*) @nogc nothrow;
    cl_object si_make_structure(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object ecl_structure_ref(cl_lispunion*, cl_lispunion*, int) @nogc nothrow;
    cl_object ecl_structure_set(cl_lispunion*, cl_lispunion*, int, cl_lispunion*) @nogc nothrow;
    cl_object cl_make_symbol(cl_lispunion*) @nogc nothrow;
    cl_object cl_remprop(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_symbol_plist(cl_lispunion*) @nogc nothrow;
    cl_object cl_get_properties(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_symbol_name(cl_lispunion*) @nogc nothrow;
    cl_object cl_symbol_package(cl_lispunion*) @nogc nothrow;
    cl_object cl_keywordp(cl_lispunion*) @nogc nothrow;
    cl_object si_put_f(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_rem_f(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_set_symbol_plist(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_putprop(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_Xmake_special(cl_lispunion*) @nogc nothrow;
    cl_object si_Xmake_constant(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_get(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_getf(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_copy_symbol(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_gensym(c_long, ...) @nogc nothrow;
    cl_object cl_gentemp(c_long, ...) @nogc nothrow;
    cl_object si_put_properties(c_long, cl_lispunion*, ...) @nogc nothrow;
    void ecl_defvar(cl_object, cl_lispunion*) @nogc nothrow;
    void ecl_defparameter(cl_object, cl_lispunion*) @nogc nothrow;
    cl_object ecl_make_symbol(const(char)*, const(char)*) @nogc nothrow;
    cl_object ecl_make_keyword(const(char)*) @nogc nothrow;
    cl_object ecl_symbol_value(cl_lispunion*) @nogc nothrow;
    cl_object ecl_setq(cl_env_struct*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_symbol_name(cl_lispunion*) @nogc nothrow;
    cl_object ecl_symbol_package(cl_lispunion*) @nogc nothrow;
    int ecl_symbol_type(cl_object) @nogc nothrow;
    void ecl_symbol_type_set(cl_object, int) @nogc nothrow;
    cl_object ecl_getf(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_get(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    int ecl_keywordp(cl_object) @nogc nothrow;
    cl_object si_open_client_stream(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_open_server_stream(cl_lispunion*) @nogc nothrow;
    cl_object si_open_unix_socket_stream(cl_lispunion*) @nogc nothrow;
    cl_object si_lookup_host_entry(cl_lispunion*) @nogc nothrow;
    void ecl_tcp_close_all() @nogc nothrow;
    cl_object mp_own_process() @nogc nothrow;
    cl_object mp_all_processes() @nogc nothrow;
    cl_object mp_exit_process() @nogc nothrow;
    cl_object mp_interrupt_process(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object mp_make_process(c_long, ...) @nogc nothrow;
    cl_object mp_process_active_p(cl_lispunion*) @nogc nothrow;
    cl_object mp_process_enable(cl_lispunion*) @nogc nothrow;
    cl_object mp_process_yield() @nogc nothrow;
    cl_object mp_process_join(cl_lispunion*) @nogc nothrow;
    cl_object mp_process_interrupt(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object mp_process_kill(cl_lispunion*) @nogc nothrow;
    cl_object mp_process_suspend(cl_lispunion*) @nogc nothrow;
    cl_object mp_process_resume(cl_lispunion*) @nogc nothrow;
    cl_object mp_process_name(cl_lispunion*) @nogc nothrow;
    cl_object mp_process_preset(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object mp_process_run_function(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object mp_process_run_function_wait(c_long, ...) @nogc nothrow;
    cl_object mp_process_whostate(cl_lispunion*) @nogc nothrow;
    cl_object mp_make_condition_variable() @nogc nothrow;
    cl_object mp_condition_variable_wait(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object mp_condition_variable_timedwait(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object mp_condition_variable_signal(cl_lispunion*) @nogc nothrow;
    cl_object mp_condition_variable_broadcast(cl_lispunion*) @nogc nothrow;
    cl_object mp_current_process() @nogc nothrow;
    cl_object mp_block_signals() @nogc nothrow;
    cl_object mp_restore_signals(cl_lispunion*) @nogc nothrow;
    int ecl_import_current_thread(cl_object, cl_lispunion*) @nogc nothrow;
    void ecl_release_current_thread() @nogc nothrow;
    cl_object mp_make_semaphore(c_long, ...) @nogc nothrow;
    cl_object mp_semaphore_count(cl_lispunion*) @nogc nothrow;
    cl_object mp_semaphore_name(cl_lispunion*) @nogc nothrow;
    cl_object mp_semaphore_wait_count(cl_lispunion*) @nogc nothrow;
    cl_object mp_wait_on_semaphore(cl_lispunion*) @nogc nothrow;
    cl_object mp_try_get_semaphore(cl_lispunion*) @nogc nothrow;
    cl_object mp_signal_semaphore(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object ecl_make_semaphore(cl_lispunion*, c_long) @nogc nothrow;
    cl_object ecl_make_barrier(cl_lispunion*, c_ulong) @nogc nothrow;
    cl_object mp_make_barrier(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object mp_barrier_count(cl_lispunion*) @nogc nothrow;
    cl_object mp_barrier_name(cl_lispunion*) @nogc nothrow;
    cl_object mp_barrier_arrivers_count(cl_lispunion*) @nogc nothrow;
    cl_object mp_barrier_wait(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object mp_barrier_unblock(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object mp_make_mailbox(c_long, ...) @nogc nothrow;
    cl_object mp_mailbox_name(cl_lispunion*) @nogc nothrow;
    cl_object mp_mailbox_count(cl_lispunion*) @nogc nothrow;
    cl_object mp_mailbox_empty_p(cl_lispunion*) @nogc nothrow;
    cl_object mp_mailbox_read(cl_lispunion*) @nogc nothrow;
    cl_object mp_mailbox_try_read(cl_lispunion*) @nogc nothrow;
    cl_object mp_mailbox_send(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object mp_mailbox_try_send(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_atomic_get(cl_lispunion**) @nogc nothrow;
    void ecl_atomic_push(cl_object*, cl_lispunion*) @nogc nothrow;
    void ecl_atomic_nconc(cl_object, cl_lispunion**) @nogc nothrow;
    cl_object ecl_atomic_pop(cl_lispunion**) @nogc nothrow;
    c_ulong ecl_atomic_index_incf(c_ulong*) @nogc nothrow;
    cl_object mp_make_lock(c_long, ...) @nogc nothrow;
    cl_object mp_recursive_lock_p(cl_lispunion*) @nogc nothrow;
    cl_object mp_lock_name(cl_lispunion*) @nogc nothrow;
    cl_object mp_lock_owner(cl_lispunion*) @nogc nothrow;
    cl_object mp_lock_count(cl_lispunion*) @nogc nothrow;
    cl_object mp_get_lock(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object mp_get_lock_wait(cl_lispunion*) @nogc nothrow;
    cl_object mp_get_lock_nowait(cl_lispunion*) @nogc nothrow;
    cl_object mp_giveup_lock(cl_lispunion*) @nogc nothrow;
    cl_object ecl_make_lock(cl_lispunion*, int) @nogc nothrow;
    cl_object mp_make_rwlock(c_long, ...) @nogc nothrow;
    cl_object mp_rwlock_name(cl_lispunion*) @nogc nothrow;
    cl_object mp_get_rwlock_read(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object mp_get_rwlock_write(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object mp_giveup_rwlock_read(cl_lispunion*) @nogc nothrow;
    cl_object mp_giveup_rwlock_write(cl_lispunion*) @nogc nothrow;
    cl_object ecl_make_rwlock(cl_lispunion*) @nogc nothrow;
    cl_object cl_sleep(cl_lispunion*) @nogc nothrow;
    cl_object cl_get_internal_run_time() @nogc nothrow;
    cl_object cl_get_internal_real_time() @nogc nothrow;
    cl_object cl_get_universal_time() @nogc nothrow;
    void assert_type_integer(cl_object) @nogc nothrow;
    void assert_type_non_negative_integer(cl_object) @nogc nothrow;
    void assert_type_proper_list(cl_object) @nogc nothrow;
    cl_object cl_type_of(cl_lispunion*) @nogc nothrow;
    void FEtype_error_fixnum(cl_object) @nogc nothrow;
    void FEtype_error_size(cl_object) @nogc nothrow;
    void FEtype_error_cons(cl_object) @nogc nothrow;
    void FEtype_error_list(cl_object) @nogc nothrow;
    void FEtype_error_proper_list(cl_object) @nogc nothrow;
    void FEtype_error_sequence(cl_object) @nogc nothrow;
    void FEtype_error_vector(cl_object) @nogc nothrow;
    void FEcircular_list(cl_object) @nogc nothrow;
    void FEtype_error_index(cl_object, c_long) @nogc nothrow;
    void FEtype_error_array(cl_object) @nogc nothrow;
    void FEdivision_by_zero(cl_object, cl_lispunion*) @nogc nothrow;
    cl_object ecl_type_error(cl_lispunion*, const(char)*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object ecl_check_cl_type(cl_lispunion*, cl_lispunion*, cl_type) @nogc nothrow;
    cl_object ecl_make_integer_type(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_truename(cl_lispunion*) @nogc nothrow;
    cl_object cl_rename_file(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_delete_file(cl_lispunion*) @nogc nothrow;
    cl_object cl_probe_file(cl_lispunion*) @nogc nothrow;
    cl_object cl_file_write_date(cl_lispunion*) @nogc nothrow;
    cl_object cl_file_author(cl_lispunion*) @nogc nothrow;
    cl_object si_file_kind(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_getcwd(c_long, ...) @nogc nothrow;
    cl_object si_getpid() @nogc nothrow;
    cl_object si_getuid() @nogc nothrow;
    cl_object si_chdir(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_chmod(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_mkdir(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_directory(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_user_homedir_pathname(c_long, ...) @nogc nothrow;
    cl_object si_mkstemp(cl_lispunion*) @nogc nothrow;
    cl_object si_rmdir(cl_lispunion*) @nogc nothrow;
    cl_object ecl_cstring_to_pathname(char*) @nogc nothrow;
    int ecl_backup_open(const(char)*, int, int) @nogc nothrow;
    cl_object ecl_file_len(int) @nogc nothrow;
    cl_object ecl_homedir_pathname(cl_lispunion*) @nogc nothrow;
    cl_object si_get_library_pathname() @nogc nothrow;
    cl_object si_copy_file(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    void __gmpf_swap(__mpf_struct*, __mpf_struct*) @nogc nothrow;
    void __gmpf_sub_ui(__mpf_struct*, const(__mpf_struct)*, c_ulong) @nogc nothrow;
    cl_object si_handle_signal(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_get_signal_handler(cl_lispunion*) @nogc nothrow;
    cl_object si_set_signal_handler(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_catch_signal(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_check_pending_interrupts() @nogc nothrow;
    cl_object si_disable_interrupts() @nogc nothrow;
    cl_object si_enable_interrupts() @nogc nothrow;
    cl_object si_trap_fpe(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    void ecl_check_pending_interrupts(cl_env_struct*) @nogc nothrow;
    cl_object si_system(cl_lispunion*) @nogc nothrow;
    cl_object si_make_pipe() @nogc nothrow;
    cl_object si_run_program(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_external_process_wait(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_close_windows_handle(cl_lispunion*) @nogc nothrow;
    cl_object si_base_char_p(cl_lispunion*) @nogc nothrow;
    cl_object si_base_string_p(cl_lispunion*) @nogc nothrow;
    cl_object si_coerce_to_base_string(cl_lispunion*) @nogc nothrow;
    cl_object si_coerce_to_extended_string(cl_lispunion*) @nogc nothrow;
    cl_object ecl_alloc_adjustable_extended_string(c_ulong) @nogc nothrow;
    cl_object _ecl_ucd_code_to_name(int) @nogc nothrow;
    cl_object _ecl_ucd_name_to_code(cl_lispunion*) @nogc nothrow;
    int ecl_string_push_extend(cl_object, int) @nogc nothrow;
    cl_object cl_vector_push(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_vector_push_extend(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_make_array(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_fill_array_with_seq(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_vector(c_long, ...) @nogc nothrow;
    cl_object cl_array_dimensions(cl_lispunion*) @nogc nothrow;
    cl_object cl_array_in_bounds_p(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_array_row_major_index(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_bit(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_sbit(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_bit_and(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_bit_ior(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_bit_xor(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_bit_eqv(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_bit_nand(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_bit_nor(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_bit_andc1(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_bit_andc2(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_bit_orc1(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_bit_orc2(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_bit_not(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_vector_pop(cl_lispunion*) @nogc nothrow;
    cl_object cl_adjust_array(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_short_site_name() @nogc nothrow;
    cl_object cl_long_site_name() @nogc nothrow;
    cl_object cl_lisp_implementation_type() @nogc nothrow;
    cl_object cl_lisp_implementation_version() @nogc nothrow;
    cl_object si_lisp_implementation_vcs_id() @nogc nothrow;
    cl_object cl_machine_type() @nogc nothrow;
    cl_object cl_machine_instance() @nogc nothrow;
    cl_object cl_machine_version() @nogc nothrow;
    cl_object cl_software_type() @nogc nothrow;
    cl_object cl_software_version() @nogc nothrow;
    cl_object cl_inspect(cl_lispunion*) @nogc nothrow;
    cl_object cl_describe(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_read_from_string(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_write_to_string(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_prin1_to_string(cl_lispunion*) @nogc nothrow;
    cl_object cl_princ_to_string(cl_lispunion*) @nogc nothrow;
    cl_object cl_y_or_n_p(c_long, ...) @nogc nothrow;
    cl_object cl_yes_or_no_p(c_long, ...) @nogc nothrow;
    cl_object cl_dribble(c_long, ...) @nogc nothrow;
    cl_object si_load_encoding(cl_lispunion*) @nogc nothrow;
    cl_object si_make_encoding(cl_lispunion*) @nogc nothrow;
    cl_object cl_union(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_nunion(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_intersection(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_nintersection(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_set_difference(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_nset_difference(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_set_exclusive_or(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_nset_exclusive_or(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_subsetp(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_rassoc_if(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_rassoc_if_not(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_assoc_if(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_assoc_if_not(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_member_if(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_member_if_not(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_subst_if(c_long, cl_lispunion*, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_subst_if_not(c_long, cl_lispunion*, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_nsubst_if(c_long, cl_lispunion*, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_nsubst_if_not(c_long, cl_lispunion*, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_logical_pathname_translations(cl_lispunion*) @nogc nothrow;
    cl_object cl_load_logical_pathname_translations(cl_lispunion*) @nogc nothrow;
    cl_object cl_decode_universal_time(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_encode_universal_time(c_long, cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_get_decoded_time() @nogc nothrow;
    cl_object cl_ensure_directories_exist(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_simple_program_error(c_long, cl_lispunion*) @nogc nothrow;
    cl_object si_signal_simple_error(c_long, cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_provide(cl_lispunion*) @nogc nothrow;
    cl_object cl_require(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_isqrt(cl_lispunion*) @nogc nothrow;
    cl_object cl_phase(cl_lispunion*) @nogc nothrow;
    cl_object cl_signum(cl_lispunion*) @nogc nothrow;
    cl_object cl_cis(cl_lispunion*) @nogc nothrow;
    cl_object cl_asin(cl_lispunion*) @nogc nothrow;
    cl_object cl_acos(cl_lispunion*) @nogc nothrow;
    cl_object cl_asinh(cl_lispunion*) @nogc nothrow;
    cl_object cl_acosh(cl_lispunion*) @nogc nothrow;
    cl_object cl_atanh(cl_lispunion*) @nogc nothrow;
    cl_object cl_ffloor(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_fceiling(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_ftruncate(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_fround(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_logtest(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_byte(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_byte_size(cl_lispunion*) @nogc nothrow;
    cl_object cl_byte_position(cl_lispunion*) @nogc nothrow;
    cl_object cl_ldb(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_ldb_test(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_mask_field(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_dpb(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_deposit_field(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_find_all_symbols(cl_lispunion*) @nogc nothrow;
    cl_object cl_apropos(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_apropos_list(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_find_relative_package(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_subclassp(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_of_class_p(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_do_deftype(c_long, cl_lispunion*, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_upgraded_array_element_type(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_upgraded_complex_part_type(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_typep(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_coerce(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_subtypep(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_short_float_p(cl_lispunion*) @nogc nothrow;
    cl_object si_single_float_p(cl_lispunion*) @nogc nothrow;
    cl_object si_double_float_p(cl_lispunion*) @nogc nothrow;
    cl_object si_long_float_p(cl_lispunion*) @nogc nothrow;
    cl_object si_ratiop(cl_lispunion*) @nogc nothrow;
    cl_object si_do_defsetf(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_do_define_setf_method(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_make_sequence(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_concatenate(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_map(c_long, cl_lispunion*, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_some(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_every(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_notany(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_notevery(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_map_into(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_reduce(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_fill(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_replace(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_remove(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_remove_if(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_remove_if_not(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_delete(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_delete_if(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_delete_if_not(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_count(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_count_if(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_count_if_not(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_substitute(c_long, cl_lispunion*, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_substitute_if(c_long, cl_lispunion*, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_substitute_if_not(c_long, cl_lispunion*, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_nsubstitute(c_long, cl_lispunion*, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_nsubstitute_if(c_long, cl_lispunion*, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_nsubstitute_if_not(c_long, cl_lispunion*, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_find(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_find_if(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_find_if_not(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_position(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_position_if(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_position_if_not(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_remove_duplicates(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_delete_duplicates(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_mismatch(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_search(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_sort(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_stable_sort(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_merge(c_long, cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_constantly(cl_lispunion*) @nogc nothrow;
    cl_object cl_complement(cl_lispunion*) @nogc nothrow;
    cl_object si_traced_old_definition(cl_lispunion*) @nogc nothrow;
    cl_object cl_pprint_newline(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_pprint_indent(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_pprint_tab(c_long, cl_lispunion*, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_pprint_fill(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_pprint_linear(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_pprint_tabular(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_copy_pprint_dispatch(c_long, ...) @nogc nothrow;
    cl_object cl_pprint_dispatch(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_set_pprint_dispatch(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_method_combination_error(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_invalid_method_error(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object clos_std_compute_applicable_methods(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object clos_std_compute_effective_method(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object clos_compute_effective_method_function(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_slot_boundp(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_slot_makunbound(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_slot_exists_p(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object clos_need_to_make_load_form_p(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object clos_load_defclass(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object cl_slot_value(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object clos_slot_value_set(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object clos_standard_instance_access(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    void __gmpf_sub(__mpf_struct*, const(__mpf_struct)*, const(__mpf_struct)*) @nogc nothrow;
    cl_object clos_standard_instance_set(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object clos_extract_lambda_list(cl_lispunion*) @nogc nothrow;
    cl_object clos_extract_specializer_names(cl_lispunion*) @nogc nothrow;
    cl_object cl_abort(c_long, ...) @nogc nothrow;
    cl_object cl_continue(c_long, ...) @nogc nothrow;
    cl_object cl_compute_restarts(c_long, ...) @nogc nothrow;
    cl_object cl_find_restart(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_invoke_restart(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_invoke_restart_interactively(cl_lispunion*) @nogc nothrow;
    cl_object cl_make_condition(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_muffle_warning(c_long, ...) @nogc nothrow;
    cl_object cl_store_value(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object cl_use_value(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_bind_simple_restarts(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_bind_simple_handlers(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_assert_failure(c_long, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_wrong_type_argument(c_long, cl_lispunion*, cl_lispunion*, ...) @nogc nothrow;
    cl_object si_ccase_error(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_ecase_error(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_etypecase_error(cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_ctypecase_error(cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_do_check_type(cl_lispunion*, cl_lispunion*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    cl_object si_negative_fixnum_p(cl_lispunion*) @nogc nothrow;
    cl_object si_non_negative_fixnum_p(cl_lispunion*) @nogc nothrow;
    cl_object si_non_positive_fixnum_p(cl_lispunion*) @nogc nothrow;
    cl_object si_positive_fixnum_p(cl_lispunion*) @nogc nothrow;
    cl_object si_array_index_p(cl_lispunion*) @nogc nothrow;
    cl_object si_negative_integer_p(cl_lispunion*) @nogc nothrow;
    cl_object si_non_negative_integer_p(cl_lispunion*) @nogc nothrow;
    cl_object si_non_positive_integer_p(cl_lispunion*) @nogc nothrow;
    cl_object si_positive_integer_p(cl_lispunion*) @nogc nothrow;
    cl_object si_negative_rational_p(cl_lispunion*) @nogc nothrow;
    cl_object si_non_negative_rational_p(cl_lispunion*) @nogc nothrow;
    cl_object si_non_positive_rational_p(cl_lispunion*) @nogc nothrow;
    cl_object si_positive_rational_p(cl_lispunion*) @nogc nothrow;
    cl_object si_negative_ratio_p(cl_lispunion*) @nogc nothrow;
    cl_object si_non_negative_ratio_p(cl_lispunion*) @nogc nothrow;
    cl_object si_non_positive_ratio_p(cl_lispunion*) @nogc nothrow;
    cl_object si_positive_ratio_p(cl_lispunion*) @nogc nothrow;
    cl_object si_negative_real_p(cl_lispunion*) @nogc nothrow;
    cl_object si_non_negative_real_p(cl_lispunion*) @nogc nothrow;
    cl_object si_non_positive_real_p(cl_lispunion*) @nogc nothrow;
    cl_object si_positive_real_p(cl_lispunion*) @nogc nothrow;
    cl_object si_negative_float_p(cl_lispunion*) @nogc nothrow;
    cl_object si_non_negative_float_p(cl_lispunion*) @nogc nothrow;
    cl_object si_non_positive_float_p(cl_lispunion*) @nogc nothrow;
    cl_object si_positive_float_p(cl_lispunion*) @nogc nothrow;
    cl_object si_negative_short_float_p(cl_lispunion*) @nogc nothrow;
    cl_object si_non_negative_short_float_p(cl_lispunion*) @nogc nothrow;
    cl_object si_non_positive_short_float_p(cl_lispunion*) @nogc nothrow;
    cl_object si_positive_short_float_p(cl_lispunion*) @nogc nothrow;
    cl_object si_negative_single_float_p(cl_lispunion*) @nogc nothrow;
    cl_object si_non_negative_single_float_p(cl_lispunion*) @nogc nothrow;
    cl_object si_non_positive_single_float_p(cl_lispunion*) @nogc nothrow;
    cl_object si_positive_single_float_p(cl_lispunion*) @nogc nothrow;
    cl_object si_negative_double_float_p(cl_lispunion*) @nogc nothrow;
    cl_object si_non_negative_double_float_p(cl_lispunion*) @nogc nothrow;
    cl_object si_non_positive_double_float_p(cl_lispunion*) @nogc nothrow;
    cl_object si_positive_double_float_p(cl_lispunion*) @nogc nothrow;
    cl_object si_negative_long_float_p(cl_lispunion*) @nogc nothrow;
    cl_object si_non_negative_long_float_p(cl_lispunion*) @nogc nothrow;
    cl_object si_non_positive_long_float_p(cl_lispunion*) @nogc nothrow;
    cl_object si_positive_long_float_p(cl_lispunion*) @nogc nothrow;
    void __gmpf_sqrt_ui(__mpf_struct*, c_ulong) @nogc nothrow;
    void __gmpf_sqrt(__mpf_struct*, const(__mpf_struct)*) @nogc nothrow;
    c_ulong __gmpf_size(const(__mpf_struct)*) @nogc nothrow;
    void __gmpf_set_z(__mpf_struct*, const(__mpz_struct)*) @nogc nothrow;
    void __gmpf_set_ui(__mpf_struct*, c_ulong) @nogc nothrow;
    int __gmpf_set_str(__mpf_struct*, const(char)*, int) @nogc nothrow;
    void __gmpf_set_si(__mpf_struct*, c_long) @nogc nothrow;
    void __gmpf_set_q(__mpf_struct*, const(__mpq_struct)*) @nogc nothrow;
    void __gmpf_set_prec_raw(__mpf_struct*, c_ulong) @nogc nothrow;
    void __gmpf_set_prec(__mpf_struct*, c_ulong) @nogc nothrow;
    void __gmpf_set_default_prec(c_ulong) @nogc nothrow;
    void __gmpf_set_d(__mpf_struct*, double) @nogc nothrow;
    void __gmpf_set(__mpf_struct*, const(__mpf_struct)*) @nogc nothrow;
    void __gmpf_reldiff(__mpf_struct*, const(__mpf_struct)*, const(__mpf_struct)*) @nogc nothrow;
    void __gmpf_random2(__mpf_struct*, c_long, c_long) @nogc nothrow;
    void __gmpf_pow_ui(__mpf_struct*, const(__mpf_struct)*, c_ulong) @nogc nothrow;
    c_ulong __gmpf_out_str(_IO_FILE*, int, c_ulong, const(__mpf_struct)*) @nogc nothrow;
    void __gmpf_neg(__mpf_struct*, const(__mpf_struct)*) @nogc nothrow;
    void __gmpf_mul_ui(__mpf_struct*, const(__mpf_struct)*, c_ulong) @nogc nothrow;
    void __gmpf_mul_2exp(__mpf_struct*, const(__mpf_struct)*, c_ulong) @nogc nothrow;
    void __gmpf_mul(__mpf_struct*, const(__mpf_struct)*, const(__mpf_struct)*) @nogc nothrow;
    int __gmpf_integer_p(const(__mpf_struct)*) @nogc nothrow;
    c_ulong __gmpf_inp_str(__mpf_struct*, _IO_FILE*, int) @nogc nothrow;
    void __gmpf_init_set_ui(__mpf_struct*, c_ulong) @nogc nothrow;
    int __gmpf_init_set_str(__mpf_struct*, const(char)*, int) @nogc nothrow;
    void __gmpf_init_set_si(__mpf_struct*, c_long) @nogc nothrow;
    void __gmpf_init_set_d(__mpf_struct*, double) @nogc nothrow;
    void __gmpf_init_set(__mpf_struct*, const(__mpf_struct)*) @nogc nothrow;
    void __gmpf_inits(__mpf_struct*, ...) @nogc nothrow;
    void __gmpf_init2(__mpf_struct*, c_ulong) @nogc nothrow;
    void __gmpf_init(__mpf_struct*) @nogc nothrow;
    c_ulong __gmpf_get_ui(const(__mpf_struct)*) @nogc nothrow;
    alias bds_ptr = ecl_bds_frame*;
    alias ihs_ptr = ecl_ihs_frame*;
    char* __gmpf_get_str(char*, c_long*, int, c_ulong, const(__mpf_struct)*) @nogc nothrow;
    enum _Anonymous_4
    {
        smm_input = 0,
        smm_input_file = 1,
        smm_output = 2,
        smm_output_file = 3,
        smm_io = 4,
        smm_io_file = 5,
        smm_synonym = 6,
        smm_broadcast = 7,
        smm_concatenated = 8,
        smm_two_way = 9,
        smm_echo = 10,
        smm_string_input = 11,
        smm_string_output = 12,
        smm_probe = 13,
        smm_sequence_input = 14,
        smm_sequence_output = 15,
    }
    enum smm_input = _Anonymous_4.smm_input;
    enum smm_input_file = _Anonymous_4.smm_input_file;
    enum smm_output = _Anonymous_4.smm_output;
    enum smm_output_file = _Anonymous_4.smm_output_file;
    enum smm_io = _Anonymous_4.smm_io;
    enum smm_io_file = _Anonymous_4.smm_io_file;
    enum smm_synonym = _Anonymous_4.smm_synonym;
    enum smm_broadcast = _Anonymous_4.smm_broadcast;
    enum smm_concatenated = _Anonymous_4.smm_concatenated;
    enum smm_two_way = _Anonymous_4.smm_two_way;
    enum smm_echo = _Anonymous_4.smm_echo;
    enum smm_string_input = _Anonymous_4.smm_string_input;
    enum smm_string_output = _Anonymous_4.smm_string_output;
    enum smm_probe = _Anonymous_4.smm_probe;
    enum smm_sequence_input = _Anonymous_4.smm_sequence_input;
    enum smm_sequence_output = _Anonymous_4.smm_sequence_output;
    c_long __gmpf_get_si(const(__mpf_struct)*) @nogc nothrow;
    c_ulong __gmpf_get_prec(const(__mpf_struct)*) @nogc nothrow;
    c_ulong __gmpf_get_default_prec() @nogc nothrow;
    double __gmpf_get_d_2exp(c_long*, const(__mpf_struct)*) @nogc nothrow;
    pragma(mangle, "_ecl_big_set_fixnum") cl_object _ecl_big_set_fixnum_(cl_lispunion*, c_long) @nogc nothrow;
    pragma(mangle, "_ecl_big_set_index") cl_object _ecl_big_set_index_(cl_lispunion*, c_ulong) @nogc nothrow;
    c_long _ecl_big_get_fixnum(cl_object) @nogc nothrow;
    c_ulong _ecl_big_get_index(cl_object) @nogc nothrow;
    real _ecl_big_to_long_double(cl_object) @nogc nothrow;
    alias _ecl_big_binary_op = void function(cl_object, cl_lispunion*, cl_lispunion*);
    void function(cl_object, cl_lispunion*, cl_lispunion*) _ecl_big_boole_operator(int) @nogc nothrow;
    double __gmpf_get_d(const(__mpf_struct)*) @nogc nothrow;
    void __gmpf_floor(__mpf_struct*, const(__mpf_struct)*) @nogc nothrow;
    int __gmpf_fits_ushort_p(const(__mpf_struct)*) @nogc nothrow;
    int __gmpf_fits_ulong_p(const(__mpf_struct)*) @nogc nothrow;
    int __gmpf_fits_uint_p(const(__mpf_struct)*) @nogc nothrow;
    int __gmpf_fits_sshort_p(const(__mpf_struct)*) @nogc nothrow;
    static c_long ecl_to_fix(cl_object) @nogc nothrow;
    static c_ulong ecl_to_size(cl_object) @nogc nothrow;
    alias bool_ = int;
    alias byte_ = ubyte;
    alias cl_type = _Anonymous_5;
    enum _Anonymous_5 {
        t_start = 0,
        t_list = 1,
        t_character = 2,
        t_fixnum = 3,
        t_bignum = 4,
        t_ratio = 5,
        t_singlefloat = 6,
        t_doublefloat = 7,
        t_longfloat = 8,
        t_complex = 9,
        t_symbol = 10,
        t_package = 11,
        t_hashtable = 12,
        t_array = 13,
        t_vector = 14,
        t_string = 15,
        t_base_string = 16,
        t_bitvector = 17,
        t_stream = 18,
        t_random = 19,
        t_readtable = 20,
        t_pathname = 21,
        t_bytecodes = 22,
        t_bclosure = 23,
        t_cfun = 24,
        t_cfunfixed = 25,
        t_cclosure = 26,
        t_instance = 27,
        t_structure = 27,
        t_process = 28,
        t_lock = 29,
        t_rwlock = 30,
        t_condition_variable = 31,
        t_semaphore = 32,
        t_barrier = 33,
        t_mailbox = 34,
        t_codeblock = 35,
        t_foreign = 36,
        t_frame = 37,
        t_weak_pointer = 38,
        t_end = 39,
        t_other = 40,
        t_contiguous = 41,
        FREE = 127,
    }
    enum t_start = _Anonymous_5.t_start;
    enum t_list = _Anonymous_5.t_list;
    enum t_character = _Anonymous_5.t_character;
    enum t_fixnum = _Anonymous_5.t_fixnum;
    enum t_bignum = _Anonymous_5.t_bignum;
    enum t_ratio = _Anonymous_5.t_ratio;
    enum t_singlefloat = _Anonymous_5.t_singlefloat;
    enum t_doublefloat = _Anonymous_5.t_doublefloat;
    enum t_longfloat = _Anonymous_5.t_longfloat;
    enum t_complex = _Anonymous_5.t_complex;
    enum t_symbol = _Anonymous_5.t_symbol;
    enum t_package = _Anonymous_5.t_package;
    enum t_hashtable = _Anonymous_5.t_hashtable;
    enum t_array = _Anonymous_5.t_array;
    enum t_vector = _Anonymous_5.t_vector;
    enum t_string = _Anonymous_5.t_string;
    enum t_base_string = _Anonymous_5.t_base_string;
    enum t_bitvector = _Anonymous_5.t_bitvector;
    enum t_stream = _Anonymous_5.t_stream;
    enum t_random = _Anonymous_5.t_random;
    enum t_readtable = _Anonymous_5.t_readtable;
    enum t_pathname = _Anonymous_5.t_pathname;
    enum t_bytecodes = _Anonymous_5.t_bytecodes;
    enum t_bclosure = _Anonymous_5.t_bclosure;
    enum t_cfun = _Anonymous_5.t_cfun;
    enum t_cfunfixed = _Anonymous_5.t_cfunfixed;
    enum t_cclosure = _Anonymous_5.t_cclosure;
    enum t_instance = _Anonymous_5.t_instance;
    enum t_structure = _Anonymous_5.t_structure;
    enum t_process = _Anonymous_5.t_process;
    enum t_lock = _Anonymous_5.t_lock;
    enum t_rwlock = _Anonymous_5.t_rwlock;
    enum t_condition_variable = _Anonymous_5.t_condition_variable;
    enum t_semaphore = _Anonymous_5.t_semaphore;
    enum t_barrier = _Anonymous_5.t_barrier;
    enum t_mailbox = _Anonymous_5.t_mailbox;
    enum t_codeblock = _Anonymous_5.t_codeblock;
    enum t_foreign = _Anonymous_5.t_foreign;
    enum t_frame = _Anonymous_5.t_frame;
    enum t_weak_pointer = _Anonymous_5.t_weak_pointer;
    enum t_end = _Anonymous_5.t_end;
    enum t_other = _Anonymous_5.t_other;
    enum t_contiguous = _Anonymous_5.t_contiguous;
    enum FREE = _Anonymous_5.FREE;
    alias cl_object = cl_object;
    union cl_lispunion
    {
        ecl_bignum big;
        ecl_ratio ratio;
        ecl_singlefloat SF;
        ecl_doublefloat DF;
        ecl_long_float longfloat;
        ecl_complex complex;
        ecl_symbol symbol;
        ecl_package pack;
        ecl_hashtable hash;
        ecl_array array;
        ecl_vector vector;
        ecl_base_string base_string;
        ecl_string string;
        ecl_stream stream;
        ecl_random random;
        ecl_readtable readtable;
        ecl_pathname pathname;
        ecl_bytecodes bytecodes;
        ecl_bclosure bclosure;
        ecl_cfun cfun;
        ecl_cfunfixed cfunfixed;
        ecl_cclosure cclosure;
        ecl_dummy d;
        ecl_instance instance;
        ecl_process process;
        ecl_queue queue;
        ecl_lock lock;
        ecl_rwlock rwlock;
        ecl_condition_variable condition_variable;
        ecl_semaphore semaphore;
        ecl_barrier barrier;
        ecl_mailbox mailbox;
        ecl_codeblock cblock;
        ecl_foreign foreign;
        ecl_stack_frame frame;
        ecl_weak_pointer weak;
    }
    alias cl_return = cl_object;
    alias cl_narg = c_long;
    alias cl_objectfn = cl_object function(c_long);
    alias cl_objectfn_fixed = cl_object function();
    int __gmpf_fits_slong_p(const(__mpf_struct)*) @nogc nothrow;
    int __gmpf_fits_sint_p(const(__mpf_struct)*) @nogc nothrow;
    int __gmpf_eq(const(__mpf_struct)*, const(__mpf_struct)*, c_ulong) @nogc nothrow;
    void __gmpf_dump(const(__mpf_struct)*) @nogc nothrow;
    void __gmpf_div_ui(__mpf_struct*, const(__mpf_struct)*, c_ulong) @nogc nothrow;
    void __gmpf_div_2exp(__mpf_struct*, const(__mpf_struct)*, c_ulong) @nogc nothrow;
    void __gmpf_div(__mpf_struct*, const(__mpf_struct)*, const(__mpf_struct)*) @nogc nothrow;
    int __gmpf_cmp_ui(const(__mpf_struct)*, c_ulong) @nogc nothrow;
    int __gmpf_cmp_si(const(__mpf_struct)*, c_long) @nogc nothrow;
    int __gmpf_cmp_d(const(__mpf_struct)*, double) @nogc nothrow;
    int __gmpf_cmp_z(const(__mpf_struct)*, const(__mpz_struct)*) @nogc nothrow;
    int __gmpf_cmp(const(__mpf_struct)*, const(__mpf_struct)*) @nogc nothrow;
    struct ecl_singlefloat
    {
        byte t;
        byte m;
        byte padding1;
        byte padding2;
        float SFVAL;
    }
    struct ecl_doublefloat
    {
        byte t;
        byte m;
        byte padding1;
        byte padding2;
        double DFVAL;
    }
    void __gmpf_clears(__mpf_struct*, ...) @nogc nothrow;
    struct ecl_long_float
    {
        byte t;
        byte m;
        byte padding1;
        byte padding2;
        real value;
    }
    struct ecl_bignum
    {
        byte t;
        byte m;
        byte padding1;
        byte padding2;
        __mpz_struct[1] big_num;
    }
    void __gmpf_clear(__mpf_struct*) @nogc nothrow;
    struct ecl_ratio
    {
        byte t;
        byte m;
        byte padding1;
        byte padding2;
        cl_object den;
        cl_object num;
    }
    struct ecl_complex
    {
        byte t;
        byte m;
        byte padding1;
        byte padding2;
        cl_object real_;
        cl_object imag;
    }
    enum ecl_stype
    {
        ecl_stp_ordinary = 0,
        ecl_stp_constant = 1,
        ecl_stp_special = 2,
        ecl_stp_macro = 4,
        ecl_stp_special_form = 8,
    }
    enum ecl_stp_ordinary = ecl_stype.ecl_stp_ordinary;
    enum ecl_stp_constant = ecl_stype.ecl_stp_constant;
    enum ecl_stp_special = ecl_stype.ecl_stp_special;
    enum ecl_stp_macro = ecl_stype.ecl_stp_macro;
    enum ecl_stp_special_form = ecl_stype.ecl_stp_special_form;
    void __gmpf_ceil(__mpf_struct*, const(__mpf_struct)*) @nogc nothrow;
    void __gmpf_add_ui(__mpf_struct*, const(__mpf_struct)*, c_ulong) @nogc nothrow;
    void __gmpf_add(__mpf_struct*, const(__mpf_struct)*, const(__mpf_struct)*) @nogc nothrow;
    struct ecl_symbol
    {
        byte t;
        byte m;
        byte stype;
        byte dynamic;
        cl_object value;
        cl_object gfdef;
        cl_object plist;
        cl_object name;
        cl_object hpack;
        c_ulong binding;
    }
    struct ecl_package
    {
        byte t;
        byte m;
        byte locked;
        byte padding;
        cl_object name;
        cl_object nicknames;
        cl_object shadowings;
        cl_object uses;
        cl_object usedby;
        cl_object internal;
        cl_object external;
    }
    enum _Anonymous_6
    {
        ECL_INTERNAL = 1,
        ECL_EXTERNAL = 2,
        ECL_INHERITED = 3,
    }
    enum ECL_INTERNAL = _Anonymous_6.ECL_INTERNAL;
    enum ECL_EXTERNAL = _Anonymous_6.ECL_EXTERNAL;
    enum ECL_INHERITED = _Anonymous_6.ECL_INHERITED;
    void __gmpf_abs(__mpf_struct*, const(__mpf_struct)*) @nogc nothrow;
    void __gmpq_swap(__mpq_struct*, __mpq_struct*) @nogc nothrow;
    void __gmpq_sub(__mpq_struct*, const(__mpq_struct)*, const(__mpq_struct)*) @nogc nothrow;
    struct ecl_cons
    {
        cl_object car;
        cl_object cdr;
    }
    enum ecl_httest
    {
        ecl_htt_eq = 0,
        ecl_htt_eql = 1,
        ecl_htt_equal = 2,
        ecl_htt_equalp = 3,
        ecl_htt_pack = 4,
    }
    enum ecl_htt_eq = ecl_httest.ecl_htt_eq;
    enum ecl_htt_eql = ecl_httest.ecl_htt_eql;
    enum ecl_htt_equal = ecl_httest.ecl_htt_equal;
    enum ecl_htt_equalp = ecl_httest.ecl_htt_equalp;
    enum ecl_htt_pack = ecl_httest.ecl_htt_pack;
    enum ecl_htweak
    {
        ecl_htt_not_weak = 0,
        ecl_htt_weak_key = 1,
        ecl_htt_weak_value = 2,
        ecl_htt_weak_key_and_value = 3,
    }
    enum ecl_htt_not_weak = ecl_htweak.ecl_htt_not_weak;
    enum ecl_htt_weak_key = ecl_htweak.ecl_htt_weak_key;
    enum ecl_htt_weak_value = ecl_htweak.ecl_htt_weak_value;
    enum ecl_htt_weak_key_and_value = ecl_htweak.ecl_htt_weak_key_and_value;
    struct ecl_hashtable_entry
    {
        cl_object key;
        cl_object value;
    }
    struct ecl_hashtable
    {
        byte t;
        byte m;
        byte test;
        byte weak;
        ecl_hashtable_entry* data;
        c_ulong entries;
        c_ulong size;
        c_ulong limit;
        cl_object rehash_size;
        cl_object threshold;
        double factor;
        cl_object function(cl_lispunion*, cl_lispunion*, cl_lispunion*) get;
        cl_object function(cl_lispunion*, cl_lispunion*, cl_lispunion*) set;
        int function(cl_object, cl_lispunion*) rem;
    }
    alias cl_elttype = cl_elttype;
    enum cl_elttype
    {
        ecl_aet_object = 0,
        ecl_aet_sf = 1,
        ecl_aet_df = 2,
        ecl_aet_bit = 3,
        ecl_aet_fix = 4,
        ecl_aet_index = 5,
        ecl_aet_b8 = 6,
        ecl_aet_i8 = 7,
        ecl_aet_b16 = 8,
        ecl_aet_i16 = 9,
        ecl_aet_b32 = 10,
        ecl_aet_i32 = 11,
        ecl_aet_b64 = 12,
        ecl_aet_i64 = 13,
        ecl_aet_ch = 14,
        ecl_aet_bc = 15,
        ecl_aet_last_type = 15,
    }
    enum ecl_aet_object = cl_elttype.ecl_aet_object;
    enum ecl_aet_sf = cl_elttype.ecl_aet_sf;
    enum ecl_aet_df = cl_elttype.ecl_aet_df;
    enum ecl_aet_bit = cl_elttype.ecl_aet_bit;
    enum ecl_aet_fix = cl_elttype.ecl_aet_fix;
    enum ecl_aet_index = cl_elttype.ecl_aet_index;
    enum ecl_aet_b8 = cl_elttype.ecl_aet_b8;
    enum ecl_aet_i8 = cl_elttype.ecl_aet_i8;
    enum ecl_aet_b16 = cl_elttype.ecl_aet_b16;
    enum ecl_aet_i16 = cl_elttype.ecl_aet_i16;
    enum ecl_aet_b32 = cl_elttype.ecl_aet_b32;
    enum ecl_aet_i32 = cl_elttype.ecl_aet_i32;
    enum ecl_aet_b64 = cl_elttype.ecl_aet_b64;
    enum ecl_aet_i64 = cl_elttype.ecl_aet_i64;
    enum ecl_aet_ch = cl_elttype.ecl_aet_ch;
    enum ecl_aet_bc = cl_elttype.ecl_aet_bc;
    enum ecl_aet_last_type = cl_elttype.ecl_aet_last_type;
    union ecl_array_data
    {
        cl_object* t;
        ubyte* bc;
        int* c;
        ubyte* b8;
        byte* i8;
        ushort* b16;
        short* i16;
        uint* b32;
        int* i32;
        c_ulong* b64;
        c_long* i64;
        float* sf;
        double* df;
        c_long* fix;
        c_ulong* index;
        ubyte* bit;
    }
    struct ecl_array {
        byte t;
        byte m;
        byte elttype;
        byte flags;
        cl_object displaced;
        c_ulong dim;
        c_ulong* dims;
        ecl_array_data self;
        ubyte offset;
        ubyte rank;
    }
    struct ecl_vector {
        byte t;
        byte m;
        byte elttype;
        byte flags;
        cl_object displaced;
        c_ulong dim;
        c_ulong fillp;
        ecl_array_data self;
        ubyte offset;
    }
    struct ecl_base_string {
        byte t;
        byte m;
        byte elttype;
        byte flags;
        cl_object displaced;
        c_ulong dim;
        c_ulong fillp;
        ubyte* self;
    }
    struct ecl_string {
        byte t;
        byte m;
        byte elttype;
        byte flags;
        cl_object displaced;
        c_ulong dim;
        c_ulong fillp;
        int* self;
    }
    enum ecl_smmode {
        ecl_smm_input = 0,
        ecl_smm_input_file = 1,
        ecl_smm_output = 2,
        ecl_smm_output_file = 3,
        ecl_smm_io = 4,
        ecl_smm_io_file = 5,
        ecl_smm_synonym = 6,
        ecl_smm_broadcast = 7,
        ecl_smm_concatenated = 8,
        ecl_smm_two_way = 9,
        ecl_smm_echo = 10,
        ecl_smm_string_input = 11,
        ecl_smm_string_output = 12,
        ecl_smm_probe = 13,
        ecl_smm_sequence_input = 14,
        ecl_smm_sequence_output = 15,
    }
    enum ecl_smm_input = ecl_smmode.ecl_smm_input;
    enum ecl_smm_input_file = ecl_smmode.ecl_smm_input_file;
    enum ecl_smm_output = ecl_smmode.ecl_smm_output;
    enum ecl_smm_output_file = ecl_smmode.ecl_smm_output_file;
    enum ecl_smm_io = ecl_smmode.ecl_smm_io;
    enum ecl_smm_io_file = ecl_smmode.ecl_smm_io_file;
    enum ecl_smm_synonym = ecl_smmode.ecl_smm_synonym;
    enum ecl_smm_broadcast = ecl_smmode.ecl_smm_broadcast;
    enum ecl_smm_concatenated = ecl_smmode.ecl_smm_concatenated;
    enum ecl_smm_two_way = ecl_smmode.ecl_smm_two_way;
    enum ecl_smm_echo = ecl_smmode.ecl_smm_echo;
    enum ecl_smm_string_input = ecl_smmode.ecl_smm_string_input;
    enum ecl_smm_string_output = ecl_smmode.ecl_smm_string_output;
    enum ecl_smm_probe = ecl_smmode.ecl_smm_probe;
    enum ecl_smm_sequence_input = ecl_smmode.ecl_smm_sequence_input;
    enum ecl_smm_sequence_output = ecl_smmode.ecl_smm_sequence_output;
    struct ecl_file_ops {
        c_ulong function(cl_object, ubyte*, c_ulong) write_byte8;
        c_ulong function(cl_object, ubyte*, c_ulong) read_byte8;
        void function(cl_object, cl_lispunion*) write_byte;
        cl_object function(cl_lispunion*) read_byte;
        int function(cl_object) read_char;
        int function(cl_object, int) write_char;
        void function(cl_object, int) unread_char;
        int function(cl_object) peek_char;
        c_ulong function(cl_object, cl_lispunion*, c_ulong, c_ulong) read_vector;
        c_ulong function(cl_object, cl_lispunion*, c_ulong, c_ulong) write_vector;
        int function(cl_object) listen;
        void function(cl_object) clear_input;
        void function(cl_object) clear_output;
        void function(cl_object) finish_output;
        void function(cl_object) force_output;
        int function(cl_object) input_p;
        int function(cl_object) output_p;
        int function(cl_object) interactive_p;
        cl_object function(cl_lispunion*) element_type;
        cl_object function(cl_lispunion*) length;
        cl_object function(cl_lispunion*) get_position;
        cl_object function(cl_lispunion*, cl_lispunion*) set_position;
        int function(cl_object) column;
        cl_object function(cl_lispunion*) close;
    }
    enum {
        ECL_STREAM_BINARY = 0,
        ECL_STREAM_FORMAT = 15,
        ECL_STREAM_DEFAULT_FORMAT = 2,
        ECL_STREAM_ISO_8859_1 = 1,
        ECL_STREAM_LATIN_1 = 1,
        ECL_STREAM_UTF_8 = 2,
        ECL_STREAM_UCS_2 = 3,
        ECL_STREAM_UCS_2LE = 133,
        ECL_STREAM_UCS_2BE = 5,
        ECL_STREAM_UCS_4 = 6,
        ECL_STREAM_UCS_4LE = 135,
        ECL_STREAM_UCS_4BE = 7,
        ECL_STREAM_USER_FORMAT = 8,
        ECL_STREAM_US_ASCII = 10,
        ECL_STREAM_CR = 16,
        ECL_STREAM_LF = 32,
        ECL_STREAM_SIGNED_BYTES = 64,
        ECL_STREAM_LITTLE_ENDIAN = 128,
        ECL_STREAM_C_STREAM = 256,
        ECL_STREAM_MIGHT_SEEK = 512,
        ECL_STREAM_CLOSE_COMPONENTS = 1024,
    }
    enum ECL_STREAM_BINARY = ECL_STREAM_BINARY;
    enum ECL_STREAM_FORMAT = ECL_STREAM_FORMAT;
    enum ECL_STREAM_DEFAULT_FORMAT = ECL_STREAM_DEFAULT_FORMAT;
    enum ECL_STREAM_ISO_8859_1 = ECL_STREAM_ISO_8859_1;
    enum ECL_STREAM_LATIN_1 = ECL_STREAM_LATIN_1;
    enum ECL_STREAM_UTF_8 = ECL_STREAM_UTF_8;
    enum ECL_STREAM_UCS_2 = ECL_STREAM_UCS_2;
    enum ECL_STREAM_UCS_2LE = ECL_STREAM_UCS_2LE;
    enum ECL_STREAM_UCS_2BE = ECL_STREAM_UCS_2BE;
    enum ECL_STREAM_UCS_4 = ECL_STREAM_UCS_4;
    enum ECL_STREAM_UCS_4LE = ECL_STREAM_UCS_4LE;
    enum ECL_STREAM_UCS_4BE = ECL_STREAM_UCS_4BE;
    enum ECL_STREAM_USER_FORMAT = ECL_STREAM_USER_FORMAT;
    enum ECL_STREAM_US_ASCII = ECL_STREAM_US_ASCII;
    enum ECL_STREAM_CR = ECL_STREAM_CR;
    enum ECL_STREAM_LF = ECL_STREAM_LF;
    enum ECL_STREAM_SIGNED_BYTES = ECL_STREAM_SIGNED_BYTES;
    enum ECL_STREAM_LITTLE_ENDIAN = ECL_STREAM_LITTLE_ENDIAN;
    enum ECL_STREAM_C_STREAM = ECL_STREAM_C_STREAM;
    enum ECL_STREAM_MIGHT_SEEK = ECL_STREAM_MIGHT_SEEK;
    enum ECL_STREAM_CLOSE_COMPONENTS = ECL_STREAM_CLOSE_COMPONENTS;
    alias cl_eformat_decoder = int function(cl_object);
    alias cl_eformat_encoder = int function(cl_object, ubyte*, int);
    alias cl_eformat_read_byte8 = c_ulong function(cl_object, ubyte*, c_ulong);
    struct ecl_stream {
        byte t;
        byte m;
        byte mode;
        byte closed;
        ecl_file_ops* ops;
        static union _anonf {
            _IO_FILE* stream;
            c_long descriptor;
        }
        _anonf file;
        cl_object object0;
        cl_object object1;
        cl_object byte_stack;
        c_ulong column;
        c_long last_char;
        c_long[2] last_code;
        c_long int0;
        c_long int1;
        c_ulong byte_size;
        c_long last_op;
        char* buffer;
        cl_object format;
        int function(cl_object, ubyte*, int) encoder;
        int function(cl_object) decoder;
        cl_object format_table;
        int flags;
        int eof_char;
    }
    struct ecl_random {
        byte t;
        byte m;
        byte padding1;
        byte padding2;
        cl_object value;
    }
    enum ecl_chattrib {
        whitespace = 0,
        terminating = 1,
        non_terminating = 2,
        single_escape = 3,
        multiple_escape = 4,
        constituent = 5,
    }
    struct ecl_readtable_entry {
        ecl_chattrib syntax_type;
        cl_object dispatch;
    }
    enum ecl_readtable_case
    {
        upcase,
        downcase,
        invert,
        preserve,
    }
    struct ecl_readtable {
        byte t;
        byte m;
        byte locked;
        byte padding;
        ecl_readtable_case read_case;
        ecl_readtable_entry* table;
        cl_object hash;
    }
    struct ecl_pathname {
        byte t;
        byte m;
        byte logical;
        byte padding;
        cl_object host;
        cl_object device;
        cl_object directory;
        cl_object name;
        cl_object type;
        cl_object version_;
    }
    struct ecl_codeblock {
        byte t;
        byte m;
        byte self_destruct;
        byte locked;
        void* handle;
        void* entry;
        cl_object* data;
        int data_size;
        cl_object* temp_data;
        int temp_data_size;
        const(cl_object)* data_text;
        cl_object next;
        cl_object name;
        cl_object links;
        c_ulong cfuns_size;
        const(ecl_cfun)* cfuns;
        cl_object source;
        cl_object refs;
        cl_object error;
    }
    struct ecl_cfun {
        byte t;
        byte m;
        byte narg;
        byte padding;
        cl_object name;
        cl_object block;
        cl_object function(c_long, ...) entry;
        cl_object file;
        cl_object file_position;
    }
    struct ecl_bytecodes {
        byte t;
        byte m;
        byte padding1;
        byte padding2;
        cl_object name;
        cl_object definition;
        cl_object function(c_long, ...) entry;
        c_ulong code_size;
        char* code;
        cl_object data;
        cl_object file;
        cl_object file_position;
    }
    struct ecl_bclosure {
        byte t;
        byte m;
        byte padding1;
        byte padding2;
        cl_object code;
        cl_object lex;
        cl_object function(c_long, ...) entry;
    }
    struct ecl_cfunfixed {
        byte t;
        byte m;
        byte narg;
        byte padding;
        cl_object name;
        cl_object block;
        cl_object function(c_long, ...) entry;
        cl_object function() entry_fixed;
        cl_object file;
        cl_object file_position;
    }
    struct ecl_cclosure {
        byte t;
        byte m;
        byte padding1;
        byte padding2;
        cl_object env;
        cl_object block;
        cl_object function(c_long, ...) entry;
        cl_object file;
        cl_object file_position;
    }
    enum ecl_ffi_tag {
        Char = 0,
        Unsigned_char = 1,
        Byte = 2,
        Unsigned_byte = 3,
        Short = 4,
        Unsigned_short = 5,
        Int = 6,
        Unsigned_int = 7,
        Long = 8,
        Unsigned_long = 9,
        Int8_t = 10,
        Uint8_t = 11,
        Int16_t = 12,
        Uint16_t = 13,
        Int32_t = 14,
        Uint32_t = 15,
        Int64_t = 16,
        Uint64_t = 17,
        Long_long = 18,
        Unsigned_long_long = 19,
        Pointer_void = 20,
        Cstring = 21,
        Object_ = 22,
        Float = 23,
        Double = 24,
        Void = 25,
    }
    union ecl_ffi_values {
        char c;
        ubyte uc;
        byte b;
        ubyte ub;
        int i;
        uint ui;
        short s;
        ushort us;
        c_long l;
        c_ulong ul;
        byte i8;
        ubyte u8;
        short i16;
        ushort u16;
        int i32;
        uint u32;
        c_long i64;
        c_ulong u64;
        long ll;
        ulong ull;
        c_ulong[2] l2;
        void* pv;
        char* pc;
        cl_object o;
        float f;
        double d;
    }
    enum ecl_ffi_calling_convention {
        cdecl,
        stdcall,
    }
    struct ecl_foreign {
        byte t;
        byte m;
        byte padding1;
        byte padding2;
        cl_object tag;
        c_ulong size;
        char* data;
    }
    struct ecl_stack_frame {
        byte t;
        byte m;
        byte padding1;
        byte padding2;
        cl_object* stack;
        cl_object* base;
        c_ulong size;
        cl_env_struct* env;
    }
    struct cl_env_struct {
        int disable_interrupts;
        c_ulong nvalues;
        cl_object[64] values;
        cl_object function_;
        c_ulong stack_size;
        c_ulong stack_limit_size;
        cl_object* stack;
        cl_object* stack_top;
        cl_object* stack_limit;
        c_ulong thread_local_bindings_size;
        cl_object* thread_local_bindings;
        cl_object bindings_array;
        c_ulong bds_size;
        c_ulong bds_limit_size;
        ecl_bds_frame* bds_org;
        ecl_bds_frame* bds_top;
        ecl_bds_frame* bds_limit;
        ecl_ihs_frame* ihs_top;
        c_ulong frs_size;
        c_ulong frs_limit_size;
        ecl_frame* frs_org;
        ecl_frame* frs_top;
        ecl_frame* frs_limit;
        ecl_frame* nlj_fr;
        c_ulong frame_id;
        c_ulong cs_size;
        c_ulong cs_limit_size;
        c_ulong cs_max_size;
        char* cs_org;
        char* cs_limit;
        char* cs_barrier;
        cl_object string_pool;
        cl_compiler_env* c_env;
        cl_object fmt_aux_stream;
        cl_object[3] big_register;
        cl_object own_process;
        cl_object pending_interrupt;
        cl_object signal_queue;
        cl_object signal_queue_spinlock;
        void* default_sigmask;
        ecl_cache* method_cache;
        ecl_cache* slot_cache;
        c_ulong ffi_args_limit;
	void **ffi_types; //_ffi_type** ffi_types; //idk where to get a struct _ffi_type
        ecl_ffi_values* ffi_values;
        ecl_ffi_values** ffi_values_ptrs;
        void* altstack;
        c_ulong altstack_size;
        int trap_fpe_bits;
        void* old_exception_filter;
        cl_object packages_to_be_created;
        cl_object packages_to_be_created_p;
        void* fault_address;
        int cleanup;
    }
    struct ecl_weak_pointer {
        byte t;
        byte m;
        byte padding1;
        byte padding2;
        cl_object value;
    }

    enum _Anonymous_10 {
        ECL_PROCESS_INACTIVE = 0,
        ECL_PROCESS_BOOTING = 1,
        ECL_PROCESS_ACTIVE = 2,
        ECL_PROCESS_EXITING = 3,
    }
    enum ECL_PROCESS_INACTIVE = _Anonymous_10.ECL_PROCESS_INACTIVE;
    enum ECL_PROCESS_BOOTING = _Anonymous_10.ECL_PROCESS_BOOTING;
    enum ECL_PROCESS_ACTIVE = _Anonymous_10.ECL_PROCESS_ACTIVE;
    enum ECL_PROCESS_EXITING = _Anonymous_10.ECL_PROCESS_EXITING;
    struct ecl_process
    {
        byte t;
        byte m;
        byte padding1;
        byte padding2;
        cl_object name;
        cl_object function_;
        cl_object args;
        cl_env_struct* env;
        cl_object interrupt;
        cl_object initial_bindings;
        cl_object parent;
        cl_object exit_barrier;
        cl_object exit_values;
        cl_object woken_up;
        cl_object queue_record;
        cl_object start_spinlock;
        c_ulong phase;
        c_ulong thread;
        int trap_fpe_bits;
    }
    enum _Anonymous_11
    {
        ECL_WAKEUP_ONE = 0,
        ECL_WAKEUP_ALL = 1,
        ECL_WAKEUP_RESET_FLAG = 2,
        ECL_WAKEUP_KILL = 4,
        ECL_WAKEUP_DELETE = 8,
    }
    enum ECL_WAKEUP_ONE = _Anonymous_11.ECL_WAKEUP_ONE;
    enum ECL_WAKEUP_ALL = _Anonymous_11.ECL_WAKEUP_ALL;
    enum ECL_WAKEUP_RESET_FLAG = _Anonymous_11.ECL_WAKEUP_RESET_FLAG;
    enum ECL_WAKEUP_KILL = _Anonymous_11.ECL_WAKEUP_KILL;
    enum ECL_WAKEUP_DELETE = _Anonymous_11.ECL_WAKEUP_DELETE;
    struct ecl_queue
    {
        byte t;
        byte m;
        byte padding1;
        byte padding2;
        cl_object list;
        cl_object spinlock;
    }
    struct ecl_semaphore
    {
        byte t;
        byte m;
        byte padding1;
        byte padding2;
        cl_object queue_list;
        cl_object queue_spinlock;
        cl_object name;
        c_long counter;
    }
    struct ecl_barrier
    {
        byte t;
        byte m;
        byte padding1;
        byte padding2;
        cl_object queue_list;
        cl_object queue_spinlock;
        cl_object name;
        c_long count;
        c_long arrivers_count;
    }
    struct ecl_lock
    {
        byte t;
        byte m;
        byte recursive;
        byte padding;
        cl_object queue_list;
        cl_object queue_spinlock;
        cl_object owner;
        cl_object name;
        c_long counter;
    }
    struct ecl_mailbox
    {
        byte t;
        byte m;
        byte padding1;
        byte padding2;
        cl_object name;
        cl_object data;
        cl_object reader_semaphore;
        cl_object writer_semaphore;
        c_ulong read_pointer;
        c_ulong write_pointer;
        c_ulong mask;
    }
    struct ecl_rwlock
    {
        byte t;
        byte m;
        byte padding1;
        byte padding2;
        cl_object name;
        pthread_rwlock_t mutex;
    }
    struct ecl_condition_variable
    {
        byte t;
        byte m;
        byte padding1;
        byte padding2;
        cl_object queue_list;
        cl_object queue_spinlock;
        cl_object lock;
    }
    void __gmpq_set_num(__mpq_struct*, const(__mpz_struct)*) @nogc nothrow;
    void __gmpq_set_f(__mpq_struct*, const(__mpf_struct)*) @nogc nothrow;
    void __gmpq_set_den(__mpq_struct*, const(__mpz_struct)*) @nogc nothrow;
    void __gmpq_set_d(__mpq_struct*, double) @nogc nothrow;
    void __gmpq_set(__mpq_struct*, const(__mpq_struct)*) @nogc nothrow;
    struct ecl_instance
    {
        byte t;
        byte m;
        byte isgf;
        byte padding;
        c_ulong length;
        cl_object clas;
        cl_object function(c_long, ...) entry;
        cl_object sig;
        cl_object* slots;
    }
    c_ulong __gmpq_out_str(_IO_FILE*, int, const(__mpq_struct)*) @nogc nothrow;
    alias ecl_va_list = _ecl_va_list[1];
    alias ecl_bds_ptr = ecl_bds_frame*;
    ecl_bds_frame* ecl_bds_overflow() @nogc nothrow;
    void ecl_bds_bind(cl_env_struct*, cl_object, cl_lispunion*) @nogc nothrow;
    void ecl_bds_push(cl_env_struct*, cl_object) @nogc nothrow;
    void ecl_bds_unwind1(cl_env_struct*) @nogc nothrow;
    void ecl_bds_unwind_n(cl_env_struct*, int) @nogc nothrow;
    cl_object ecl_bds_read(cl_env_struct*, cl_lispunion*) @nogc nothrow;
    cl_object* ecl_bds_ref(cl_env_struct*, cl_lispunion*) @nogc nothrow;
    pragma(mangle, "ecl_bds_set") cl_object ecl_bds_set_(cl_env_struct*, cl_lispunion*, cl_lispunion*) @nogc nothrow;
    void __gmpq_neg(__mpq_struct*, const(__mpq_struct)*) @nogc nothrow;
    static void ecl_bds_bind_inl(cl_env_struct*, cl_object, cl_lispunion*) @nogc nothrow;
    static void ecl_bds_push_inl(cl_env_struct*, cl_object) @nogc nothrow;
    static void ecl_bds_unwind1_inl(cl_env_struct*) @nogc nothrow;
    static cl_object ecl_bds_read_inl(cl_env_struct*, cl_lispunion*) @nogc nothrow;
    static cl_object* ecl_bds_ref_inl(cl_env_struct*, cl_lispunion*) @nogc nothrow;
    void __gmpq_mul_2exp(__mpq_struct*, const(__mpq_struct)*, c_ulong) @nogc nothrow;
    void __gmpq_mul(__mpq_struct*, const(__mpq_struct)*, const(__mpq_struct)*) @nogc nothrow;
    alias ecl_ihs_ptr = ecl_ihs_frame*;
    alias ecl_frame_ptr = ecl_frame*;
    ecl_frame* _ecl_frs_push(cl_env_struct*, cl_object) @nogc nothrow;
    void __gmpq_inv(__mpq_struct*, const(__mpq_struct)*) @nogc nothrow;
    c_ulong __gmpq_inp_str(__mpq_struct*, _IO_FILE*, int) @nogc nothrow;
    void __gmpq_inits(__mpq_struct*, ...) @nogc nothrow;
    void __gmpq_init(__mpq_struct*) @nogc nothrow;
    char* __gmpq_get_str(char*, int, const(__mpq_struct)*) @nogc nothrow;
    double __gmpq_get_d(const(__mpq_struct)*) @nogc nothrow;
    void __gmpq_get_den(__mpz_struct*, const(__mpq_struct)*) @nogc nothrow;
    void __gmpq_get_num(__mpz_struct*, const(__mpq_struct)*) @nogc nothrow;
    int __gmpq_equal(const(__mpq_struct)*, const(__mpq_struct)*) @nogc nothrow;
    void __gmpq_div_2exp(__mpq_struct*, const(__mpq_struct)*, c_ulong) @nogc nothrow;
    void __gmpq_div(__mpq_struct*, const(__mpq_struct)*, const(__mpq_struct)*) @nogc nothrow;
    int __gmpq_cmp_z(const(__mpq_struct)*, const(__mpz_struct)*) @nogc nothrow;
    int __gmpq_cmp_ui(const(__mpq_struct)*, c_ulong, c_ulong) @nogc nothrow;
    int __gmpq_cmp_si(const(__mpq_struct)*, c_long, c_ulong) @nogc nothrow;
    int __gmpq_cmp(const(__mpq_struct)*, const(__mpq_struct)*) @nogc nothrow;
    void __gmpq_clears(__mpq_struct*, ...) @nogc nothrow;
    void __gmpq_clear(__mpq_struct*) @nogc nothrow;
    void __gmpq_canonicalize(__mpq_struct*) @nogc nothrow;
    void __gmpq_add(__mpq_struct*, const(__mpq_struct)*, const(__mpq_struct)*) @nogc nothrow;
    void __gmpq_abs(__mpq_struct*, const(__mpq_struct)*) @nogc nothrow;
    const(__mpz_struct)* __gmpz_roinit_n(__mpz_struct*, const(c_ulong)*, c_long) @nogc nothrow;
    void __gmpz_limbs_finish(__mpz_struct*, c_long) @nogc nothrow;
    c_ulong* __gmpz_limbs_modify(__mpz_struct*, c_long) @nogc nothrow;
    c_ulong* __gmpz_limbs_write(__mpz_struct*, c_long) @nogc nothrow;
    const(c_ulong)* __gmpz_limbs_read(const(__mpz_struct)*) @nogc nothrow;
    void __gmpz_xor(__mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    void __gmpz_urandomm(__mpz_struct*, __gmp_randstate_struct*, const(__mpz_struct)*) @nogc nothrow;
    void __gmpz_urandomb(__mpz_struct*, __gmp_randstate_struct*, c_ulong) @nogc nothrow;
    alias GC_PTR = void*;
    alias GC_word = c_ulong;
    alias GC_signed_word = c_long;
    uint GC_get_version() @nogc nothrow;
    extern __gshared c_ulong GC_gc_no;
    c_ulong GC_get_gc_no() @nogc nothrow;
    extern __gshared int GC_parallel;
    int GC_get_parallel() @nogc nothrow;
    alias GC_oom_func = void* function(c_ulong);
    extern __gshared void* function(c_ulong) GC_oom_fn;
    void GC_set_oom_fn(void* function(c_ulong)) @nogc nothrow;
    void* function(c_ulong) GC_get_oom_fn() @nogc nothrow;
    alias GC_on_heap_resize_proc = void function(c_ulong);
    extern __gshared void function(c_ulong) GC_on_heap_resize;
    void GC_set_on_heap_resize(void function(c_ulong)) @nogc nothrow;
    void function(c_ulong) GC_get_on_heap_resize() @nogc nothrow;
    alias GC_EventType = _Anonymous_12;
    enum _Anonymous_12
    {
        GC_EVENT_START = 0,
        GC_EVENT_MARK_START = 1,
        GC_EVENT_MARK_END = 2,
        GC_EVENT_RECLAIM_START = 3,
        GC_EVENT_RECLAIM_END = 4,
        GC_EVENT_END = 5,
        GC_EVENT_PRE_STOP_WORLD = 6,
        GC_EVENT_POST_STOP_WORLD = 7,
        GC_EVENT_PRE_START_WORLD = 8,
        GC_EVENT_POST_START_WORLD = 9,
        GC_EVENT_THREAD_SUSPENDED = 10,
        GC_EVENT_THREAD_UNSUSPENDED = 11,
    }
    enum GC_EVENT_START = _Anonymous_12.GC_EVENT_START;
    enum GC_EVENT_MARK_START = _Anonymous_12.GC_EVENT_MARK_START;
    enum GC_EVENT_MARK_END = _Anonymous_12.GC_EVENT_MARK_END;
    enum GC_EVENT_RECLAIM_START = _Anonymous_12.GC_EVENT_RECLAIM_START;
    enum GC_EVENT_RECLAIM_END = _Anonymous_12.GC_EVENT_RECLAIM_END;
    enum GC_EVENT_END = _Anonymous_12.GC_EVENT_END;
    enum GC_EVENT_PRE_STOP_WORLD = _Anonymous_12.GC_EVENT_PRE_STOP_WORLD;
    enum GC_EVENT_POST_STOP_WORLD = _Anonymous_12.GC_EVENT_POST_STOP_WORLD;
    enum GC_EVENT_PRE_START_WORLD = _Anonymous_12.GC_EVENT_PRE_START_WORLD;
    enum GC_EVENT_POST_START_WORLD = _Anonymous_12.GC_EVENT_POST_START_WORLD;
    enum GC_EVENT_THREAD_SUSPENDED = _Anonymous_12.GC_EVENT_THREAD_SUSPENDED;
    enum GC_EVENT_THREAD_UNSUSPENDED = _Anonymous_12.GC_EVENT_THREAD_UNSUSPENDED;
    alias GC_on_collection_event_proc = void function(GC_EventType);
    void GC_set_on_collection_event(void function(GC_EventType)) @nogc nothrow;
    void function(GC_EventType) GC_get_on_collection_event() @nogc nothrow;
    alias GC_on_thread_event_proc = void function(GC_EventType, void*);
    void GC_set_on_thread_event(void function(GC_EventType, void*)) @nogc nothrow;
    void function(GC_EventType, void*) GC_get_on_thread_event() @nogc nothrow;
    extern __gshared int GC_find_leak;
    void GC_set_find_leak(int) @nogc nothrow;
    int GC_get_find_leak() @nogc nothrow;
    extern __gshared int GC_all_interior_pointers;
    void GC_set_all_interior_pointers(int) @nogc nothrow;
    int GC_get_all_interior_pointers() @nogc nothrow;
    extern __gshared int GC_finalize_on_demand;
    void GC_set_finalize_on_demand(int) @nogc nothrow;
    int GC_get_finalize_on_demand() @nogc nothrow;
    extern __gshared int GC_java_finalization;
    void GC_set_java_finalization(int) @nogc nothrow;
    int GC_get_java_finalization() @nogc nothrow;
    alias GC_finalizer_notifier_proc = void function();
    extern __gshared void function() GC_finalizer_notifier;
    void GC_set_finalizer_notifier(void function()) @nogc nothrow;
    void function() GC_get_finalizer_notifier() @nogc nothrow;
    extern __gshared int GC_dont_gc;
    extern __gshared int GC_dont_expand;
    void GC_set_dont_expand(int) @nogc nothrow;
    int GC_get_dont_expand() @nogc nothrow;
    extern __gshared int GC_use_entire_heap;
    extern __gshared int GC_full_freq;
    void GC_set_full_freq(int) @nogc nothrow;
    int GC_get_full_freq() @nogc nothrow;
    extern __gshared c_ulong GC_non_gc_bytes;
    void GC_set_non_gc_bytes(c_ulong) @nogc nothrow;
    c_ulong GC_get_non_gc_bytes() @nogc nothrow;
    extern __gshared int GC_no_dls;
    void GC_set_no_dls(int) @nogc nothrow;
    int GC_get_no_dls() @nogc nothrow;
    extern __gshared c_ulong GC_free_space_divisor;
    void GC_set_free_space_divisor(c_ulong) @nogc nothrow;
    c_ulong GC_get_free_space_divisor() @nogc nothrow;
    extern __gshared c_ulong GC_max_retries;
    void GC_set_max_retries(c_ulong) @nogc nothrow;
    c_ulong GC_get_max_retries() @nogc nothrow;
    extern __gshared char* GC_stackbottom;
    extern __gshared int GC_dont_precollect;
    void GC_set_dont_precollect(int) @nogc nothrow;
    int GC_get_dont_precollect() @nogc nothrow;
    extern __gshared c_ulong GC_time_limit;
    void GC_set_time_limit(c_ulong) @nogc nothrow;
    c_ulong GC_get_time_limit() @nogc nothrow;
    void GC_set_pages_executable(int) @nogc nothrow;
    int GC_get_pages_executable() @nogc nothrow;
    void GC_set_handle_fork(int) @nogc nothrow;
    void GC_atfork_prepare() @nogc nothrow;
    void GC_atfork_parent() @nogc nothrow;
    void GC_atfork_child() @nogc nothrow;
    void GC_init() @nogc nothrow;
    int GC_is_init_called() @nogc nothrow;
    void* GC_malloc(c_ulong) @nogc nothrow;
    void* GC_malloc_atomic(c_ulong) @nogc nothrow;
    char* GC_strdup(const(char)*) @nogc nothrow;
    char* GC_strndup(const(char)*, c_ulong) @nogc nothrow;
    void* GC_malloc_uncollectable(c_ulong) @nogc nothrow;
    void* GC_malloc_stubborn(c_ulong) @nogc nothrow;
    void* GC_memalign(c_ulong, c_ulong) @nogc nothrow;
    int GC_posix_memalign(void**, c_ulong, c_ulong) @nogc nothrow;
    void GC_free(void*) @nogc nothrow;
    void GC_change_stubborn(const(void)*) @nogc nothrow;
    void GC_end_stubborn_change(const(void)*) @nogc nothrow;
    void* GC_base(void*) @nogc nothrow;
    int GC_is_heap_ptr(const(void)*) @nogc nothrow;
    c_ulong GC_size(const(void)*) @nogc nothrow;
    void* GC_realloc(void*, c_ulong) @nogc nothrow;
    int GC_expand_hp(c_ulong) @nogc nothrow;
    void GC_set_max_heap_size(c_ulong) @nogc nothrow;
    void GC_exclude_static_roots(void*, void*) @nogc nothrow;
    void GC_clear_roots() @nogc nothrow;
    void GC_add_roots(void*, void*) @nogc nothrow;
    void GC_remove_roots(void*, void*) @nogc nothrow;
    void GC_register_displacement(c_ulong) @nogc nothrow;
    void GC_debug_register_displacement(c_ulong) @nogc nothrow;
    void GC_gcollect() @nogc nothrow;
    void GC_gcollect_and_unmap() @nogc nothrow;
    alias GC_stop_func = int function();
    int GC_try_to_collect(int function()) @nogc nothrow;
    void GC_set_stop_func(int function()) @nogc nothrow;
    int function() GC_get_stop_func() @nogc nothrow;
    c_ulong GC_get_heap_size() @nogc nothrow;
    c_ulong GC_get_free_bytes() @nogc nothrow;
    c_ulong GC_get_unmapped_bytes() @nogc nothrow;
    c_ulong GC_get_bytes_since_gc() @nogc nothrow;
    c_ulong GC_get_total_bytes() @nogc nothrow;
    void GC_get_heap_usage_safe(c_ulong*, c_ulong*, c_ulong*, c_ulong*, c_ulong*) @nogc nothrow;
    struct GC_prof_stats_s
    {
        c_ulong heapsize_full;
        c_ulong free_bytes_full;
        c_ulong unmapped_bytes;
        c_ulong bytes_allocd_since_gc;
        c_ulong allocd_bytes_before_gc;
        c_ulong non_gc_bytes;
        c_ulong gc_no;
        c_ulong markers_m1;
        c_ulong bytes_reclaimed_since_gc;
        c_ulong reclaimed_bytes_before_gc;
    }
    c_ulong GC_get_prof_stats(GC_prof_stats_s*, c_ulong) @nogc nothrow;
    c_ulong GC_get_prof_stats_unsafe(GC_prof_stats_s*, c_ulong) @nogc nothrow;
    c_ulong GC_get_memory_use() @nogc nothrow;
    pragma(mangle, "GC_disable") void GC_disable_() @nogc nothrow;
    int GC_is_disabled() @nogc nothrow;
    pragma(mangle, "GC_enable") void GC_enable_() @nogc nothrow;
    void GC_enable_incremental() @nogc nothrow;
    void __gmpz_ui_pow_ui(__mpz_struct*, c_ulong, c_ulong) @nogc nothrow;
    int GC_incremental_protection_needs() @nogc nothrow;
    int GC_collect_a_little() @nogc nothrow;
    void* GC_malloc_ignore_off_page(c_ulong) @nogc nothrow;
    void* GC_malloc_atomic_ignore_off_page(c_ulong) @nogc nothrow;
    int __gmpz_tstbit(const(__mpz_struct)*, c_ulong) @nogc nothrow;
    void* GC_malloc_atomic_uncollectable(c_ulong) @nogc nothrow;
    void* GC_debug_malloc_atomic_uncollectable(c_ulong, const(char)*, int) @nogc nothrow;
    void* GC_debug_malloc(c_ulong, const(char)*, int) @nogc nothrow;
    void* GC_debug_malloc_atomic(c_ulong, const(char)*, int) @nogc nothrow;
    char* GC_debug_strdup(const(char)*, const(char)*, int) @nogc nothrow;
    char* GC_debug_strndup(const(char)*, c_ulong, const(char)*, int) @nogc nothrow;
    void* GC_debug_malloc_uncollectable(c_ulong, const(char)*, int) @nogc nothrow;
    void* GC_debug_malloc_stubborn(c_ulong, const(char)*, int) @nogc nothrow;
    void* GC_debug_malloc_ignore_off_page(c_ulong, const(char)*, int) @nogc nothrow;
    void* GC_debug_malloc_atomic_ignore_off_page(c_ulong, const(char)*, int) @nogc nothrow;
    void GC_debug_free(void*) @nogc nothrow;
    void* GC_debug_realloc(void*, c_ulong, const(char)*, int) @nogc nothrow;
    void GC_debug_change_stubborn(const(void)*) @nogc nothrow;
    void GC_debug_end_stubborn_change(const(void)*) @nogc nothrow;
    void* GC_debug_malloc_replacement(c_ulong) @nogc nothrow;
    void* GC_debug_realloc_replacement(void*, c_ulong) @nogc nothrow;
    c_ulong __gmpz_tdiv_r_ui(__mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    void __gmpz_tdiv_r_2exp(__mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    void __gmpz_tdiv_r(__mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    c_ulong __gmpz_tdiv_qr_ui(__mpz_struct*, __mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    void __gmpz_tdiv_qr(__mpz_struct*, __mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    c_ulong __gmpz_tdiv_q_ui(__mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    void __gmpz_tdiv_q_2exp(__mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    void __gmpz_tdiv_q(__mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    alias GC_finalization_proc = void function(void*, void*);
    void GC_register_finalizer(void*, void function(void*, void*), void*, void function(void*, void*)*, void**) @nogc nothrow;
    void GC_debug_register_finalizer(void*, void function(void*, void*), void*, void function(void*, void*)*, void**) @nogc nothrow;
    void GC_register_finalizer_ignore_self(void*, void function(void*, void*), void*, void function(void*, void*)*, void**) @nogc nothrow;
    void GC_debug_register_finalizer_ignore_self(void*, void function(void*, void*), void*, void function(void*, void*)*, void**) @nogc nothrow;
    void GC_register_finalizer_no_order(void*, void function(void*, void*), void*, void function(void*, void*)*, void**) @nogc nothrow;
    void GC_debug_register_finalizer_no_order(void*, void function(void*, void*), void*, void function(void*, void*)*, void**) @nogc nothrow;
    void GC_register_finalizer_unreachable(void*, void function(void*, void*), void*, void function(void*, void*)*, void**) @nogc nothrow;
    void GC_debug_register_finalizer_unreachable(void*, void function(void*, void*), void*, void function(void*, void*)*, void**) @nogc nothrow;
    int GC_register_disappearing_link(void**) @nogc nothrow;
    int GC_general_register_disappearing_link(void**, const(void)*) @nogc nothrow;
    int GC_move_disappearing_link(void**, void**) @nogc nothrow;
    int GC_unregister_disappearing_link(void**) @nogc nothrow;
    int GC_register_long_link(void**, const(void)*) @nogc nothrow;
    int GC_move_long_link(void**, void**) @nogc nothrow;
    int GC_unregister_long_link(void**) @nogc nothrow;
    alias GC_ToggleRefStatus = _Anonymous_13;
    enum _Anonymous_13
    {
        GC_TOGGLE_REF_DROP = 0,
        GC_TOGGLE_REF_STRONG = 1,
        GC_TOGGLE_REF_WEAK = 2,
    }
    enum GC_TOGGLE_REF_DROP = _Anonymous_13.GC_TOGGLE_REF_DROP;
    enum GC_TOGGLE_REF_STRONG = _Anonymous_13.GC_TOGGLE_REF_STRONG;
    enum GC_TOGGLE_REF_WEAK = _Anonymous_13.GC_TOGGLE_REF_WEAK;
    alias GC_toggleref_func = GC_ToggleRefStatus function(void*);
    void GC_set_toggleref_func(GC_ToggleRefStatus function(void*)) @nogc nothrow;
    GC_ToggleRefStatus function(void*) GC_get_toggleref_func() @nogc nothrow;
    int GC_toggleref_add(void*, int) @nogc nothrow;
    alias GC_await_finalize_proc = void function(void*);
    void GC_set_await_finalize_proc(void function(void*)) @nogc nothrow;
    void function(void*) GC_get_await_finalize_proc() @nogc nothrow;
    int GC_should_invoke_finalizers() @nogc nothrow;
    int GC_invoke_finalizers() @nogc nothrow;
    alias GC_warn_proc = void function(char*, c_ulong);
    void GC_set_warn_proc(void function(char*, c_ulong)) @nogc nothrow;
    void function(char*, c_ulong) GC_get_warn_proc() @nogc nothrow;
    void GC_ignore_warn_proc(char*, c_ulong) @nogc nothrow;
    void GC_set_log_fd(int) @nogc nothrow;
    alias GC_abort_func = void function(const(char)*);
    void GC_set_abort_func(void function(const(char)*)) @nogc nothrow;
    void function(const(char)*) GC_get_abort_func() @nogc nothrow;
    alias GC_hidden_pointer = c_ulong;
    c_ulong __gmpz_tdiv_ui(const(__mpz_struct)*, c_ulong) @nogc nothrow;
    alias GC_fn_type = void* function(void*);
    void* GC_call_with_alloc_lock(void* function(void*), void*) @nogc nothrow;
    struct GC_stack_base
    {
        void* mem_base;
    }
    alias GC_stack_base_func = void* function(GC_stack_base*, void*);
    void* GC_call_with_stack_base(void* function(GC_stack_base*, void*), void*) @nogc nothrow;
    void __gmpz_swap(__mpz_struct*, __mpz_struct*) @nogc nothrow;
    void GC_set_suspend_signal(int) @nogc nothrow;
    void GC_set_thr_restart_signal(int) @nogc nothrow;
    int GC_get_suspend_signal() @nogc nothrow;
    int GC_get_thr_restart_signal() @nogc nothrow;
    void GC_start_mark_threads() @nogc nothrow;
    void GC_allow_register_threads() @nogc nothrow;
    int GC_register_my_thread(const(GC_stack_base)*) @nogc nothrow;
    int GC_thread_is_registered() @nogc nothrow;
    void GC_register_altstack(void*, c_ulong, void*, c_ulong) @nogc nothrow;
    int GC_unregister_my_thread() @nogc nothrow;
    void* GC_do_blocking(void* function(void*), void*) @nogc nothrow;
    void* GC_call_with_gc_active(void* function(void*), void*) @nogc nothrow;
    int GC_get_stack_base(GC_stack_base*) @nogc nothrow;
    void* GC_same_obj(void*, void*) @nogc nothrow;
    void* GC_pre_incr(void**, c_long) @nogc nothrow;
    void* GC_post_incr(void**, c_long) @nogc nothrow;
    void* GC_is_visible(void*) @nogc nothrow;
    void* GC_is_valid_displacement(void*) @nogc nothrow;
    void GC_dump() @nogc nothrow;
    void GC_dump_regions() @nogc nothrow;
    void GC_dump_finalization() @nogc nothrow;
    void __gmpz_submul_ui(__mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    void __gmpz_submul(__mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    extern __gshared void function(void*, void*) GC_same_obj_print_proc;
    extern __gshared void function(void*) GC_is_valid_displacement_print_proc;
    extern __gshared void function(void*) GC_is_visible_print_proc;
    void* GC_malloc_many(c_ulong) @nogc nothrow;
    alias GC_has_static_roots_func = int function(const(char)*, void*, c_ulong);
    void GC_register_has_static_roots_callback(int function(const(char)*, void*, c_ulong)) @nogc nothrow;
    void GC_set_force_unmap_on_gcollect(int) @nogc nothrow;
    int GC_get_force_unmap_on_gcollect() @nogc nothrow;
    void __gmpz_ui_sub(__mpz_struct*, c_ulong, const(__mpz_struct)*) @nogc nothrow;
    void __gmpz_sub_ui(__mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    void __gmpz_sub(__mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    void __gmpz_sqrtrem(__mpz_struct*, __mpz_struct*, const(__mpz_struct)*) @nogc nothrow;
    void __gmpz_sqrt(__mpz_struct*, const(__mpz_struct)*) @nogc nothrow;
    void GC_win32_free_heap() @nogc nothrow;
    c_ulong __gmpz_sizeinbase(const(__mpz_struct)*, int) @nogc nothrow;
    c_ulong __gmpz_size(const(__mpz_struct)*) @nogc nothrow;
    void __gmpz_setbit(__mpz_struct*, c_ulong) @nogc nothrow;
    void* GC_dlopen(const(char)*, int) @nogc nothrow;
    int GC_pthread_sigmask(int, const(__sigset_t)*, __sigset_t*) @nogc nothrow;
    void __gmpz_set_ui(__mpz_struct*, c_ulong) @nogc nothrow;
    int GC_pthread_create(c_ulong*, const(pthread_attr_t)*, void* function(void*), void*) @nogc nothrow;
    int GC_pthread_join(c_ulong, void**) @nogc nothrow;
    int GC_pthread_detach(c_ulong) @nogc nothrow;
    int GC_pthread_cancel(c_ulong) @nogc nothrow;
    void GC_pthread_exit(void*) @nogc nothrow;
    int __gmpz_set_str(__mpz_struct*, const(char)*, int) @nogc nothrow;
    void __gmpz_set_si(__mpz_struct*, c_long) @nogc nothrow;
    void __gmpz_set_q(__mpz_struct*, const(__mpq_struct)*) @nogc nothrow;
    void __gmpz_set_f(__mpz_struct*, const(__mpf_struct)*) @nogc nothrow;
    void __gmpz_set_d(__mpz_struct*, double) @nogc nothrow;
    void __gmpz_set(__mpz_struct*, const(__mpz_struct)*) @nogc nothrow;
    c_ulong __gmpz_scan1(const(__mpz_struct)*, c_ulong) @nogc nothrow;
    c_ulong __gmpz_scan0(const(__mpz_struct)*, c_ulong) @nogc nothrow;
    void __gmpz_rrandomb(__mpz_struct*, __gmp_randstate_struct*, c_ulong) @nogc nothrow;
    void __gmpz_rootrem(__mpz_struct*, __mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    enum _Anonymous_14
    {
        PTHREAD_CREATE_JOINABLE = 0,
        PTHREAD_CREATE_DETACHED = 1,
    }
    enum PTHREAD_CREATE_JOINABLE = _Anonymous_14.PTHREAD_CREATE_JOINABLE;
    enum PTHREAD_CREATE_DETACHED = _Anonymous_14.PTHREAD_CREATE_DETACHED;
    enum _Anonymous_15
    {
        PTHREAD_MUTEX_TIMED_NP = 0,
        PTHREAD_MUTEX_RECURSIVE_NP = 1,
        PTHREAD_MUTEX_ERRORCHECK_NP = 2,
        PTHREAD_MUTEX_ADAPTIVE_NP = 3,
        PTHREAD_MUTEX_NORMAL = 0,
        PTHREAD_MUTEX_RECURSIVE = 1,
        PTHREAD_MUTEX_ERRORCHECK = 2,
        PTHREAD_MUTEX_DEFAULT = 0,
    }
    enum PTHREAD_MUTEX_TIMED_NP = _Anonymous_15.PTHREAD_MUTEX_TIMED_NP;
    enum PTHREAD_MUTEX_RECURSIVE_NP = _Anonymous_15.PTHREAD_MUTEX_RECURSIVE_NP;
    enum PTHREAD_MUTEX_ERRORCHECK_NP = _Anonymous_15.PTHREAD_MUTEX_ERRORCHECK_NP;
    enum PTHREAD_MUTEX_ADAPTIVE_NP = _Anonymous_15.PTHREAD_MUTEX_ADAPTIVE_NP;
    enum PTHREAD_MUTEX_NORMAL = _Anonymous_15.PTHREAD_MUTEX_NORMAL;
    enum PTHREAD_MUTEX_RECURSIVE = _Anonymous_15.PTHREAD_MUTEX_RECURSIVE;
    enum PTHREAD_MUTEX_ERRORCHECK = _Anonymous_15.PTHREAD_MUTEX_ERRORCHECK;
    enum PTHREAD_MUTEX_DEFAULT = _Anonymous_15.PTHREAD_MUTEX_DEFAULT;
    enum _Anonymous_16
    {
        PTHREAD_MUTEX_STALLED = 0,
        PTHREAD_MUTEX_STALLED_NP = 0,
        PTHREAD_MUTEX_ROBUST = 1,
        PTHREAD_MUTEX_ROBUST_NP = 1,
    }
    enum PTHREAD_MUTEX_STALLED = _Anonymous_16.PTHREAD_MUTEX_STALLED;
    enum PTHREAD_MUTEX_STALLED_NP = _Anonymous_16.PTHREAD_MUTEX_STALLED_NP;
    enum PTHREAD_MUTEX_ROBUST = _Anonymous_16.PTHREAD_MUTEX_ROBUST;
    enum PTHREAD_MUTEX_ROBUST_NP = _Anonymous_16.PTHREAD_MUTEX_ROBUST_NP;
    enum _Anonymous_17
    {
        PTHREAD_PRIO_NONE = 0,
        PTHREAD_PRIO_INHERIT = 1,
        PTHREAD_PRIO_PROTECT = 2,
    }
    enum PTHREAD_PRIO_NONE = _Anonymous_17.PTHREAD_PRIO_NONE;
    enum PTHREAD_PRIO_INHERIT = _Anonymous_17.PTHREAD_PRIO_INHERIT;
    enum PTHREAD_PRIO_PROTECT = _Anonymous_17.PTHREAD_PRIO_PROTECT;
    int __gmpz_root(__mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    enum _Anonymous_18
    {
        PTHREAD_RWLOCK_PREFER_READER_NP = 0,
        PTHREAD_RWLOCK_PREFER_WRITER_NP = 1,
        PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP = 2,
        PTHREAD_RWLOCK_DEFAULT_NP = 0,
    }
    enum PTHREAD_RWLOCK_PREFER_READER_NP = _Anonymous_18.PTHREAD_RWLOCK_PREFER_READER_NP;
    enum PTHREAD_RWLOCK_PREFER_WRITER_NP = _Anonymous_18.PTHREAD_RWLOCK_PREFER_WRITER_NP;
    enum PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP = _Anonymous_18.PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP;
    enum PTHREAD_RWLOCK_DEFAULT_NP = _Anonymous_18.PTHREAD_RWLOCK_DEFAULT_NP;
    enum _Anonymous_19
    {
        PTHREAD_INHERIT_SCHED = 0,
        PTHREAD_EXPLICIT_SCHED = 1,
    }
    enum PTHREAD_INHERIT_SCHED = _Anonymous_19.PTHREAD_INHERIT_SCHED;
    enum PTHREAD_EXPLICIT_SCHED = _Anonymous_19.PTHREAD_EXPLICIT_SCHED;
    c_ulong __gmpz_remove(__mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    enum _Anonymous_20
    {
        PTHREAD_SCOPE_SYSTEM = 0,
        PTHREAD_SCOPE_PROCESS = 1,
    }
    enum PTHREAD_SCOPE_SYSTEM = _Anonymous_20.PTHREAD_SCOPE_SYSTEM;
    enum PTHREAD_SCOPE_PROCESS = _Anonymous_20.PTHREAD_SCOPE_PROCESS;
    enum _Anonymous_21
    {
        PTHREAD_PROCESS_PRIVATE = 0,
        PTHREAD_PROCESS_SHARED = 1,
    }
    enum PTHREAD_PROCESS_PRIVATE = _Anonymous_21.PTHREAD_PROCESS_PRIVATE;
    enum PTHREAD_PROCESS_SHARED = _Anonymous_21.PTHREAD_PROCESS_SHARED;
    void __gmpz_realloc2(__mpz_struct*, c_ulong) @nogc nothrow;
    struct _pthread_cleanup_buffer
    {
        void function(void*) __routine;
        void* __arg;
        int __canceltype;
        _pthread_cleanup_buffer* __prev;
    }
    enum _Anonymous_22
    {
        PTHREAD_CANCEL_ENABLE = 0,
        PTHREAD_CANCEL_DISABLE = 1,
    }
    enum PTHREAD_CANCEL_ENABLE = _Anonymous_22.PTHREAD_CANCEL_ENABLE;
    enum PTHREAD_CANCEL_DISABLE = _Anonymous_22.PTHREAD_CANCEL_DISABLE;
    void __gmpz_random2(__mpz_struct*, c_long) @nogc nothrow;
    enum _Anonymous_23
    {
        PTHREAD_CANCEL_DEFERRED = 0,
        PTHREAD_CANCEL_ASYNCHRONOUS = 1,
    }
    enum PTHREAD_CANCEL_DEFERRED = _Anonymous_23.PTHREAD_CANCEL_DEFERRED;
    enum PTHREAD_CANCEL_ASYNCHRONOUS = _Anonymous_23.PTHREAD_CANCEL_ASYNCHRONOUS;
    void __gmpz_random(__mpz_struct*, c_long) @nogc nothrow;
    int pthread_create(c_ulong*, const(pthread_attr_t)*, void* function(void*), void*) @nogc nothrow;
    void pthread_exit(void*) @nogc nothrow;
    int pthread_join(c_ulong, void**) @nogc nothrow;
    int pthread_detach(c_ulong) @nogc nothrow;
    c_ulong pthread_self() @nogc nothrow;
    int pthread_equal(c_ulong, c_ulong) @nogc nothrow;
    int pthread_attr_init(pthread_attr_t*) @nogc nothrow;
    int pthread_attr_destroy(pthread_attr_t*) @nogc nothrow;
    int pthread_attr_getdetachstate(const(pthread_attr_t)*, int*) @nogc nothrow;
    int pthread_attr_setdetachstate(pthread_attr_t*, int) @nogc nothrow;
    int pthread_attr_getguardsize(const(pthread_attr_t)*, c_ulong*) @nogc nothrow;
    int pthread_attr_setguardsize(pthread_attr_t*, c_ulong) @nogc nothrow;
    int pthread_attr_getschedparam(const(pthread_attr_t)*, sched_param*) @nogc nothrow;
    int pthread_attr_setschedparam(pthread_attr_t*, const(sched_param)*) @nogc nothrow;
    int pthread_attr_getschedpolicy(const(pthread_attr_t)*, int*) @nogc nothrow;
    int pthread_attr_setschedpolicy(pthread_attr_t*, int) @nogc nothrow;
    int pthread_attr_getinheritsched(const(pthread_attr_t)*, int*) @nogc nothrow;
    int pthread_attr_setinheritsched(pthread_attr_t*, int) @nogc nothrow;
    int pthread_attr_getscope(const(pthread_attr_t)*, int*) @nogc nothrow;
    int pthread_attr_setscope(pthread_attr_t*, int) @nogc nothrow;
    int pthread_attr_getstackaddr(const(pthread_attr_t)*, void**) @nogc nothrow;
    int pthread_attr_setstackaddr(pthread_attr_t*, void*) @nogc nothrow;
    int pthread_attr_getstacksize(const(pthread_attr_t)*, c_ulong*) @nogc nothrow;
    int pthread_attr_setstacksize(pthread_attr_t*, c_ulong) @nogc nothrow;
    int pthread_attr_getstack(const(pthread_attr_t)*, void**, c_ulong*) @nogc nothrow;
    int pthread_attr_setstack(pthread_attr_t*, void*, c_ulong) @nogc nothrow;
    int pthread_setschedparam(c_ulong, int, const(sched_param)*) @nogc nothrow;
    int pthread_getschedparam(c_ulong, int*, sched_param*) @nogc nothrow;
    int pthread_setschedprio(c_ulong, int) @nogc nothrow;
    int pthread_once(int*, void function()) @nogc nothrow;
    int pthread_setcancelstate(int, int*) @nogc nothrow;
    int pthread_setcanceltype(int, int*) @nogc nothrow;
    int pthread_cancel(c_ulong) @nogc nothrow;
    void pthread_testcancel() @nogc nothrow;
    struct __pthread_unwind_buf_t
    {
        static struct _Anonymous_24
        {
            c_long[8] __cancel_jmp_buf;
            int __mask_was_saved;
        }
        _Anonymous_24[1] __cancel_jmp_buf;
        void*[4] __pad;
    }
    int __gmpz_probab_prime_p(const(__mpz_struct)*, int) @nogc nothrow;
    struct __pthread_cleanup_frame
    {
        void function(void*) __cancel_routine;
        void* __cancel_arg;
        int __do_it;
        int __cancel_type;
    }
    void __pthread_register_cancel(__pthread_unwind_buf_t*) @nogc nothrow;
    void __pthread_unregister_cancel(__pthread_unwind_buf_t*) @nogc nothrow;
    void __pthread_unwind_next(__pthread_unwind_buf_t*) @nogc nothrow;
    int pthread_mutex_init(pthread_mutex_t*, const(pthread_mutexattr_t)*) @nogc nothrow;
    int pthread_mutex_destroy(pthread_mutex_t*) @nogc nothrow;
    int pthread_mutex_trylock(pthread_mutex_t*) @nogc nothrow;
    int pthread_mutex_lock(pthread_mutex_t*) @nogc nothrow;
    int pthread_mutex_timedlock(pthread_mutex_t*, const(timespec)*) @nogc nothrow;
    int pthread_mutex_unlock(pthread_mutex_t*) @nogc nothrow;
    int pthread_mutex_getprioceiling(const(pthread_mutex_t)*, int*) @nogc nothrow;
    int pthread_mutex_setprioceiling(pthread_mutex_t*, int, int*) @nogc nothrow;
    int pthread_mutex_consistent(pthread_mutex_t*) @nogc nothrow;
    int pthread_mutexattr_init(pthread_mutexattr_t*) @nogc nothrow;
    int pthread_mutexattr_destroy(pthread_mutexattr_t*) @nogc nothrow;
    int pthread_mutexattr_getpshared(const(pthread_mutexattr_t)*, int*) @nogc nothrow;
    int pthread_mutexattr_setpshared(pthread_mutexattr_t*, int) @nogc nothrow;
    int pthread_mutexattr_gettype(const(pthread_mutexattr_t)*, int*) @nogc nothrow;
    int pthread_mutexattr_settype(pthread_mutexattr_t*, int) @nogc nothrow;
    int pthread_mutexattr_getprotocol(const(pthread_mutexattr_t)*, int*) @nogc nothrow;
    int pthread_mutexattr_setprotocol(pthread_mutexattr_t*, int) @nogc nothrow;
    int pthread_mutexattr_getprioceiling(const(pthread_mutexattr_t)*, int*) @nogc nothrow;
    int pthread_mutexattr_setprioceiling(pthread_mutexattr_t*, int) @nogc nothrow;
    int pthread_mutexattr_getrobust(const(pthread_mutexattr_t)*, int*) @nogc nothrow;
    int pthread_mutexattr_setrobust(pthread_mutexattr_t*, int) @nogc nothrow;
    int pthread_rwlock_init(pthread_rwlock_t*, const(pthread_rwlockattr_t)*) @nogc nothrow;
    int pthread_rwlock_destroy(pthread_rwlock_t*) @nogc nothrow;
    int pthread_rwlock_rdlock(pthread_rwlock_t*) @nogc nothrow;
    int pthread_rwlock_tryrdlock(pthread_rwlock_t*) @nogc nothrow;
    int pthread_rwlock_timedrdlock(pthread_rwlock_t*, const(timespec)*) @nogc nothrow;
    int pthread_rwlock_wrlock(pthread_rwlock_t*) @nogc nothrow;
    int pthread_rwlock_trywrlock(pthread_rwlock_t*) @nogc nothrow;
    int pthread_rwlock_timedwrlock(pthread_rwlock_t*, const(timespec)*) @nogc nothrow;
    int pthread_rwlock_unlock(pthread_rwlock_t*) @nogc nothrow;
    int pthread_rwlockattr_init(pthread_rwlockattr_t*) @nogc nothrow;
    int pthread_rwlockattr_destroy(pthread_rwlockattr_t*) @nogc nothrow;
    int pthread_rwlockattr_getpshared(const(pthread_rwlockattr_t)*, int*) @nogc nothrow;
    int pthread_rwlockattr_setpshared(pthread_rwlockattr_t*, int) @nogc nothrow;
    int pthread_rwlockattr_getkind_np(const(pthread_rwlockattr_t)*, int*) @nogc nothrow;
    int pthread_rwlockattr_setkind_np(pthread_rwlockattr_t*, int) @nogc nothrow;
    int pthread_cond_init(pthread_cond_t*, const(pthread_condattr_t)*) @nogc nothrow;
    int pthread_cond_destroy(pthread_cond_t*) @nogc nothrow;
    int pthread_cond_signal(pthread_cond_t*) @nogc nothrow;
    int pthread_cond_broadcast(pthread_cond_t*) @nogc nothrow;
    int pthread_cond_wait(pthread_cond_t*, pthread_mutex_t*) @nogc nothrow;
    int pthread_cond_timedwait(pthread_cond_t*, pthread_mutex_t*, const(timespec)*) @nogc nothrow;
    int pthread_condattr_init(pthread_condattr_t*) @nogc nothrow;
    int pthread_condattr_destroy(pthread_condattr_t*) @nogc nothrow;
    int pthread_condattr_getpshared(const(pthread_condattr_t)*, int*) @nogc nothrow;
    int pthread_condattr_setpshared(pthread_condattr_t*, int) @nogc nothrow;
    int pthread_condattr_getclock(const(pthread_condattr_t)*, int*) @nogc nothrow;
    int pthread_condattr_setclock(pthread_condattr_t*, int) @nogc nothrow;
    int pthread_spin_init(int*, int) @nogc nothrow;
    int pthread_spin_destroy(int*) @nogc nothrow;
    int pthread_spin_lock(int*) @nogc nothrow;
    int pthread_spin_trylock(int*) @nogc nothrow;
    int pthread_spin_unlock(int*) @nogc nothrow;
    int pthread_barrier_init(pthread_barrier_t*, const(pthread_barrierattr_t)*, uint) @nogc nothrow;
    int pthread_barrier_destroy(pthread_barrier_t*) @nogc nothrow;
    int pthread_barrier_wait(pthread_barrier_t*) @nogc nothrow;
    int pthread_barrierattr_init(pthread_barrierattr_t*) @nogc nothrow;
    int pthread_barrierattr_destroy(pthread_barrierattr_t*) @nogc nothrow;
    int pthread_barrierattr_getpshared(const(pthread_barrierattr_t)*, int*) @nogc nothrow;
    int pthread_barrierattr_setpshared(pthread_barrierattr_t*, int) @nogc nothrow;
    int pthread_key_create(uint*, void function(void*)) @nogc nothrow;
    int pthread_key_delete(uint) @nogc nothrow;
    void* pthread_getspecific(uint) @nogc nothrow;
    int pthread_setspecific(uint, const(void)*) @nogc nothrow;
    int pthread_getcpuclockid(c_ulong, int*) @nogc nothrow;
    int pthread_atfork(void function(), void function(), void function()) @nogc nothrow;
    void __gmpz_powm_ui(__mpz_struct*, const(__mpz_struct)*, c_ulong, const(__mpz_struct)*) @nogc nothrow;
    void __gmpz_powm_sec(__mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    int sched_setparam(int, const(sched_param)*) @nogc nothrow;
    int sched_getparam(int, sched_param*) @nogc nothrow;
    int sched_setscheduler(int, int, const(sched_param)*) @nogc nothrow;
    int sched_getscheduler(int) @nogc nothrow;
    int sched_yield() @nogc nothrow;
    int sched_get_priority_max(int) @nogc nothrow;
    int sched_get_priority_min(int) @nogc nothrow;
    int sched_rr_get_interval(int, timespec*) @nogc nothrow;
    struct __jmp_buf_tag
    {
        c_long[8] __jmpbuf;
        int __mask_was_saved;
        __sigset_t __saved_mask;
    }
    alias jmp_buf = __jmp_buf_tag[1];
    pragma(mangle, "setjmp") int setjmp_(__jmp_buf_tag*) @nogc nothrow;
    int __sigsetjmp(__jmp_buf_tag*, int) @nogc nothrow;
    int _setjmp(__jmp_buf_tag*) @nogc nothrow;
    void __gmpz_powm(__mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    void longjmp(__jmp_buf_tag*, int) @nogc nothrow;
    void _longjmp(__jmp_buf_tag*, int) @nogc nothrow;
    alias sigjmp_buf = __jmp_buf_tag[1];
    void siglongjmp(__jmp_buf_tag*, int) @nogc nothrow;
    alias __sighandler_t = void function(int);
    void function(int) __sysv_signal(int, void function(int)) @nogc nothrow;
    void function(int) signal(int, void function(int)) @nogc nothrow;
    int kill(int, int) @nogc nothrow;
    int killpg(int, int) @nogc nothrow;
    int raise(int) @nogc nothrow;
    void function(int) ssignal(int, void function(int)) @nogc nothrow;
    int gsignal(int) @nogc nothrow;
    void psignal(int, const(char)*) @nogc nothrow;
    void psiginfo(const(siginfo_t)*, const(char)*) @nogc nothrow;
    void __gmpz_pow_ui(__mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    int sigblock(int) @nogc nothrow;
    int sigsetmask(int) @nogc nothrow;
    int siggetmask() @nogc nothrow;
    alias sig_t = void function(int);
    int sigemptyset(__sigset_t*) @nogc nothrow;
    int sigfillset(__sigset_t*) @nogc nothrow;
    int sigaddset(__sigset_t*, int) @nogc nothrow;
    int sigdelset(__sigset_t*, int) @nogc nothrow;
    int sigismember(const(__sigset_t)*, int) @nogc nothrow;
    int sigprocmask(int, const(__sigset_t)*, __sigset_t*) @nogc nothrow;
    int sigsuspend(const(__sigset_t)*) @nogc nothrow;
    pragma(mangle, "sigaction") int sigaction_(int, const(sigaction)*, sigaction*) @nogc nothrow;
    int sigpending(__sigset_t*) @nogc nothrow;
    int sigwait(const(__sigset_t)*, int*) @nogc nothrow;
    int sigwaitinfo(const(__sigset_t)*, siginfo_t*) @nogc nothrow;
    int sigtimedwait(const(__sigset_t)*, siginfo_t*, const(timespec)*) @nogc nothrow;
    int sigqueue(int, int, const(sigval)) @nogc nothrow;
    extern __gshared const(const(char)*)[65] _sys_siglist;
    extern __gshared const(const(char)*)[65] sys_siglist;
    int sigreturn(sigcontext*) @nogc nothrow;
    int siginterrupt(int, int) @nogc nothrow;
    int sigaltstack(const(stack_t)*, stack_t*) @nogc nothrow;
    pragma(mangle, "sigstack") int sigstack_(sigstack*, sigstack*) @nogc nothrow;
    int __libc_current_sigrtmin() @nogc nothrow;
    int __libc_current_sigrtmax() @nogc nothrow;
    c_ulong __gmpz_popcount(const(__mpz_struct)*) @nogc nothrow;
    int __gmpz_perfect_square_p(const(__mpz_struct)*) @nogc nothrow;
    alias int_least8_t = byte;
    alias int_least16_t = short;
    alias int_least32_t = int;
    alias int_least64_t = c_long;
    alias uint_least8_t = ubyte;
    alias uint_least16_t = ushort;
    alias uint_least32_t = uint;
    alias uint_least64_t = c_ulong;
    alias int_fast8_t = byte;
    alias int_fast16_t = c_long;
    alias int_fast32_t = c_long;
    alias int_fast64_t = c_long;
    alias uint_fast8_t = ubyte;
    alias uint_fast16_t = c_ulong;
    alias uint_fast32_t = c_ulong;
    alias uint_fast64_t = c_ulong;
    alias intptr_t = c_long;
    alias uintptr_t = c_ulong;
    alias intmax_t = c_long;
    alias uintmax_t = c_ulong;
    int __gmpz_perfect_power_p(const(__mpz_struct)*) @nogc nothrow;
    c_ulong __gmpz_out_str(_IO_FILE*, int, const(__mpz_struct)*) @nogc nothrow;
    c_ulong __gmpz_out_raw(_IO_FILE*, const(__mpz_struct)*) @nogc nothrow;
    void __gmpz_nextprime(__mpz_struct*, const(__mpz_struct)*) @nogc nothrow;
    void __gmpz_neg(__mpz_struct*, const(__mpz_struct)*) @nogc nothrow;
    void __gmpz_mul_ui(__mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    void __gmpz_mul_si(__mpz_struct*, const(__mpz_struct)*, c_long) @nogc nothrow;
    void __gmpz_mul_2exp(__mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    void __gmpz_mul(__mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    void __gmpz_mod(__mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    int __gmpz_millerrabin(const(__mpz_struct)*, int) @nogc nothrow;
    void __gmpz_lucnum2_ui(__mpz_struct*, __mpz_struct*, c_ulong) @nogc nothrow;
    void __gmpz_lucnum_ui(__mpz_struct*, c_ulong) @nogc nothrow;
    void __gmpz_lcm_ui(__mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    void __gmpz_lcm(__mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    int __gmpz_ui_kronecker(c_ulong, const(__mpz_struct)*) @nogc nothrow;
    int __gmpz_si_kronecker(c_long, const(__mpz_struct)*) @nogc nothrow;
    int __gmpz_kronecker_ui(const(__mpz_struct)*, c_ulong) @nogc nothrow;
    int __gmpz_kronecker_si(const(__mpz_struct)*, c_long) @nogc nothrow;
    alias fpos_t = _G_fpos_t;
    int __gmpz_jacobi(const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    void __gmpz_ior(__mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    int __gmpz_invert(__mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    extern __gshared _IO_FILE* stdin;
    extern __gshared _IO_FILE* stdout;
    extern __gshared _IO_FILE* stderr;
    c_ulong __gmpz_inp_str(__mpz_struct*, _IO_FILE*, int) @nogc nothrow;
    int remove(const(char)*) @nogc nothrow;
    int rename(const(char)*, const(char)*) @nogc nothrow;
    int renameat(int, const(char)*, int, const(char)*) @nogc nothrow;
    _IO_FILE* tmpfile() @nogc nothrow;
    char* tmpnam(char*) @nogc nothrow;
    char* tmpnam_r(char*) @nogc nothrow;
    char* tempnam(const(char)*, const(char)*) @nogc nothrow;
    int fclose(_IO_FILE*) @nogc nothrow;
    int fflush(_IO_FILE*) @nogc nothrow;
    int fflush_unlocked(_IO_FILE*) @nogc nothrow;
    _IO_FILE* fopen(const(char)*, const(char)*) @nogc nothrow;
    _IO_FILE* freopen(const(char)*, const(char)*, _IO_FILE*) @nogc nothrow;
    _IO_FILE* fdopen(int, const(char)*) @nogc nothrow;
    _IO_FILE* fmemopen(void*, c_ulong, const(char)*) @nogc nothrow;
    _IO_FILE* open_memstream(char**, c_ulong*) @nogc nothrow;
    void setbuf(_IO_FILE*, char*) @nogc nothrow;
    int setvbuf(_IO_FILE*, char*, int, c_ulong) @nogc nothrow;
    void setbuffer(_IO_FILE*, char*, c_ulong) @nogc nothrow;
    void setlinebuf(_IO_FILE*) @nogc nothrow;
    int fprintf(_IO_FILE*, const(char)*, ...) @nogc nothrow;
    int printf(const(char)*, ...) @nogc nothrow;
    int sprintf(char*, const(char)*, ...) @nogc nothrow;
    int vfprintf(_IO_FILE*, const(char)*, va_list*) @nogc nothrow;
    int vprintf(const(char)*, va_list*) @nogc nothrow;
    int vsprintf(char*, const(char)*, va_list*) @nogc nothrow;
    int snprintf(char*, c_ulong, const(char)*, ...) @nogc nothrow;
    int vsnprintf(char*, c_ulong, const(char)*, va_list*) @nogc nothrow;
    int vdprintf(int, const(char)*, va_list*) @nogc nothrow;
    int dprintf(int, const(char)*, ...) @nogc nothrow;
    int fscanf(_IO_FILE*, const(char)*, ...) @nogc nothrow;
    int scanf(const(char)*, ...) @nogc nothrow;
    int sscanf(const(char)*, const(char)*, ...) @nogc nothrow;
    int vfscanf(_IO_FILE*, const(char)*, va_list*) @nogc nothrow;
    int vscanf(const(char)*, va_list*) @nogc nothrow;
    int vsscanf(const(char)*, const(char)*, va_list*) @nogc nothrow;
    int fgetc(_IO_FILE*) @nogc nothrow;
    int getc(_IO_FILE*) @nogc nothrow;
    int getchar() @nogc nothrow;
    int getc_unlocked(_IO_FILE*) @nogc nothrow;
    int getchar_unlocked() @nogc nothrow;
    int fgetc_unlocked(_IO_FILE*) @nogc nothrow;
    int fputc(int, _IO_FILE*) @nogc nothrow;
    int putc(int, _IO_FILE*) @nogc nothrow;
    int putchar(int) @nogc nothrow;
    int putc_unlocked(int, _IO_FILE*) @nogc nothrow;
    int putchar_unlocked(int) @nogc nothrow;
    int getw(_IO_FILE*) @nogc nothrow;
    int putw(int, _IO_FILE*) @nogc nothrow;
    char* fgets(char*, int, _IO_FILE*) @nogc nothrow;
    c_long __getdelim(char**, c_ulong*, int, _IO_FILE*) @nogc nothrow;
    c_long getdelim(char**, c_ulong*, int, _IO_FILE*) @nogc nothrow;
    c_long getline(char**, c_ulong*, _IO_FILE*) @nogc nothrow;
    int fputs(const(char)*, _IO_FILE*) @nogc nothrow;
    int puts(const(char)*) @nogc nothrow;
    int ungetc(int, _IO_FILE*) @nogc nothrow;
    c_ulong fread(void*, c_ulong, c_ulong, _IO_FILE*) @nogc nothrow;
    c_ulong fread_unlocked(void*, c_ulong, c_ulong, _IO_FILE*) @nogc nothrow;
    c_ulong fwrite_unlocked(const(void)*, c_ulong, c_ulong, _IO_FILE*) @nogc nothrow;
    int fseek(_IO_FILE*, c_long, int) @nogc nothrow;
    c_long ftell(_IO_FILE*) @nogc nothrow;
    void rewind(_IO_FILE*) @nogc nothrow;
    int fseeko(_IO_FILE*, c_long, int) @nogc nothrow;
    c_long ftello(_IO_FILE*) @nogc nothrow;
    int fgetpos(_IO_FILE*, _G_fpos_t*) @nogc nothrow;
    int fsetpos(_IO_FILE*, const(_G_fpos_t)*) @nogc nothrow;
    void clearerr(_IO_FILE*) @nogc nothrow;
    int feof(_IO_FILE*) @nogc nothrow;
    int ferror(_IO_FILE*) @nogc nothrow;
    void clearerr_unlocked(_IO_FILE*) @nogc nothrow;
    int feof_unlocked(_IO_FILE*) @nogc nothrow;
    int ferror_unlocked(_IO_FILE*) @nogc nothrow;
    void perror(const(char)*) @nogc nothrow;
    int fileno(_IO_FILE*) @nogc nothrow;
    int fileno_unlocked(_IO_FILE*) @nogc nothrow;
    _IO_FILE* popen(const(char)*, const(char)*) @nogc nothrow;
    int pclose(_IO_FILE*) @nogc nothrow;
    char* ctermid(char*) @nogc nothrow;
    void flockfile(_IO_FILE*) @nogc nothrow;
    int ftrylockfile(_IO_FILE*) @nogc nothrow;
    void funlockfile(_IO_FILE*) @nogc nothrow;
    int __uflow(_IO_FILE*) @nogc nothrow;
    int __overflow(_IO_FILE*, int) @nogc nothrow;
    c_ulong __gmpz_inp_raw(__mpz_struct*, _IO_FILE*) @nogc nothrow;
    struct sigevent
    {
        sigval sigev_value;
        int sigev_signo;
        int sigev_notify;
        static union _Anonymous_25
        {
            int[12] _pad;
            int _tid;
            static struct _Anonymous_26
            {
                void function(sigval) _function;
                pthread_attr_t* _attribute;
            }
            _Anonymous_26 _sigev_thread;
        }
        _Anonymous_25 _sigev_un;
    }
    c_long clock() @nogc nothrow;
    c_long time(c_long*) @nogc nothrow;
    double difftime(c_long, c_long) @nogc nothrow;
    c_long mktime(tm*) @nogc nothrow;
    c_ulong strftime(char*, c_ulong, const(char)*, const(tm)*) @nogc nothrow;
    c_ulong strftime_l(char*, c_ulong, const(char)*, const(tm)*, __locale_struct*) @nogc nothrow;
    tm* gmtime(const(c_long)*) @nogc nothrow;
    tm* localtime(const(c_long)*) @nogc nothrow;
    tm* gmtime_r(const(c_long)*, tm*) @nogc nothrow;
    tm* localtime_r(const(c_long)*, tm*) @nogc nothrow;
    char* asctime(const(tm)*) @nogc nothrow;
    char* ctime(const(c_long)*) @nogc nothrow;
    char* asctime_r(const(tm)*, char*) @nogc nothrow;
    char* ctime_r(const(c_long)*, char*) @nogc nothrow;
    extern __gshared char*[2] __tzname;
    extern __gshared int __daylight;
    extern __gshared c_long __timezone;
    extern __gshared char*[2] tzname;
    void tzset() @nogc nothrow;
    extern __gshared int daylight;
    extern __gshared c_long timezone;
    int stime(const(c_long)*) @nogc nothrow;
    c_long timegm(tm*) @nogc nothrow;
    c_long timelocal(tm*) @nogc nothrow;
    int dysize(int) @nogc nothrow;
    int nanosleep(const(timespec)*, timespec*) @nogc nothrow;
    int clock_getres(int, timespec*) @nogc nothrow;
    int clock_gettime(int, timespec*) @nogc nothrow;
    int clock_settime(int, const(timespec)*) @nogc nothrow;
    int clock_nanosleep(int, int, const(timespec)*, timespec*) @nogc nothrow;
    int clock_getcpuclockid(int, int*) @nogc nothrow;
    int timer_create(int, sigevent*, void**) @nogc nothrow;
    int timer_delete(void*) @nogc nothrow;
    int timer_settime(void*, int, const(itimerspec)*, itimerspec*) @nogc nothrow;
    int timer_gettime(void*, itimerspec*) @nogc nothrow;
    int timer_getoverrun(void*) @nogc nothrow;
    int timespec_get(timespec*, int) @nogc nothrow;
    void __gmpz_init_set_ui(__mpz_struct*, c_ulong) @nogc nothrow;
    static ushort __bswap_16(ushort) @nogc nothrow;
    static uint __bswap_32(uint) @nogc nothrow;
    int __gmpz_init_set_str(__mpz_struct*, const(char)*, int) @nogc nothrow;
    static c_ulong __bswap_64(c_ulong) @nogc nothrow;
    void __gmpz_init_set_si(__mpz_struct*, c_long) @nogc nothrow;
    alias __cpu_mask = c_ulong;
    struct cpu_set_t
    {
        c_ulong[16] __bits;
    }
    void __gmpz_init_set_d(__mpz_struct*, double) @nogc nothrow;
    void __gmpz_init_set(__mpz_struct*, const(__mpz_struct)*) @nogc nothrow;
    void __gmpz_inits(__mpz_struct*, ...) @nogc nothrow;
    void __gmpz_init2(__mpz_struct*, c_ulong) @nogc nothrow;
    int __sched_cpucount(c_ulong, const(cpu_set_t)*) @nogc nothrow;
    cpu_set_t* __sched_cpualloc(c_ulong) @nogc nothrow;
    void __sched_cpufree(cpu_set_t*) @nogc nothrow;
    void __gmpz_init(__mpz_struct*) @nogc nothrow;
    void __gmpz_import(__mpz_struct*, c_ulong, int, c_ulong, int, c_ulong, const(void)*) @nogc nothrow;
    c_ulong __gmpz_hamdist(const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    c_ulong __gmpz_getlimbn(const(__mpz_struct)*, c_long) @nogc nothrow;
    c_ulong __gmpz_get_ui(const(__mpz_struct)*) @nogc nothrow;
    char* __gmpz_get_str(char*, int, const(__mpz_struct)*) @nogc nothrow;
    c_long __gmpz_get_si(const(__mpz_struct)*) @nogc nothrow;
    double __gmpz_get_d_2exp(c_long*, const(__mpz_struct)*) @nogc nothrow;
    double __gmpz_get_d(const(__mpz_struct)*) @nogc nothrow;
    void __gmpz_gcdext(__mpz_struct*, __mpz_struct*, __mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    c_ulong __gmpz_gcd_ui(__mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    void __gmpz_gcd(__mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    int __gmpz_fits_ushort_p(const(__mpz_struct)*) @nogc nothrow;
    int __gmpz_fits_ulong_p(const(__mpz_struct)*) @nogc nothrow;
    int __gmpz_fits_uint_p(const(__mpz_struct)*) @nogc nothrow;
    int __gmpz_fits_sshort_p(const(__mpz_struct)*) @nogc nothrow;
    int __gmpz_fits_slong_p(const(__mpz_struct)*) @nogc nothrow;
    int __gmpz_fits_sint_p(const(__mpz_struct)*) @nogc nothrow;
    void __gmpz_fib2_ui(__mpz_struct*, __mpz_struct*, c_ulong) @nogc nothrow;
    void __gmpz_fib_ui(__mpz_struct*, c_ulong) @nogc nothrow;
    c_ulong __gmpz_fdiv_ui(const(__mpz_struct)*, c_ulong) @nogc nothrow;
    c_ulong __gmpz_fdiv_r_ui(__mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    void __gmpz_fdiv_r_2exp(__mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    void __gmpz_fdiv_r(__mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    c_ulong __gmpz_fdiv_qr_ui(__mpz_struct*, __mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    void __gmpz_fdiv_qr(__mpz_struct*, __mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    struct __pthread_rwlock_arch_t
    {
        uint __readers;
        uint __writers;
        uint __wrphase_futex;
        uint __writers_futex;
        uint __pad3;
        uint __pad4;
        int __cur_writer;
        int __shared;
        byte __rwelision;
        ubyte[7] __pad1;
        c_ulong __pad2;
        uint __flags;
    }
    c_ulong __gmpz_fdiv_q_ui(__mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    alias pthread_t = c_ulong;
    union pthread_mutexattr_t
    {
        char[4] __size;
        int __align;
    }
    union pthread_condattr_t
    {
        char[4] __size;
        int __align;
    }
    alias pthread_key_t = uint;
    alias pthread_once_t = int;
    union pthread_attr_t
    {
        char[56] __size;
        c_long __align;
    }
    void __gmpz_fdiv_q_2exp(__mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    union pthread_mutex_t
    {
        __pthread_mutex_s __data;
        char[40] __size;
        c_long __align;
    }
    union pthread_cond_t
    {
        __pthread_cond_s __data;
        char[48] __size;
        long __align;
    }
    union pthread_rwlock_t
    {
        __pthread_rwlock_arch_t __data;
        char[56] __size;
        c_long __align;
    }
    union pthread_rwlockattr_t
    {
        char[8] __size;
        c_long __align;
    }
    alias pthread_spinlock_t = int;
    union pthread_barrier_t
    {
        char[32] __size;
        c_long __align;
    }
    union pthread_barrierattr_t
    {
        char[4] __size;
        int __align;
    }
    void __gmpz_fdiv_q(__mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    void __gmpz_primorial_ui(__mpz_struct*, c_ulong) @nogc nothrow;
    void __gmpz_mfac_uiui(__mpz_struct*, c_ulong, c_ulong) @nogc nothrow;
    alias __jmp_buf = c_long[8];
    struct sigaction
    {
        static union _Anonymous_27
        {
            void function(int) sa_handler;
            void function(int, siginfo_t*, void*) sa_sigaction;
        }
        _Anonymous_27 __sigaction_handler;
        __sigset_t sa_mask;
        int sa_flags;
        void function() sa_restorer;
    }
    void __gmpz_2fac_ui(__mpz_struct*, c_ulong) @nogc nothrow;
    void __gmpz_fac_ui(__mpz_struct*, c_ulong) @nogc nothrow;
    void* __gmpz_export(void*, c_ulong*, int, c_ulong, int, c_ulong, const(__mpz_struct)*) @nogc nothrow;
    void __gmpz_dump(const(__mpz_struct)*) @nogc nothrow;
    int __gmpz_divisible_2exp_p(const(__mpz_struct)*, c_ulong) @nogc nothrow;
    int __gmpz_divisible_ui_p(const(__mpz_struct)*, c_ulong) @nogc nothrow;
    struct _fpx_sw_bytes
    {
        uint magic1;
        uint extended_size;
        c_ulong xstate_bv;
        uint xstate_size;
        uint[7] __glibc_reserved1;
    }
    struct _fpreg
    {
        ushort[4] significand;
        ushort exponent;
    }
    struct _fpxreg
    {
        ushort[4] significand;
        ushort exponent;
        ushort[3] __glibc_reserved1;
    }
    struct _xmmreg
    {
        uint[4] element;
    }
    struct _fpstate
    {
        ushort cwd;
        ushort swd;
        ushort ftw;
        ushort fop;
        c_ulong rip;
        c_ulong rdp;
        uint mxcsr;
        uint mxcr_mask;
        _fpxreg[8] _st;
        _xmmreg[16] _xmm;
        uint[24] __glibc_reserved1;
    }
    struct sigcontext
    {
        c_ulong r8;
        c_ulong r9;
        c_ulong r10;
        c_ulong r11;
        c_ulong r12;
        c_ulong r13;
        c_ulong r14;
        c_ulong r15;
        c_ulong rdi;
        c_ulong rsi;
        c_ulong rbp;
        c_ulong rbx;
        c_ulong rdx;
        c_ulong rax;
        c_ulong rcx;
        c_ulong rsp;
        c_ulong rip;
        c_ulong eflags;
        ushort cs;
        ushort gs;
        ushort fs;
        ushort __pad0;
        c_ulong err;
        c_ulong trapno;
        c_ulong oldmask;
        c_ulong cr2;
        static union _Anonymous_28
        {
            _fpstate* fpstate;
            c_ulong __fpstate_word;
        }
        _Anonymous_28 _anonymous_29;
        auto fpstate() @property @nogc pure nothrow { return _anonymous_29.fpstate; }
        void fpstate(_T_)(auto ref _T_ val) @property @nogc pure nothrow { _anonymous_29.fpstate = val; }
        auto __fpstate_word() @property @nogc pure nothrow { return _anonymous_29.__fpstate_word; }
        void __fpstate_word(_T_)(auto ref _T_ val) @property @nogc pure nothrow { _anonymous_29.__fpstate_word = val; }
        c_ulong[8] __reserved1;
    }
    struct _xsave_hdr
    {
        c_ulong xstate_bv;
        c_ulong[2] __glibc_reserved1;
        c_ulong[5] __glibc_reserved2;
    }
    struct _ymmh_state
    {
        uint[64] ymmh_space;
    }
    struct _xstate
    {
        _fpstate fpstate;
        _xsave_hdr xstate_hdr;
        _ymmh_state ymmh;
    }
    enum _Anonymous_30
    {
        SIGEV_SIGNAL = 0,
        SIGEV_NONE = 1,
        SIGEV_THREAD = 2,
        SIGEV_THREAD_ID = 4,
    }
    enum SIGEV_SIGNAL = _Anonymous_30.SIGEV_SIGNAL;
    enum SIGEV_NONE = _Anonymous_30.SIGEV_NONE;
    enum SIGEV_THREAD = _Anonymous_30.SIGEV_THREAD;
    enum SIGEV_THREAD_ID = _Anonymous_30.SIGEV_THREAD_ID;
    int __gmpz_divisible_p(const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    void __gmpz_divexact_ui(__mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    enum _Anonymous_31
    {
        SI_ASYNCNL = -60,
        SI_TKILL = -6,
        SI_SIGIO = -5,
        SI_ASYNCIO = -4,
        SI_MESGQ = -3,
        SI_TIMER = -2,
        SI_QUEUE = -1,
        SI_USER = 0,
        SI_KERNEL = 128,
    }
    enum SI_ASYNCNL = _Anonymous_31.SI_ASYNCNL;
    enum SI_TKILL = _Anonymous_31.SI_TKILL;
    enum SI_SIGIO = _Anonymous_31.SI_SIGIO;
    enum SI_ASYNCIO = _Anonymous_31.SI_ASYNCIO;
    enum SI_MESGQ = _Anonymous_31.SI_MESGQ;
    enum SI_TIMER = _Anonymous_31.SI_TIMER;
    enum SI_QUEUE = _Anonymous_31.SI_QUEUE;
    enum SI_USER = _Anonymous_31.SI_USER;
    enum SI_KERNEL = _Anonymous_31.SI_KERNEL;
    void __gmpz_divexact(__mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    int __gmpz_congruent_ui_p(const(__mpz_struct)*, c_ulong, c_ulong) @nogc nothrow;
    int __gmpz_congruent_2exp_p(const(__mpz_struct)*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    enum _Anonymous_32
    {
        ILL_ILLOPC = 1,
        ILL_ILLOPN = 2,
        ILL_ILLADR = 3,
        ILL_ILLTRP = 4,
        ILL_PRVOPC = 5,
        ILL_PRVREG = 6,
        ILL_COPROC = 7,
        ILL_BADSTK = 8,
    }
    enum ILL_ILLOPC = _Anonymous_32.ILL_ILLOPC;
    enum ILL_ILLOPN = _Anonymous_32.ILL_ILLOPN;
    enum ILL_ILLADR = _Anonymous_32.ILL_ILLADR;
    enum ILL_ILLTRP = _Anonymous_32.ILL_ILLTRP;
    enum ILL_PRVOPC = _Anonymous_32.ILL_PRVOPC;
    enum ILL_PRVREG = _Anonymous_32.ILL_PRVREG;
    enum ILL_COPROC = _Anonymous_32.ILL_COPROC;
    enum ILL_BADSTK = _Anonymous_32.ILL_BADSTK;
    int __gmpz_congruent_p(const(__mpz_struct)*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    void __gmpz_combit(__mpz_struct*, c_ulong) @nogc nothrow;
    enum _Anonymous_33
    {
        FPE_INTDIV = 1,
        FPE_INTOVF = 2,
        FPE_FLTDIV = 3,
        FPE_FLTOVF = 4,
        FPE_FLTUND = 5,
        FPE_FLTRES = 6,
        FPE_FLTINV = 7,
        FPE_FLTSUB = 8,
    }
    enum FPE_INTDIV = _Anonymous_33.FPE_INTDIV;
    enum FPE_INTOVF = _Anonymous_33.FPE_INTOVF;
    enum FPE_FLTDIV = _Anonymous_33.FPE_FLTDIV;
    enum FPE_FLTOVF = _Anonymous_33.FPE_FLTOVF;
    enum FPE_FLTUND = _Anonymous_33.FPE_FLTUND;
    enum FPE_FLTRES = _Anonymous_33.FPE_FLTRES;
    enum FPE_FLTINV = _Anonymous_33.FPE_FLTINV;
    enum FPE_FLTSUB = _Anonymous_33.FPE_FLTSUB;
    void __gmpz_com(__mpz_struct*, const(__mpz_struct)*) @nogc nothrow;
    int __gmpz_cmpabs_ui(const(__mpz_struct)*, c_ulong) @nogc nothrow;
    enum _Anonymous_34
    {
        SEGV_MAPERR = 1,
        SEGV_ACCERR = 2,
        SEGV_BNDERR = 3,
        SEGV_PKUERR = 4,
    }
    enum SEGV_MAPERR = _Anonymous_34.SEGV_MAPERR;
    enum SEGV_ACCERR = _Anonymous_34.SEGV_ACCERR;
    enum SEGV_BNDERR = _Anonymous_34.SEGV_BNDERR;
    enum SEGV_PKUERR = _Anonymous_34.SEGV_PKUERR;
    int __gmpz_cmpabs_d(const(__mpz_struct)*, double) @nogc nothrow;
    enum _Anonymous_35
    {
        BUS_ADRALN = 1,
        BUS_ADRERR = 2,
        BUS_OBJERR = 3,
        BUS_MCEERR_AR = 4,
        BUS_MCEERR_AO = 5,
    }
    enum BUS_ADRALN = _Anonymous_35.BUS_ADRALN;
    enum BUS_ADRERR = _Anonymous_35.BUS_ADRERR;
    enum BUS_OBJERR = _Anonymous_35.BUS_OBJERR;
    enum BUS_MCEERR_AR = _Anonymous_35.BUS_MCEERR_AR;
    enum BUS_MCEERR_AO = _Anonymous_35.BUS_MCEERR_AO;
    int __gmpz_cmpabs(const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    enum _Anonymous_36
    {
        CLD_EXITED = 1,
        CLD_KILLED = 2,
        CLD_DUMPED = 3,
        CLD_TRAPPED = 4,
        CLD_STOPPED = 5,
        CLD_CONTINUED = 6,
    }
    enum CLD_EXITED = _Anonymous_36.CLD_EXITED;
    enum CLD_KILLED = _Anonymous_36.CLD_KILLED;
    enum CLD_DUMPED = _Anonymous_36.CLD_DUMPED;
    enum CLD_TRAPPED = _Anonymous_36.CLD_TRAPPED;
    enum CLD_STOPPED = _Anonymous_36.CLD_STOPPED;
    enum CLD_CONTINUED = _Anonymous_36.CLD_CONTINUED;
    int __gmpz_cmp_ui(const(__mpz_struct)*, c_ulong) @nogc nothrow;
    enum _Anonymous_37
    {
        POLL_IN = 1,
        POLL_OUT = 2,
        POLL_MSG = 3,
        POLL_ERR = 4,
        POLL_PRI = 5,
        POLL_HUP = 6,
    }
    enum POLL_IN = _Anonymous_37.POLL_IN;
    enum POLL_OUT = _Anonymous_37.POLL_OUT;
    enum POLL_MSG = _Anonymous_37.POLL_MSG;
    enum POLL_ERR = _Anonymous_37.POLL_ERR;
    enum POLL_PRI = _Anonymous_37.POLL_PRI;
    enum POLL_HUP = _Anonymous_37.POLL_HUP;
    int __gmpz_cmp_si(const(__mpz_struct)*, c_long) @nogc nothrow;
    int __gmpz_cmp_d(const(__mpz_struct)*, double) @nogc nothrow;
    int __gmpz_cmp(const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    void __gmpz_clrbit(__mpz_struct*, c_ulong) @nogc nothrow;
    void __gmpz_clears(__mpz_struct*, ...) @nogc nothrow;
    void __gmpz_clear(__mpz_struct*) @nogc nothrow;
    c_ulong __gmpz_cdiv_ui(const(__mpz_struct)*, c_ulong) @nogc nothrow;
    c_ulong __gmpz_cdiv_r_ui(__mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    void __gmpz_cdiv_r_2exp(__mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    void __gmpz_cdiv_r(__mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    c_ulong __gmpz_cdiv_qr_ui(__mpz_struct*, __mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    void __gmpz_cdiv_qr(__mpz_struct*, __mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    c_ulong __gmpz_cdiv_q_ui(__mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    void __gmpz_cdiv_q_2exp(__mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    void __gmpz_cdiv_q(__mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    void __gmpz_bin_uiui(__mpz_struct*, c_ulong, c_ulong) @nogc nothrow;
    void __gmpz_bin_ui(__mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    void __gmpz_array_init(__mpz_struct*, c_long, c_long) @nogc nothrow;
    void __gmpz_and(__mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    int pthread_sigmask(int, const(__sigset_t)*, __sigset_t*) @nogc nothrow;
    int pthread_kill(c_ulong, int) @nogc nothrow;
    void __gmpz_addmul_ui(__mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    enum _Anonymous_38
    {
        SS_ONSTACK = 1,
        SS_DISABLE = 2,
    }
    enum SS_ONSTACK = _Anonymous_38.SS_ONSTACK;
    enum SS_DISABLE = _Anonymous_38.SS_DISABLE;
    void __gmpz_addmul(__mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    alias int8_t = byte;
    alias int16_t = short;
    alias int32_t = int;
    alias int64_t = c_long;
    alias uint8_t = ubyte;
    alias uint16_t = ushort;
    alias uint32_t = uint;
    alias uint64_t = ulong;
    void __gmpz_add_ui(__mpz_struct*, const(__mpz_struct)*, c_ulong) @nogc nothrow;
    void __gmpz_add(__mpz_struct*, const(__mpz_struct)*, const(__mpz_struct)*) @nogc nothrow;
    extern __gshared int sys_nerr;
    extern __gshared const(const(char)*)[0] sys_errlist;
    alias __pthread_list_t = __pthread_internal_list;
    struct __pthread_internal_list
    {
        __pthread_internal_list* __prev;
        __pthread_internal_list* __next;
    }
    void __gmpz_abs(__mpz_struct*, const(__mpz_struct)*) @nogc nothrow;
    struct __pthread_mutex_s
    {
        int __lock;
        uint __count;
        int __owner;
        uint __nusers;
        int __kind;
        short __spins;
        short __elision;
        __pthread_internal_list __list;
    }
    struct __pthread_cond_s
    {
        static union _Anonymous_39
        {
            ulong __wseq;
            static struct _Anonymous_40
            {
                uint __low;
                uint __high;
            }
            _Anonymous_40 __wseq32;
        }
        _Anonymous_39 _anonymous_41;
        auto __wseq() @property @nogc pure nothrow { return _anonymous_41.__wseq; }
        void __wseq(_T_)(auto ref _T_ val) @property @nogc pure nothrow { _anonymous_41.__wseq = val; }
        auto __wseq32() @property @nogc pure nothrow { return _anonymous_41.__wseq32; }
        void __wseq32(_T_)(auto ref _T_ val) @property @nogc pure nothrow { _anonymous_41.__wseq32 = val; }
        static union _Anonymous_42
        {
            ulong __g1_start;
            static struct _Anonymous_43
            {
                uint __low;
                uint __high;
            }
            _Anonymous_43 __g1_start32;
        }
        _Anonymous_42 _anonymous_44;
        auto __g1_start() @property @nogc pure nothrow { return _anonymous_44.__g1_start; }
        void __g1_start(_T_)(auto ref _T_ val) @property @nogc pure nothrow { _anonymous_44.__g1_start = val; }
        auto __g1_start32() @property @nogc pure nothrow { return _anonymous_44.__g1_start32; }
        void __g1_start32(_T_)(auto ref _T_ val) @property @nogc pure nothrow { _anonymous_44.__g1_start32 = val; }
        uint[2] __g_refs;
        uint[2] __g_size;
        uint __g1_orig_size;
        uint __wrefs;
        uint[2] __g_signals;
    }
    void* __gmpz_realloc(__mpz_struct*, c_long) @nogc nothrow;
    int __gmp_vsscanf(const(char)*, const(char)*, va_list*) @nogc nothrow;
    int __gmp_vscanf(const(char)*, va_list*) @nogc nothrow;
    int __gmp_vfscanf(_IO_FILE*, const(char)*, va_list*) @nogc nothrow;
    alias __u_char = ubyte;
    alias __u_short = ushort;
    alias __u_int = uint;
    alias __u_long = c_ulong;
    alias __int8_t = byte;
    alias __uint8_t = ubyte;
    alias __int16_t = short;
    alias __uint16_t = ushort;
    alias __int32_t = int;
    alias __uint32_t = uint;
    alias __int64_t = c_long;
    alias __uint64_t = c_ulong;
    alias __int_least8_t = byte;
    alias __uint_least8_t = ubyte;
    alias __int_least16_t = short;
    alias __uint_least16_t = ushort;
    alias __int_least32_t = int;
    alias __uint_least32_t = uint;
    alias __int_least64_t = c_long;
    alias __uint_least64_t = c_ulong;
    alias __quad_t = c_long;
    alias __u_quad_t = c_ulong;
    alias __intmax_t = c_long;
    alias __uintmax_t = c_ulong;
    int __gmp_sscanf(const(char)*, const(char)*, ...) @nogc nothrow;
    int __gmp_scanf(const(char)*, ...) @nogc nothrow;
    int __gmp_fscanf(_IO_FILE*, const(char)*, ...) @nogc nothrow;
    int __gmp_vsprintf(char*, const(char)*, va_list*) @nogc nothrow;
    alias __dev_t = c_ulong;
    alias __uid_t = uint;
    alias __gid_t = uint;
    alias __ino_t = c_ulong;
    alias __ino64_t = c_ulong;
    alias __mode_t = uint;
    alias __nlink_t = c_ulong;
    alias __off_t = c_long;
    alias __off64_t = c_long;
    alias __pid_t = int;
    struct __fsid_t
    {
        int[2] __val;
    }
    alias __clock_t = c_long;
    alias __rlim_t = c_ulong;
    alias __rlim64_t = c_ulong;
    alias __id_t = uint;
    alias __time_t = c_long;
    alias __useconds_t = uint;
    alias __suseconds_t = c_long;
    alias __daddr_t = int;
    alias __key_t = int;
    alias __clockid_t = int;
    alias __timer_t = void*;
    alias __blksize_t = c_long;
    alias __blkcnt_t = c_long;
    alias __blkcnt64_t = c_long;
    alias __fsblkcnt_t = c_ulong;
    alias __fsblkcnt64_t = c_ulong;
    alias __fsfilcnt_t = c_ulong;
    alias __fsfilcnt64_t = c_ulong;
    alias __fsword_t = c_long;
    alias __ssize_t = c_long;
    alias __syscall_slong_t = c_long;
    alias __syscall_ulong_t = c_ulong;
    alias __loff_t = c_long;
    alias __caddr_t = char*;
    alias __intptr_t = c_long;
    alias __socklen_t = uint;
    alias __sig_atomic_t = int;
    alias FILE = _IO_FILE;
    int __gmp_vsnprintf(char*, c_ulong, const(char)*, va_list*) @nogc nothrow;
    struct _IO_FILE {
        int _flags;
        char* _IO_read_ptr;
        char* _IO_read_end;
        char* _IO_read_base;
        char* _IO_write_base;
        char* _IO_write_ptr;
        char* _IO_write_end;
        char* _IO_buf_base;
        char* _IO_buf_end;
        char* _IO_save_base;
        char* _IO_backup_base;
        char* _IO_save_end;
        _IO_marker* _markers;
        _IO_FILE* _chain;
        int _fileno;
        int _flags2;
        c_long _old_offset;
        ushort _cur_column;
        byte _vtable_offset;
        char[1] _shortbuf;
        void* _lock;
        c_long _offset;
        _IO_codecvt* _codecvt;
        _IO_wide_data* _wide_data;
        _IO_FILE* _freeres_list;
        void* _freeres_buf;
        c_ulong __pad5;
        int _mode;
        char[20] _unused2;
    }
    alias __FILE = _IO_FILE;
    alias __fpos64_t = _G_fpos64_t;
    struct _G_fpos64_t
    {
        c_long __pos;
        __mbstate_t __state;
    }
    alias __fpos_t = _G_fpos_t;
    struct _G_fpos_t
    {
        c_long __pos;
        __mbstate_t __state;
    }
    struct __locale_struct
    {
        __locale_data*[13] __locales;
        const(ushort)* __ctype_b;
        const(int)* __ctype_tolower;
        const(int)* __ctype_toupper;
        const(char)*[13] __names;
    }
    alias __locale_t = __locale_struct*;
    int __gmp_vprintf(const(char)*, va_list*) @nogc nothrow;
    struct __mbstate_t
    {
        int __count;
        static union _Anonymous_45
        {
            uint __wch;
            char[4] __wchb;
        }
        _Anonymous_45 __value;
    }
    struct __sigset_t
    {
        c_ulong[16] __val;
    }
    union sigval
    {
        int sival_int;
        void* sival_ptr;
    }
    alias __sigval_t = sigval;
    int __gmp_vfprintf(_IO_FILE*, const(char)*, va_list*) @nogc nothrow;
    alias clock_t = c_long;
    alias clockid_t = int;
    alias locale_t = __locale_struct*;
    alias sig_atomic_t = int;
    int __gmp_vasprintf(char**, const(char)*, va_list*) @nogc nothrow;
    alias sigevent_t = sigevent;
    int __gmp_sprintf(char*, const(char)*, ...) @nogc nothrow;
    int __gmp_snprintf(char*, c_ulong, const(char)*, ...) @nogc nothrow;
    int __gmp_printf(const(char)*, ...) @nogc nothrow;
    struct siginfo_t
    {
        int si_signo;
        int si_errno;
        int si_code;
        int __pad0;
        static union _Anonymous_46
        {
            int[28] _pad;
            static struct _Anonymous_47
            {
                int si_pid;
                uint si_uid;
            }
            _Anonymous_47 _kill;
            static struct _Anonymous_48
            {
                int si_tid;
                int si_overrun;
                sigval si_sigval;
            }
            _Anonymous_48 _timer;
            static struct _Anonymous_49
            {
                int si_pid;
                uint si_uid;
                sigval si_sigval;
            }
            _Anonymous_49 _rt;
            static struct _Anonymous_50
            {
                int si_pid;
                uint si_uid;
                int si_status;
                c_long si_utime;
                c_long si_stime;
            }
            _Anonymous_50 _sigchld;
            static struct _Anonymous_51
            {
                void* si_addr;
                short si_addr_lsb;
                static union _Anonymous_52
                {
                    static struct _Anonymous_53
                    {
                        void* _lower;
                        void* _upper;
                    }
                    _Anonymous_53 _addr_bnd;
                    uint _pkey;
                }
                _Anonymous_52 _bounds;
            }
            _Anonymous_51 _sigfault;
            static struct _Anonymous_54
            {
                c_long si_band;
                int si_fd;
            }
            _Anonymous_54 _sigpoll;
            static struct _Anonymous_55
            {
                void* _call_addr;
                int _syscall;
                uint _arch;
            }
            _Anonymous_55 _sigsys;
        }
        _Anonymous_46 _sifields;
    }
    int __gmp_fprintf(_IO_FILE*, const(char)*, ...) @nogc nothrow;
    int __gmp_asprintf(char**, const(char)*, ...) @nogc nothrow;
    c_ulong __gmp_urandomm_ui(__gmp_randstate_struct*, c_ulong) @nogc nothrow;
    c_ulong __gmp_urandomb_ui(__gmp_randstate_struct*, c_ulong) @nogc nothrow;
    void __gmp_randclear(__gmp_randstate_struct*) @nogc nothrow;
    void __gmp_randseed_ui(__gmp_randstate_struct*, c_ulong) @nogc nothrow;
    void __gmp_randseed(__gmp_randstate_struct*, const(__mpz_struct)*) @nogc nothrow;
    alias sigset_t = __sigset_t;
    alias sigval_t = sigval;
    void __gmp_randinit_set(__gmp_randstate_struct*, const(__gmp_randstate_struct)*) @nogc nothrow;
    struct stack_t
    {
        void* ss_sp;
        int ss_flags;
        c_ulong ss_size;
    }
    struct _IO_marker;
    struct _IO_codecvt;
    struct _IO_wide_data;
    alias _IO_lock_t = void;
    void __gmp_randinit_mt(__gmp_randstate_struct*) @nogc nothrow;
    int __gmp_randinit_lc_2exp_size(__gmp_randstate_struct*, c_ulong) @nogc nothrow;
    void __gmp_randinit_lc_2exp(__gmp_randstate_struct*, const(__mpz_struct)*, c_ulong, c_ulong) @nogc nothrow;
    struct itimerspec
    {
        timespec it_interval;
        timespec it_value;
    }
    struct sched_param
    {
        int sched_priority;
    }
    struct sigstack
    {
        void* ss_sp;
        int ss_onstack;
    }
    void __gmp_randinit_default(__gmp_randstate_struct*) @nogc nothrow;
    struct timespec
    {
        c_long tv_sec;
        c_long tv_nsec;
    }
    struct timeval
    {
        c_long tv_sec;
        c_long tv_usec;
    }
    struct tm
    {
        int tm_sec;
        int tm_min;
        int tm_hour;
        int tm_mday;
        int tm_mon;
        int tm_year;
        int tm_wday;
        int tm_yday;
        int tm_isdst;
        c_long tm_gmtoff;
        const(char)* tm_zone;
    }
    void __gmp_randinit(__gmp_randstate_struct*, gmp_randalg_t, ...) @nogc nothrow;
    alias time_t = c_long;
    alias timer_t = void*;
    extern __gshared const(const(char)*) __gmp_version;
    extern __gshared int __gmp_errno;
    extern __gshared const(int) __gmp_bits_per_limb;
    void __gmp_get_memory_functions(void* function(c_ulong)*, void* function(void*, c_ulong, c_ulong)*, void function(void*, c_ulong)*) @nogc nothrow;
    void __gmp_set_memory_functions(void* function(c_ulong), void* function(void*, c_ulong, c_ulong), void function(void*, c_ulong)) @nogc nothrow;
    alias mpq_ptr = __mpq_struct*;
    alias mpq_srcptr = const(__mpq_struct)*;
    alias mpf_ptr = __mpf_struct*;
    alias mpf_srcptr = const(__mpf_struct)*;
    alias mpz_ptr = __mpz_struct*;
    alias mpz_srcptr = const(__mpz_struct)*;
    alias gmp_randstate_t = __gmp_randstate_struct[1];
    static ushort __uint16_identity(ushort) @nogc nothrow;
    static uint __uint32_identity(uint) @nogc nothrow;
    static c_ulong __uint64_identity(c_ulong) @nogc nothrow;
    struct __gmp_randstate_struct
    {
        __mpz_struct[1] _mp_seed;
        gmp_randalg_t _mp_alg;
        static union _Anonymous_56
        {
            void* _mp_lc;
        }
        _Anonymous_56 _mp_algdata;
    }
    enum _Anonymous_57
    {
        GMP_RAND_ALG_DEFAULT = 0,
        GMP_RAND_ALG_LC = 0,
    }
    enum GMP_RAND_ALG_DEFAULT = _Anonymous_57.GMP_RAND_ALG_DEFAULT;
    enum GMP_RAND_ALG_LC = _Anonymous_57.GMP_RAND_ALG_LC;
    alias gmp_randalg_t = _Anonymous_57;
    alias mpf_t = __mpf_struct[1];
    struct __mpf_struct
    {
        int _mp_prec;
        int _mp_size;
        c_long _mp_exp;
        c_ulong* _mp_d;
    }
    alias mpq_t = __mpq_struct[1];
    alias MP_RAT = __mpq_struct;
    struct __mpq_struct
    {
        __mpz_struct _mp_num;
        __mpz_struct _mp_den;
    }
    alias mp_exp_t = c_long;
    alias mp_size_t = c_long;
    alias mp_srcptr = const(c_ulong)*;
    alias mp_ptr = c_ulong*;
    alias mpz_t = __mpz_struct[1];
    alias MP_INT = __mpz_struct;
    struct __mpz_struct
    {
        int _mp_alloc;
        int _mp_size;
        c_ulong* _mp_d;
    }
    alias mp_bitcnt_t = c_ulong;
    alias mp_limb_signed_t = c_long;
    alias mp_limb_t = c_ulong;


    enum DPP_ENUM___SYSCALL_WORDSIZE = 64;


    enum DPP_ENUM___GMP_HAVE_HOST_CPU_FAMILY_power = 0;


    enum DPP_ENUM___GMP_HAVE_HOST_CPU_FAMILY_powerpc = 0;


    enum DPP_ENUM_GMP_LIMB_BITS = 64;


    enum DPP_ENUM_GMP_NAIL_BITS = 0;
    enum DPP_ENUM___GNU_MP__ = 6;


    enum DPP_ENUM___GMP_LIBGMP_DLL = 0;






    enum DPP_ENUM___WORDSIZE_TIME64_COMPAT32 = 1;




    enum DPP_ENUM___GMP_MP_SIZE_T_INT = 0;


    enum DPP_ENUM___WORDSIZE = 64;






    enum DPP_ENUM__BITS_WCHAR_H = 1;


    enum DPP_ENUM__BITS_UINTN_IDENTITY_H = 1;


    enum DPP_ENUM___FD_SETSIZE = 1024;


    enum DPP_ENUM___RLIM_T_MATCHES_RLIM64_T = 1;


    enum DPP_ENUM___INO_T_MATCHES_INO64_T = 1;


    enum DPP_ENUM___OFF_T_MATCHES_OFF64_T = 1;
    enum DPP_ENUM__GMP_H_HAVE_FILE = 1;




    enum DPP_ENUM__GMP_H_HAVE_VA_LIST = 1;
    enum DPP_ENUM___GMP_INLINE_PROTOTYPES = 1;
    enum DPP_ENUM__BITS_TYPESIZES_H = 1;




    enum DPP_ENUM___timer_t_defined = 1;


    enum DPP_ENUM___time_t_defined = 1;


    enum DPP_ENUM___struct_tm_defined = 1;




    enum DPP_ENUM___timeval_defined = 1;


    enum DPP_ENUM__STRUCT_TIMESPEC = 1;


    enum DPP_ENUM___sigstack_defined = 1;




    enum DPP_ENUM__BITS_TYPES_STRUCT_SCHED_PARAM = 1;


    enum DPP_ENUM___itimerspec_defined = 1;
    enum DPP_ENUM___struct_FILE_defined = 1;




    enum DPP_ENUM___stack_t_defined = 1;






    enum DPP_ENUM___sigset_t_defined = 1;
    enum DPP_ENUM___SI_HAVE_SIGSYS = 1;


    enum DPP_ENUM___SI_ERRNO_THEN_CODE = 1;
    enum DPP_ENUM___SI_MAX_SIZE = 128;


    enum DPP_ENUM___siginfo_t_defined = 1;
    enum DPP_ENUM___SIGEV_MAX_SIZE = 64;


    enum DPP_ENUM___sigevent_t_defined = 1;




    enum DPP_ENUM___sig_atomic_t_defined = 1;


    enum DPP_ENUM__BITS_TYPES_LOCALE_T_H = 1;


    enum DPP_ENUM___clockid_t_defined = 1;


    enum DPP_ENUM___clock_t_defined = 1;
    enum DPP_ENUM_____mbstate_t_defined = 1;


    enum DPP_ENUM__BITS_TYPES___LOCALE_T_H = 1;




    enum DPP_ENUM______fpos_t_defined = 1;


    enum DPP_ENUM______fpos64_t_defined = 1;


    enum DPP_ENUM_____FILE_defined = 1;


    enum DPP_ENUM___FILE_defined = 1;
    enum DPP_ENUM__BITS_TYPES_H = 1;


    enum DPP_ENUM_TIMER_ABSTIME = 1;


    enum DPP_ENUM_CLOCK_TAI = 11;


    enum DPP_ENUM_CLOCK_BOOTTIME_ALARM = 9;




    enum DPP_ENUM_CLOCK_REALTIME_ALARM = 8;


    enum DPP_ENUM_CLOCK_BOOTTIME = 7;


    enum DPP_ENUM_CLOCK_MONOTONIC_COARSE = 6;


    enum DPP_ENUM_CLOCK_REALTIME_COARSE = 5;




    enum DPP_ENUM_CLOCK_MONOTONIC_RAW = 4;


    enum DPP_ENUM_CLOCK_THREAD_CPUTIME_ID = 3;


    enum DPP_ENUM_CLOCK_PROCESS_CPUTIME_ID = 2;


    enum DPP_ENUM_CLOCK_MONOTONIC = 1;






    enum DPP_ENUM_CLOCK_REALTIME = 0;




    enum DPP_ENUM__BITS_TIME_H = 1;




    enum DPP_ENUM___PTHREAD_MUTEX_HAVE_PREV = 1;
    enum DPP_ENUM__THREAD_SHARED_TYPES_H = 1;




    enum DPP_ENUM_FOPEN_MAX = 16;


    enum DPP_ENUM_L_ctermid = 9;


    enum DPP_ENUM_FILENAME_MAX = 4096;




    enum DPP_ENUM_TMP_MAX = 238328;


    enum DPP_ENUM_L_tmpnam = 20;


    enum DPP_ENUM__BITS_STDIO_LIM_H = 1;




    enum DPP_ENUM__BITS_STDINT_UINTN_H = 1;


    enum DPP_ENUM__BITS_STDINT_INTN_H = 1;
    enum DPP_ENUM__BITS_SS_FLAGS_H = 1;


    enum DPP_ENUM__BITS_SIGTHREAD_H = 1;




    enum DPP_ENUM_SIGSTKSZ = 8192;


    enum DPP_ENUM_MINSIGSTKSZ = 2048;


    enum DPP_ENUM__BITS_SIGSTACK_H = 1;




    enum DPP_ENUM___SIGRTMAX = 64;


    enum DPP_ENUM_SIGSYS = 31;


    enum DPP_ENUM_SIGPOLL = 29;




    enum DPP_ENUM_SIGURG = 23;


    enum DPP_ENUM_SIGTSTP = 20;


    enum DPP_ENUM_SIGSTOP = 19;




    enum DPP_ENUM_SIGCONT = 18;


    enum DPP_ENUM_SIGCHLD = 17;


    enum DPP_ENUM_SIGUSR2 = 12;




    enum DPP_ENUM_SIGUSR1 = 10;


    enum DPP_ENUM_SIGBUS = 7;


    enum DPP_ENUM_SIGPWR = 30;




    enum DPP_ENUM_SIGSTKFLT = 16;


    enum DPP_ENUM__BITS_SIGNUM_H = 1;






    enum DPP_ENUM___SIGRTMIN = 32;
    enum DPP_ENUM_SIGWINCH = 28;




    enum DPP_ENUM_SIGPROF = 27;




    enum DPP_ENUM_SIGVTALRM = 26;


    enum DPP_ENUM_SIGXFSZ = 25;


    enum DPP_ENUM_SIGXCPU = 24;




    enum DPP_ENUM_SIGTTOU = 22;


    enum DPP_ENUM_SIGTTIN = 21;






    enum DPP_ENUM_SIGALRM = 14;


    enum DPP_ENUM_SIGPIPE = 13;




    enum DPP_ENUM_SIGKILL = 9;




    enum DPP_ENUM_SIGTRAP = 5;


    enum DPP_ENUM_SIGQUIT = 3;


    enum DPP_ENUM_SIGHUP = 1;




    enum DPP_ENUM_SIGTERM = 15;


    enum DPP_ENUM_SIGSEGV = 11;


    enum DPP_ENUM_SIGFPE = 8;




    enum DPP_ENUM_SIGABRT = 6;


    enum DPP_ENUM_SIGILL = 4;


    enum DPP_ENUM_SIGINT = 2;
    enum DPP_ENUM__BITS_SIGNUM_GENERIC_H = 1;
    enum DPP_ENUM___SI_ASYNCIO_AFTER_SIGIO = 1;




    enum DPP_ENUM__BITS_SIGINFO_CONSTS_H = 1;


    enum DPP_ENUM__BITS_SIGINFO_ARCH_H = 1;
    enum DPP_ENUM__BITS_SIGEVENT_CONSTS_H = 1;
    enum DPP_ENUM__BITS_SIGCONTEXT_H = 1;




    enum DPP_ENUM_SIG_SETMASK = 2;


    enum DPP_ENUM_SIG_UNBLOCK = 1;


    enum DPP_ENUM_SIG_BLOCK = 0;
    enum DPP_ENUM_SA_SIGINFO = 4;


    enum DPP_ENUM_SA_NOCLDWAIT = 2;


    enum DPP_ENUM_SA_NOCLDSTOP = 1;
    enum DPP_ENUM__BITS_SIGACTION_H = 1;




    enum DPP_ENUM__BITS_SETJMP_H = 1;
    enum DPP_ENUM_SCHED_RR = 2;


    enum DPP_ENUM_SCHED_FIFO = 1;


    enum DPP_ENUM_SCHED_OTHER = 0;




    enum DPP_ENUM__BITS_SCHED_H = 1;


    enum DPP_ENUM___have_pthread_attr_t = 1;


    enum DPP_ENUM__BITS_PTHREADTYPES_COMMON_H = 1;




    enum DPP_ENUM___PTHREAD_RWLOCK_INT_FLAGS_SHARED = 1;
    enum DPP_ENUM___PTHREAD_MUTEX_USE_UNION = 0;


    enum DPP_ENUM___PTHREAD_MUTEX_NUSERS_AFTER_KIND = 0;




    enum DPP_ENUM___PTHREAD_MUTEX_LOCK_ELISION = 1;
    enum DPP_ENUM___SIZEOF_PTHREAD_BARRIERATTR_T = 4;


    enum DPP_ENUM___SIZEOF_PTHREAD_RWLOCKATTR_T = 8;


    enum DPP_ENUM___SIZEOF_PTHREAD_CONDATTR_T = 4;




    enum DPP_ENUM___SIZEOF_PTHREAD_COND_T = 48;


    enum DPP_ENUM___SIZEOF_PTHREAD_MUTEXATTR_T = 4;


    enum DPP_ENUM___SIZEOF_PTHREAD_BARRIER_T = 32;




    enum DPP_ENUM___SIZEOF_PTHREAD_RWLOCK_T = 56;


    enum DPP_ENUM___SIZEOF_PTHREAD_MUTEX_T = 40;


    enum DPP_ENUM___SIZEOF_PTHREAD_ATTR_T = 56;




    enum DPP_ENUM__BITS_PTHREADTYPES_ARCH_H = 1;




    enum DPP_ENUM_CHARCLASS_NAME_MAX = 2048;
    enum DPP_ENUM_COLL_WEIGHTS_MAX = 255;
    enum DPP_ENUM__POSIX2_CHARCLASS_NAME_MAX = 14;


    enum DPP_ENUM__POSIX2_RE_DUP_MAX = 255;


    enum DPP_ENUM__POSIX2_LINE_MAX = 2048;


    enum DPP_ENUM__POSIX2_EXPR_NEST_MAX = 32;




    enum DPP_ENUM__POSIX2_COLL_WEIGHTS_MAX = 2;


    enum DPP_ENUM__POSIX2_BC_STRING_MAX = 1000;


    enum DPP_ENUM__POSIX2_BC_SCALE_MAX = 99;


    enum DPP_ENUM__POSIX2_BC_DIM_MAX = 2048;


    enum DPP_ENUM__POSIX2_BC_BASE_MAX = 99;




    enum DPP_ENUM__BITS_POSIX2_LIM_H = 1;




    enum DPP_ENUM__POSIX_CLOCKRES_MIN = 20000000;


    enum DPP_ENUM__POSIX_TZNAME_MAX = 6;


    enum DPP_ENUM__POSIX_TTY_NAME_MAX = 9;




    enum DPP_ENUM__POSIX_TIMER_MAX = 32;


    enum DPP_ENUM__POSIX_SYMLOOP_MAX = 8;


    enum DPP_ENUM__POSIX_SYMLINK_MAX = 255;


    enum DPP_ENUM__POSIX_STREAM_MAX = 8;


    enum DPP_ENUM__POSIX_SSIZE_MAX = 32767;


    enum DPP_ENUM__POSIX_SIGQUEUE_MAX = 32;




    enum DPP_ENUM__POSIX_SEM_VALUE_MAX = 32767;


    enum DPP_ENUM__POSIX_SEM_NSEMS_MAX = 256;


    enum DPP_ENUM__POSIX_RTSIG_MAX = 8;


    enum DPP_ENUM__POSIX_RE_DUP_MAX = 255;


    enum DPP_ENUM__POSIX_PIPE_BUF = 512;


    enum DPP_ENUM__POSIX_PATH_MAX = 256;




    enum DPP_ENUM__POSIX_OPEN_MAX = 20;


    enum DPP_ENUM__POSIX_NGROUPS_MAX = 8;


    enum DPP_ENUM__POSIX_NAME_MAX = 14;


    enum DPP_ENUM__POSIX_MQ_PRIO_MAX = 32;


    enum DPP_ENUM__POSIX_MQ_OPEN_MAX = 8;


    enum DPP_ENUM__POSIX_MAX_INPUT = 255;




    enum DPP_ENUM__POSIX_MAX_CANON = 255;


    enum DPP_ENUM__POSIX_LOGIN_NAME_MAX = 9;


    enum DPP_ENUM__POSIX_LINK_MAX = 8;




    enum DPP_ENUM__POSIX_HOST_NAME_MAX = 255;


    enum DPP_ENUM__POSIX_DELAYTIMER_MAX = 32;


    enum DPP_ENUM__POSIX_CHILD_MAX = 25;




    enum DPP_ENUM__POSIX_ARG_MAX = 4096;


    enum DPP_ENUM__POSIX_AIO_MAX = 1;


    enum DPP_ENUM__POSIX_AIO_LISTIO_MAX = 2;




    enum DPP_ENUM__BITS_POSIX1_LIM_H = 1;




    enum DPP_ENUM_MQ_PRIO_MAX = 32768;


    enum DPP_ENUM_HOST_NAME_MAX = 64;




    enum DPP_ENUM_LOGIN_NAME_MAX = 256;


    enum DPP_ENUM_TTY_NAME_MAX = 32;


    enum DPP_ENUM_DELAYTIMER_MAX = 2147483647;




    enum DPP_ENUM_PTHREAD_STACK_MIN = 16384;


    enum DPP_ENUM_AIO_PRIO_DELTA_MAX = 20;


    enum DPP_ENUM__POSIX_THREAD_THREADS_MAX = 64;




    enum DPP_ENUM__POSIX_THREAD_DESTRUCTOR_ITERATIONS = 4;




    enum DPP_ENUM_PTHREAD_KEYS_MAX = 1024;


    enum DPP_ENUM__POSIX_THREAD_KEYS_MAX = 128;
    enum DPP_ENUM___GLIBC_USE_IEC_60559_TYPES_EXT = 0;




    enum DPP_ENUM___GLIBC_USE_IEC_60559_FUNCS_EXT = 0;


    enum DPP_ENUM___GLIBC_USE_IEC_60559_BFP_EXT = 0;




    enum DPP_ENUM___GLIBC_USE_LIB_EXT2 = 0;
    enum DPP_ENUM_RTLD_LOCAL = 0;
    enum DPP_ENUM___CPU_SETSIZE = 1024;




    enum DPP_ENUM__BITS_CPU_SET_H = 1;
    enum DPP_ENUM__BITS_BYTESWAP_H = 1;







    enum DPP_ENUM_TIME_UTC = 1;




    enum DPP_ENUM__TIME_H = 1;
    enum DPP_ENUM_SEEK_END = 2;


    enum DPP_ENUM_SEEK_CUR = 1;


    enum DPP_ENUM_SEEK_SET = 0;






    enum DPP_ENUM_BUFSIZ = 8192;


    enum DPP_ENUM__IONBF = 2;




    enum DPP_ENUM__IOLBF = 1;


    enum DPP_ENUM__IOFBF = 0;
    enum DPP_ENUM__STDIO_H = 1;
    enum DPP_ENUM__STDINT_H = 1;




    enum DPP_ENUM__STDC_PREDEF_H = 1;
    enum DPP_ENUM__SETJMP_H = 1;
    enum DPP_ENUM__SCHED_H = 1;
    enum DPP_ENUM_PTHREAD_BARRIER_SERIAL_THREAD = -1;


    enum DPP_ENUM_PTHREAD_ONCE_INIT = 0;
    enum DPP_ENUM__PTHREAD_H = 1;


    enum DPP_ENUM_RTSIG_MAX = 32;




    enum DPP_ENUM_XATTR_LIST_MAX = 65536;


    enum DPP_ENUM_XATTR_SIZE_MAX = 65536;


    enum DPP_ENUM_XATTR_NAME_MAX = 255;




    enum DPP_ENUM_PIPE_BUF = 4096;


    enum DPP_ENUM_PATH_MAX = 4096;


    enum DPP_ENUM_NAME_MAX = 255;


    enum DPP_ENUM_MAX_INPUT = 255;


    enum DPP_ENUM_MAX_CANON = 255;




    enum DPP_ENUM_LINK_MAX = 127;


    enum DPP_ENUM_ARG_MAX = 131072;


    enum DPP_ENUM_NGROUPS_MAX = 65536;


    enum DPP_ENUM_NR_OPEN = 1024;
    enum DPP_ENUM_MB_LEN_MAX = 16;


    enum DPP_ENUM__LIBC_LIMITS_H_ = 1;
    enum DPP_ENUM_GC_TMP_VERSION_MICRO = 4;


    enum DPP_ENUM_GC_TMP_VERSION_MINOR = 6;


    enum DPP_ENUM_GC_TMP_VERSION_MAJOR = 7;
    enum DPP_ENUM_GC_NOT_FOUND = 4;




    enum DPP_ENUM_GC_UNIMPLEMENTED = 3;


    enum DPP_ENUM_GC_NO_THREADS = 2;


    enum DPP_ENUM_GC_DUPLICATE = 1;


    enum DPP_ENUM_GC_SUCCESS = 0;
    enum DPP_ENUM_GC_NO_MEMORY = 2;
    enum DPP_ENUM_GC_PROTECTS_NONE = 0;


    enum DPP_ENUM_GC_PROTECTS_STACK = 8;


    enum DPP_ENUM_GC_PROTECTS_STATIC_DATA = 4;




    enum DPP_ENUM_GC_PROTECTS_PTRFREE_HEAP = 2;


    enum DPP_ENUM_GC_PROTECTS_POINTER_HEAP = 1;


    enum DPP_ENUM_GC_TIME_UNLIMITED = 999999;
    enum DPP_ENUM___GLIBC_MINOR__ = 28;




    enum DPP_ENUM___GLIBC__ = 2;


    enum DPP_ENUM___GNU_LIBRARY__ = 6;


    enum DPP_ENUM___GLIBC_USE_DEPRECATED_GETS = 0;






    enum DPP_ENUM___USE_FORTIFY_LEVEL = 0;


    enum DPP_ENUM___USE_ATFILE = 1;


    enum DPP_ENUM___USE_MISC = 1;




    enum DPP_ENUM__ATFILE_SOURCE = 1;


    enum DPP_ENUM___USE_XOPEN2K8 = 1;


    enum DPP_ENUM___USE_ISOC99 = 1;




    enum DPP_ENUM___USE_ISOC95 = 1;


    enum DPP_ENUM___USE_XOPEN2K = 1;


    enum DPP_ENUM___USE_POSIX199506 = 1;




    enum DPP_ENUM___USE_POSIX199309 = 1;


    enum DPP_ENUM___USE_POSIX2 = 1;


    enum DPP_ENUM___USE_POSIX = 1;






    enum DPP_ENUM__POSIX_SOURCE = 1;


    enum DPP_ENUM___USE_POSIX_IMPLICITLY = 1;




    enum DPP_ENUM___USE_ISOC11 = 1;






    enum DPP_ENUM__DEFAULT_SOURCE = 1;
    enum DPP_ENUM__FEATURES_H = 1;
    enum DPP_ENUM___PDP_ENDIAN = 3412;


    enum DPP_ENUM___BIG_ENDIAN = 4321;


    enum DPP_ENUM___LITTLE_ENDIAN = 1234;


    enum DPP_ENUM__ENDIAN_H = 1;
    enum DPP_ENUM_ECL_USER_DISPATCH = 5;




    enum DPP_ENUM_ECL_WRITER_DISPATCH = 4;


    enum DPP_ENUM_ECL_READER_DISPATCH = 3;


    enum DPP_ENUM_ECL_RESTRICTED_DISPATCH = 2;




    enum DPP_ENUM_ECL_STANDARD_DISPATCH = 1;


    enum DPP_ENUM_ECL_NOT_FUNCALLABLE = 0;
    enum DPP_ENUM_ECL_FFICALL_LIMIT = 256;
    enum DPP_ENUM_ECL_FLAG_ADJUSTABLE = 2;


    enum DPP_ENUM_ECL_FLAG_HAS_FILL_POINTER = 1;
    enum DPP_ENUM_ECL_CHAR_CODE_LINEFEED = 10;


    enum DPP_ENUM_ECL_CHAR_CODE_NEWLINE = 10;




    enum DPP_ENUM_ECL_CHAR_CODE_RETURN = 13;
    enum DPP_ENUM_ECL_IMMEDIATE_TAG = 3;






    enum DPP_ENUM_ECL_TAG_BITS = 2;




    enum DPP_ENUM_FALSE = 0;


    enum DPP_ENUM_TRUE = 1;
    enum DPP_ENUM_ECL_BIG_REGISTER_SIZE = 32;
    enum DPP_ENUM_ECL_NAMESTRING_FORCE_BASE_STRING = 2;


    enum DPP_ENUM_ECL_NAMESTRING_TRUNCATE_IF_ERROR = 1;




    enum DPP_ENUM_ECL_BOOLSET = std.conv.octal!17;


    enum DPP_ENUM_ECL_BOOLNAND = std.conv.octal!16;


    enum DPP_ENUM_ECL_BOOLORC1 = std.conv.octal!15;




    enum DPP_ENUM_ECL_BOOLC1 = std.conv.octal!14;


    enum DPP_ENUM_ECL_BOOLORC2 = std.conv.octal!13;


    enum DPP_ENUM_ECL_BOOLC2 = std.conv.octal!12;


    enum DPP_ENUM_ECL_BOOLEQV = std.conv.octal!11;




    enum DPP_ENUM_ECL_BOOLNOR = std.conv.octal!10;


    enum DPP_ENUM_ECL_BOOLIOR = std.conv.octal!7;


    enum DPP_ENUM_ECL_BOOLXOR = std.conv.octal!6;


    enum DPP_ENUM_ECL_BOOL2 = std.conv.octal!5;


    enum DPP_ENUM_ECL_BOOLANDC1 = std.conv.octal!4;




    enum DPP_ENUM_ECL_BOOL1 = std.conv.octal!3;


    enum DPP_ENUM_ECL_BOOLANDC2 = std.conv.octal!2;


    enum DPP_ENUM_ECL_BOOLAND = std.conv.octal!1;




    enum DPP_ENUM_ECL_BOOLCLR = 0;
    enum DPP_ENUM_ECL_LISTEN_EOF = -1;


    enum DPP_ENUM_ECL_LISTEN_AVAILABLE = 1;


    enum DPP_ENUM_ECL_LISTEN_NO_CHAR = 0;
    enum DPP_ENUM_ECL_CAN_INLINE = 1;




    enum DPP_ENUM_ECL_CMU_FORMAT = 1;
    enum DPP_ENUM_ECL_SLOTS_LIMIT = 32768;
    enum DPP_ENUM_ECL_MULTIPLE_VALUES_LIMIT = 64;


    enum DPP_ENUM_ECL_C_ARGUMENTS_LIMIT = 63;






    enum DPP_ENUM_ECL_CALL_ARGUMENTS_LIMIT = 65536;
    enum DPP_ENUM_ECL_ARRAY_RANK_LIMIT = 64;


    enum DPP_ENUM_ECL_CHAR_CODE_LIMIT = 1114112;
    enum DPP_ENUM_ECL_FIXNUM_BITS = 64;


    enum DPP_ENUM_ECL_LONG_BITS = 64;


    enum DPP_ENUM_ECL_INT_BITS = 32;
    enum DPP_ENUM_ECL_RELATIVE_PACKAGE_NAMES = 1;






    enum DPP_ENUM_ECL_DOWN_STACK = 1;


    enum DPP_ENUM_ECL_CLOS_STREAMS = 1;


    enum DPP_ENUM_ECL_UNICODE_NAMES = 1;


    enum DPP_ENUM_ECL_UNICODE = 21;




    enum DPP_ENUM_TCP = 1;






    enum DPP_ENUM_GBC_BOEHM = 1;






    enum DPP_ENUM_ECL_THREADS = 1;


    enum DPP_ENUM_ECL_VERSION_NUMBER = 160102;
    enum DPP_ENUM__DLFCN_H = 1;
    enum DPP_ENUM___GNUC_VA_LIST = 1;
    enum DPP_ENUM___GNU_MP_VERSION = 6;


    enum DPP_ENUM___GNU_MP_VERSION_MINOR = 1;


    enum DPP_ENUM___GNU_MP_VERSION_PATCHLEVEL = 2;
    enum DPP_ENUM__SYS_CDEFS_H = 1;
    enum DPP_ENUM___glibc_c99_flexarr_available = 1;
    enum DPP_ENUM___HAVE_GENERIC_SELECTION = 1;


    enum DPP_ENUM__SYS_SELECT_H = 1;
    enum DPP_ENUM__SYS_TYPES_H = 1;
    enum DPP_ENUM___BIT_TYPES_DEFINED__ = 1;
    enum DPP_ENUM__SYS_UCONTEXT_H = 1;




    enum DPP_ENUM___NGREG = 23;



struct ecl_cache;
struct cl_compiler_env;
}
+/
