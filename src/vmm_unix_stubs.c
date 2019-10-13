#include <stdio.h>
#include <strings.h>
#include <sys/utsname.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>

/*
 * copy utsname.sysname into an OCaml option,
 * returning None if something went wrong.
 */
CAMLprim value
caml_albatros_vmm_unix_uname_sysname(void)
{
	CAMLparam0();
	CAMLlocal2(some_res, sysname_wrapped);
	struct utsname un;
	explicit_bzero(&un, sizeof(un));

	int err  = uname(&un);
	if (err) {
		/* return None: */
		CAMLreturn(Val_int(0));
	}

	sysname_wrapped = caml_copy_string(un.sysname);
	/* allocate an `'a option`: */
	some_res = caml_alloc(1, 0);

	if (!sysname_wrapped || !some_res) {
		CAMLreturn(Val_int(0));
	}

	/* make it a `Some "Linux"` for example and return: */
	Store_field(some_res, 0, sysname_wrapped);
	CAMLreturn(some_res);
}
