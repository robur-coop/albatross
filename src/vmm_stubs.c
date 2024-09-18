// (c) 2017, 2018 Hannes Mehnert, all rights reserved

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>

#include <unistd.h>

CAMLprim value vmm_cpu_count (value unit) {
  CAMLparam1(unit);
  int r;
  r = (int)sysconf(_SC_NPROCESSORS_CONF);
  CAMLreturn(Val_int(r));
}

CAMLprim value vmm_memory (value unit) {
  CAMLparam1(unit);
  long pages = sysconf(_SC_PHYS_PAGES);
  long page_size = sysconf(_SC_PAGE_SIZE);
  CAMLreturn(Val_int(pages * (page_size / 1024) / 1024));
}

#ifdef __linux__
#include <sys/vfs.h>
#else
#include <sys/param.h>
#include <sys/mount.h>
#endif

CAMLprim value vmm_disk_space (value path) {
  CAMLparam1(path);
  struct statfs s;
  const char *p = String_val(path);
  if (statfs(p, &s) < 0)
    uerror("statfs", Nothing);
  CAMLreturn(Val_int(s.f_blocks * s.f_bsize / 1024 / 1024));
}

