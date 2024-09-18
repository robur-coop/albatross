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
  r = (int)sysconf(_SC_NPROCESSORS_ONLN);
  CAMLreturn(Val_int(r));
}

int to_mb (long a, long b) {
  int r = 0;
  if (a % (1024 * 1024) == 0)
    r = (a / 1024 / 1024) * b;
  else {
    if (b % (1024 * 1024) == 0)
      r = a * (b / 1024 / 1024);
    else {
      if (a % 1024 == 0) {
        if (b % 1024 == 0)
          r = (a / 1024) * (b / 1024);
        else
          r = (a / 1024) * b / 1024;
      } else {
        if (b % 1024 == 0)
          r = a * (b / 1024) / 1024;
        else
          r = a * b / 1024 / 1024;
      }
    }
  }
  return r;
}

CAMLprim value vmm_memory (value unit) {
  CAMLparam1(unit);
  long pages = sysconf(_SC_PHYS_PAGES);
  long page_size = sysconf(_SC_PAGE_SIZE);
  CAMLreturn(Val_int(to_mb(pages, page_size)));
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
  CAMLreturn(Val_int(to_mb(s.f_blocks, s.f_bsize)));
}

