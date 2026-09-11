#include <windows.h>
#include <caml/mlvalues.h>

/* Number of kernel handles this process currently holds, used to check that
   completion ports are actually given back. */
CAMLprim value ocaml_iocp_test_handle_count(value unit) {
  DWORD n = 0;
  (void)unit;
  GetProcessHandleCount(GetCurrentProcess(), &n);
  return Val_int((int)n);
}
