// (c) 2017, 2018 Hannes Mehnert, all rights reserved

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>

#include <sys/param.h>
#include <sys/types.h>
#include <net/if.h>

#define Val32 caml_copy_int32
#define Val64 caml_copy_int64

#ifdef __FreeBSD__
#include <sys/sysctl.h>
#include <sys/socket.h>
#include <net/if_mib.h>

CAMLprim value vmmanage_get_ifindex_by_name (value name) {
  CAMLparam1(name);
  const char *devname;
  int ifindex = 0;

  if (! caml_string_is_c_safe(name)) caml_raise_not_found();

  devname = String_val(name);

  ifindex = if_nametoindex(devname);
  if (ifindex == 0)
    caml_raise_not_found();

  CAMLreturn(Val_long(ifindex));
}

CAMLprim value vmmanage_sysctl_ifdata (value num) {
  CAMLparam1(num);
  CAMLlocal1(res);
  size_t datalen;
  int name[6];
  struct ifmibdata data;

  name[0] = CTL_NET;
  name[1] = PF_LINK;
  name[2] = NETLINK_GENERIC;
  name[3] = IFMIB_IFDATA;
  name[4] = Int_val(num);
  name[5] = IFDATA_GENERAL;
  datalen = sizeof(data);

  if (sysctl(name, nitems(name), &data, &datalen, NULL, 0) != 0)
    uerror("sysctl", Nothing);

  res = caml_alloc(18, 0);
  Store_field(res, 0, caml_copy_string(data.ifmd_name));
  Store_field(res, 1, Val_int(data.ifmd_flags));
  Store_field(res, 2, Val_int(data.ifmd_snd_len));
  Store_field(res, 3, Val_int(data.ifmd_snd_maxlen));
  Store_field(res, 4, Val_int(data.ifmd_snd_drops));
  Store_field(res, 5, Val_int(data.ifmd_data.ifi_mtu));
  Store_field(res, 6, Val_int(data.ifmd_data.ifi_baudrate));
  Store_field(res, 7, Val_int(data.ifmd_data.ifi_ipackets));
  Store_field(res, 8, Val_int(data.ifmd_data.ifi_ierrors));
  Store_field(res, 9, Val_int(data.ifmd_data.ifi_opackets));
  Store_field(res, 10, Val_int(data.ifmd_data.ifi_oerrors));
  Store_field(res, 11, Val_int(data.ifmd_data.ifi_collisions));
  Store_field(res, 12, Val_int(data.ifmd_data.ifi_ibytes));
  Store_field(res, 13, Val_int(data.ifmd_data.ifi_obytes));
  Store_field(res, 14, Val_int(data.ifmd_data.ifi_imcasts));
  Store_field(res, 15, Val_int(data.ifmd_data.ifi_omcasts));
  Store_field(res, 16, Val_int(data.ifmd_data.ifi_iqdrops));
  Store_field(res, 17, Val_int(data.ifmd_data.ifi_oqdrops));

  CAMLreturn(res);
}
#elif __linux__ /* FreeBSD */
#include <netlink/netlink.h>
#include <netlink/socket.h>
#include <netlink/route/link.h>

#define get_stat(link, stat) rtnl_link_get_stat(link, RTNL_LINK_##stat)

CAMLprim value vmmanage_get_ifindex_by_name(value name) {
  CAMLparam1(name);
  const char *devname;
  int ifindex;

  if (! caml_string_is_c_safe(name)) caml_raise_not_found();

  devname = String_val(name);

  ifindex = if_nametoindex(devname);
  if (ifindex == 0)
    caml_raise_not_found();

  CAMLreturn(Val_long(ifindex));
}

CAMLprim value vmmanage_sysctl_ifdata(value num) {
  CAMLparam1(num);
  CAMLlocal1(res);
  int err;
  struct nl_sock *nl_sock;
  struct nl_cache *link_cache;
  struct rtnl_link *link;

  nl_sock = nl_socket_alloc();
  if (nl_sock == 0)
    uerror("nl_socket_alloc", Nothing);
  err = nl_connect(nl_sock, NETLINK_ROUTE);
  if (err < 0) {
    nl_socket_free(nl_sock);
    uerror("nl_connect", Nothing);
  }
  err = rtnl_link_alloc_cache(nl_sock, AF_UNSPEC, &link_cache);
  if (err < 0) {
    nl_socket_free(nl_sock);
    uerror("rtnl_link_alloc_cache", Nothing);
  }
  link = rtnl_link_get(link_cache, Int_val(num));
  if (link == NULL) {
    nl_socket_free(nl_sock);
    nl_cache_free(link_cache);
    uerror("rtnl_link_get", Nothing);
  }
  res = caml_alloc(18, 0);
  Store_field(res, 0, caml_copy_string(rtnl_link_get_name(link)));
  Store_field(res, 1, Val_int(rtnl_link_get_flags(link)));
  Store_field(res, 2, Val_int(0)); /* send_length */
  Store_field(res, 3, Val_int(0)); /* max_send_length */
  Store_field(res, 4, Val_int(0)); /* send_drops */
  Store_field(res, 5, Val_int(rtnl_link_get_mtu(link)));
  Store_field(res, 6, Val_int(0)); /* baudrate */
  Store_field(res, 7, Val_int(get_stat(link, RX_PACKETS)));
  Store_field(res, 8, Val_int(get_stat(link, RX_ERRORS)));
  Store_field(res, 9, Val_int(get_stat(link, TX_PACKETS)));
  Store_field(res, 10, Val_int(get_stat(link, TX_ERRORS)));
  Store_field(res, 11, Val_int(get_stat(link, COLLISIONS)));
  Store_field(res, 12, Val_int(get_stat(link, RX_BYTES)));
  Store_field(res, 13, Val_int(get_stat(link, TX_BYTES)));
  Store_field(res, 14, Val_int(get_stat(link, MULTICAST)));
  Store_field(res, 15, Val_int(0));
  Store_field(res, 16, Val_int(get_stat(link, RX_DROPPED)));
  Store_field(res, 17, Val_int(get_stat(link, TX_DROPPED)));
  nl_cache_free(link_cache);
  nl_socket_free(nl_sock);
  CAMLreturn(res);
}

#else /* Linux */

/* stub symbols for OS currently not supported */
CAMLprim value vmmanage_get_ifindex_by_name (value unit) {
  CAMLparam1(unit);
  uerror("get_ifindex_by_name", Nothing);
}

CAMLprim value vmmanage_sysctl_ifdata (value num) {
  CAMLparam1(num);
  uerror("sysctl_ifdata", Nothing);
}

#endif
