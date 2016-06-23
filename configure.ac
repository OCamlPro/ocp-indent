AC_INIT(ocp-indent,1.5.3)
AC_COPYRIGHT(Copyright 2013-2014 OcamlPro SAS)

AC_CONFIG_MACRO_DIR([m4])
AC_PROG_CC

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
   AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_CONFIG_FILES(
  Makefile.config
  version.ocp
)
AC_OUTPUT

if test "$prefix" = "NONE"; then
  prefix=$ac_default_prefix
fi

bindir="`eval echo ${bindir}`"
bindir="`eval echo ${bindir}`"

echo
echo Executables will be installed in ${bindir}
