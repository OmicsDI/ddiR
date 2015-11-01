.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste("\nThis is ddiR version",packageVersion("ddiR"),"\n",
          " Read '?ddiR' and references therein for information\n",
          " about the package and how getting started.\n"))
}
