.onUnload <- function (libpath) {
    library.dynam.unload("dplR", libpath)
}
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This is dplR version ", 
                        packageVersion("dplR"),
                        ".\n",
                        "New users can visit https://opendendro.github.io/dplR-workshop/ to get started.")
}
