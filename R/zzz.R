.onUnload <- function (libpath) {
    library.dynam.unload("dplR", libpath)
}
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This is dplR version ", 
                        packageVersion("dplR"),
                        ".\n",
                        "dplR is part of openDendro https://opendendro.org",
                        ".\n",
                        "New users can start with vignette(\"intro-dplR\") or visit\n",
                        "https://opendendro.github.io/dplR-workshop/")
}
