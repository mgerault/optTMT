.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to optTMT package! To launch the app, run runoptTMT() function.\n",
    "To access the documentation type browseVignettes(package = 'optTMT') or \n",
    "with vignette('optTMT_doc', package = 'optTMT')\n"
    )
}
