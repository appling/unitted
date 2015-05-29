.onAttach <- function(libname, pkgname) {
  
  # Copied verbatim from dplyr zzz.R for plyr & dplyr (& utils-format.R for
  # rule()). Necessary because dplyr masks as_data_frame from unitted.
  setHook(packageEvent("dplyr", "attach"), function(...) {
    rule <- function(char = "-") {
      paste0(rep(char, getOption("width") - 2), collapse = "")
    }
    packageStartupMessage(rule())
    packageStartupMessage("As Hadley says for dplyr+plyr: You have loaded dplyr after unitted - this is likely ",
                          "to cause problems.\nIf you need functions from both dplyr and unitted, ",
                          "please load dplyr first, then unitted:\nlibrary(dplyr); library(unitted)")
    packageStartupMessage(rule())
  })
}