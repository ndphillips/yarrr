
## set default options for afex.options:
# .afexEnv <- new.env()
# assign("type", 3, envir = .afexEnv)
# assign("check.contrasts", TRUE, envir = .afexEnv)
# assign("method_mixed",  "KR", envir = .afexEnv)
# assign("return_aov",  "afex_aov", envir = .afexEnv)
# assign("es_aov",  "ges", envir = .afexEnv)
# assign("correction_aov",  "GG", envir = .afexEnv)
# assign("factorize", TRUE, envir = .afexEnv)

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("************\nWelcome to YaRrr! The Pirate's Guide to R")
  packageStartupMessage("Run yarrr() to see the main package guide")
  packageStartupMessage("Email me at yarrr.book@gmail.com with questions, comments, or movie recommendations\n************")
}
