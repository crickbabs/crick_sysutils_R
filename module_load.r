##' Load LMod environment while still within R
##'
##' Replaces having to exit R and do "module load" and restarting R.  Doesn't support unloading
##' environments etc, and only limited commands within an environment.  It also doesn't report errors such as
##' missing dependencies, so if the command-line version doesn't work, this will not work either (but
##' invisibly so)
##' 
##' @title Load Module Environments
##' @param module name to be loaded
##' @param eval whether to execute alter the environments (default TRUE) or return an R script that will effect the changes instead
##' @param preloaded A character vector of modules that won't be loaded (keep as default, it's an internal param for recursion purposes)
##' @param verbose Whether to list each dependency as it gets processed
##' @return 
##' @author Gavin Kelly
module_load <- function(module, eval=TRUE,  preloaded=character(), verbose=FALSE) {
  print(paste("Processing module", module))
  lua <- system2("module", paste("show", module), stderr=TRUE, stdout=TRUE)
  load_lines <- grep("^load", lua)
  mods <- sub(".*\\(\"(.*)\"\\)", "\\1", lua[load_lines])
  lua <- as.list(paste("#", lua))
  for (i in seq(along=load_lines)) {
    if (!(mods[i] %in% preloaded)) {
      preloaded <-c(preloaded, mods[i])
      rec <- module_load(mods[i], eval=FALSE,  preloaded)
      lua[[load_lines[i]]] <- rec$lua
      preloaded <- union(preloaded, rec$preloaded)
    }
  }
  lua <- unlist(lua)
  lua <- sub("^# prepend_path", "prepend_path", lua)
  lua <- sub("^# append_path", "append_path", lua)
  lua <- sub("^# setenv", "setenv", lua)
  lua <- sub("^# pushenv", "setenv", lua) # messes up unloadx
  if (eval) {
    eval( parse(text = lua) )
  } else {
    list(lua=lua, preloaded=preloaded)
  }
}
prepend_path <- function(pth, nw, sep=":") {
  setenv(pth, paste(nw, Sys.getenv(pth), sep=sep))
}
append_path <- function(pth, nw, sep=":") {
  setenv(pth, paste(Sys.getenv(pth), nw, sep=sep))
}
setenv <- function(k,v) {
  do.call(Sys.setenv,setNames(list(v), k))
}
