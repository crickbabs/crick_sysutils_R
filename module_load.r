prepend_path <- function(pth, nw, sep=":") {
  setenv(pth, paste(nw, Sys.getenv(pth), sep=sep))
}
append_path <- function(pth, nw, sep=":") {
  setenv(pth, paste(Sys.getenv(pth), nw, sep=sep))
}
setenv <- function(k,v) {
  do.call(Sys.setenv,setNames(list(v), k))
}
module_load <- function(arg, eval=TRUE,  preloaded=character(), verbose=FALSE) {
  print(paste("Processing module", arg))
  lua <- system2("module", paste("show", arg), stderr=TRUE, stdout=TRUE)
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

