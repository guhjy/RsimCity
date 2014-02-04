.simCityEnv <- new.env()

.setEnv <- function(name, value) {
  assign(as.character(name), value, envir=.simCityEnv)
}

.unsetEnv <- function(name) {
  value <- .getEnv(name) 
  if (!is.null(value)) {
    rm(list=name, envir=.simCityEnv)
  }
}

.getEnv <- function(name) {
  if (missing(name)) {
     utils::ls.str(.simCityEnv)
  } else {
    unlist(lapply(name,function(e) get(e,envir=.simCityEnv)))
  }
}

.setEnv('verbose',F)
