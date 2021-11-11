# Functions belonging to the ARGV class


#' @method print ARGV
#' @export
print.ARGV <- function(x, ...){
  if(is.null(attr(x, "sub"))){
    cat("Argument list\n")
  }else{
    cat("Argument sublist: ",paste(attr(x, "sub"), collapse = ":"), "\n")
  }
  n <- names(x)
  attributes(x) <- NULL
  names(x) <- n
  str(x)
}

#' @method $ ARGV
#' @export
`$.ARGV` <- function(x, name){
  x[[as.character(name)]]
}

#' @method [[ ARGV
#' @export
`[[.ARGV` <- function(x, name){
  if(!is.character(name)) name <- argv_get_name(x, name)
  stopifnot(is.character(name))
  e <- argv_extract(x)[[name]]
  class(e) <- class(x)
  attr(e, "sub") <- c(attr(e, "sub"), name)
  e
}

#' @method [ ARGV
#' @export
`[.ARGV` <- function(x, what){
  y <- x
  a <- attributes(x)
  a$names <- NULL
  attributes(y) <- NULL
  y <- y[what]
  attributes(y) <- a
  y
}

#' @method str ARGV
#' @export
str.ARGV <- print.ARGV

argv_extract <- function(ARGV){
  attr(ARGV, "sub") <- NULL
  unclass(ARGV)
}

# If ARGV is indexed with an integer, get the corresponding name
argv_get_name <- function(ARGV, i){
  n <- names(ARGV)
  if(is.null(n)) return(paste0("[[",i,"]]"))
  else return(n[[i]])
}
