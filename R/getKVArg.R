#' Parse the command line containing key=value pairs
#'
#' Convert a command line (character vector) into a list of key/value pairs.
#'
#' @examples
#'
#' getKVArgs(c("A=1", "B=2"))
#' ## $A
#' ## [1] "1"
#' ##
#' ## $B
#' ## [1] "2"
#'
#' # Keys with colons lead to a list argument:
#' getKVArgs(c("A:X=1", "A:Y = 2", "B=2"))
#' ## $A
#' ## $A$X
#' ## [1] "1"
#' ##
#' ## $A$Y
#' ## [1] "2"
#' ##
#' ##
#' ## $B
#' ## [1] "2"
#'
#' # Use "+=" to create argument vectors
#' getKVArgs(c("A+=1", "A+=3", "B=2"))
#' ## $A
#' ## [1] "1" "3"
#' ##
#' ## $B
#' ## [1] "2"
#'
#' \dontrun{
#' getKVArgs(c("A:X=1", "A:X:Y=3", "B=2"))
#'
#' ## Error in setL(x[[path[1]]], path[-1], val, c(basePath)) :
#' ##   Element A:X:Y -- cannot mix list and non-list elements
#'
#' getKVArgs(c("A+=1", "A:X=3", "B=2"))
#' }
#'
#' getKVArgs(c("A:B:C=1", "A:B?:test=hello", "A:B:D=3"))
#' ## $A
#' ## $A$B
#' ## [1] 1
#' ## -attr(, "test")
#' ## [1] "hello"
#'
#' @import assertthat
#' @import stringr
#' @export

getKVArgs <- function(args = commandArgs(TRUE)){
  assert_that(is.character(args))

  setL <- function(x, path, val, basePath = NULL){ # basePath just for error msg
    basePath <- c(basePath, path[1])
    if(is.null(x)) x <- list()
    else if(!is.list(x)){
      stop("Element ",paste(basePath, collapse = ":"),
           " -- cannot mix list and non-list elements")
    }
    stopifnot(is.character(path))
    if(rlang::is_scalar_character(path)){
      x[[path]] <- val
      return(x)
    }else{
      x[[path[1]]] <- setL(x[[path[1]]], path[-1], val, c(basePath))
      return(x)
    }
  }

  kv <- str_split(args, "\\s*\\+?=\\s*", n = 2)
  kv_type <- str_extract(args, "\\+?=") # is "=" or "+="
  iErr <- which(vapply(kv, function(k) length(k), numeric(1)) != 2)
  if(any(iErr)) stop("Argument(s) ", toString(iErr), " are not name=value pairs")
  keys <- vapply(kv, function(k) k[1], character(1))
  keys <- lapply(keys, str_split_fixed, pattern = ":", n = Inf)

  vals <- vapply(kv, function(k) k[2], character(1))
  vals <- as.list(vals)

  l <- list()
  for(i in seq_along(keys)){
    if(kv_type[i] == "=")
      l <- setL(l, keys[[i]], vals[[i]])
    else{
      v <- l[[keys[[i]]]]
      if(is.null(v) || is.character(v))
        l[[keys[[i]]]] <- c(l[[keys[[i]]]], vals[[i]])
      else
        stop(keys[[i]], ": Can't mix A:B and A+= arguments")
    }
  }


  l <- assign_attributes(l)
  class(l) <- "ARGV"

  return(l)
}

#' Assign L$`X?`$a as attr(L$X, "a")
#'
assign_attributes <- function(l, path = NULL){
  if(!is.list(l)) return(l)
  key_name <- function(n) paste(c(path,n), collapse = ":")

  n <- names(l)
  stopifnot(!anyDuplicated(n))
  n_attr <- n[grepl("\\?$", n)]

  # Transfer attributes to corresponding keys
  for(a in n_attr){
    e <- substr(a, 1, nchar(a)-1) # key to add attribute to
    if(e %in% n_attr)
      stop(key_name(a), " - cannot give attributes to attributes")
    if(!e %in% n)
      stop(key_name(a), " but no ", key_name(e), " given")
    if(!is.list(l[[a]]) || is.null(names(l[[a]])))
      stop(key_name(e), " - attributes must be of format key1:key2?:attr=val")
    for(e_n in names(l[[a]])){
      attr(l[[e]], e_n) <- l[[a]][[e_n]]
    }
    l[[a]] <- NULL
  }

  # Repeat for each list element
  n <- names(l)
  i_list <- which(vapply(l, is.list, logical(1)))
  for(i in i_list){
    l[[i]] <- assign_attributes(l[[i]], path = c(path, n[i]))
  }
  return(l)
}
