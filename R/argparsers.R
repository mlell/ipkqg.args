#' Expect arguments with certain format
#'
#' Define the expected command line arguments of a script and check their
#' format.
#'
#' This functions should be called at the top of a parameterized script to show
#' the command line interface of the script and assert the correct format of
#' the arguments.
#'
#' @name argparsers
NULL

#' @describeIn argparsers Return the argument or a default without any type checks
#'
#' @param ARGV (All functions) The output of [getKVArgs()]
#' @param name (string, all functions) The name of the argument
#' @param default (All functions) Value to return if the command line does not contain the argument.
#'   If missing, an error is thrown if the command line misses this argument.
#' @param ... One of these listed arguments. Forwarded to more generic argument parsers.
#'
#' @examples
#' ARGS <- getKVArgs(c("A=1", "B:X=10", "B:Y = 11"))
#' arg_any(ARGS, "A")
#' ## [1] "1"
#' arg_string(ARGS, c("B", "X"))
#' ## [1] "10"
#'
#' # ==== Files ==========
#' \dontrun{
#' # save list(a = 1, b = 2)
#' f <- tempfile(fileext=".qs")
#' qs::qsave(list(a=1, b=2), f)
#'
#' # Whole list can be read by just giving A=<filename>.qs
#' ARGS <- getKVArgs(c(paste0("A=",f)))
#' arg_filename(ARGS, "A", read = TRUE)
#' ## $a
#' ## [1] 1
#' ## $b
#' ## [1] 2
#'
#' # Load and extract only element "a" by adding "A?:extract=a"
#' ARGS <- getKVArgs(c(paste0("A=",f),"A?:extract=a"))
#' arg_filename(ARGS, "A", read = TRUE)
#' ## [1] 1
#'
#' }
#'
#' @export
arg_any <- function(ARGV, name = NULL, default, print = FALSE)
  unwrap(.arg_any(ARGV = ARGV, name = name, default = default), print = print)
.arg_any <- function(ARGV, name = NULL, default, ...){
  parse_arg(ARGV, name, default)
}

#' @describeIn argparsers Expect a character vector (`arg=text` or `arg+=text1 arg+=text2`)
#' @export
arg_character <- function(ARGV, name = NULL, ...)
  unwrap(.arg_character(ARGV = ARGV, name = name, ...), ...)
.arg_character <- function(ARGV, name = NULL, ...){
  .arg_type(ARGV, name, ..., type = "character")
}

#' @describeIn argparsers Expect an argument that is a single string (character scalar)
#' @export
arg_string <- function(ARGV, name = NULL, ...)
  unwrap(.arg_string(ARGV = ARGV, name = name, ...), ...)
.arg_string <- function(ARGV, name = NULL, ...){
  arg <- .arg_any(ARGV, name, ...)
  u <- unwrap(arg)
  if(!is_default(arg) && (length(u) != 1 || !is.character(u)))
    stop("Argument '",printArgName(ARGV, name),"' must be a string (character scalar)")
  return(arg)
}

#' @describeIn argparsers Expect a scalar integer argument
#' @export
arg_int <- function(ARGV, name = NULL, ...)
  unwrap(.arg_int(ARGV = ARGV, name = NAME, ...), ...)
.arg_int <- function(ARGV, name = NULL, ...){
  .arg_coerce(ARGV, name, ..., converter = as.integer, type = "integer", scalar = TRUE)
}

#' @describeIn argparsers Expect a scalar numeric argument
#' @export
arg_num <- function(ARGV, name = NULL, ...)
  unwrap(.arg_num(ARGV = ARGV, name = name, ...), ...)
.arg_num <- function(ARGV, name = NULL, ...){
  .arg_coerce(ARGV, name, ..., converter = as.numeric, type = "numeric", scalar = TRUE)
}

#' @describeIn argparsers Expect a scalar logical argument
#' @export
arg_flag <- function(ARGV, name = NULL, ...)
  unwrap(.arg_flag(ARGV = ARGV, name = name, ...), ...)
.arg_flag <- function(ARGV, name = NULL, ...){
  .arg_coerce(ARGV, name, ..., converter = as.logical, type = "logical", scalar = TRUE)
}

#' @describeIn argparsers Expect an integer vector
#' @export
arg_int <- function(ARGV, name = NULL, ...)
  unwrap(.arg_int(ARGV = ARGV, name = name, ...), ...)
.arg_int <- function(ARGV, name = NULL, ...){
  .arg_coerce(ARGV, name, ..., converter = as.integer, type = "integer", scalar = FALSE)
}

#' @describeIn argparsers Expect a numeric vector
#' @export
arg_numeric <- function(ARGV, name = NULL, ...)
  unwrap(.arg_numeric(ARGV = ARGV, name = name, ...), ...)
.arg_numeric <- function(ARGV, name = NULL, ...){
  .arg_coerce(ARGV, name, ..., converter = as.numeric, type = "numeric", scalar = TRUE)
}

#' @describeIn argparsers Expect a logical vector
#' @export
arg_logical <- function(ARGV, name = NULL, ...)
  unwrap(.arg_logical(ARGV = ARGV, name = name, ...), ...)
.arg_logical <- function(ARGV, name = NULL, ...){
  .arg_coerce(ARGV, name, ..., converter = as.logical, type = "logical", scalar = TRUE)
}

#' @describeIn argparsers Expect a character that is a list (`arg:X=1 arg:Y=2`)
#' @export
arg_list <- function(ARGV, name = NULL, ...)
  unwrap(.arg_list(ARGV = ARGV, name = name, ...), ...)
.arg_list <- function(ARGV, name = NULL, ...){
  .arg_type(ARGV, name, ..., type = "list")
}

#' @describeIn argparsers Expect a file name. Optionally check file attributes and read the file
#'
#' @param exists (logical scalar) If `TRUE` or `FALSE`, the file
#'   given on the command line is checked to exist/not exist. If `NA`, no check
#'   is performed.
#' @param isDir (logical scalar) If `TRUE` or `FALSE`, the file given on the
#'   command line is checked to be a directory/not a directory. If `NA`, no
#'   check is performed.
#' @param read (logical scalar) If `TRUE`, the file is read in and the resulting
#'   R object is returned instead of the file name.
#' @param dash (character scalar) If not NULL a dash input denotes standard
#'   input (value "stdin") or output (value "stdout"). A file object for the
#'   respective stream (file("stdin") or stdout()) is returned. If
#'
#' @export
arg_filename <- function(
  ARGV, name = NULL, ..., exists = NA, isDir = NA, read = FALSE,
  dash = NULL
){
  unwrap(.arg_filename(
      ARGV = ARGV, name = name, ..., exists = exists, isDir = isDir,
      read = read, dash = dash),
    ...)
}
.arg_filename <- function(
  ARGV, name = NULL, ..., exists = NA, isDir = NA, read = FALSE,
  dash = NULL
){
  is_true  <- rlang::is_true
  is_false <- rlang::is_false
  pname <- printArgName(ARGV, name)
  arg <- .arg_string(ARGV, name, ...)
  u <- unwrap(arg)
  u_is_stream <- FALSE

  if(is_default(arg)) return(arg)

  # Check non-compatible parameters and illegal argument values
  if(!is.na(isDir) || !is_false(read)){
    if(is_false(exists)) stop("Cannot mix exists=FALSE with isDir= or read=")
    exists <- TRUE
  }
  if(!is.null(dash) && isTRUE(read)){
    stop("For dash != NULL, read= must be a function or FALSE")
  }
  if(identical(dash, "stdout") && !identical(read, FALSE)){
    stop("read=FALSE needed for dash='stdout'")
  }
  if(!is.null(dash) && !dash %in% c("stdin", "stdout")){
    stop("dash= must be NULL, 'stdin' or 'stdout'")
  }

  # Dash can be used to denote standard in or out
  if(!is.null(dash) && identical(u, "-")){

    if(identical(dash, "stdin")){
      u <- file("stdin")
    }else if(identical(dash, "stdout")){
      u <- stdout()
    }else stop("dash= must be NULL, 'stdin' or 'stdout'")

    u_is_stream <- TRUE
  }
  if(!is.na(exists) && !u_is_stream){
    if(is_true(exists) && !file.exists(u))
      stop("Arg ",pname,": File ",u," should exist but doesn't")
    if(is_false(exists) && file.exists(u))
      stop("Arg ",pname,": File ",u," already exists")
    info <- file.info(u)
  }
  if(!is.na(isDir)){
     if(!identical(info$isdir,isDir))
       stop("Arg ", pname, ": File ",u," is ",if(isDir) "not ", "a directory")
  }
  if(!is.logical(read)){
      read <- rlang::as_function(read)
      r <- read(u)
  }else if(is_true(read)){
    ext = if(grepl("\\.",u)) sub("(.*\\.)([^.]+)$","\\2",u) else ""
    r <- switch(ext,
      fst = load_table(u),
      qs  = qs::qread(u),
      stop("Cannot handle file extension '", ext,"'"))
    if(!is.null(attr(u, "extract"))){
      r <- r[[attr(u, "extract")]]
    }
  }else{
    r <- u
  }
  return(wrap(r, like = arg))
}

#' @describeIn argparsers Expect a JSON string.
#' 
#' @param simplify (bool) Whether to turn lists of scalars into vectors
#' @export
arg_json <- function(
    ARGV, name = NULL, ..., simplify = FALSE
){
    unwrap(.arg_json(
      ARGV = ARGV, name = name, ..., simplify = simplify),
      ...)
}
.arg_json <- function(
  ARGV, name = NULL, ..., simplify = FALSE
){
  if(!requireNamespace("jsonlite", quietly = TRUE))
    stop("jsonlite package required for arg_json()")
  
  pname <- printArgName(ARGV, name)
  arg <- .arg_string(ARGV, name, ...)
  u <- unwrap(arg)
  if(is_default(arg)) return(arg)
  r <- jsonlite::parse_json(u, simplifyVector = simplify)
  
  return(wrap(r, like = arg))
}
# Multiple arguments -----------------------------------------------------

#' Parse multiple arguments
#'
#' Parse all matching arguments in `ARGV` using any `arg_*` parsing function.
#'
#' @param ARGV (All functions) The output of [getKVArgs()]
#' @param parser One of the `arg_*` argument parsers
#' @param matching A regular expression. Process only arguments matching this.
#' @param exlude A regular expression. Do not process arguments matching this.
#'   This has precedence over `matching=`.
#' @param ... Arguments forwarded to the function specified as `parser=`.
#'
#' @examples
#' ARGS <- getKVArgs(c("A=1", "B:X=10", "B:Y = 11"))
#' args(ARGS$B, arg_int)
#'
#' \dontrun{
#' ARGS <- getKVArgs(c("A=1", "B:X=aaa", "B:Y = 11"))
#' args(ARGS$B, arg_int)
#' ## Error: Cannot parse integer argument B:X: NAs introduced by coercion
#' }
#' @export
args <- function(ARGV, parser, ...){
  # Loop through gtop-level arguments only
  names <- names(ARGV)[!str_detect(names(ARGV), fixed(":"))]
  names <- setNames(nm = names)
  Map(function(name) parser(ARGV = ARGV, name = name, ...),names)
}

# Helper functions -------------------------------------------------------

# Parse arguments of the form A=x, A:B=x
# `name`: character vector, e.g. `c("A","B")` for the second above example
# `default`: Value to return if the argument was not given in the command line
#
# Returns the argument value, wrapped by the wrap() helper function
parse_arg <- function(ARGV, name, default){
  a <- function(L, name, upstream, default){
    if(length(name) == 0){
      r <- wrap(argv_extract(L), default = FALSE)
      return(r)
    }
    if(!name[1] %in% names(L)){
      if(missing(default))
        stop("Argument '", printArgName(ARGV, name) ,"' is missing.", call. = FALSE)
      r = wrap(default, default = TRUE)
      return(r)
    }else{
      return(a(L[[name[1]]], name[-1], c(upstream, name[1]), default))
    }
  }
  return(a(ARGV, name, attr(ARGV, "sub"), default))
}

# Argument name printing for error messages
printArgName <- function(ARGV, name){
  # Print path for sub_ARGV()-returned argument lists
  name <- c(attr(ARGV, "sub"), name)
  paste0(name, collapse = ":")
}

# Check that an argument has the given type.
.arg_type <- function(ARGV, name, ..., type){
  is <- methods::is
  arg <- .arg_any(ARGV, name, ...)
  u <- unwrap(arg)

  if(!is_default(arg) && !is(u, type))
    stop("Argument '",printArgName(ARGV, name),"' must be of type ",type)

  return(arg)
}

# Coerce character arguments to another type.
.arg_coerce <- function(ARGV, name, ..., converter, type, scalar){
  arg <- .arg_any(ARGV, name, ...)
  u <- unwrap(arg)
  if(is_default(arg)) return(arg)

  pname <- printArgName(ARGV, name)
  if(scalar && length(u) != 1)
    stop("Argument ",pname," must be a single value")

  r <- tryCatch(converter(u), warning = function(w){
    m <- paste0("Cannot parse ",type," argument ",printArgName(ARGV, name), ": ",w$message)
    stop(m, call. = FALSE)
  })
  if(any(is.na(r)))
    stop("Cannot parse ",type," argument ",printArgName(ARGV, name),": converter returned NA")
  r <- wrap(r, like = arg)
  return(r)
}

# Private functions for argument wrapping -------------------------------

# Arguments must be wrapped to detect values returned as default. Those should
# not be subjected to format checking because they are supplied by the programmer,
# not the user.
wrap <- function(arg, like = NULL, default = FALSE){
  x <- list(value = arg)
  if(!is.null(like)){
    like$value <- NULL
    x <- modifyList(x, like)
  }else{
    x$default <- default
  }
  return(x)
}
# ... = ignore arguments going to argparsers (we and they get the same arg list)
unwrap <- function(arg, print = FALSE, ...){
  if(print) dataSummary(arg$value)
  return(arg$value)
}


# Test if a wrapped argument value has a default value
is_default <- function(arg)
  isTRUE(arg$default)

# Handle argument printing
.print_arg <- function(x, print = FALSE, ...){
  if(!print) return(invisible())
  dataSummary(x)
}

