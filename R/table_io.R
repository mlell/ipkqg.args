#' Load a table
#'
#' @export
load_table <- function(filename){
  if(!requireNamespace("tibble"))
    stop("Cannot read tables without the tibble package installed. Install 'tibble' or do not use read=TRUE")
  tibble::as_tibble(fst::read_fst(filename), .name_repair = identity)
}

#' Save a table
#'
#' @export
save_table <- function(x, filename){
  fst::write_fst(x, filename)
}

#' Save the global environment
#'
#' @param filename File to write to
#' @param envir Environment to save
#' @param ... Arguments to [qs::qsave()]
#'
#' @export
save_image <- function(filename, envir = .GlobalEnv, all = FALSE, ...){
  e <- new.env()
  for(n in ls(envir, all.names = all)){
    assign(n, envir[[n]], envir = e)
  }
  qs::qsave(x = e, file = filename, ...)
}

#' Load an environment
#'
#' @param filename File to read
#' @param envir Environment to write into
#' @param ... Arguments to [qs::qread()]
#'
#' @export
load_image <- function(filename, envir = .GlobalEnv, ...){
  e <- qs::qread(file = filename, ...)
  if(!is.environment(e)) stop("Object is no environment")
  for(n in ls(e,all.names = TRUE)){
    assign(n, e[[n]], envir = envir)
  }
}
