#' Serialization with multithreaded compression
#'
#' @export
saveRDS <- function(o, file, compress = TRUE, ...){
  stopifnot(length(file) == 1)

  if(identical(compress, FALSE) ||
     is.null(p <- getPigzPipe(file, nCores = if(is.numeric(compress)) compress else NULL))
  ){
    # Fallback on traditional saveRDS
    if(inherits(file, "connection")){
      # If file is a connection, base::saveRDS ignores the compress= argument
      # and gives a warning. To avoid the warning, don't use that argument
      return(base::saveRDS(
        object = o, file = file, ...))
    }else{
      return(base::saveRDS(
        object = o, file = file, compress = FALSE, ...))
    }
  }

  on.exit(close(p))
  serialize(o, connection = p)
  invisible(NULL)
}

#' Save the workspace using multithreaded compression
#'
#' @export
save.image <- function(file, compress = TRUE, envir = .GlobalEnv, precheck = FALSE, ...){
  stopifnot(length(file) == 1)

  if(identical(compress, FALSE) ||
     is.null(p <- getPigzPipe(file, nCores = if(is.numeric(compress)) compress else NULL))
  ){
    # Fallback on traditional saveRDS
    return(base::save.image(
      file = file, compress = FALSE, ...))
  }

  on.exit(close(p))
  base::save(list = names(.GlobalEnv), file = p, envir = envir,
       compress = FALSE, precheck = precheck, ...)
  invisible(NULL)
}


# Return a pipe to a pigz process, or NULL if pigz is not available.
getPigzPipe <- function(filename, nCores = NULL){
  if(!file.exists(dirname(filename)) || !isTRUE(file.info(dirname(filename))$isdir))
    stop("Output directory for ", filename, " doesn't exist")

  if(system("pigz --version", ignore.stdout = TRUE, ignore.stderr = TRUE) != 0){
    warning("pigz can not be executed. Saving without compression")
    return(NULL)
  }

  p <- if(!is.null(nCores)) paste0("-p ",as.integer(nCores)) else ""
  s <- pipe(paste0("pigz -c ",p," > ",shQuote(filename)), open = "wb")

  return(s)
}
