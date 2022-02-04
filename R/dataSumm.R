#' Summarise data columns
#'
#' For each column of the data.frame `d`, print a line of summary. Elements and
#' their counts are summarized for characters and factors, for numeric values,
#' minimum, quartile 1, median, quartile 3, maximum, mean and number of NAs are
#' printed, in that order. For matrices, the dimensions are shown. Data frames
#' are summarized using a shorter form of the above descriptions for each
#' column.
#'
#' @param d A data.frame to print a summary of
#' @param con Connection to print to. Forwarded to [base::cat()].
#'
#' @examples
#' dataSummary(diamonds)
#' ## 53940 x 10 tbl_df, tbl, data.frame:
#' ## $ carat   <dbl>:    0.200 -[    0.400     0.700      1.04 ]-     5.01 µ=    0.798 NAs: 0
#' ## $ cut     <fct>: Ideal: 21551 Premium: 13791 Very Good: 12082 Good: 4906 Fair: 1610  NA: 0
#' ## $ color   <fct>: G: 11292 E: 9797 F: 9542 H: 8304 D: 6775 I: 5422 J: 2808  NA: 0
#' ## $ clarity <fct>: SI1: 13065 VS2: 12258 SI2: 9194 VS1: 8171 VVS2: 5066 VVS1: 3655 IF: 1790 I1: 741  NA: 0
#' ## $ depth   <dbl>:     43.0 -[     61.0      61.8      62.5 ]-     79.0 µ=     61.7 NAs: 0
#' ## $ table   <dbl>:     43.0 -[     56.0      57.0      59.0 ]-     95.0 µ=     57.5 NAs: 0
#' ## $ price   <int>:     326. -[     950.  2.40e+03  5.32e+03 ]- 1.88e+04 µ= 3.93e+03 NAs: 0
#' ## $ x       <dbl>:     0.00 -[     4.71      5.70      6.54 ]-     10.7 µ=     5.73 NAs: 0
#' ## $ y       <dbl>:     0.00 -[     4.72      5.71      6.54 ]-     58.9 µ=     5.73 NAs: 0
#' ## $ z       <dbl>:     0.00 -[     2.91      3.53      4.04 ]-     31.8 µ=     3.54 NAs: 0
#' @export
dataSummary <- function(d, file = "", signif = 3, ...){
  m <- function(...){
    if(file == "") message(sprintf(...))
    else cat(sprintf(...), file = file)
  }
  cl <- function(...) paste0(..., collapse = ", ")

  type_abb <- function(t){
    types <- c(logical = "lgl", integer = "int", double = "dbl",
               complex = "cplx", character = "chr", raw = "raw",
               environment = "env")
    if(t %in% names(types)){
      return(types[t])
    }else return(t)
  }

  type_str <- function(x, recurse = TRUE){
    if(is.null(x)) return("<NULL>")
    if(is.array(x)) return(paste0("<",type_abb(typeof(x)),"(",length(dim(x)),"D)>"))
    if(is.factor(x)) return("<fct>")
    if(is.atomic(x)) return(paste0("<",type_abb(typeof(x)),">"))
    if(is.data.frame(x)) return("<df>")
    if(recurse && inherits(x, "vctrs_list_of"))
      return(paste0("<list",type_str(attr(x,"ptype"), recurse = FALSE),">"))
    return(paste0("<",typeof(x),">"))
  }



  summ.num <- function(x){
    s <- signif(c(quantile(x, c(0,0.25,0.5,0.75,1),na.rm = TRUE),
           mean(x, na.rm = TRUE)),signif)

    z <- c(""," -[ "," | "," ]- ","","µ")
    fmt <- paste0("%#",signif+5,".",signif,"g")
    return(do.call(sprintf, c(list(
      gsub("XX",fmt,"XX -[ XX  XX  XX ]- XX µ= XX NAs: %d")),
      s, sum(is.na(x)))))

    return(paste0(paste(
      z, sprintf(, s),
      sep = " ", collapse = " "
    ), ", NAs:", sum(is.na(x))))
  }

  summ.fac <- function(x){
    t <- table(x)
    names(t) <- dimnames(t)[[1]]
    t <- sort(t, decreasing = TRUE)
    n <- min(length(t), 8)
    long <- length(t) > n
    first <- paste(sprintf("%s: %d", names(t)[seq_len(n)], t[seq_len(n)]), collapse = " ")
    return(paste(
      first,
      if(long) sprintf("... (%d lv.)",nlevels(x)),
      sprintf("NA: %d", sum(is.na(x)))))
  }

  summ.chr <- function(x){
    u <- sort(unique(x))
    l <- length(u)
    smpl <- if(l > 20) c(u[1:4], " ... ",u[l-(2:0)]) else u
    smpl <- vapply(smpl, function(x) if(nchar(x) > 10) paste0(substr(x,1,10),"…") else x,
                   character(1))
    paste0("{",cl(smpl),"}",
           if(l > 20) sprintf(" (%d unique el.)",l) else "",
           if(sum(is.na(x)) > 0) sprintf(" (%d NAs)", sum(is.na(x))))
  }
  summ.lgl <- function(x){
    sprintf("TRUE: %d, FALSE: %d, NA: %d",
            sum(!is.na(x) & x),  sum(!is.na(x) & !x),  sum(is.na(x)))
  }

  summ.df <- function(x){
    s <- sprintf("%d x %d %s:",nrow(x), ncol(x), cl(class(x)))

    # Align column names
    nl <- min(max(vapply(colnames(x), nchar, integer(1))),20)

    p <- function(x){
      if(is.numeric(x)) return(summ.num(x))
      if(is.factor(x)) return(summ.fac(x))
      if(is.logical(x)) return(summ.lgl(x))
      if(is.character(x)) return(summ.chr(x))
      return("")
    }
    n <- names(x)
    for(i in seq_along(x)){
      s <- c(s, sprintf(paste0("$ %-",nl,"s %s: %s"), n[i], type_str(x[[i]]), p(x[[i]])))
    }
    return(s)
  }

  if(is.null(d)) m("NULL") else
  if(is.data.frame(d)) m("%s\n",summ.df(d)) else{
    t <- type_str(d)
    l <- if(length(dim(d)) >= 2) paste(dim(d), collapse="x") else
      as.character(length(d))
    p <- if(is.character(d)) summ.chr(d) else
         if(is.factor(d))    summ.fct(d) else
         if(is.numeric(d))   summ.num(d) else
         if(is.logical(d))   summ.num(d) else
         sprintf("%d elements", length(d))
    m(paste(t, l, p, sep =" "))
  }
}


