#' @useDynLib ggpointgrid
#' @importFrom Rcpp evalCpp
NULL

#' @export
.onUnload <- function(libpath) {
  library.dynam.unload("ggpointgrid", libpath)
}

#'@importFrom rlang .data 
#'
NULL
