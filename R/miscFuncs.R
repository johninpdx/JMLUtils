#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
#' 'Without' operator (complement of %in%):
#' @details A binary operator, i.e. x %w/o% y (x and y vectors, or lists) gives
#'    the elements of x that are NOT IN y.
#' @export
"%w/o%" <- function(x,y) {x[!x %in% y]} #Bin operator, x NOT IN y (vectors)
