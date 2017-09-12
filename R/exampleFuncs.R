#' Make a 'Matrix' class matrix, use row/col names to assign values
#'
#' @details This example assigns RSiena 'structural zeros' to
#'    edges where individuals have different houses. The house
#'    information is encoded in a set (list, perhaps) of vectors
#'    that show which rows (& cols) belong in which houses.
#' @return Nothing. This is just example code
exampleStruc0Code <- function(){
#' @import Matrix
  M <- Matrix(0, 5, 5)
  rownames(M) <- colnames(M) <- letters[1:5]
  house1 <- letters[1:3]
  house2 <- letters[4:5]
  M[house1,house2] <- 10
  M[house2,house1] <- 10
  M
}
