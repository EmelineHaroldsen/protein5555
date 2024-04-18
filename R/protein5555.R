# Create a protein constructor
#' @export
protein5555 <- function(sequence, family){
  stopifnot(is.character(sequence))
  structure(sequence, family = family, class = "protein5555")
}

#' @export
# Create a protein list constructor
protein5555_list <- function(proteins){
  stopifnot(is.list(proteins))
  structure(proteins, class = "protein5555_list")
}
