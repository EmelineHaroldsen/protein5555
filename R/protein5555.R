#' Create a protein constructor
#' @param sequence Requires a protein sequence to put inputted
#' @param family Requires the family, either polymerase or kinase, to be input
#' @export
protein5555 <- function(sequence, family){
  stopifnot(is.character(sequence))
  structure(sequence, family = family, class = "protein5555")
}
#' Create a protein list constructor
#' @param proteins Input is a list of proteins to create an overall list of
#'     proteins
#' @export
protein5555_list <- function(proteins){
  stopifnot(is.list(proteins))
  structure(proteins, class = "protein5555_list")
}
