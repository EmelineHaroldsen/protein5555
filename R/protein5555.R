# Create a protein constructor
protein5555 <- function(sequence, family){
  stopifnot(is.character(sequence))
  structure(sequence, family = family, class = "protein5555")
}

# Create a protein list constructor
protein5555_list <- function(proteins){
  stopifnot(is.list(proteins))
  structure(proteins, family = family, class = "protein5555_list")
}
