#' Classifies Proteins
#'
#' Takes a vector and will characterize each amino acid with a charge or
#'     polarity
#' @param data Input protein string of amino acids
#' @return returns data frame with amino acid and its classification
#' @export
classify <- function(data) {
  positive <- c("LYS", "ARG", "HIS")
  negative <- c("ASP", "GLU")
  polar <- c("SER", "THR", "ASN", "GLN", "TYR", "CYS")
  nonpolar <- c("ALA", "VAL", "LEU", "ILE", "MET", "PHE", "TRP", "PRO", "GLY")

  # Create a vector to store labels
  labels <- character(length(data))

  # Label each amino acid
  for (i in seq_along(data)) {
    aa <- data[i]
    if (aa %in% positive) {
      labels[i] <- "positive"
    } else if (aa %in% negative) {
      labels[i] <- "negative"
    } else if (aa %in% polar) {
      labels[i] <- "polar"
    } else if (aa %in% nonpolar) {
      labels[i] <- "nonpolar"
    }
  }

  # Create a data frame with amino acid sequence and labels
  protein_df <- data.frame(protein = data, type = labels)

  # Return the original protein data frame
  return(protein_df)
}
