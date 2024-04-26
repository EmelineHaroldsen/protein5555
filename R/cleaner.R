#' Cleans pdb file of proteins
#'
#' This function takes a protein PDB file or previously read in PDB file and
#'    locates the longest protein sequence for further analyzation.
#'
#' @param pdb_input Location of pdb file on local device or variable name of
#'     previously read in PDB file
#' @return returns string that is longer of A or B protein sequence
#' @export
cleaner <- function(pdb_input) {
  # Check if pdb_input is a file path or a PDB object
  if (is.character(pdb_input)) {
    # Read the PDB file
    file <- bio3d::read.pdb(pdb_input)
  } else if (inherits(pdb_input, "pdb")) {
    # If pdb_input is already a PDB object, assign it directly
    file <- pdb_input
  } else {
    stop("Input must be either a file path to a PDB file or a PDB object.")
  }

  # Extract sequence
  seq <- file$seqres

  # Filter sequences of length 3
  filtered <- seq[nchar(seq) == 3]

  # Check if there are any filtered sequences
  if (length(filtered) == 0) {
    return("No sequences of length 3 found.")
  }

  # Create a data frame with the names and values
  df <- data.frame(pro_seq = names(filtered), value = as.character(filtered))

  # Check if there are any names
  if (is.null(df$pro_seq)) {
    return("No names found.")
  }
  # Filter for A and B separately
  counts <- df |>
    dplyr::filter(.data$pro_seq %in% c("A", "B")) |>
    dplyr::group_by(.data$pro_seq) |>
    dplyr::summarize(count = dplyr::n())

  # Check if counts for both A and B exist
  if (nrow(counts) == 2) {
    # Determine which class has more counts
    class <- ifelse(counts$count[1] >= counts$count[2], "A", "B")

    # Get the filtered sequence for the chosen class
    filt_seq <- filtered[df$pro_seq == class]
    return(filt_seq)
  } else {
    # Handle case where counts are missing
    return("Unable to determine class: Counts for A and/or B are missing.")
  }
}
