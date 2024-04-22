#' Cleans pdb file of proteins
#'
#' @param pdb_location Input location of pdb file on local device
#' @return returns string that is longer of A or B protein sequence
#' @export
cleaner <- function(pdb_location) {
  # Read the PDB file
  file <- bio3d::read.pdb(pdb_location)

  # Extract sequence
  seq <- file$seqres

  # Filter sequences of length 3
  filtered <- seq[nchar(seq) == 3]

  # Create a data frame with the names and values
  df <- data.frame(name = names(filtered), value = as.character(filtered))

  # Filter for A and B separately
  counts <- df %>%
    dplyr::filter(name %in% c("A", "B")) %>%
    dplyr::group_by(name) %>%
    dplyr::summarize(count = n())

  # Check if counts for both A and B exist
  if (nrow(counts) == 2) {
    # Determine which class has more counts
    class <- ifelse(counts$count[1] >= counts$count[2], "A", "B")

    # Get the filtered sequence for the chosen class
    filt_seq <- filtered[df$name == class]

    # Store the longer string without printing it
    invisible(assign("longer_string", filt_seq, envir = .GlobalEnv))

    # Return the class
    return(class)
  } else {
    # Handle case where counts are missing
    return("Unable to determine class: Counts for A and/or B are missing.")
  }
}