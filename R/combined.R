class_string <- function(pdb_location) {
  # Read the PDB file
  file <- bio3d::read.pdb(pdb_location)

  # Extract sequence
  seq <- file$seqres

  # Filter sequences of length 3
  filtered <- filt_seq[nchar(filt_seq) == 3]

  # Create a data frame with the names and values
  df <- data.frame(name = names(filtered), value = as.character(filtered))

  # Filter for A and B separately
  counts <- df |>
    dplyr::filter(name == "A" | name == "B") |>
    dplyr::group_by(name) |>
    dplyr::summarize(count = dplyr::n())

  # Determine which count is greater
  if (counts$count["A"] >= counts$count["B"]) {
    return("A")
  } else if (counts$count["A"] < counts$count["B"]) {
    return("B")
  }
}
