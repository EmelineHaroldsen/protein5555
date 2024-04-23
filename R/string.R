string <- function(seqres) {
  filtered <- seqres[nchar(seqres) == 3]
  # Create a data frame with the names and values
  df <- data.frame(name = names(filtered), value = as.character(filtered))

  # Filter for A and B separately
  counts <- df |>
    dplyr::filter(name == "A" | name == "B") |>
    dplyr::group_by(name) |>
    dplyr::summarize(count = dplyr::n())

  if (counts$count["A"] >= counts$count["B"]) {
    return("A")
  } else if (counts$count["A"] < counts$count["B"]) {
    return("B")
  }
}
