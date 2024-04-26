# Just as a note, I used chatgpt quite a bit while writing most of my functions
# (the plot and summary functions). I just want to give credit where it's due!
#' Plot protein information
#'
#' Ability to plot
#' @param x protein5555 object
#' @param y another parameter
#' @param kind "aminoDis", "charge", "polar"
#' @param ... other arguments that are generic to plot function
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot
#' @export
plot.protein5555 <- function(x, y, kind = "aminoDis", ...) {
  if (kind == "aminoDis") {
    plot_seq(x)
  } else if (kind == "charge") {
    plot_charge(x)
  } else if (kind == "polar") {
    plot_polar(x)
  } else if (kind == "length") {
    plot_length()
  }
}

#' @export
plot.protein5555_list <- function(x, y, kind = "aminoDis", ...) {
  if (kind == "aminoDis") {
    plot_seq(x)
  } else if (kind == "charge") {
    plot_charge(x)
  } else if (kind == "polar") {
    plot_polar(x)
  } else if (kind == "length") {
    plot_length(x)
  }
}

globalVariables(c("AminoAcid", "Frequency", "Count"))

#' @importFrom stats family
#' @import ggplot2
plot_seq <- function(x, ...) {
  my_palette <- c(
    "#ff5050", "#527dff", "#ffa852", "#ffd452", "#ffff52", "#d4ff52",
    "#7dff52", "#52ff7d", "#52ff7d", "#52ffa8", "#52ffff", "#52d4ff",
    "#52a8ff", "#ff7d52", "#7d52ff", "#d452ff", "#a852ff", "#ff52ff",
    "#ff52d4", "#ff52a8", "#ff5252"
  )
  if (is.list(x)) {
    fams <- unique_families(x)
    alldf <- data.frame()
    for (f in fams) {
      loc <- which(sapply(x, function(y) f %in% attr(y, "family")))
      aminoacids <- c()
      for (l in loc) {
        aminoacids <- c(aminoacids, unlist(x[l]))
      }
      df <- as.data.frame(table(aminoacids) / length(aminoacids))
      colnames(df) <- c("AminoAcid", "Frequency")
      df$family <- c(f)
      alldf <- rbind(alldf, df)
    }
    p <- ggplot2::ggplot(data = alldf, ggplot2::aes(
      x = AminoAcid,
      y = Frequency,
      fill = family
    )) +
      ggplot2::geom_bar(stat = "identity", width = 0.5, position = "dodge")
    p + ggplot2::scale_fill_manual(values = my_palette) + ggplot2::theme_bw() +
      theme(text = element_text(size = 13))
  } else {
    data <- as.data.frame(table(x))
    colnames(data) <- c("AminoAcid", "Count")
    p <- ggplot2::ggplot(data = data, ggplot2::aes(
      x = AminoAcid,
      y = Count, fill = AminoAcid
    )) +
      ggplot2::geom_bar(stat = "identity", width = 0.5)
    p + ggplot2::scale_fill_manual(values = my_palette) + ggplot2::theme_bw() +
      ggplot2::theme(text = element_text(size = 13))
  }
}



unique_families <- function(x) {
  char <- unlist(lapply(x, function(y) {
    attr(y, "family")
  }))
  return(unique(char))
}

count_label_occurrences <- function(data_frame, column_name, label) {
  # Check if the column exists in the data frame
  if (!(column_name %in% colnames(data_frame))) {
    stop("Column does not exist in the data frame.")
  }

  # Count occurrences of the label in the specified column
  occurrences <- sum(data_frame[[column_name]] == label, na.rm = TRUE)

  return(occurrences)
}
#' @import ggplot2
plot_charge <- function(x, ...) {
  my_palette <- c("#ff5050", "#527dff", "#7d52ff")

  if (is.list(x)) {
    # Get all the unique families
    fams <- unique_families(x)

    # Create a new dataframe for all of them.
    alldf <- data.frame()

    # loop through all the families
    for (f in fams) {
      # Get the proteins that are in the family.
      loc <- which(sapply(x, function(y) f %in% attr(y, "family")))
      pos <- 0
      neg <- 0
      total <- 0
      for (l in loc) {
        df <- classify(unlist(x[l]))
        pos <- pos + count_label_occurrences(df, "type", "positive")
        neg <- neg + count_label_occurrences(df, "type", "negative")
        total <- total + length(df$protein)
      }
      tog <- (pos + neg) / total
      pos <- (pos) / total
      neg <- (neg) / total
      charge <- c("positive", "negative", "total")
      charge_freq <- c(pos, neg, tog)
      df <- data.frame(charge = charge, charge_freq = charge_freq)
      df$family <- c(f)
      alldf <- rbind(alldf, df)
    }
    p <- ggplot2::ggplot(alldf, ggplot2::aes(
      x = charge,
      y = charge_freq, fill = family
    )) +
      ggplot2::geom_bar(stat = "identity", width = 0.5, position = "dodge")
    p + ggplot2::scale_fill_manual(values = my_palette) + ggplot2::theme_bw() +
      ggplot2::labs(y = "Charge Frequency") +
      ggplot2::theme(text = element_text(size = 16))
  } else {
    df <- classify(x)
    pos <- count_label_occurrences(df, "type", "positive")
    neg <- count_label_occurrences(df, "type", "negative")
    charge_count <- c(pos, neg, pos + neg)
    charge <- c("positive", "negative", "total")
    new_df <- data.frame(charge = charge, charge_count = charge_count)
    p <- ggplot2::ggplot(new_df, ggplot2::aes(
      x = charge,
      y = charge_count,
      fill = charge
    )) +
      ggplot2::geom_bar(stat = "identity", width = 0.5)
    p + ggplot2::scale_fill_manual(values = my_palette) + ggplot2::theme_bw() +
      ggplot2::labs(y = "Charge Frequency") +
      ggplot2::theme(text = element_text(size = 16))
  }
}

#' @import ggplot2
plot_polar <- function(x, ...) {
  my_palette <- c("#ff5050", "#527dff", "#7d52ff")

  if (is.list(x)) {
    # Get all the unique families
    fams <- unique_families(x)

    # Create a new dataframe for all of them.
    alldf <- data.frame()

    # loop through all the families
    for (f in fams) {
      # Get the proteins that are in the family.
      loc <- which(sapply(x, function(y) f %in% attr(y, "family")))
      polar <- 0
      nonpolar <- 0
      total <- 0
      for (l in loc) {
        df <- classify(unlist(x[l]))
        polar <- polar + count_label_occurrences(df, "type", "polar") +
          count_label_occurrences(df, "type", "negative") +
          count_label_occurrences(df, "type", "positive")
        nonpolar <- nonpolar + count_label_occurrences(df, "type", "nonpolar")
        total <- total + length(df$protein)
      }
      polar <- polar / total
      nonpolar <- nonpolar / total
      polarity <- c("polar", "nonpolar")
      pol_freq <- c(polar, nonpolar)
      df <- data.frame(polarity = polarity, pol_freq = pol_freq)
      df$family <- c(f)
      alldf <- rbind(alldf, df)
    }
    p <- ggplot2::ggplot(alldf, ggplot2::aes(
      x = polarity,
      y = pol_freq, fill = family
    )) +
      ggplot2::geom_bar(stat = "identity", width = 0.5, position = "dodge")
    p + ggplot2::scale_fill_manual(values = my_palette) + ggplot2::theme_bw() +
      ggplot2::labs(y = "Polarity Frequency") +
      ggplot2::theme(text = element_text(size = 16))
  } else {
    df <- classify(x)
    pos <- count_label_occurrences(df, "type", "positive")
    neg <- count_label_occurrences(df, "type", "negative")
    polar <- count_label_occurrences(df, "type", "polar")
    nonpolar <- count_label_occurrences(df, "type", "nonpolar")
    count <- c(pos + neg + polar, nonpolar)
    polarity <- c("polar", "nonpolar")
    new_df <- data.frame(polarity = polarity, count = count)
    p <- ggplot2::ggplot(
      new_df,
      ggplot2::aes(x = polarity, y = count, fill = polarity)
    ) +
      ggplot2::geom_bar(stat = "identity", width = 0.5)
    p + ggplot2::scale_fill_manual(values = my_palette) + ggplot2::theme_bw() +
      ggplot2::labs(y = "Polarity Frequency") +
      ggplot2::theme(text = element_text(size = 16))
  }
}
#' @import ggplot2
plot_length <- function(x, ...) {
  # Get all the unique families
  fams <- unique_families(x)

  # Create a new dataframe for all of them.
  alldf <- data.frame()

  # loop through all the families
  for (f in fams) {
    # Get the proteins that are in the family.
    loc <- which(sapply(x, function(y) f %in% attr(y, "family")))
    seq <- c()
    for (l in loc) {
      seq_length <- length(unlist(x[l]))
      seq <- c(seq, seq_length)
    }
    df <- data.frame(seq_len = seq)
    df$family <- c(f)
    alldf <- rbind(alldf, df)
  }
  p <- ggplot2::ggplot(alldf, ggplot2::aes(x = seq_len, y = family)) +
    ggplot2::geom_boxplot()
  p + ggplot2::theme_bw() + ggplot2::theme(text = element_text(size = 16)) +
    ggplot2::labs(y = "Sequence Length")
}

# https://www.w3schools.com/colors/colors_picker.asp
# https://r-graph-gallery.com/42-colors-names.html
# https://r-graph-gallery.com/ggplot2-color.html
# http://www.sthda.com/english/wiki/ggplot2-barplots-
#   quick-start-guide-r-software-and-data-visualization
