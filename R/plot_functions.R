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
    plotSeq(x)
  } else if (kind == "charge") {
    plotCharge(x)
  } else if (kind == "polar") {
    plotPolar(x)
  } else if (kind == "length") {
    plotLength()
  }
}

#' @export
plot.protein5555_list <- function(x, y, kind = "aminoDis", ...) {
  if (kind == "aminoDis") {
    plotSeq(x)
  } else if (kind == "charge") {
    plotCharge(x)
  } else if (kind == "polar") {
    plotPolar(x)
  } else if (kind == "length") {
    plotLength(x)
  }
}
#' @importFrom stats family
#' @import ggplot2
#' @import ggplot2

plotSeq <- function(x, ...) {
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
      names(df) <- c("AminoAcid", "Frequency")
      df$family <- c(f)
      alldf <- rbind(alldf, df)
    }
    p <- ggplot2::ggplot(alldf, aes(x = AminoAcid, y = Frequency, fill = family)) +
      ggplot2::geom_bar(stat = "identity", width = 0.5, position = "dodge")
    p + ggplot2::scale_fill_manual(values = my_palette)
  } else {
    data <- as.data.frame(table(x))
    names(data) <- c("AminoAcid", "Count")
    p <- ggplot2::ggplot(data, aes(x = AminoAcid, y = Count, fill = AminoAcid)) +
      ggplot2::geom_bar(stat = "identity", width = 0.5)
    p + ggplot2::scale_fill_manual(values = my_palette)
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
plotCharge <- function(x, ...) {
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
    p <- ggplot2::ggplot(alldf, ggplot2::aes(x = charge, y = charge_freq, fill = family)) +
      ggplot2::geom_bar(stat = "identity", width = 0.5, position = "dodge")
    p + ggplot2::scale_fill_manual(values = my_palette)
  } else {
    df <- classify(x)
    pos <- count_label_occurrences(df, "type", "positive")
    neg <- count_label_occurrences(df, "type", "negative")
    len <- length(df)
    charge_count <- c(pos, neg, pos + neg)
    charge <- c("positive", "negative", "total")
    new_df <- data.frame(charge = charge, charge_count = charge_count)
    p <- ggplot2::ggplot(new_df, ggplot2::aes(x = charge, y = charge_count, fill = charge)) +
      ggplot2::geom_bar(stat = "identity", width = 0.5)
    p + ggplot2::scale_fill_manual(values = my_palette)
  }
}

#' @import ggplot2
plotPolar <- function(x, ...) {
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
      other <- 0
      total <- 0
      for (l in loc) {
        df <- classify(unlist(x[l]))
        polar <- polar + count_label_occurrences(df, "type", "polar") + count_label_occurrences(df, "type", "negative") + count_label_occurrences(df, "type", "positive")
        nonpolar <- nonpolar + count_label_occurrences(df, "type", "nonpolar")
        other <- other + count_label_occurrences(df, "type", "other")
        total <- total + length(df$protein)
      }
      polar <- polar / total
      nonpolar <- nonpolar / total
      other <- other / total
      polarity <- c("polar", "nonpolar", "other")
      pol_freq <- c(polar, nonpolar, other)
      df <- data.frame(polarity = polarity, pol_freq = pol_freq)
      df$family <- c(f)
      alldf <- rbind(alldf, df)
    }
    p <- ggplot(alldf, aes(x = polarity, y = pol_freq, fill = family)) +
      geom_bar(stat = "identity", width = 0.5, position = "dodge")
    p + scale_fill_manual(values = my_palette)
  } else {
    df <- classify(x)
    pos <- count_label_occurrences(df, "type", "positive")
    neg <- count_label_occurrences(df, "type", "negative")
    polar <- count_label_occurrences(df, "type", "polar")
    nonpolar <- count_label_occurrences(df, "type", "nonpolar")
    other <- count_label_occurrences(df, "type", "other")
    count <- c(pos + neg + polar, nonpolar, other)
    polarity <- c("polar", "nonpolar", "other")
    new_df <- data.frame(polarity = polarity, count = count)
    p <- ggplot(new_df, aes(x = polarity, y = count, fill = polarity)) +
      geom_bar(stat = "identity", width = 0.5)
    p + scale_fill_manual(values = my_palette)
  }
}
#' @import ggplot2
plotLength <- function(x, ...) {
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
  p <- ggplot(alldf, aes(x = seq_len, y = family)) +
    geom_boxplot()
  p #+ #scale_fill_manual(values=my_palette)
}

# https://www.w3schools.com/colors/colors_picker.asp
# https://r-graph-gallery.com/42-colors-names.html
# https://r-graph-gallery.com/ggplot2-color.html
# http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
