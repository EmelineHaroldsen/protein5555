#' @export
summary.protein5555 <- function(object, ...) {
  len <- length(object)
  df <- classify(object)
  pos <- count_label_occurrences(df, "type", "positive")
  neg <- count_label_occurrences(df, "type", "negative")
  polar <- count_label_occurrences(df, "type", "polar") + pos + neg
  nonpolar <- count_label_occurrences(df, "type", "nonpolar")
  count <- c(pos, neg, pos + neg, polar, nonpolar, len)
  freq <- round(count / len, 3)
  feature <- c("positive", "negative", "total_charge",
               "polar", "nonpolar", "length")
  new_df <- data.frame(feature = feature, count = count, frequency = freq)
  print(paste("Family:", attr(object, "family")))
  print(new_df)
}

#' @export
summary.protein5555_list <- function(object, ...) {
  u_fam <- unique_families(object)
  print("Families: ")
  print(u_fam)
  get_full_count(object, u_fam)
}

get_full_count <- function(object, u_fam) {
  prot <- 0
  pos <- 0
  neg <- 0
  polar <- 0
  non_pol <- 0
  total <- 0
  for (l in seq_len(object)) {
    prot <- prot + 1
    df <- classify(unlist(object[l]))
    pos <- pos + count_label_occurrences(df, "type", "positive")
    neg <- neg + count_label_occurrences(df, "type", "negative")
    polar <- polar + count_label_occurrences(df, "type", "polar")
    + count_label_occurrences(df, "type", "negative") +
      count_label_occurrences(df, "type", "positive")
    non_pol <- non_pol + count_label_occurrences(df, "type", "nonpolar")
    total <- total + length(df$protein)
  }

  count <- c(pos, neg, pos + neg, polar, non_pol, total)
  freq <- round(count / total, 3)
  feature <- c("positive", "negative", "total_charge",
               "polar", "nonpolar", "length")
  all_df <- data.frame(feature = feature, count = count, frequency = freq)
  print(all_df)
}
