#' @export
summary.protein5555 <- function(object, ...){
  len <- length(object)
  df <- classify(object)
  pos <- count_label_occurrences(df, "type", "positive")
  neg <- count_label_occurrences(df, "type", "negative")
  polar <- count_label_occurrences(df, "type", "polar") + pos + neg
  nonpolar <- count_label_occurrences(df, "type", "nonpolar")
  count <- c(pos, neg, pos + neg, polar, nonpolar, len)
  freq <- round(count / len, 3)
  charge <- c("positive", "negative", "total_charge", "polar", "nonpolar", "length")
  new_df <- data.frame(charge = charge, count = count, frequency = freq)
  print(paste("Family:", attr(object, "family")))
  print(new_df)
}

#' @export
summary.protein5555_list <- function(object, ...){
  uFam <- unique_families(object)
  print("Families: ")
  print(uFam)
  get_full_count(object, uFam)
}

get_full_count <- function(object, uFam){
  prot <- 0
  pos <- 0
  neg <- 0
  polar <- 0
  nonPol <- 0
  total <- 0
  for (l in 1:length(object)){
    prot <- prot + 1
    df <- classify(unlist(object[l]))
    pos <- pos + count_label_occurrences(df, "type", "positive")
    neg <- neg + count_label_occurrences(df, "type", "negative")
    polar <- polar + count_label_occurrences(df, "type", "polar") + count_label_occurrences(df, "type", "negative") + count_label_occurrences(df, "type", "positive")
    nonPol <- nonPol + count_label_occurrences(df, "type", "nonpolar")
    total <- total + length(df$protein)
  }

  count <- c(pos, neg, pos + neg, polar, nonPol, total)
  freq <- round(count / total, 3)
  charge <- c("positive", "negative", "total_charge", "polar", "nonpolar", "length")
  allDf <- data.frame(charge = charge, count = count, frequency = freq)
  print(allDf)
}
