#' @export
summary.protein5555 <- function(object, ...){
  len <- length(object)
  df <- classify(object)
  pos <- count_label_occurrences(df, "type", "positive")
  neg <- count_label_occurrences(df, "type", "negative")
  polar <- count_label_occurrences(df, "type", "polar") + pos + neg
  nonpolar <- count_label_occurrences(df, "type", "nonpolar")
  count <- c(pos, neg, pos + neg, polar, nonpolar, len)
  charge <- c("positive", "negative", "total_charge", "polar", "nonpolar", "length")
  new_df <- data.frame(charge = charge, count = count)
  print(new_df)
}

summary.protein5555_list <- function(object, ...){

}
