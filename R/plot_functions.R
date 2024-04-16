#' @export
plot.protein5555 <- function(x, ...) {
   barplot(table(x))
}

plotSeq <- function(x){
  my_palette <- c("#ff5050","#527dff", "#ffa852", "#ffd452", "#ffff52","#d4ff52",
                  "#7dff52", "#52ff7d", "#52ff7d", "#52ffa8", "#52ffff", "#52d4ff",
                  "#52a8ff","#ff7d52", "#7d52ff", "#d452ff","#a852ff","#ff52ff",
                  "#ff52d4","#ff52a8", "#ff5252")
  if (is.list(x)){
    fams <- unique_families(x)
    alldf <- data.frame()
    for (f in fams){
      loc <- which(sapply(x, function(y) f %in% attr(y, "family")))
      aminoacids <- c()
      for (l in loc){
        aminoacids <- c(aminoacids, unlist(x[l]))
      }
      df <- as.data.frame(table(aminoacids) / length(aminoacids))
      names(df) <- c("AminoAcid", "Frequency")
      df$family <- c(f)
      alldf <- rbind(alldf, df)
    }
    palette <-
    p <- ggplot(alldf, aes(x=AminoAcid, y=Frequency, fill=family)) +
      geom_bar(stat="identity", width=0.5, position = "dodge")
    p + scale_fill_manual(values=my_palette)
  }else{
    data <- as.data.frame(table(x))
    names(data) <- c("AminoAcid", "Frequency")
    p <- ggplot(data, aes(x=AminoAcid, y=Frequency, fill=AminoAcid)) +
      geom_bar(stat="identity", width=0.5)
    p + scale_fill_manual(values=my_palette)
  }
}

unique_families <- function(x){
  char <- unlist(lapply(x, function(y){attr(y, "family")}))
  return(unique(char))
}

#https://www.w3schools.com/colors/colors_picker.asp
#https://r-graph-gallery.com/42-colors-names.html
#https://r-graph-gallery.com/ggplot2-color.html
#http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
