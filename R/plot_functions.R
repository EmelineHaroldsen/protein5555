#' @export
plot.protein5555 <- function(x, ...) {
   barplot(table(x))
}

plotSeq <- function(x){
  if (is.list(x)){
    fams <- unique_families(x)
    alldf <- data.frame()
    for (f in fams){
      loc <- which(sapply(aa, function(y) f %in% attr(y, "family")))
      aminoacids <- c()
      for (l in loc){
        aminoacids <- c(aminoacids, unlist(x[l]))
      }
      df <- as.data.frame(table(aminoacids) / length(aminoacids))
      names(df) <- c("AminoAcid", "Frequency")
      df$family <- c(f)
      alldf <- rbind(alldf, df)
    }
    p <- ggplot(alldf, aes(x=AminoAcid, y=Frequency, fill=family)) +
      geom_bar(stat="identity", width=0.5, position = "dodge")
    p + scale_fill_brewer(palette="Spectral")
  }else{
    data <- as.data.frame(table(x))
    names(data) <- c("AminoAcid", "Frequency")
    p <- ggplot(data, aes(x=AminoAcid, y=Frequency, fill=AminoAcid)) +
      geom_bar(stat="identity", width=0.5)
    p + scale_fill_brewer(palette="Spectral")
  }
}

unique_families <- function(x){
  char <- unlist(lapply(x, function(y){attr(y, "family")}))
  return(unique(char))
  }

# a <- protein5555(c("ALA", "GLY", "HIS", "HIS", "TYR", "LYS", "VAL", "THR"), "pol")
#data <- as.data.frame(table(x))
#names(data) <- c("AminoAcid", "Frequency")
#p <- ggplot(data, aes(x=AminoAcid, y=Frequency, fill=AminoAcid)) +
#  geom_bar(stat="identity", width=0.5)
#p + scale_fill_brewer(palette="Spectral")
#k <- protein5555(c("ALA", "ALA", "GLY", "HIS", "TYR", "HIS", "HIS", "HIS", "ALA"), "pol")
#w <- protein5555(c("ALA", "GLY", "GLY", "HIS", "HIS", "HIS", "HIS", "HIS", "ALA"), "kin")
#q <- protein5555(c("SER", "VAL", "GLY", "HIS", "TYR", "LYS", "CYS", "HIS", "GLU"), "kin")


#aa <- protein5555_list(list(a, k, w, q))
