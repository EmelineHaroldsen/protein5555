# plot.protein5555 <- function(x, ...) {
#   barplot(table(x))
# }

plotSeq <- function(x){
  barplot(table(x))
}

a <- protein5555(c("ALA", "GLY", "HIS", "HIS"), "pol")
data <- table(a)
