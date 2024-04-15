#clean code here
library(bio3d)
library(tidyverse)
huo1 <- read.pdb("data/Pol/1huo.pdb")
sl01 <- read.pdb("data/Pol/1sl0.pdb")
bqr1 <- read.pdb("data/Pol/2bqr.pdb")

fpu1 <- read.pdb("data/Kinase/1fpu.pdb")
qgy1 <- read.pdb("data/Kinase/8qgy.pdb")
r7g1 <- read.pdb("data/Kinase/8r7g.pdb")

#overall generic function
cleanser <- function(pdb_location){
  file <- bio3d::read.pdb(pdb_location)
  seq <- file$seqres
  filt_seq <- seq[names(seq) == "A"]
  filt_seq <- as.vector(filt_seq)
}


# Filter POL to find chain A
huos <- huo1$seqres
huo <- huos[names(huo) == "A"]

sl0s <- sl01$seqres
sl0 <- sl0s[names(sl0) == "A"]

bqrs <- bqr1$seqres
bqr <- bqrs[names(bqr) == "A"]

#filter Kinase
fpus <- fpu1$seqres
fpu <- fpus[names(fpu) == "A"]

qgy <- qgy1$seqres
qgy <- qgys[names(qgy) == "A"]

r7g <- r7g1$seqres
r7g <- r7gs[names(r7g) == "A"]

