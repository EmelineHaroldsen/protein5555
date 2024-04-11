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
huo <- huo1$seqres
filt_huo <- huo[names(huo) == "A"]

sl0 <- sl01$seqres
filt_sl0 <- sl0[names(sl0) == "A"]

bqr <- bqr1$seqres
filt_bqr <- bqr[names(bqr) == "A"]

#filter Kinase
fpu <- fpu1$seqres
filt_fpu <- fpu[names(fpu) == "A"]

qgy <- qgy1$seqres
filt_qgy <- qgy[names(qgy) == "A"]

r7g <- r7g1$seqres
filt_r7g <- r7g[names(r7g) == "A"]

