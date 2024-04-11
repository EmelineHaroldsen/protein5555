#clean code here
library(bio3d)
library(tidyverse)
huo <- read.pdb("data/Pol/1huo.pdb")
sl0 <- read.pdb("data/Pol/1sl0.pdb")
bqr <- read.pdb("data/Pol/2bqr.pdb")

# Filter 'atom' to only include entries where the atom type is 'SEQRES'
seq <- huo$seqres
filtered_seq <- seq[names(seq) == "A"]


seq <- sl0$seqres
filtered_seq <- seq[names(seq) == "A"]

seq <- bqr$seqres
filtered_seq <- seq[names(seq) == "A"]

