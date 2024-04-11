#clean code here
library(bio3d)
library(tidyverse)
huo <- read.pdb("data/Pol/1huo.pdb")
sl0 <- read.pdb("data/Pol/1sl0.pdb")
bqr <- read.pdb("data/Pol/2bqr.pdb")

fpu <- read.pdb("data/Kinase/1fpu.pdb")
qgy <- read.pdb("data/Kinase/8qgy.pdb")
r7g <- read.pdb("data/Kinase/8r7g.pdb")

# Filter POL to find chain A
seq <- huo$seqres
filtered_seq <- seq[names(seq) == "A"]

seq <- sl0$seqres
filtered_seq <- seq[names(seq) == "A"]

seq <- bqr$seqres
filtered_seq <- seq[names(seq) == "A"]

seq <- fpu$seqres
filtered_seq <- seq[names(seq) == "A"]

seq <- qgy$seqres
filtered_seq <- seq[names(seq) == "A"]

seq <- r7g$seqres
filtered_seq <- seq[names(seq) == "A"]
