# #clean code here
# huo1 <- bio3d::read.pdb("data-raw/Pol/1huo.pdb")
# sl01 <- bio3d::read.pdb("data-raw/Pol/1sl0.pdb")
# bqr1 <- bio3d::read.pdb("data-raw/Pol/2bqr.pdb")
#
# fpu1 <- bio3d::read.pdb("data-raw/Kinase/1fpu.pdb")
# qgy1 <- bio3d::read.pdb("data-raw/Kinase/8qgy.pdb")
# r7g1 <- bio3d::read.pdb("data-raw/Kinase/8r7g.pdb")
#
# #overall generic function
# cleanser <- function(pdb_location){
#   file <- bio3d::read.pdb(pdb_location)
#   seq <- file$seqres
#   filt_seq <- seq[names(seq) == "A"]
#   filt_seq <- as.vector(filt_seq)
# }
#
#
# # Filter POL to find chain A
# huos <- huo1$seqres
# huo <- huos[names(huos) == "A"]
#
# sl0s <- sl01$seqres
# sl0 <- sl0s[names(sl0s) == "A"]
#
# bqrs <- bqr1$seqres
# bqr <- bqrs[names(bqrs) == "A"]
#
# #filter Kinase
# fpus <- fpu1$seqres
# fpu <- fpus[names(fpus) == "A"]
#
# qgys <- qgy1$seqres
# qgy <- qgys[names(qgys) == "A"]
#
# r7gs <- r7g1$seqres
# r7g <- r7gs[names(r7gs) == "A"]
#
