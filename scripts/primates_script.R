install.packages("adegenet")
install.packages("phangorn")
install.packages("rentrez")
install.packages("BiocManager")
BiocManager::install("ggtree")
install.packages("rphylopic")

library(stats)
library(ade4)
library(ape)
library(adegenet)
library(phangorn)
library(tidyverse)
library(here)
library(rentrez)
library(ggtree)
library(rphylopic)

species <-scan(file = "species.txt", what = "character", sep = "\n", comment.char = "#") #import list of species to be analyzed

fasta_seqs <- entrez_fetch(db = "nucleotide",
             id = species,
             rettype = "fasta") #retrieve the fasta sequences of the mitochondrial genome of every species in the list

write(fasta_seqs, 
      file="temp")

grep("^*$", readLines("temp"), invert = TRUE, value = TRUE) %>% write(file = "sequences.fasta") #remove empty lines so read.FASTA works

file.remove("temp")

dna <- read.FASTA("sequences.fasta", type = "DNA") #store the sequences in a vector of the type "DNAbin"

dna_aligned <- clustalomega(dna, exec = here("clustalo")) #use clustal omega 1.2.3 to perform multiple sequence alignment

saveRDS(dna_aligned, "dna_aligned.RDS")
dna_aligned <- readRDS("dna_aligned.RDS")

dna_dist <- dist.dna(dna_aligned, model = "TN93") #calculate genetic distances and store in a vector of the type "dist"

treebionj <- bionj(dna_dist) #create a tree of the class "phylo" using the method neighbor joining

ggtree(treebionj, branch.length = "none", ladderize = F)+ #visualize the tree as a cladogram
  theme_tree()+
  geom_tiplab(size = 6, as_ylab = T)+
  labs(title = "Unrooted neighbor joining phylogenetic tree of\nmitochondrial genomes of 15 primates")

dna_phydat <- as.phyDat(dna_aligned)

modeltest <- modelTest(dna_phydat)

optmodel <- modeltest$Model[modeltest$AIC==min(modeltest$AIC)]
saveRDS(optmodel, "optmodel")
optmodel <- readRDS("optmodel")

mlparsed <- pml(treebionj, dna_phydat)
optmodel <- gsub("\\+.*","", optmodel)
mlparsedoptim <- optim.pml(mlparsed, optNni=TRUE, model = optmodel)

treeml <- ladderize(mlparsedoptim$tree) 

ggtree(treeml, branch.length = "none", ladderize = F)+ #visualize the tree as a cladogram
  theme_tree()+
  geom_tiplab(size = 6, as_ylab = T)+
  labs(title = "Unrooted maximum likelihood phylogenetic tree of\nmitochondrial genomes of 15 primates")

rootedml <- root(treeml, 
                 outgroup = "NC_013276.1 Mesocricetus auratus mitochondrion, complete genome", 
                 resolve.root = TRUE)

rootedml$tip.label <- gsub(".*\\.[12]", "", rootedml$tip.label)
rootedml$tip.label <- gsub("(mitochondrion, | isolate | voucher).*genome", "", rootedml$tip.label)

ggtree(rootedml, ladderize = F)+ #visualize the tree as a cphylogenetic tree
  theme_tree()+
  geom_tiplab(size = 2.5, as_ylab = F)+
  xlim(0, 0.35)+
  labs(title = "Rooted maximum likelihood phylogenetic tree of\nmitochondrial genomes of 15 primates")




