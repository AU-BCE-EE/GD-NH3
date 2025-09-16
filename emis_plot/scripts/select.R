# Select field plots for plotting

set.seed(2)
n <- 7
names(pdat)
psub <- pdat[n.ints > 20 & ct.max < 200, ]
samp <- sample(psub$pmid, n)
isub <- idat[pmid %in% samp, ] 
