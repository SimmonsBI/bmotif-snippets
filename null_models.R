library(bmotif)
library(vegan)

# generate null networks from your original network
nm <- vegan::nullmodel(network, "curveball") 
sm1 <- simulate(nm, nsim = 1000)

# calculate motifs for all null networks
null_motifs <- do.call("rbind", lapply(1:dim(sm1)[3], function(x) mcount(M = sm1[,,x], size_node = TRUE, normalise = FALSE)$frequency))

# calculate motifs for observed network
observed_motifs <- mcount(network, size_node = TRUE, normalise = FALSE)$frequency

# calculate over/under-representation of motifs using z-scores
(observed_motifs - apply(null_motifs, 2, "mean"))/apply(null_motifs, 2, "sd")
