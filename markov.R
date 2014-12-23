library(markovchain)
require(markovchain)
library(igraph)
require(igraph)

mcDefault <- new("markovchain", states = c("I", "NI", "D"),
                 transitionMatrix = matrix(data = c(0.75, 0.15, 0.10,
                                                    0.20, 0.65, 0.15,
                                                    0.0, 0.0 , 1.0), byrow = TRUE, nrow = 3),
                                           name = "Default")

initialState <- c(1,0,0)

mcDf <- as(mcDefault, "data.frame")
mcNew <- as(mcDf, "markovchain")


defaultStates <- rmarkovchain(n = 100, object = mcDefault, t0 = "I")
defaultFittedMLE <- markovchainFit(data = defaultStates, method = "mle", name = "DefaultMLE")
defaultFittedMLE$estimate

mcIgraph <- as(mcDf, "igraph")
plot.igraph(mcIgraph)

