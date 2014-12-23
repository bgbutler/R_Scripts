library(markovchain)

#create matrix

mcCredit <-new("markovchain", states = c("active", "prepay", "default"),
               transitionMatrix = matrix(c(0.9,0.05,0.05,1.0,0.0,0.0,0.05, 0.0, 0.95),
                                         byrow = TRUE, nrow = 3))

lgdVector <- as.matrix(c(0,500, 1000))

t0 <- t(as.matrix(c(1,0,0)))
t1 <- t0 * mcCredit
t2 <- t1 * mcCredit
t3 <- t2 * mcCredit

PVFB <- t0 %*% lgdVector * 1.02 ^ -0 + t1 %*% lgdVector * 1.02^-1 + t2 %*% lgdVector * 1.02^-2 + 
  t3 %*% lgdVector * 1.02^-3

# annual mortage payment
P <- PVFB/(t0[1]* 1.02^-0 + t1[1]* 1.02^-1 + t2[1]*1.02^-2)

