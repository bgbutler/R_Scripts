require(markovchain)
stateNames = c("A", "PP", "D")
Q0 <- new("markovchain", states = stateNames, 
          transitionMatrix =matrix(c(0.92, 0.02, 0.06,1.0, 0.0, 0.0,0.05, 0, .95),
                                   byrow = TRUE, nrow = 3), name = "state t0")
Q1 <- new("markovchain", states = stateNames, 
          transitionMatrix = matrix(c(0.95, 0.0, 0.05,1.0, 0.0, 0.0,0.02, 0, .98),
                                    byrow = TRUE, nrow = 3), name = "state t1")
Q2 <- new("markovchain", states = stateNames, 
          transitionMatrix = matrix(c(0.98, 0.0, 0.02,1.0, 0.0, 0.0,0, 0, 1),
                                    byrow = TRUE,nrow = 3), name = "state t2")
Q3 <- new("markovchain", states = stateNames,
          transitionMatrix = matrix(c(0.92, 0.02, 0.06,1.0, 0.0, 0.0,0.05, 0, .95
                                      , byrow = TRUE, nrow = 3), name = "state t3")
mcLoan <- new("markovchainList",markovchains = list(Q0,Q1,Q2,Q3), name = "Loans")
?''

loanStates <- rmarkovchain(n=1, object = mcLoan, t0 = "A", include.t0 = T)

