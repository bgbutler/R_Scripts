library(rJava)
library(RWeka)


# special code for local repos

local({
    r <- getOption("repos")
    r["CRAN"] <- "https://cran.r-project.org/"
    options(repos = r)
})