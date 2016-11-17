
.libPaths(c("C:\\Users\\n846490\\Documents\\R", .libPaths()))

library("devtools")

install.packages("plotly")

library(plotly)

p <- plot_ly(midwest, x = percollege, color = state, type = "box")
p

set.seed(100)
d <- diamonds[sample(nrow(diamonds), 1000), ]

p <- ggplot(data = d, aes(x = carat, y = price)) +
    geom_point(aes(text = paste("Clarity:", clarity)), size = 4) +
    geom_smooth(aes(colour = cut, fill = cut)) + facet_wrap(~ cut)

(gg <- ggplotly(p))