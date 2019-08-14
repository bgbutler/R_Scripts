library(lattice)
library(sp)

data(meuse)
coordinates(meuse) <- ~x+y
proj4string(meuse) <- CRS("+init=epsg:28992")

p <- xyplot(copper ~ cadmium, data = meuse@data, col = "grey", pch = 20, cex = 2)
p <- mget(rep("p", length(meuse)))

clr <- rep("grey", length(meuse))
p <- lapply(1:length(p), function(i) {
  clr[i] <- "red"
  update(p[[i]], col = clr)
})


mapview(meuse,
        zcol = "cadmium",
        popup = popupGraph(p))