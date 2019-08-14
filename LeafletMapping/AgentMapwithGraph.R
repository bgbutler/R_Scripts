
library(ggplot2)

p <- ggplot(Jun, aes(x = Auto, y = Home)) + geom_point(col = "orange", pch = 20, cex = 2) + 
  geom_smooth(method = 'lm', formula = y~x) + xlim(0,1500)



p <- xyplot(Home ~ Auto, data = Jun, col = "orange", pch = 20, cex = 2)


p <- mget(rep("p", length(Jun)))

clr <- rep("orange", length(Jun))
p <- lapply(1:length(p), function(i) {
  clr[i] <- "dark green"
  update(p[[i]], col = clr)
})



m1 <- leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = 'Image')  %>%
  setView(lng = -72, lat = 41, zoom = 8) %>%
  
  
  # june - Home - compare
  addCircleMarkers(data = Jun, lat = ~Lat, lng = ~Lon,
                   color = ~beatCol(BeatHomeLvl), popup = popupGraph(p),
                   radius = ~sqrt(BeatHome*50), group = 'Home - Jun') %>%
  
  
  # layer control
  addLayersControl(
    overlayGroups = c('Home - Jun'
    ),
    options = layersControlOptions(collapsed = F)
  ) %>%
  
  
  
  # legend for compare to average
  
  addLegend('bottomright', pal = beatCol, values = last$BeatTotalLvl,
            title = 'Compare Home<br>Quote Count to<br>3Mos State Avg',
            opacity = 1)

m1