rm(list = ls())
getwd()
setwd('/Users/jimanryu/Desktop/R')
getwd()
dir()

require(googleVis)
G <- gvisGeoChart(Exports, "Country", "Profit", options = list(width = 250, 
                                                               height = 120))
B <- gvisBarChart(Exports[, 1:2], yvar = "Profit", xvar = "Country", options = list(width = 250, 
                                                                                    height = 260, legend = "none"))
M <- gvisMotionChart(Fruits, "Fruit", "Year", options = list(width = 400, height = 380))
GBM <- gvisMerge(gvisMerge(G, B, horizontal = FALSE), M, horizontal = TRUE, 
                 tableOptions = "cellspacing=5")
cat(GBM$html$chart, file = "test10.html")