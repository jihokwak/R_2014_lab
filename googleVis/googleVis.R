rm(list = ls())
getwd()
setwd('/Users/jimanryu/Desktop/R')
getwd()
dir()

install.packages("ggplot2")
install.packages("sqldf")
install.packages("gridExtra")
require(ggplot2)
require(sqldf)
require(gridExtra)


head(diamonds)

qplot(cut, data = diamonds, geom = "bar")

qplot(cut, data = diamonds, color = color, geom = "bar")

qplot(cut, data = diamonds, fill = color, geom = "bar")

qplot(cut, data = diamonds, fill = color, geom = "bar", position = "dodge")

qplot(cut, data = diamonds, fill = color, geom = "bar", position = "dodge") + 
  facet_grid(color ~ .)

qplot(cut, data = diamonds, fill = color, geom = "bar", position = "dodge") + 
  facet_grid(color ~ .) + coord_flip()

plot.1 <- qplot(cut, data = diamonds, fill = color, geom = "bar", position = "dodge")
plot.1 + facet_grid(color ~ .) + coord_flip()


plot.2 <- plot.1 + facet_grid(color ~ .) + coord_flip()
plot.2 + scale_fill_manual(values = c("#ffffcc", "#c7e9b4", "#7fcdbb", "#41b6c4", 
                                      "#1d91c0", "#225ea8", "#0c2c84"))

plot.2 + scale_fill_brewer(palette = "YlGnBu")

# If you want to view all the pre-loaded color pallates just run the
# following command:
RColorBrewer::display.brewer.all()

qplot(carat, price, data = diamonds)

# now let's add color using the `color` argument
plot.3 <- qplot(carat, price, data = diamonds, color = color)
plot.3

# let's change the color
plot.3 + scale_colour_brewer(palette = "Spectral", name = "Diamond Color")
# now let's add axis titles
plot.4 <- plot.3 + scale_colour_brewer(palette = "Spectral", name = "Diamond Color") + 
  xlab("Diamond Carat") + ylab("Diamond Price") + ggtitle("Diamonds are Expensive")
plot.4

# add trend lines and shade by using `geom_smooth()` argument default
# method=`gam` when n>1000, otherwise, method=`loess`
plot.4 + geom_smooth()

qplot(price, geom = "density", data = diamonds, colour = color) + facet_grid(color ~ 
                                                                               .)

qplot(color, price, geom = "violin", data = diamonds, colour = color)
# here we are combining 2 types of charts using the concatenate function to
# create a vector of geoms for our argument notice how the second argument
# appears on top of the first
qplot(color, price, geom = c("jitter", "violin"), data = diamonds, colour = color)

# qplot(carat,price, data=diamonds)
ggplot(data = diamonds, aes(x = carat, y = price)) + geom_point()
# plot.3 <- qplot(carat,price, data=diamonds, color=color)
plot.5 <- ggplot(data = diamonds, aes(x = carat, y = price)) + geom_point(aes(colour = color))
plot.5

# plot.3 + scale_colour_brewer(palette='Spectral',name='Diamond Color')
plot.5 + scale_colour_brewer(palette = "Spectral", name = "Diamond Color")

# qplot(price, geom='density', data=diamonds, colour=color) +
# facet_grid(color~.)
ggplot(data = diamonds) + geom_density(aes(x = price, colour = color)) + facet_grid(color ~ 
                                                                                      .)


# let's go back to the multiple geoms concept we haven't covered 'subset'
# yet but it's a simple way to isolate specific data points
ggplot(data = diamonds, aes(x = carat, y = price)) + geom_point(aes(colour = color)) + 
  stat_smooth(data = subset(diamonds, color == "I"), colour = "white", fill = "grey50") + 
  stat_smooth(data = subset(diamonds, color == "E"), method = "lm", colour = "black", 
              fill = "white")
## geom_smooth: method="auto" and size of largest group is >=1000, so using gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the smoothing method.

# qplot(color, price, geom='violin', data=diamonds, colour=color)
ggplot(data = diamonds, aes(x = color, y = price)) + geom_violin(aes(colour = color))

# qplot(color, price, geom=c('jitter','violin'), data=diamonds,
# colour=color)
ggplot(data = diamonds, aes(x = color, y = price)) + geom_jitter(aes(colour = color)) + 
  geom_violin(aes(colour = color))

# Let's pull in a US Open Dataset You'll have to set your working directory
# to read in the file I sent over which will be different for each of you
# depending on where you downloaded it
setwd("/Users/jkraut/Documents/@Code/R_Code/Training")
tennis <- read.csv("spa_train_data.csv", header = TRUE)
head(tennis)

# Our goal here is to explore how the number of aces for a particular player
# relates to their fastest serve & serve accuracy
ggplot(data = tennis, aes(x = firstServe1, y = ace1, size = fastServe1)) + geom_point(colour = "#2C7FB8")
# when you get a plot like this, it's always a good idea to check the
# variable type.  It's actually usually a good idea to do this prior to
# plotting the easiest way is by utilizing the `str` function which stands
# for structure
str(tennis)

# alternatively you can just use the class funciton for a single variable
class(tennis$firstServe1)

# so how do we convert this to a numeric? There are lots of ways but I think
# this is the easiest:
tennis$firstServe1.a <- as.numeric(sub("%", "", tennis$firstServe1))
head(tennis$firstServe1.a)

# Now lets plot again
ggplot(data = tennis, aes(x = firstServe1.a, y = ace1, size = fastServe1)) + 
  geom_point(colour = "#99d8c9")



# now save base plot and we'll add layers
plot.1 <- ggplot(data = tennis, aes(x = firstServe1.a, y = ace1, size = fastServe1)) + 
  geom_point(colour = "#99d8c9")
plot.1 + xlab("1st Serve Accuracy %") + ylab("# of Aces") + ggtitle("US Open 1st Serve Dynamics") + 
  scale_size("Top Speed of First Serve") + theme(panel.grid = element_blank(), 
                                                 axis.title = element_text(size = 9, face = "bold"), plot.title = element_text(size = 10, 
                                                                                                                               face = "bold"))

# Let's change the shading of the color to match serve speed and add in some
# more style
plot.2 <- ggplot(data = tennis, aes(x = firstServe1.a, y = ace1, size = fastServe1)) + 
  geom_point(aes(colour = fastServe1))
plot.2 + xlab("1st Serve Accuracy %") + ylab("# of Aces") + ggtitle("US Open 1st Serve Dynamics") + 
  scale_size("Top Speed of First Serve") + scale_colour_gradient("Top Speed of First Serve", 
                                                                 low = "#fff5f0", high = "#cb181d") + theme(panel.background = element_rect(fill = "black"), 
                                                                                                            panel.grid = element_blank(), axis.title = element_text(size = 9, face = "bold"), 
                                                                                                            plot.title = element_text(size = 10, face = "bold"))

require(ggthemes)
# bulid a plot and store it in g.1
g.1 <- ggplot(diamonds, aes(x = carat, y = price)) + geom_point(aes(colour = color))

# try some themes
g.1 + theme_economist() + scale_color_economist()
# yup, some people still like Excel 2003 ...
g.1 + theme_excel() + scale_colour_excel()
# certain themes require a specified palette in which case you can use the
# following command to view the pallates `ggthemes_data$theme.name$palettes`
# let's try this for the WSJ
ggthemes_data$wsj$palettes
g.1 + theme_wsj() + scale_colour_wsj("colors6")
g.1 + theme_igray() + scale_colour_tableau("colorblind10")
# nice tribute here ...
g.1 + theme_gdocs() + scale_colour_gdocs()



# Set up the trial dataset
data <- NULL
data$x <- seq(1, 10, 0.1)
data$y1 <- sin(data$x)
data$y2 <- cos(data$x)
data$xaxis <- -1.5

data <- as.data.frame(data)

# Let's make the XKCD theme
theme_xkcd <- theme(panel.background = element_rect(fill = "white"), axis.ticks = element_line(colour = NA), 
                    panel.grid = element_line(colour = "white"), axis.text.y = element_text(colour = NA), 
                    axis.text.x = element_text(colour = "black"), text = element_text(size = 16, 
                                                                                      family = "sans"))

# Plot the chart
p <- ggplot(data = data, aes(x = x, y = y1)) + geom_line(aes(y = y2), position = "jitter") + 
  geom_line(colour = "white", size = 3, position = "jitter") + geom_line(colour = "red", 
                                                                         size = 1, position = "jitter") + geom_text(family = "sans", x = 6, y = -1.2, 
                                                                                                                    label = "A SIN AND COS CURVE") + geom_line(aes(y = xaxis), position = position_jitter(h = 0.005), 
                                                                                                                                                               colour = "black") + scale_x_continuous(breaks = c(2, 5, 6, 9), labels = c("YARD", 
                                                                                                                                                                                                                                         "STEPS", "DOOR", "INSIDE")) + labs(x = "", y = "") + theme_xkcd

p


require(gridExtra)
g.1 <- ggplot(diamonds, aes(x = carat, y = price)) + geom_point(aes(colour = color))
g.2 <- g.1 + theme_economist() + scale_color_economist()
g.3 <- g.1 + theme_excel() + scale_colour_excel()
g.4 <- g.1 + theme_wsj() + scale_colour_wsj("colors6")
g.5 <- g.1 + theme_gdocs() + scale_colour_gdocs()

grid.arrange(g.2, g.3, g.4, g.5, ncol = 2)


# using the ncol and nrow arguments allows you to customize your grid
grid.arrange(g.1, g.2, g.3, g.4, g.5, g.1, ncol = 2, nrow = 3)

require(sqldf)
wine <- read.table("http://www.jaredlander.com/data/wine.csv", header = TRUE, 
                   sep = ",")
# now lets look at the data
head(wine)

# A cultivar[nb 1] is a plant or grouping of plants selected for desirable
# characteristics that can be maintained by propagation We'll use this
# column in some of our sqldf queries
unique(wine$Cultivar)


# lets start with simple select *
sqldf("SELECT * 
      FROM wine 
      limit 3")
## Loading required package: tcltk

sqldf("SELECT 
        COUNT(*) 
      FROM wine")


# now lets combine with a group
sqldf("SELECT 
        Cultivar,
        COUNT(*) 
      FROM wine 
      GROUP BY Cultivar")


# WHERE
sqldf("SELECT 
        Cultivar, 
        COUNT(*) 
      FROM wine 
      WHERE Magnesium > 100 
      GROUP BY Cultivar")

# ALIASING
sqldf("SELECT 
        Cultivar, 
        AVG(Malic_acid) as Average_MA, 
        MAX(Magnesium) as MAX_Mag 
      FROM wine 
      GROUP BY Cultivar")


# SIMPLE JOIN
# first we need to save our query results
r1 <- sqldf("SELECT 
              Cultivar,
              COUNT(*) as Mag_100p 
            FROM wine 
            WHERE Magnesium > 100 
            GROUP BY Cultivar")

r2 <- sqldf("SELECT 
              Cultivar, 
              COUNT(*) as Mag_100n 
            FROM wine 
              WHERE Magnesium < 100 
              GROUP BY Cultivar")
# now join
sqldf("SELECT
        r1.Cultivar,
        r1.Mag_100p,
        r2.Mag_100n
      FROM r1 
      LEFT JOIN r2
        ON r1.Cultivar = r2. Cultivar")

# CASE STATEMENTS
sqldf("SELECT Cultivar,
        SUM(CASE WHEN Magnesium > 100 THEN 1 ELSE 0 END) AS Mag100p,
        SUM(CASE WHEN Magnesium < 100 THEN 1 ELSE 0 END) AS Mag100n
      FROM wine
      GROUP BY Cultivar")


# Example 1
# start by reminding ourselves what the variable names are for the data set
colnames(tennis)


# next we need to check country names so we know how to limit.  You can do this a few ways, but I prefer the former since it sorts for you:
levels(tennis$country1)

unique(tennis$country1)

# remember that we have to make our % variable numeric; this time we'll just overwrite it for both variables 
tennis$firstServe1 <- as.numeric(sub("%", "", tennis$firstServe1))
tennis$firstServe2 <- as.numeric(sub("%", "", tennis$firstServe2))
# next we'll use sqldf to subset the data to players 
data.final <- sqldf("SELECT
                      tennis.firstServe1 as serve1_acc,
                      tennis.ace1 as aces,
                      tennis.fastServe1 as serve1_fast,
                      tennis.country1 as country
                    FROM tennis
                    WHERE tennis.country1 IN ('BEL', 'ESP', 'USA') 
                    UNION ALL
                    SELECT
                      tennis.firstServe2,
                      tennis.ace2,
                      tennis.fastServe2,
                      tennis.country2
                    FROM tennis
                    WHERE tennis.country2 IN ('BEL', 'ESP', 'USA')")
## Loading required package: tcltk
head(data.final)

# now let's plot aces x serve accuracy, size by top serve speed, and color by country 
plot.3 <- ggplot(data = data.final, aes(x = serve1_acc, y = aces, size = serve1_fast)) + geom_point(aes(colour = country))
plot.3 + xlab("1st Serve Accuracy %") + ylab("# of Aces") + ggtitle("US Open 1st Serve Dynamics for players hailing from BEL, ESP, USA") + 
  scale_size("Top Speed of First Serve") + scale_colour_brewer('Country', palette='Pastel1') +
  theme(panel.background = element_rect(fill = "black"), 
        panel.grid = element_blank(), axis.title = element_text(size = 9, face = "bold"), 
        plot.title = element_text(size = 10, face = "bold"))


# what if you want to grid this?
plot.4 <- plot.3 + xlab("1st Serve Accuracy %") + ylab("# of Aces") + ggtitle("US Open 1st Serve Dynamics for players hailing from BEL, ESP, USA") + 
  scale_size("Top Speed of First Serve") + scale_colour_brewer('Country', palette='Pastel1') +
  theme(panel.background = element_rect(fill = "black"), 
        panel.grid = element_blank(), axis.title = element_text(size = 9, face = "bold"), 
        plot.title = element_text(size = 10, face = "bold"))


# OR
plot.4 + facet_wrap( ~ country, nrow=3)

# Load up the car package
require("car")
# read in the data file
health <- read.csv("spa_train_data2.csv", header = TRUE)
# checkout data struct

# create scatter plot matrix, but subset data to remove variables that don't
# make sense
scatterplotMatrix(health[, -c(5, 7)], diagonal = "none", smoother = F, reg.line = lm)

# with smoother
scatterplotMatrix(health[, -c(5, 7)], diagonal = "none", reg.line = lm)



# must load in a few things before you can use r-charts
require(devtools)
install_github("rCharts", "ramnathv", ref = "dev")
# load the package
library(rCharts)

# utility function to add required assets such as CSS and JS libraries
add_lib_assets <- function(lib, cdn = F, css = NULL) {
  assets = get_assets(get_lib(lib), cdn = cdn)
  if (!is.null(css)) {
    assets$css = c(assets$css, css)
  }
  styles <- lapply(assets$css, function(style) {
    sprintf("<link rel='stylesheet' href=%s>", style)
  })
  
  scripts <- lapply(assets$jshead, function(script) {
    sprintf("<script type='text/javascript' src=%s></script>", script)
  })
  cat(paste(c(styles, scripts), collapse = "\n"))
}



# get assets from online repositories
add_lib_assets("NVD3", cdn = TRUE, css = "http://rawgithub.com/ramnathv/rCharts/master/inst/libraries/nvd3/css/rNVD3.css")
add_lib_assets("Polycharts", cdn = TRUE)
# NVD3

# histograms
data(iris)
sepalw <- iris[, c(1, 5)]
hst = hist(sepalw[, 1], plot = FALSE, breaks = 20)

data = by(sepalw, sepalw$Species, function(x) data.frame(mid = hst$mids, counts = hist(x[, 
                                                                                         1], breaks = hst$breaks, plot = FALSE)$counts, Species = rep(x[1, 2], length(hst$breaks) - 
                                                                                                                                                        1)))
data = do.call("rbind", data)

# now that data is cleaned, make chart
n3 <- nPlot(counts ~ mid, data = data, type = "multiBarChart", group = "Species")
n3$xAxis(axisLabel = "Sepal.Width")
n3$yAxis(axisLabel = "counts")
n3$chart(color = c("red", "blue", "green"))
n3$print("nvd3Hist", include_assets = TRUE)


## Poly Charts

# simple scatter plot
names(iris) = gsub("\\.", "", names(iris))
r1 <- rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = "Species", 
            type = "point")


# Multi barchart
hair_eye = as.data.frame(HairEyeColor)
p2 <- rPlot(Freq ~ Hair, color = "Eye", data = hair_eye, type = "bar")
p2$facet(var = "Eye", type = "wrap", rows = 2)
p2$save("test1.html", cdn = TRUE)

# Example 3
require(plyr)
dat = count(mtcars, .(gear, am))
p4 <- rPlot(x = "bin(gear, 1)", y = "freq", data = dat, type = "bar", list(var = "am", 
                                                                           type = "wrap"))
p4$save("test3.html", cdn = TRUE)

# Example 4
dat = expand.grid(x = 1:5, y = 1:5)
dat = transform(dat, value = sample(1:5, 25, replace = T))
p5 <- rPlot(x = "bin(x, 1)", y = "bin(y, 1)", color = "value", data = dat, type = "tile")
p5$save("test4.html", cdn = TRUE)


# Example 5
require(reshape2)
require(scales)
require(plyr)
nba <- read.csv("http://datasets.flowingdata.com/ppg2008.csv")
nba.m <- ddply(melt(nba), .(variable), transform, rescale = rescale(value))
## Using Name as id variables
p6 <- rPlot(Name ~ variable, color = "rescale", data = nba.m, type = "tile", 
            height = 600)
p6$guides("{color: {scale: {type: gradient, lower: white, upper: steelblue}}}")
p6$save("test5.html", cdn = TRUE)


## Now let's check out Highcharts

# Example 7
p7 <- hPlot(Pulse ~ Height, data = MASS::survey, type = "scatter", group = "Exer")
p7$save("test7.html", cdn = TRUE)

# Example 8
a <- hPlot(Pulse ~ Height, data = MASS::survey, type = "bubble", title = "bubble demo", 
           subtitle = "bubble chart", size = "Age", group = "Exer")
a$chart(zoomType = "xy")
a$save("test8.html", cdn = TRUE)

require(googleVis)
G <- gvisGeoChart(Exports, "Country", "Profit", options = list(width = 250, 
                                                               height = 120))
B <- gvisBarChart(Exports[, 1:2], yvar = "Profit", xvar = "Country", options = list(width = 250, 
                                                                                    height = 260, legend = "none"))
M <- gvisMotionChart(Fruits, "Fruit", "Year", options = list(width = 400, height = 380))
GBM <- gvisMerge(gvisMerge(G, B, horizontal = FALSE), M, horizontal = TRUE, 
                 tableOptions = "cellspacing=5")
cat(GBM$html$chart, file = "test10.html")