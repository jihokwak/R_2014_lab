rm(list = ls())
getwd()
setwd('/Users/jimanryu/Desktop/R')
getwd()
dir()

library(ggthemes)
dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
(qplot(carat, price, data = dsamp, colour = clarity) + theme_solarized() + scale_colour_solarized("blue"))


## Dark version
(qplot(carat, price, data = dsamp, colour = clarity) + theme_solarized(light = FALSE) + 
   scale_colour_solarized("blue"))


dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
q <- (qplot(carat, price, data = dsamp, colour = clarity) + ggtitle("Diamonds Are Forever"))

## Standard
q + theme_economist() + scale_colour_economist()


## Stata colors
q + theme_economist(stata = TRUE) + scale_colour_economist(stata = TRUE)

## Darker plot region
q + theme_economist(dkpanel = TRUE) + scale_colour_economist(stata = TRUE)

## Darker plot region is best for for facets
dkblue <- ggthemes_data$economist$fg["blue_dark"]
(ggplot(data = dsamp, aes(x = carat, y = price)) + geom_point(colour = dkblue) + 
   facet_grid(. ~ cut) + theme_economist(dkpanel = TRUE))


##' ## Change axis lines to vertical
(q + theme_economist(horizontal = FALSE) + scale_colour_economist() + coord_flip())

## White panel/light gray background
(q + theme_economist_white() + scale_colour_economist())

## All white variant
(q + theme_economist_white(gray_bg = FALSE) + scale_colour_economist())


## Not run: The Economist uses ITC Officina Sans



dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
# Old line color palette
(qplot(carat, price, data = dsamp, colour = clarity) + theme_excel() + scale_colour_excel())


# Old fill color palette
(ggplot(diamonds, aes(clarity, fill = cut)) + geom_bar() + scale_fill_excel("fill") + 
   theme_excel())

dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
(qplot(carat, price, data = dsamp, colour = clarity) + theme_few() + scale_colour_few())

(qplot(carat, price, data = dsamp, colour = clarity) + theme_few() + scale_colour_few("dark"))

(ggplot(diamonds, aes(clarity, fill = cut)) + geom_bar() + theme_few() + scale_fill_few("light"))

dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
(d <- qplot(carat, price, data = dsamp, colour = clarity) + theme_igray())


dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
q1 <- (qplot(carat, price, data = dsamp, colour = clarity) + ggtitle("Diamonds"))
q2 <- (qplot(carat, price, data = dsamp) + facet_wrap(~clarity) + ggtitle("Diamonds"))
q1mono <- (qplot(carat, price, shape = clarity, color = clarity, data = dsamp) + 
             scale_shape_stata() + ggtitle("Diamonds"))
## s2color
(q1 + theme_stata() + scale_colour_stata("s2color"))


(q2 + theme_stata())


## s2mono
(q1mono + theme_stata("s2mono") + scale_colour_stata("mono"))

(q2 + theme_stata("s2mono"))

## s1color
(q1 + theme_stata("s1color") + scale_colour_stata("s1color"))

(q2 + theme_stata("s1color"))

## Not run: s1rcolor
(q1 + theme_stata("s1rcolor") + scale_colour_stata("s1rcolor"))


(ggplot(dsamp, aes(x = carat, y = price)) + geom_point(colour = "white") + facet_wrap(~clarity) + 
   scale_colour_stata("s1rcolor") + ggtitle("Diamonds"))


## s1mono
(q1mono + theme_stata("s1mono") + scale_colour_stata("mono"))

(q2 + theme_stata("s1mono"))


# with ticks and range frames
(ggplot(mtcars, aes(wt, mpg)) + geom_point() + geom_rangeframe() + theme_tufte())


# with geom_rug
(ggplot(mtcars, aes(wt, mpg)) + geom_point() + geom_rug() + theme_tufte(ticks = FALSE))


## Not run: Using the Bembo serif family
library(extrafont)
(ggplot(mtcars, aes(wt, mpg)) + geom_point() + geom_rangeframe() + theme_tufte(base_family = "BemboStd"))


## Using the Gill Sans sans serif family
(ggplot(mtcars, aes(wt, mpg)) + geom_point() + geom_rangeframe() + theme_tufte(base_family = "GillSans"))


(qplot(hp, mpg, data = mtcars, geom = "point") + scale_colour_wsj("colors6", 
                                                                  "") + ggtitle("Diamond Prices") + theme_wsj())


## Use a gray background instead
(qplot(hp, mpg, data = mtcars, geom = "point") + scale_colour_wsj("colors6", 
                                                                  "") + ggtitle("Diamond Prices") + theme_wsj(color = "gray"))


