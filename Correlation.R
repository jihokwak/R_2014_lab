library(corrplot)
mcor <- cor(mtcars)
round(mcor, 2) 
corrplot(mcor, method='shade', shade.col=NA, tl.col='black', tl.srt=45)