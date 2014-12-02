library(maps)
# 본토에서 떨어진 Alaska, Hawaii 데이터 제외
sub.usa <- subset(USArrests,!rownames(USArrests) %in% c("Alaska", "Hawaii")) 
# 주이름, 폭행범 수를 갖는 데이터 프레임 생성
usa.data <- data.frame(states=rownames(sub.usa), 
                       Assault=sub.usa$Assault) 
# 범례 데이터 생성
col.level <- cut(sub.usa[, 2], c(0, 100, 150, 200, 250, 300, 350))
legends <- levels(col.level) 
# 주이름, 폭행범 수, 색상을 갖는 데이터 프레임 생성
levels(col.level) <- sort(heat.colors(6), decreasing=TRUE)
usa.data <- data.frame(usa.data, col.level=col.level) 
# Map 데이터 출력
map('state', region=usa.data$states, fill=TRUE, 
    col=as.character(usa.data$col.level))
title("USA Assault map")
legend(-76, 35, legends, fill=sort(heat.colors(6), decreasing=TRUE), 
       cex=0.7)

