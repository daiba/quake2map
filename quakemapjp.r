#!/usr/bin/env rscript

library(ggplot2)
library(maps)
library(xts)

normDepth <- function(x){
     if (x > 700) return(700)
     if (x > 200) return(200)
     if (x > 100) return(100)
     if (x > 50 ) return(50)
     if (x > 30 ) return(30)
     if (x > 20 ) return(20)
     if (x > 10 ) return(10)
     0
}

normMagnitude <- function(x){
     if (x > 5) return(5)
     if (x > 4) return(4)
     if (x > 3) return(3)
     if (x > 2) return(2)
     1
}

catalog <- read.csv(
     "http://earthquake.usgs.gov/earthquakes/catalogs/eqs7day-M2.5.txt"
)
japan <- catalog[grep("Japan", catalog$Region),]
time <- strptime(japan$Datetime, "%A, %B %d, %Y %H:%M:%S UTC") + 60*60*9
Depth <- sapply(japan$Depth, normDepth)
Magnitude <- sapply(japan$Magnitude, normMagnitude)


quake <- cbind(time, japan[5:6], Depth, Magnitude)

xydata <- as.data.frame(
     map("world", xlim = c(120, 150), ylim = c(25, 50), plot = FALSE)
     [c("x", "y")]
)
map <- ggplot(xydata, aes(x, y))
map <- map + geom_path()

index <- 1

plotdata <- function(x){
      j <- data.frame(coredata(as.numeric(x$Lat)),
          coredata(as.numeric(x$Lon)),
          coredata(x$Magnitude),
          coredata(x$Depth)
     )
     colnames(j) <- c("Lat", "Lon", "Magnitude", "Depth")
     map.point <- map + geom_point(
          data = j,
          aes(x = Lon, y = Lat,
               size = factor(Magnitude, c(1, 2, 3, 4, 5, 6, 7, 8, 9)),
               colour = factor(Depth, c(0, 10, 20, 30, 50, 100, 200, 700))
          )
     )
     map.point <- map.point + scale_colour_discrete("Depth")
     map.point <- map.point + scale_size_discrete("Magnitude")
     png(filename = paste(sprintf("data/img%03d", index), "png", sep = "."))
     index <<- index + 1
     print(map.point)
     dev.off()
}

quake.xts <- as.xts(read.zoo(quake))
days <- split(quake.xts, "days")
for ( day in days ) plotdata(day)
