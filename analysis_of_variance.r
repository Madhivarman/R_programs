#average of covarience example
#library lattice is used
#loading data from external file source
dataURL <- "http://www.stat.ubc.ca/~jenny/notOcto/STAT545A/examples/gapminder/data/gapminderDataFiveYear.txt"
data <- read.delim(file = dataURL)

str(data)

#dropping the country Ocenia which only has two countries
data <- droplevels(subset(data,continent != "Oceania"))
str(data)

#after  running above command you can notice that observation is reduced
library(lattice)
#getting a scatter plot with xyplot
#save the image
png(filename = "lattice_point.png")
xyplot(lifeExp~gdpPercap,data,grid=TRUE)
dev.off()

#large the x-axis so data can be clearly viewed
png(filename = "lattice_point_x_greater_log.png")
xyplot(lifeExp~gdpPercap,data,grid=TRUE,scales=list(x=list(log=10)))
dev.off()

#simple  linear regression model now takes place
png(filename = "lattice_points_with_simplelinear.png")
xyplot(lifeExp~gdpPercap,data,grid=TRUE,
            scales=list(x=list(log=10,equispaced.log=FALSE)),
             type=c("p","r"),col.line="darkorange",lwd=3)
dev.off()

#group according to continents
png(filename = "lattice_points_grouped.png")
xyplot(lifeExp ~ gdpPercap, data,
       grid = TRUE,
       scales = list(x = list(log = 10, equispaced.log = FALSE)),
       group = continent, auto.key = TRUE)
dev.off()

#linear regression according to the continent group
png(filename = "lattice_points_grouped_lm.png")
xyplot(lifeExp ~ gdpPercap, data,
       grid = TRUE,
       scales = list(x = list(log = 10, equispaced.log = FALSE)),
       group = continent, auto.key = TRUE,type=c("p","smooth"),lwd=4)
dev.off()

#all scattering plot in single panel is hectic
#so we go to multiplanel

png(filename="lattice_points_multipanel.png")
xyplot(lifeExp~gdpPercap | continent,data,
       group = continent,
       grid = TRUE,
       scales = list(x = list(log = 10, equispaced.log = FALSE)))
dev.off()

#linear model in different panel
png(filename="lattice_points_ml_lm.png")
xyplot(lifeExp ~ gdpPercap | continent, data,
       grid = TRUE, group = continent,
       scales = list(x = list(log = 10, equispaced.log = FALSE)),
       type = c("p", "smooth"), lwd = 4)
dev.off()

#alpha transparency
#panel attribute will do this
png(filename="lattice_points_alpha.png")
xyplot(lifeExp ~ gdpPercap | continent, data,
       grid = TRUE,
       scales = list(x = list(log = 10, equispaced.log = FALSE)),
       panel = panel.smoothScatter)
dev.off()