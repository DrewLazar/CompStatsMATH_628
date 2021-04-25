# Data visualization example 2 
# Set working directory for reading or writing files
setwd("/Users/User1/Desktop/MyDocs/classes/SPRING/S17/628/Rcodes")

marsdat=read.table("marsbig.dat", header=TRUE)
head(marsdat)
dim(marsdat)
tail(marsdat)

# creating 4-plots for a univariate distribution

par(mfrow=c(2,2))
hist(marsdat$radius)
qqnorm(marsdat$radius)
plot(marsdat$radius)
plot(lag(marsdat$radius, k=1), marsdat$radius)

# creating emperical cumulative distribution function plot

radius.ecdf = ecdf(marsdat$radius)
plot(radius.ecdf, xlab = 'Sample Quantiles of Radius', ylab = '', main = 'Empirical Cumluative Distribution')

# Scatterplot  in ggplot2
par(mfrow=c(1,1))
library(ggplot2)
ggplot(marsdat, aes(x = radius, y = pressure))+geom_jitter(alpha=.8)
ggplot(marsdat, aes(x = radius, y = pressure))+geom_jitter(alpha=.6)
ggplot(marsdat, aes(x = radius, y = pressure))+geom_jitter(alpha=.6)+facet_grid(. ~ orbit)

# scatterplot matrix

pairs(marsdat[,1:3], col=marsdat$orbit)

# Saving plots as pdf or jpeg

pdf("splom.pdf")
par(mfrow=c(1,1))

# Basic Scatterplot Matrix
pairs(~radius+latitude+longitude+geopotential+pressure+sigmapressure+temperature+sigmatemperature+mnd+sigmamnd+orbit,data=marsdat, 
      main="Simple Scatterplot Matrix")

dev.off()

# More in depth ggplot:  three layers to create a basic scatter plot: Data>Aesthetics>Geometry

str(iris)

# Geometry: geom_point ve goem_jitter
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))+geom_point(alpha=.6)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))+geom_jitter(alpha=.6)
# add facet layer to get cleaner visualization
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))+geom_jitter(alpha=.6)+facet_grid(. ~ Species)
# add statistics layer to add more parameters to the graphics
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))+geom_jitter(alpha=.6)+facet_grid(. ~ Species)+stat_smooth(method = "lm", se = F, col = "red")
# add coordinates layer to clean up the graph 
levels(iris$Species) = c("Sertosa", "Versicolor", "Virginica")
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))+geom_jitter(alpha=.6)+facet_grid(. ~ Species)+stat_smooth(method = "lm", se = F, col = "red")+scale_y_continuous("Sepal Width (cm)", limits=c(2,5), expand=c(0,0))+scale_x_continuous("Sepal Length (cm)", limits=c(4,8), expand=c(0,0))+coord_equal()


# Scatterplot matrix for iris data
head(iris)

pairs(iris[,1:4], col=iris$Species)
