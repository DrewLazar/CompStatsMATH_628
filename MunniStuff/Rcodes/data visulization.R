# Data visualization with R
# Load the ggplot2 package
library(ggplot2)
# Use str() to explore the structure of mtcars dataset
head(mtcars)
str(mtcars)
ggplot(mtcars, aes(x = cyl, y = mpg)) +geom_point()

# Change the ggplot() command by warpping factor()
# around cyl

ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +geom_point()

# Use grammers of graphics to create publication worthy graphics

str(iris)
# three layers to create a basic scatter plot: Data>Aesthetics>Geometry

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
