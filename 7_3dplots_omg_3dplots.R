#################### 3D plots  ###



require(MASS)
library(scatterplot3d)
attach(UScrime)

#
library(scatterplot3d)   ##load scatterplot3d function
require(MASS)
attach(UScrime)   ## use data from UScrime dataset
scatterplot3d(y,GDP, Ineq) ## graph 3 variables, y
#

require(MASS)
library(scatterplot3d)
attach(UScrime)
model <- scatterplot3d(GDP, Ineq, y,type=h, main=”3D Scatterplot with Vertical Lines”)
help(scatterplot3d)



model <- scatterplot3d(GDP, Ineq, y)
## now calculate and add the linear regression data
model1 <- lm(y ~ GDP + Ineq)   #
model$plane3d(model1)   ## link the 3d scatterplot in ‘model’ to the ‘plane3d’ option with ‘model1’ regression information
#


x <- df$koen.gns.kvinder.andel
z <- df$within.mob
y <- df$timelon.mean.gns

x <- df$koen.gns.kvinder.andel
z <- df$roede.mean.gns 
y <- df$timelon.mean.gns

plot3d(x,z,y,type='s', size=0.30)


# Linear model
fit <- lm(z ~ x + y)
coefs <- coef(fit)
a <- coefs["x"]; b <- coefs["z"]; c <- -1
d <- coefs["(Intercept)"]
rgl.planes(a, b, c, d, alpha=0.5, color = "#D95F02")




library(rgl)
colrs <- ifelse(mtcars$cyl==4, 'red', ifelse(mtcars$cyl==6, 'blue', 'darkgreen'))
with(mtcars, plot3d(df$koen.gns.kvinder.andel, df$ledighed.mean.gns, df$timelon.mean.gns),type='s', size=0.5))




rgl_init()
rgl.spheres(x, y, z, r = 0.2, color = "#D95F02") 
rgl_add_axes(x, y, z, show.bbox = FALSE)
aspect3d(1,1,1)


# Compute the linear regression (y = ax + bz + d)
fit <- lm(y ~ x + z)
# predict values on regular xz grid
grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
z.pred <- seq(min(z), max(z), length.out = grid.lines)
xz <- expand.grid( x = x.pred, z = z.pred)
y.pred <- matrix(predict(fit, newdata = xz), 
                 nrow = grid.lines, ncol = grid.lines)
# Add regression surface
rgl.surface(x.pred, z.pred, y.pred, color = "steelblue", 
                alpha = 0.5, lit = FALSE)  
# Add grid lines
rgl.surface(x.pred, z.pred, y.pred, color = "black",
    alpha = 0.5, lit = FALSE, front = "lines", back = "lines")






view(df)
model <- scatterplot3d(x,z,y,type="h", color="black",lty.hplot=3)
  ,angle=-85)

#angle


## now calculate and add the linear regression data
model1 <- lm(y ~ x + z)   #
model$plane3d(model1)   ## link the 3d scatterplot in ‘model’ to the ‘plane3d’ option with ‘model1’ regression information
#


