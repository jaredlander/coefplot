context("Coefficient path plotting")

library(glmnet)
library(ggplot2)
library(useful)
data(diamonds)
diaX <- useful::build.x(price ~ carat + cut + x - 1, data=diamonds, contrasts = TRUE)
diaY <- useful::build.y(price ~ carat + cut + x - 1, data=diamonds)
modG1 <- glmnet(x=diaX, y=diaY)
coefpath(modG1)

modG2 <- cv.glmnet(x=diaX, y=diaY, nfolds=5)
coefpath(modG2)

x <- matrix(rnorm(100*20),100,20)
y <- rnorm(100)
fit1 <- glmnet(x, y)
coefpath(fit1)

test_that('dygraph objects are returned', {
    expect_is(coefpath(modG1), 'dygraphs')
    expect_is(coefpath(modG2), 'dygraphs')
    expect_is(coefpath(fit1), 'dygraphs')
    
    expect_is(coefpath(modG1, annotate=FALSE), 'dygraphs')
    expect_is(coefpath(modG2, annotate=FALSE), 'dygraphs')
    expect_is(coefpath(fit1, annotate=FALSE), 'dygraphs')
})
