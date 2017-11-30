context("Extracting coefs from glmnet models")

library(xgboost)
diaX <- useful::build.x(price ~ carat + cut + x - 1, data=diamonds, contrasts=FALSE)
diaY <- useful::build.y(price ~ carat + cut + x - 1, data=diamonds)

modG1 <- glmnet(x=diaX, y=diaY)
coef1 <- extract.coef(modG1)

test_that("The correct class is returned", {
    expect_is(coef1, 'data.frame')
})

test_that("The correct number and names of columns are returned", {
    expect_named(coef1)
    
    expect_length(coef1, 3)
    
    expect_equal(names(coef1), c('Value', 'SE', 'Coefficient'))
})

# this depends on the selected variables so need a different way to test
# test_that("The correct number of rows are returned", {
#     expect_equal(nrow(coef1), ncol(diaX))
# })

