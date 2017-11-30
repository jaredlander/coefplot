context("Extracting coefs from xgboost models")

library(xgboost)
diaX <- useful::build.x(price ~ carat + cut + x, data=diamonds, contrasts=FALSE)
diaY <- useful::build.y(price ~ carat + cut + x, data=diamonds)

xg1 <- xgboost(data=diaX, label=diaY, 
               booster='gblinear',
               objective='reg:linear', eval_metric='rmse',
               nrounds=50,
               verbose=FALSE,
               save_name='TestModelXG.model'
)

xgCoef <- extract.coef(xg1)
xgCoef_named <- extract.coef(xg1, feature_names=colnames(diaX))

test_that("The correct class is returned", {
    expect_is(xgCoef, 'tbl')
    expect_is(xgCoef_named, 'tbl')
})

test_that("The correct number and names of columns are returned", {
    expect_named(xgCoef)
    expect_named(xgCoef_named)
    
    expect_length(xgCoef, 3)
    expect_length(xgCoef_named, 3)
    
    expect_equal(names(xgCoef), c('Value', 'SE', 'Coefficient'))
    expect_equal(names(xgCoef_named), c('Value', 'SE', 'Coefficient'))
})

test_that("The correct number of rows are returned", {
    expect_equal(nrow(xgCoef), ncol(diaX))
    expect_equal(nrow(xgCoef_named), ncol(diaX))
})

test_that("The correct coefficient names are returned", {
    expect_equal(xgCoef$Coefficient, 1:ncol(diaX))
    expect_equal(xgCoef_named$Coefficient, colnames(diaX))
})

unlink('TestModelXG.model')