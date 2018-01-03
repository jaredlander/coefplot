context("buildModelCI builds the appropriate model")

library(glmnet)
library(xgboost)

data(diamonds, package='ggplot2')

model1 <- lm(price ~ carat + cut, data=diamonds)
ci1 <- coefplot:::buildModelCI(model1)

diaX <- useful::build.x(price ~ carat + cut + x, data=diamonds, contrasts=FALSE)
diaY <- useful::build.y(price ~ carat + cut + x, data=diamonds)

xg1 <- xgboost(data=diaX, label=diaY, 
               booster='gblinear',
               objective='reg:linear', eval_metric='rmse',
               nrounds=50,
               verbose=FALSE,
               save_name='TestModelXG.model'
)

xgCI <- coefplot:::buildModelCI(xg1)
xgCI_named <- coefplot:::buildModelCI(xg1, feature_names=colnames(diaX))

modG1 <- glmnet(x=diaX, y=diaY)
coefG1 <- coefplot:::buildModelCI(modG1)

correctNames <- c('Value', 'Coefficient', 'HighInner', 'LowInner', 'HighOuter', 'LowOuter', 'Model')

test_that('buildModelCI returns a data.frame', {
    expect_is(ci1, 'data.frame')
    expect_is(xgCI, 'tbl')
    expect_is(xgCI_named, 'tbl')
    expect_is(coefG1, 'data.frame')
})

test_that('buildModelCI returns correct number and names of columns', {
    expect_length(ci1, 7)
    expect_length(xgCI, 7)
    expect_length(xgCI_named, 7)
    expect_length(coefG1, 7)
    
    expect_named(ci1)
    expect_named(xgCI)
    expect_named(xgCI_named)
    expect_named(coefG1)
    
    expect_equal(names(ci1), correctNames)
    expect_equal(names(xgCI), correctNames)
    expect_equal(names(xgCI_named), correctNames)
    expect_equal(names(coefG1), correctNames)
})


test_that('buildModelCI returns correct number of rows', {
    expect_equal(nrow(ci1), 6)
    expect_equal(nrow(xgCI), 8)
    expect_equal(nrow(xgCI_named), 8)
    expect_equal(nrow(coefG1), 5)
})

