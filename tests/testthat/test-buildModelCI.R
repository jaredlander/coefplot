context("buildModelCI builds the appropriate model")

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

test_that('', {
    
})
