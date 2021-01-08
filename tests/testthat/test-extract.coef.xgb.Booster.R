context("Extracting coefs from xgboost models")

library(xgboost)
diaX <- useful::build.x(price ~ carat + cut + x, data=diamonds, contrasts=FALSE)
diaY <- useful::build.y(price ~ carat + cut + x, data=diamonds)

xg1 <- xgboost(data=diaX, label=diaY, 
               booster='gblinear',
               objective='reg:squarederror', eval_metric='rmse',
               nrounds=50,
               verbose=FALSE,
               save_name='TestModelXG.model'
)

xgCoef <- extract.coef(xg1)
xgCoef_named <- extract.coef(xg1, feature_names=colnames(diaX))
xgCoef_named_all <- extract.coef(xg1, feature_names=colnames(diaX), removeNonSelected=FALSE)

xg2 <- xgb.train(data=xgb.DMatrix(data=diaX, label=diaY), 
               booster='gblinear',
               objective='reg:squarederror', eval_metric='rmse',
               alpha=65.65,
               nrounds=50,
               verbose=FALSE,
               save_name='TestModelXG.model'
)

xgCoef2.1 <- extract.coef(xg2)
xgCoef_named2.1 <- extract.coef(xg2, feature_names=colnames(diaX))
xgCoef_named_all2.1 <- extract.coef(xg2, feature_names=colnames(diaX), 
                                    removeNonSelected=FALSE)

xgCoef2.2 <- extract.coef(xg2, zero_threshold=0)
xgCoef_named2.2 <- extract.coef(xg2, feature_names=colnames(diaX), zero_threshold=0)
xgCoef_named_all2.2 <- extract.coef(xg2, feature_names=colnames(diaX), 
                                    removeNonSelected=FALSE, zero_threshold=0)

test_that("The correct class is returned", {
    expect_is(xgCoef, 'tbl')
    expect_is(xgCoef_named, 'tbl')
    expect_is(xgCoef_named_all, 'tbl')
    
    expect_is(xgCoef2.1, 'tbl')
    expect_is(xgCoef_named2.1, 'tbl')
    expect_is(xgCoef_named_all2.1, 'tbl')
    
    expect_is(xgCoef2.2, 'tbl')
    expect_is(xgCoef_named2.2, 'tbl')
    expect_is(xgCoef_named_all2.2, 'tbl')
})

test_that("The correct number and names of columns are returned", {
    expect_named(xgCoef)
    expect_named(xgCoef_named)
    expect_named(xgCoef_named_all)
    
    expect_length(xgCoef, 3)
    expect_length(xgCoef_named, 3)
    expect_length(xgCoef_named_all, 3)
    
    expect_equal(names(xgCoef), c('Value', 'SE', 'Coefficient'))
    expect_equal(names(xgCoef_named), c('Value', 'SE', 'Coefficient'))
    expect_equal(names(xgCoef_named_all), c('Value', 'SE', 'Coefficient'))
    
    ####################
    # 2.1
    expect_named(xgCoef2.1)
    expect_named(xgCoef_named2.1)
    expect_named(xgCoef_named_all2.1)
    
    expect_length(xgCoef2.1, 3)
    expect_length(xgCoef_named2.1, 3)
    expect_length(xgCoef_named_all2.1, 3)
    
    expect_equal(names(xgCoef2.1), c('Value', 'SE', 'Coefficient'))
    expect_equal(names(xgCoef_named2.1), c('Value', 'SE', 'Coefficient'))
    expect_equal(names(xgCoef_named_all2.1), c('Value', 'SE', 'Coefficient'))
    
    ###################
    # 2.2
    expect_named(xgCoef2.2)
    expect_named(xgCoef_named2.2)
    expect_named(xgCoef_named_all2.2)
    
    expect_length(xgCoef2.2, 3)
    expect_length(xgCoef_named2.2, 3)
    expect_length(xgCoef_named_all2.2, 3)
    
    expect_equal(names(xgCoef2.2), c('Value', 'SE', 'Coefficient'))
    expect_equal(names(xgCoef_named2.2), c('Value', 'SE', 'Coefficient'))
    expect_equal(names(xgCoef_named_all2.2), c('Value', 'SE', 'Coefficient'))
})

test_that("The correct number of rows are returned", {
    expect_equal(nrow(xgCoef), ncol(diaX))
    expect_equal(nrow(xgCoef_named), ncol(diaX))
    expect_equal(nrow(xgCoef_named_all), ncol(diaX))
    
    ################
    # 2.1
    expect_equal(nrow(xgCoef2.1), 4)
    expect_equal(nrow(xgCoef_named2.1), 4)
    expect_equal(nrow(xgCoef_named_all2.1), ncol(diaX))
    
    ################
    # 2.2
    expect_equal(nrow(xgCoef2.2), ncol(diaX))
    expect_equal(nrow(xgCoef_named2.2), ncol(diaX))
    expect_equal(nrow(xgCoef_named_all2.2), ncol(diaX))
})

test_that("The correct coefficient names are returned", {
    expect_equal(xgCoef$Coefficient, xg1$feature_names)
    expect_equal(xgCoef_named$Coefficient, colnames(diaX))
    expect_equal(xgCoef_named_all$Coefficient, colnames(diaX))
    
    expect_equal(xgCoef2.1$Coefficient, c("(Intercept)", "carat", "cutIdeal", "x"))
    expect_equal(xgCoef_named2.1$Coefficient, c("(Intercept)", "carat", "cutIdeal", "x"))
    expect_equal(xgCoef_named_all2.1$Coefficient, colnames(diaX))
    
    expect_equal(xgCoef2.2$Coefficient, xg2$feature_names)
    expect_equal(xgCoef_named2.2$Coefficient, colnames(diaX))
    expect_equal(xgCoef_named_all2.2$Coefficient, colnames(diaX))
})

unlink('TestModelXG.model')
