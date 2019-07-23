context("Coefficient path extraction")

library(glmnet)
data(diamonds, package='ggplot2')
diaX <- useful::build.x(price ~ carat + cut + x - 1, data=diamonds, contrasts = TRUE)
diaY <- useful::build.y(price ~ carat + cut + x - 1, data=diamonds)
modG1 <- glmnet(x=diaX, y=diaY)
modG2 <- cv.glmnet(x=diaX, y=diaY, nfolds=5)

extracted <- extractPath(modG1)
extracted2 <- extractPath(modG2)

test_that('extractPath returns the correct type and number of columns', {
    expect_length(extracted, c(8))
    expect_is(extracted, 'tbl_df')
    
    expect_length(extracted2, c(8))
    expect_is(extracted2, 'tbl_df')
})

test_that('extractPath returns the correct columns', {
    expect_named(extracted)
    expect_named(extracted2)
    
    expect_equal(names(extracted), c("lambda", "carat", "cutFair", "cutGood", 
                                     "cutVery Good", "cutPremium", "cutIdeal", 
                                     "x" ))
    expect_equal(names(extracted2), c("lambda", "carat", "cutFair", "cutGood", 
                                     "cutVery Good", "cutPremium", "cutIdeal", 
                                     "x" ))
})
