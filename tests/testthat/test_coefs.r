context("Testing for coefficient matching")

test_that("lm has proper data frame for matching predictor to coefficient", {
    # fit lm model
    data("tips", package="reshape2")
    mod1 <- lm(tip ~ total_bill + sex + size, data=tips)
    mod2 <- lm(tip ~ total_bill + sex + size - 1, data=tips)
    mod3 <- lm(tip ~ total_bill + sex + size*time, data=tips)
    mod4 <- lm(tip ~ total_bill + sex + size*time - 1, data=tips)
    mod5 <- lm(tip ~ total_bill + sex + size:time - 1, data=tips)
    mod6 <- lm(tip ~ total_bill + sex + size + time + size:time - 1, data=tips)
    
    df1 <- data.frame(Term=c("(Intercept)", "total_bill", "sex", "size"), Coefficient=c("(Intercept)", "total_bill", "sexMale", "size"), .Pred=c("(Intercept)", "total_bill", "sex", "size"), .Type=c("(Intercept)", "numeric", "factor", "numeric"), value=c(1, 1, 1, 1), stringsAsFactors=FALSE)
    df2 <- data.frame(Term=c("total_bill", "sex", "sex", "size"), Coefficient=c("total_bill", "sexFemale", "sexMale", "size"), .Pred=c("total_bill", "sex", "sex", "size"), .Type=c("numeric", "factor", "factor", "numeric"), value=c(1, 1, 1, 1), stringsAsFactors=FALSE)
    df3 <- data.frame(Term=c("(Intercept)", "total_bill", "sex", "size", "time", "size:time", "size:time"), Coefficient=c("(Intercept)", "total_bill", "sexMale", "size", "timeLunch", "size:timeLunch", "size:timeLunch"), .Pred=c("(Intercept)", "total_bill", "sex", "size", "time", "size", "time"), .Type=c("(Intercept)", "numeric", "factor", "numeric", "factor", "numeric", "factor"), value=c(1, 1, 1, 1, 1, 1, 1), stringsAsFactors=FALSE)
    df4 <- data.frame(Term=c("total_bill", "sex", "sex", "size", "time", "size:time", "size:time"), Coefficient=c("total_bill", "sexFemale", "sexMale", "size", "timeLunch", "size:timeLunch", "size:timeLunch"), .Pred=c("total_bill", "sex", "sex", "size", "time", "size", "time"), .Type=c("numeric", "factor", "factor", "numeric", "factor", "numeric", "factor"), value=c(1, 1, 1, 1, 1, 1, 1), stringsAsFactors=FALSE)
    df5 <- data.frame(Term=c("total_bill", "sex", "sex", "size:time", "size:time", "size:time", "size:time"), Coefficient=c("total_bill", "sexFemale", "sexMale", "size:timeDinner", "size:timeDinner", "size:timeLunch", "size:timeLunch"), .Pred=c("total_bill", "sex", "sex", "size", "time", "size", "time"), .Type=c("numeric", "factor", "factor", "numeric", "factor", "numeric", "factor"), value=c(1, 1, 1, 2, 2, 2, 2), stringsAsFactors=FALSE)
    df6 <- data.frame(Term=c("total_bill", "sex", "sex", "size", "time", "size:time", "size:time"), Coefficient=c("total_bill", "sexFemale", "sexMale", "size", "timeLunch", "size:timeLunch", "size:timeLunch"), .Pred=c("total_bill", "sex", "sex", "size", "time", "size", "time"), .Type=c("numeric", "factor", "factor", "numeric", "factor", "numeric", "factor"), value=c(1, 1, 1, 1, 1, 1, 1), stringsAsFactors=FALSE)
    
    expect_that(matchCoefs(mod1), equals(df1))
    expect_that(matchCoefs(mod2), equals(df2))
    expect_that(matchCoefs(mod3), equals(df3))
    expect_that(matchCoefs(mod4), equals(df4))
    expect_that(matchCoefs(mod5), equals(df5))
    expect_that(matchCoefs(mod6), equals(df6))
    
})

test_that("coxph has proper data frame for matching predictor to coefficient", {
    require(survival)
    cox1 <- coxph(Surv(stop, event) ~ rx + number + size, data=bladder)
    cox2 <- coxph(Surv(stop, event) ~ rx + number + size - 1, data=bladder)
    cox3 <- coxph(Surv(stop, event) ~ rx + number * size, data=bladder)
    cox4 <- coxph(Surv(stop, event) ~ rx + number * size + 1, data=bladder)
    
    cdf1 <- data.frame(Term=c("rx", "number", "size"), Coefficient=c("rx", "number", "size"), .Pred=c("rx", "number", "size"), .Type=c("numeric", "numeric", "numeric"), value=c(1, 1, 1), stringsAsFactors=FALSE)
    cdf2 <- data.frame(Term=c("rx", "number", "size"), Coefficient=c("rx", "number", "size"), .Pred=c("rx", "number", "size"), .Type=c("numeric", "numeric", "numeric"), value=c(1, 1, 1), stringsAsFactors=FALSE)
    cdf3 <- data.frame(Term=c("rx", "number", "size", "number:size", "number:size"), Coefficient=c("rx", "number", "size", "number:size", "number:size"), .Pred=c("rx", "number", "size", "number", "size"), .Type=c("numeric", "numeric", "numeric", "numeric", "numeric"), value=c(1, 1, 1, 1, 1), stringsAsFactors=FALSE)
    cdf4 <- data.frame(Term=c("rx", "number", "size", "number:size", "number:size"), Coefficient=c("rx", "number", "size", "number:size", "number:size"), .Pred=c("rx", "number", "size", "number", "size"), .Type=c("numeric", "numeric", "numeric", "numeric", "numeric"), value=c(1, 1, 1, 1, 1), stringsAsFactors=FALSE)
    
    expect_that(matchCoefs(cox1), equals(cdf1))
    expect_that(matchCoefs(cox2), equals(cdf2))
    expect_that(matchCoefs(cox3), equals(cdf3))
    expect_that(matchCoefs(cox4), equals(cdf4))
})
 
test_that("glm has proper data frame for matching predictor to coefficient", {
    data("tips", package="reshape2")
    gmod5 <- glm(tip ~ total_bill + sex + size, data=tips)
    gmod6 <- glm(tip ~ total_bill + sex + size - 1, data=tips)
    gmod7 <- glm(tip ~ total_bill + sex + size*time, data=tips)
    gmod8 <- glm(tip ~ total_bill + sex + size*time - 1, data=tips)
    
    gdf5 <- data.frame(Term=c("(Intercept)", "total_bill", "sex", "size"), Coefficient=c("(Intercept)", "total_bill", "sexMale", "size"), .Pred=c("(Intercept)", "total_bill", "sex", "size"), .Type=c("(Intercept)", "numeric", "factor", "numeric"), value=c(1, 1, 1, 1), stringsAsFactors=FALSE)
    gdf6 <- data.frame(Term=c("total_bill", "sex", "sex", "size"), Coefficient=c("total_bill", "sexFemale", "sexMale", "size"), .Pred=c("total_bill", "sex", "sex", "size"), .Type=c("numeric", "factor", "factor", "numeric"), value=c(1, 1, 1, 1), stringsAsFactors=FALSE)
    gdf7 <- data.frame(Term=c("(Intercept)", "total_bill", "sex", "size", "time", "size:time", "size:time"), Coefficient=c("(Intercept)", "total_bill", "sexMale", "size", "timeLunch", "size:timeLunch", "size:timeLunch"), .Pred=c("(Intercept)", "total_bill", "sex", "size", "time", "size", "time"), .Type=c("(Intercept)", "numeric", "factor", "numeric", "factor", "numeric", "factor"), value=c(1, 1, 1, 1, 1, 1, 1), stringsAsFactors=FALSE)
    gdf8 <- data.frame(Term=c("total_bill", "sex", "sex", "size", "time", "size:time", "size:time"), Coefficient=c("total_bill", "sexFemale", "sexMale", "size", "timeLunch", "size:timeLunch", "size:timeLunch"), .Pred=c("total_bill", "sex", "sex", "size", "time", "size", "time"), .Type=c("numeric", "factor", "factor", "numeric", "factor", "numeric", "factor"), value=c(1, 1, 1, 1, 1, 1, 1), stringsAsFactors=FALSE)
    
    expect_that(matchCoefs(gmod5), equals(gdf5))
    expect_that(matchCoefs(gmod6), equals(gdf6))
    expect_that(matchCoefs(gmod7), equals(gdf7))
    expect_that(matchCoefs(gmod8), equals(gdf8))
})
