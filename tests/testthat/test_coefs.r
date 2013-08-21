context("Testing for coefficient matching")

test_that("lm has proper assign positions", {
    # fit lm model
    data("tips", package="reshape2")
    mod1 <- lm(tip ~ total_bill + sex + size, data=tips)
    mod2 <- lm(tip ~ total_bill + sex + size - 1, data=tips)
    mod3 <- lm(tip ~ total_bill + sex + size*time, data=tips)
    mod4 <- lm(tip ~ total_bill + sex + size*time - 1, data=tips)
    mod5 <- lm(tip ~ total_bill + sex + size:time - 1, data=tips)
    mod6 <- lm(tip ~ total_bill + sex + size + time + size:time - 1, data=tips)
    
    df1 <- data.frame(Term=c("(Intercept)", "total_bill", "sex", "size"), Coefficient=c("(Intercept)", "total_bill", "sexMale", "size"), .Pred=c("(Intercept)", "total_bill", "sex", "size"), .Type=c("(Intercept)", "numeric", "factor", "numeric"), value=c("1", "1", "1", "1"), stringsAsFactors=FALSE)
    df2 <- data.frame(Term=c("total_bill", "sex", "sex", "size"), Coefficient=c("total_bill", "sexFemale", "sexMale", "size"), .Pred=c("total_bill", "sex", "sex", "size"), .Type=c("numeric", "factor", "factor", "numeric"), value=c("1", "1", "1", "1"), stringsAsFactors=FALSE)
    df3 <- data.frame(Term=c("(Intercept)", "total_bill", "sex", "size", "time", "size:time", "size:time"), Coefficient=c("(Intercept)", "total_bill", "sexMale", "size", "timeLunch", "size:timeLunch", "size:timeLunch"), .Pred=c("(Intercept)", "total_bill", "sex", "size", "time", "size", "time"), .Type=c("(Intercept)", "numeric", "factor", "numeric", "factor", "numeric", "factor"), value=c("1", "1", "1", "1", "1", "1", "1"), stringsAsFactors=FALSE)
    df4 <- data.frame(Term=c("total_bill", "sex", "sex", "size", "time", "size:time", "size:time"), Coefficient=c("total_bill", "sexFemale", "sexMale", "size", "timeLunch", "size:timeLunch", "size:time:Lunch"), .Pred=c("total_bill", "sex", "sex", "size", "time", "size", "time"), .Type=c("numeric", "factor", "factor", "numeric", "factor", "numeric", "factor"), value=c("1", "1", "1", "1", "1", "1", "1"), stringsAsFactors=FALSE)
    df5 <- data.frame(Term=c("total_bill", "sex", "sex", "size:time", "size:time"), Coefficient=c("total_bill", "sexFemale", "sexMale", "size:timeDinner", "size:timeLunch"), .Pred=c("total_bill", "sex", "sex", ""), .Type=c("numeric", "factor", "factor", "numeric"), value=c("1", "1", "1", "1"), stringsAsFactors=FALSE)
    df6
    
    expect_that(matchCoefs(mod1), equals(df1))
    expect_that(matchCoefs(mod2), equals(df2))
    expect_that(matchCoefs(mod3), equals(df3))
    expect_that(matchCoefs(mod4), equals(df4))
    expect_that(matchCoefs(mod5), equals(df5))
    expect_that(matchCoefs(mod6), equals(df6))
    
})

test_that("coxph has proper assign positions", {
    require(survival)
    cox1 <- coxph(Surv(stop, event) ~ rx + number + size, data=bladder)
    cox2 <- coxph(Surv(stop, event) ~ rx + number + size - 1, data=bladder)
    cox3 <- coxph(Surv(stop, event) ~ rx + number * size, data=bladder)
    cox4 <- coxph(Surv(stop, event) ~ rx + number * size + 1, data=bladder)
    
    expect_that(matchCoefs(cox1), equals(c(1,2,3)))
    expect_that(matchCoefs(cox2), equals(c(1,2,3)))
    expect_that(matchCoefs(cox3), equals(c(1,2,3,4)))
    expect_that(matchCoefs(cox4), equals(c(1,2,3,4)))
})

test_that("glm has proper assign positions", {
    data("tips", package="reshape2")
    gmod5 <- glm(tip ~ total_bill + sex + size, data=tips)
    gmod6 <- glm(tip ~ total_bill + sex + size - 1, data=tips)
    gmod7 <- glm(tip ~ total_bill + sex + size*time, data=tips)
    gmod8 <- glm(tip ~ total_bill + sex + size*time - 1, data=tips)
    
    expect_that(matchCoefs(gmod5), equals(c(0,1,2,3)))
    expect_that(matchCoefs(gmod6), equals(c(1,2,2,3)))
    expect_that(matchCoefs(gmod7), equals(c(0,1,2,3,4,5)))
    expect_that(matchCoefs(gmod8), equals(c(1,2,2,3,4,5)))
})
