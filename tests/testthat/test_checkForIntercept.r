# check whether intercept is picked up correctly
context("Testing for intercept awareness")

test_that("lm as proper intercept", {
    # fit lm model
    data("tips", package="reshape2")
    mod1 <- lm(tip ~ total_bill + sex + size, data=tips)
    mod2 <- lm(tip ~ total_bill + sex + size - 1, data=tips)
    mod3 <- lm(tip ~ total_bill + sex + size*time, data=tips)
    mod4 <- lm(tip ~ total_bill + sex + size*time - 1, data=tips)
    
    # check intercept
    inter1 <- checkForIntercept(mod1)
    inter2 <- checkForIntercept(mod2)
    inter3 <- checkForIntercept(mod3)
    inter4 <- checkForIntercept(mod4)
    
    expect_that(inter1, equals(1))
    expect_that(inter2, equals(0))
    expect_that(inter3, equals(1))
    expect_that(inter4, equals(0))
})

test_that("coxph has no intercept", {
    # fit lm model
    require(survival)
    cox1 <- coxph(Surv(stop, event) ~ rx + number + size, data=bladder)
    cox2 <- coxph(Surv(stop, event) ~ rx + number + size - 1, data=bladder)
    cox3 <- coxph(Surv(stop, event) ~ rx + number * size, data=bladder)
    cox4 <- coxph(Surv(stop, event) ~ rx + number * size + 1, data=bladder)
    
    # check intercept
    inter1 <- checkForIntercept(cox1)
    inter2 <- checkForIntercept(cox2)
    inter3 <- checkForIntercept(cox3)
    inter4 <- checkForIntercept(cox4)
    
    expect_that(inter1, equals(0))
    expect_that(inter2, equals(0))
    expect_that(inter3, equals(0))
    expect_that(inter4, equals(0))
})

test_that("glm as proper intercept", {
    # fit glm model
    gmod1 <- glm(weight ~ Chick + Time, data=ChickWeight)
    gmod2 <- glm(weight ~ Chick + Time - 1, data=ChickWeight)
    gmod3 <- glm(weight ~ Chick + Time*Diet, data=ChickWeight)
    gmod4 <- glm(weight ~ Chick + Time*Diet - 1, data=ChickWeight)
    data("tips", package="reshape2")
    gmod5 <- lm(tip ~ total_bill + sex + size, data=tips)
    gmod6 <- lm(tip ~ total_bill + sex + size - 1, data=tips)
    gmod7 <- lm(tip ~ total_bill + sex + size*time, data=tips)
    gmod8 <- lm(tip ~ total_bill + sex + size*time - 1, data=tips)
    
    # check intercept
    gInter1 <- checkForIntercept(gmod1)
    gInter2 <- checkForIntercept(gmod2)
    gInter3 <- checkForIntercept(gmod3)
    gInter4 <- checkForIntercept(gmod4)
    gInter5 <- checkForIntercept(gmod5)
    gInter6 <- checkForIntercept(gmod6)
    gInter7 <- checkForIntercept(gmod7)
    gInter8 <- checkForIntercept(gmod8)
    
    expect_that(gInter1, equals(1))
    expect_that(gInter2, equals(0))
    expect_that(gInter3, equals(1))
    expect_that(gInter4, equals(0))
    expect_that(gInter5, equals(1))
    expect_that(gInter6, equals(0))
    expect_that(gInter7, equals(1))
    expect_that(gInter8, equals(0))
})
