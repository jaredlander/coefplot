context("Testing for correct assign")

test_that("lm has proper assign positions", {
    # fit lm model
    data("tips", package="reshape2")
    mod1 <- lm(tip ~ total_bill + sex + size, data=tips)
    mod2 <- lm(tip ~ total_bill + sex + size - 1, data=tips)
    mod3 <- lm(tip ~ total_bill + sex + size*time, data=tips)
    mod4 <- lm(tip ~ total_bill + sex + size*time - 1, data=tips)
    mod5 <- lm(tip ~ total_bill + sex + size:time - 1, data=tips)
    mod6 <- lm(tip ~ total_bill + sex + size + time + size:time - 1, data=tips)
    
    expect_that(get.assign(mod1), equals(c(0,1,2,3)))
    expect_that(get.assign(mod2), equals(c(1,2,2,3)))
    expect_that(get.assign(mod3), equals(c(0,1,2,3,4,5)))
    expect_that(get.assign(mod4), equals(c(1,2,2,3,4,5)))
    expect_that(get.assign(mod5), equals(c(1,2,2,3,3)))
    expect_that(get.assign(mod6), equals(c(1,2,2,3,4,5)))
    
})

test_that("coxph has proper assign positions", {
    require(survival)
    cox1 <- coxph(Surv(stop, event) ~ rx + number + size, data=bladder)
    cox2 <- coxph(Surv(stop, event) ~ rx + number + size - 1, data=bladder)
    cox3 <- coxph(Surv(stop, event) ~ rx + number * size, data=bladder)
    cox4 <- coxph(Surv(stop, event) ~ rx + number * size + 1, data=bladder)
    
    expect_that(get.assign(cox1), equals(c(1,2,3)))
    expect_that(get.assign(cox2), equals(c(1,2,3)))
    expect_that(get.assign(cox3), equals(c(1,2,3,4)))
    expect_that(get.assign(cox4), equals(c(1,2,3,4)))
})

test_that("glm has proper assign positions", {
    data("tips", package="reshape2")
    gmod5 <- glm(tip ~ total_bill + sex + size, data=tips)
    gmod6 <- glm(tip ~ total_bill + sex + size - 1, data=tips)
    gmod7 <- glm(tip ~ total_bill + sex + size*time, data=tips)
    gmod8 <- glm(tip ~ total_bill + sex + size*time - 1, data=tips)
    
    expect_that(get.assign(gmod5), equals(c(0,1,2,3)))
    expect_that(get.assign(gmod6), equals(c(1,2,2,3)))
    expect_that(get.assign(gmod7), equals(c(0,1,2,3,4,5)))
    expect_that(get.assign(gmod8), equals(c(1,2,2,3,4,5)))
})
    