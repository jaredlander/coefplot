context("ExtractCoefficients")

# we need data
data("tips", package="reshape2")
mod1 <- lm(tip ~ total_bill, data=tips)
mod2 <- lm(tip ~ total_bill + sex, data=tips)
mod3 <- lm(tip ~ total_bill + sex + smoker, data=tips)
mod4 <- lm(tip ~ total_bill + sex*smoker, data=tips)
mod5 <- lm(tip ~ total_bill + sex:smoker, data=tips)
mod6 <- lm(tip ~ total_bill*sex, data=tips)
mod7 <- lm(tip ~ sex*smoker, data=tips)

coef1 <- extract.coef(mod1)
coef2 <- extract.coef(mod2)
coef3 <- extract.coef(mod3)
coef4 <- extract.coef(mod4)
coef5 <- extract.coef(mod5)
coef6 <- extract.coef(mod6)
coef7 <- extract.coef(mod7)

tips$Threshold <- tips$tip >= 4.5
modG1 <- glm(Threshold ~ total_bill, data=tips, family=binomial(link="logit"))
modG2 <- glm(Threshold ~ total_bill + sex, data=tips, family=binomial(link="logit"))
modG3 <- glm(Threshold ~ total_bill + sex + smoker, data=tips, family=binomial(link="logit"))
modG4 <- glm(Threshold ~ total_bill + sex*smoker, data=tips, family=binomial(link="logit"))
modG5 <- glm(Threshold ~ total_bill + sex:smoker, data=tips, family=binomial(link="logit"))
modG6 <- glm(Threshold ~ total_bill*sex, data=tips, family=binomial(link="logit"))
modG7 <- glm(Threshold ~ sex*smoker, data=tips, family=binomial(link="logit"))

coefG1 <- extract.coef(modG1)
coefG2 <- extract.coef(modG2)
coefG3 <- extract.coef(modG3)
coefG4 <- extract.coef(modG4)
coefG5 <- extract.coef(modG5)
coefG6 <- extract.coef(modG6)
coefG7 <- extract.coef(modG7)

test_that("Coefficients come as data.frames", {
    expect_is(coef1, "data.frame")
    expect_is(coef2, "data.frame")
    expect_is(coef3, "data.frame")
    expect_is(coef4, "data.frame")
    expect_is(coef5, "data.frame")
    expect_is(coef6, "data.frame")
    expect_is(coef7, "data.frame")
    
    expect_is(coefG1, "data.frame")
    expect_is(coefG2, "data.frame")
    expect_is(coefG3, "data.frame")
    expect_is(coefG4, "data.frame")
    expect_is(coefG5, "data.frame")
    expect_is(coefG6, "data.frame")
    expect_is(coefG7, "data.frame")
})

test_that("Coefficients have proper dimenions", {
    expect_equal(dim(coef1), c(2, 3))
    expect_equal(dim(coef2), c(3, 3))
    expect_equal(dim(coef3), c(4, 3))
    expect_equal(dim(coef4), c(5, 3))
    expect_equal(dim(coef5), c(5, 3))
    expect_equal(dim(coef6), c(4, 3))
    expect_equal(dim(coef7), c(4, 3))
    
    expect_equal(dim(coefG1), c(2, 3))
    expect_equal(dim(coefG2), c(3, 3))
    expect_equal(dim(coefG3), c(4, 3))
    expect_equal(dim(coefG4), c(5, 3))
    expect_equal(dim(coefG5), c(5, 3))
    expect_equal(dim(coefG6), c(4, 3))
    expect_equal(dim(coefG7), c(4, 3))
})

test_that("Coefficients have proper types", {
    expect_equal(sapply(coef1, class), c(Value="numeric", SE="numeric", Coefficient="character"))
    expect_equal(sapply(coef2, class), c(Value="numeric", SE="numeric", Coefficient="character"))
    expect_equal(sapply(coef3, class), c(Value="numeric", SE="numeric", Coefficient="character"))
    expect_equal(sapply(coef4, class), c(Value="numeric", SE="numeric", Coefficient="character"))
    expect_equal(sapply(coef5, class), c(Value="numeric", SE="numeric", Coefficient="character"))
    expect_equal(sapply(coef6, class), c(Value="numeric", SE="numeric", Coefficient="character"))
    expect_equal(sapply(coef7, class), c(Value="numeric", SE="numeric", Coefficient="character"))
    
    expect_equal(sapply(coefG1, class), c(Value="numeric", SE="numeric", Coefficient="character"))
    expect_equal(sapply(coefG2, class), c(Value="numeric", SE="numeric", Coefficient="character"))
    expect_equal(sapply(coefG3, class), c(Value="numeric", SE="numeric", Coefficient="character"))
    expect_equal(sapply(coefG4, class), c(Value="numeric", SE="numeric", Coefficient="character"))
    expect_equal(sapply(coefG5, class), c(Value="numeric", SE="numeric", Coefficient="character"))
    expect_equal(sapply(coefG6, class), c(Value="numeric", SE="numeric", Coefficient="character"))
    expect_equal(sapply(coefG7, class), c(Value="numeric", SE="numeric", Coefficient="character"))
})

expectedCoef1 <- structure(list(Value = c(0.920269613554671, 0.105024517384353), 
                                SE = c(0.159734746376432, 0.0073647898487626), 
                                Coefficient = c("(Intercept)", "total_bill")), 
                           .Names = c("Value", "SE", "Coefficient"), 
                           row.names = c("(Intercept)", "total_bill"), 
                           class = "data.frame")

expectedCoef2 <- structure(list(Value = c(0.933278494035796, 0.105232356866155, -0.0266087137098722), 
                                SE = c(0.173755748089091, 0.00745817378218747, 0.138333951666825), 
                                Coefficient = c("(Intercept)", "total_bill", "sexMale")), 
                           .Names = c("Value", "SE", "Coefficient"), 
                           row.names = c("(Intercept)", "total_bill", "sexMale"), 
                           class = "data.frame")

expectedCoef3 <- structure(list(Value = c(0.97703525070222, 0.105943079358779, -0.0280925699231267, -0.14919234332629), 
                                SE = c(0.178163334196653, 0.00748273981231774, 0.13827928730744, 0.135435290301022), 
                                Coefficient = c("(Intercept)", "total_bill", "sexMale", "smokerYes")), 
                           .Names = c("Value", "SE", "Coefficient"), 
                           row.names = c("(Intercept)", "total_bill", "sexMale", "smokerYes"), 
                           class = "data.frame")

expectedCoef4 <- structure(list(Value = c(0.838665697721798, 0.106867331154389, 0.159699673580903, 0.171601527915647, -0.50028527366166), 
                                SE = c(0.193672618884891, 0.00746732438703733, 0.173479347933277, 0.225167097638273, 0.28123965022277), 
                                Coefficient = c("(Intercept)", "total_bill", "sexMale", "smokerYes", "sexMale:smokerYes")), 
                           .Names = c("Value", "SE", "Coefficient"), 
                           row.names = c("(Intercept)", "total_bill", "sexMale", "smokerYes", "sexMale:smokerYes"), 
                           class = "data.frame")

expectedCoef5 <- structure(list(Value = c(0.669681625556689, 0.106867331154389, 0.168984072165109, 0.328683745746012, 0.340585600080757), 
                                SE = c(0.212128542982023, 0.00746732438703733, 0.193681264106416, 0.168404634825236, 0.223182521333619), 
                                Coefficient = c("(Intercept)", "total_bill", "sexFemale:smokerNo", "sexMale:smokerNo", "sexFemale:smokerYes")),
                           .Names = c("Value", "SE", "Coefficient"), 
                           row.names = c("(Intercept)", "total_bill", "sexFemale:smokerNo", "sexMale:smokerNo", "sexFemale:smokerYes"), 
                           class = "data.frame")

expectedCoef6 <- structure(list(Value = c(1.0480199036793, 0.0988779199719281, -0.195872209747046, 0.0089827576377142), 
                                SE = c(0.272497600665525, 0.0138079864596611, 0.338953639877964, 0.0164171081078769), 
                                Coefficient = c("(Intercept)", "total_bill", "sexMale", "total_bill:sexMale")), 
                           .Names = c("Value", "SE", "Coefficient"), 
                           row.names = c("(Intercept)", "total_bill", "sexMale", "total_bill:sexMale"), 
                           class = "data.frame")

expectedCoef7 <- structure(list(Value = c(2.77351851851852, 0.339883543337153, 0.157996632996634, -0.220232028185638), 
                                SE = c(0.188579004667271, 0.235285923694171, 0.306193520066471, 0.381520292408129), 
                                Coefficient = c("(Intercept)", "sexMale", "smokerYes", "sexMale:smokerYes")), 
                           .Names = c("Value", "SE", "Coefficient"), 
                           row.names = c("(Intercept)", "sexMale", "smokerYes", "sexMale:smokerYes"), 
                           class = "data.frame")

expectedCoefG1 <- structure(list(Value = c(-6.20405198369694, 0.178746135713105), 
                                 SE = c(0.787846856153379, 0.0280564850923222), 
                                 Coefficient = c("(Intercept)", "total_bill")), 
                            .Names = c("Value", "SE", "Coefficient"), 
                            row.names = c("(Intercept)", "total_bill"), 
                            class = "data.frame")

expectedCoefG2 <- structure(list(Value = c(-6.16250676539459, 0.179197631542195, -0.0778168380625508), 
                                 SE = c(0.830682665606069, 0.0282263860677389, 0.510653903330164), 
                                 Coefficient = c("(Intercept)", "total_bill", "sexMale")), 
                            .Names = c("Value", "SE", "Coefficient"), 
                            row.names = c("(Intercept)", "total_bill", "sexMale"), 
                            class = "data.frame")

expectedCoefG3 <- structure(list(Value = c(-6.28879908755451, 0.19791038354303, 0.0425603970575011, -1.26080742323577), 
                                 SE = c(0.887343464175678, 0.0313202875695088, 0.525802585221906, 0.568477363122317), 
                                 Coefficient = c("(Intercept)", "total_bill", "sexMale", "smokerYes")), 
                            .Names = c("Value", "SE", "Coefficient"), 
                            row.names = c("(Intercept)", "total_bill", "sexMale", "smokerYes"), 
                            class = "data.frame")

expectedCoefG4 <- structure(list(Value = c(-6.50722534287237, 0.200398977461892, 0.277416562038954, -0.641536473204469, -0.871196107806703), 
                                 SE = c(0.957583876450863, 0.0319532718817944, 0.622811036359676, 0.980442295268331, 1.17344558101382), 
                                 Coefficient = c("(Intercept)", "total_bill", "sexMale", "smokerYes", "sexMale:smokerYes")), 
                            .Names = c("Value", "SE", "Coefficient"), 
                            row.names = c("(Intercept)", "total_bill", "sexMale", "smokerYes", "sexMale:smokerYes"), 
                            class = "data.frame")

expectedCoefG5 <- structure(list(Value = c(-7.74254136184459, 0.200398977461892, 1.23531601897222, 1.51273258101117, 0.593779545767748), 
                                 SE = c(1.15628637706478, 0.0319532718817944, 0.759703572779889, 0.673162985532241, 0.989243572422861), 
                                 Coefficient = c("(Intercept)", "total_bill", "sexFemale:smokerNo", "sexMale:smokerNo", "sexFemale:smokerYes")), 
                            .Names = c("Value", "SE", "Coefficient"), 
                            row.names = c("(Intercept)", "total_bill", "sexFemale:smokerNo", "sexMale:smokerNo", "sexFemale:smokerYes"), 
                            class = "data.frame")

expectedCoefG6 <- structure(list(Value = c(-9.02683319336376, 0.288225436284044, 3.54244163292209, -0.136784802449348), 
                                 SE = c(2.23820089584356, 0.0808437446613046, 2.39386567363978, 0.086162375938673), 
                                 Coefficient = c("(Intercept)", "total_bill", "sexMale", "total_bill:sexMale")), 
                            .Names = c("Value", "SE", "Coefficient"), 
                            row.names = c("(Intercept)", "total_bill", "sexMale", "total_bill:sexMale"), 
                            class = "data.frame")

expectedCoefG7 <- structure(list(Value = c(-2.07944154018219, 0.457581107749538, -0.223143450341104, -0.179377881333291), 
                                 SE = c(0.433002251934947, 0.512188079894551, 0.744316797288887, 0.889142445029405), 
                                 Coefficient = c("(Intercept)", "sexMale", "smokerYes", "sexMale:smokerYes")), 
                            .Names = c("Value", "SE", "Coefficient"), 
                            row.names = c("(Intercept)", "sexMale", "smokerYes", "sexMale:smokerYes"), 
                            class = "data.frame")

test_that("Data comes out as expected", {
    expect_equivalent(coef1, expectedCoef1)
    expect_equivalent(coef2, expectedCoef2)
    expect_equivalent(coef3, expectedCoef3)
    expect_equivalent(coef4, expectedCoef4)
    expect_equivalent(coef5, expectedCoef5)
    expect_equivalent(coef6, expectedCoef6)
    expect_equivalent(coef7, expectedCoef7)
    
    expect_equivalent(coefG1, expectedCoefG1)
    expect_equivalent(coefG2, expectedCoefG2)
    expect_equivalent(coefG3, expectedCoefG3)
    expect_equivalent(coefG4, expectedCoefG4)
    expect_equivalent(coefG5, expectedCoefG5)
    expect_equivalent(coefG6, expectedCoefG6)
    expect_equivalent(coefG7, expectedCoefG7)
})
