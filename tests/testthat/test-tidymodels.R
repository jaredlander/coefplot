library(workflows)
library(magrittr)

lm_spec <- parsnip::linear_reg() %>% parsnip::set_engine('lm')
flow <- workflow() %>% 
    add_model(lm_spec) %>% 
    add_formula(mpg ~ cyl + qsec + disp)
mod5 <- parsnip::fit(flow, data=mtcars)
mod6 <- parsnip::fit(lm_spec, mpg ~ cyl + qsec + disp, data=mtcars)

coef5_static_default <- coefplot(mod5)
coef5_static_false <- coefplot(mod5, interactive=FALSE)
coef5_interactive <- coefplot(mod5, interactive=TRUE)
coef5_interactive_sorted <- coefplot(mod5, interactive=TRUE, sort='magnitude')

coef6_static_default <- coefplot(mod6)
coef6_static_false <- coefplot(mod6, interactive=FALSE)
coef6_interactive <- coefplot(mod6, interactive=TRUE)
coef6_interactive_sorted <- coefplot(mod6, interactive=TRUE, sort='magnitude')

test_that("Static plots return ggplot objects", {
    expect_s3_class(coef5_static_default, 'ggplot')
    expect_s3_class(coef5_static_false, 'ggplot')
    
    expect_s3_class(coef6_static_default, 'ggplot')
    expect_s3_class(coef6_static_false, 'ggplot')
})

test_that("Interactive plots return plotly objects", {
    expect_s3_class(coef5_interactive, 'plotly')
    expect_s3_class(coef5_interactive_sorted, 'plotly')
    
    expect_s3_class(coef6_interactive, 'plotly')
    expect_s3_class(coef6_interactive_sorted, 'plotly')
})
