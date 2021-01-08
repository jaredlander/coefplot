mod2 <- lm(mpg ~ cyl + qsec - 1, data=mtcars)
mod3 <- lm(mpg ~ cyl + qsec + disp - 1, data=mtcars)
mod4 <- glmnet::glmnet(
    x=as.matrix(ggplot2::diamonds[, c('carat', 'x', 'y', 'z')]),
    y=ggplot2::diamonds$price
)

# make static plots for each
coef2_static_default <- coefplot(mod2)
coef2_static_false <- coefplot(mod2, interactive=FALSE)
coef2_static_default_sorted <- coefplot(mod2, interactive=FALSE, sort='magnitude')
coef2_static_false_sorted <- coefplot(mod2, interactive=FALSE, sort='magnitude')
coef3_static_default <- coefplot(mod3)
coef3_static_false <- coefplot(mod3, interactive=FALSE)
coef4_static_default <- coefplot(mod4, lambda=0.65)
coef4_static_false <- coefplot(mod4, lambda=0.65, interactive=FALSE)

# make interactive pots for each
coef2_interactive <- coefplot(mod2, interactive=TRUE)
coef3_interactive <- coefplot(mod3, interactive=TRUE)
coef3_interactive_sorted <- coefplot(mod3, interactive=TRUE, sorted=TRUE)
coef4_interactive <- coefplot(mod4, lambda=0.65, interactive=TRUE)

test_that("Static plots return ggplot objects", {
  expect_s3_class(coef2_static_default, 'ggplot')
  expect_s3_class(coef2_static_false, 'ggplot')
  expect_s3_class(coef3_static_default, 'ggplot')
  expect_s3_class(coef3_static_false, 'ggplot')
  expect_s3_class(coef3_static_default, 'ggplot')
  expect_s3_class(coef3_static_false, 'ggplot')
  expect_s3_class(coef2_static_false_sorted, 'ggplot')
  expect_s3_class(coef2_static_default_sorted, 'ggplot')
})

test_that("Interactive plots return plotly objects", {
    expect_s3_class(coef2_interactive, 'plotly')
    expect_s3_class(coef3_interactive, 'plotly')
    expect_s3_class(coef4_interactive, 'plotly')
    expect_s3_class(coef3_interactive_sorted, 'plotly')
})
