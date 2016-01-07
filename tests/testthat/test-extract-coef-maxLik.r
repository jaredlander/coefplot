context("Extracting Coefficients From maxLik Models")

runThis <- suppressWarnings(suppressPackageStartupMessages(require(maxLik)))

if(runThis)
{
    ## ML estimation of exponential duration model:
    t <- rexp(100, 2)
    
    loglik <- function(theta) log(theta) - theta*t
    gradlik <- function(theta) 1/theta - t
    hesslik <- function(theta) -100/theta^2
    
    ## Estimate with numeric gradient and hessian
    a <- maxLik(loglik, start=1)
    
    ## Estimate with analytic gradient and hessian
    b <- maxLik(loglik, gradlik, hesslik, start=1)
    
    ## Next, we give an example with vector argument:  Estimate the mean and
    ## variance of a random normal sample by maximum likelihood
    ##
    loglik <- function(param)
    {
        mu <- param[1]
        sigma <- param[2]
        ll <- -0.5*N*log(2*pi) - N*log(sigma) - sum(0.5*(x - mu)^2/sigma^2)
        ll
    }
    
    x <- rnorm(1000, 1, 2) # use mean=1, stdd=2
    N <- length(x)
    
    res <- maxLik(loglik, start=c(0,1)) # use 'wrong' start values
    
    maxLikA <- extract.coef.maxLik(a)
    maxLikB <- extract.coef.maxLik(b)
    maxLikRes <- extract.coef.maxLik(res)
    
    maxLikAGeneric <- extract.coef(a)
    maxLikBGeneric <- extract.coef(b)
    maxLikResGeneric <- extract.coef(res)
}

test_that("extract.coef.maxLik has correct dimensions", {
    skip_if_not_installed('maxLik')
    
    expect_equal(dim(maxLikA), c(1, 3))
    expect_equal(dim(maxLikB), c(1, 3))
    expect_equal(dim(maxLikRes), c(2, 3))
})

test_that("extract.coef has correct dimensions", {
    skip_if_not_installed('maxLik')
    
    expect_equal(dim(maxLikAGeneric), c(1, 3))
    expect_equal(dim(maxLikBGeneric), c(1, 3))
    expect_equal(dim(maxLikResGeneric), c(2, 3))
})

test_that("extract.coef.maxLik has proper types", {
    skip_if_not_installed('maxLik')
    
    expect_equal(sapply(maxLikA, class), c(Value="numeric", SE="numeric", Coefficient="integer"))
    expect_equal(sapply(maxLikB, class), c(Value="numeric", SE="numeric", Coefficient="integer"))
    expect_equal(sapply(maxLikRes, class), c(Value="numeric", SE="numeric", Coefficient="integer"))
})

test_that("extract.coef has proper types", {
    skip_if_not_installed('maxLik')
    
    expect_equal(sapply(maxLikAGeneric, class), c(Value="numeric", SE="numeric", Coefficient="integer"))
    expect_equal(sapply(maxLikBGeneric, class), c(Value="numeric", SE="numeric", Coefficient="integer"))
    expect_equal(sapply(maxLikResGeneric, class), c(Value="numeric", SE="numeric", Coefficient="integer"))
})