# this file extracts coefficients from varying models
#' extract.coef.default
#' 
#' Extract Coefficient Information from Models
#' 
#' Gets the coefficient values and standard errors, and variable names from a model.
#' 
#' @author Jared P. Lander
#' @S3method extract.coef default
#' @method extract.coef default
#' @aliases extract.coef.default
#' @param model Model object to extract information from.
#' @param \dots Further arguments
#' @return A \code{\link{data.frame}} containing the coefficient, the standard error and the variable name.
#' @examples
#' require(ggplot2)
#' data(diamonds)
#' mod1 <- lm(price ~ carat + cut + x, data=diamonds)
#' extract.coef(mod1)
#' 
extract.coef.default <- function(model, ...)
{
    # get summary of model
    theSumm <- summary(model)
    # get coef and standard error
    info <- as.data.frame(theSumm$coefficients[, 1:2])
    # make a variable tracking the name
    info$Variable <- rownames(info)
    
    return(info)
}

#' extract.coef.lm
#' 
#' Extract Coefficient Information from lm Models
#' 
#' Gets the coefficient values and standard errors, and variable names from an lm model.
#' 
#' @author Jared P. Lander
#' @aliases extract.coef.lm
#' @S3method extract.coef lm
#' @method extract.coef lm
#' @inheritParams extract.coef.default
#' @param \dots Further arguments
#' @return A \code{\link{data.frame}} containing the coefficient, the standard error and the variable name.
#' @examples
#' require(ggplot2)
#' data(diamonds)
#' mod1 <- lm(price ~ carat + cut + x, data=diamonds)
#' extract.coef(mod1)
#' 
extract.coef.lm <- function(model, ...)
{
    extract.coef.default(model=model, ...)
}


#' extract.coef.glm
#' 
#' Extract Coefficient Information from glm Models
#' 
#' Gets the coefficient values and standard errors, and variable names from a glm model.
#' 
#' @author Jared P. Lander
#' @aliases extract.coef.glm
#' @inheritParams extract.coef.default
#' @param \dots Further arguments
#' @S3method extract.coef glm
#' @method extract.coef glm
#' @return A \code{\link{data.frame}} containing the coefficient, the standard error and the variable name.
#' @examples
#' require(ggplot2)
#' data(diamonds)
#' mod2 <- glm(price > 10000 ~ carat + cut + x, data=diamonds, family=binomial(link="logit"))
#' extract.coef(mod2)
#' 
extract.coef.glm <- function(model, ...)
{
    extract.coef.default(model=model, ...)
}


#' extract.coef
#' 
#' Extract Coefficient Information from glm Models
#' 
#' Gets the coefficient values and standard errors, and variable names from a glm model.
#' 
#' @author Jared P. Lander
#' @aliases extract.coef
#' @export extract.coef
#' @param model Model object to extract information from.
#' @param \dots Further arguments
#' @return A \code{\link{data.frame}} containing the coefficient, the standard error and the variable name.
#' @examples
#' require(ggplot2)
#' data(diamonds)
#' mod1 <- lm(price ~ carat + cut + x, data=diamonds)
#' mod2 <- glm(price > 10000 ~ carat + cut + x, data=diamonds, family=binomial(link="logit"))
#' extract.coef(mod1)
#' extract.coef(mod2)
#' 
extract.coef <- function(model, ...)
{
    UseMethod(generic="extract.coef", object=model)
}