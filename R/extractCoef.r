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
#' \dontrun{extract.coef(mod1)}
#' 
extract.coef.default <- function(model, ...)
{
    # get summary of model
    theSumm <- summary(model)
    # get coef and standard error
#     print(theSumm)
#     print(head(model))
    info <- as.data.frame(theSumm$coefficients[, 1:2])
    names(info) <- c("Value", "SE")
    # make a variable tracking the name
    info$Coefficient <- rownames(info)
    
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
#' mod3 <- lm(price ~ carat*cut + x, data=diamonds)
#' extract.coef(mod1)
#' extract.coef(mod2)
#' extract.coef(mod3)
#' \dontrun{
#' mod4 <- rxLinMod(price ~ carat*cut + x, diamonds)
#' }
#' 
extract.coef <- function(model, ...)
{
    UseMethod(generic="extract.coef", object=model)
}


#' extract.coef.rxLinMod
#' 
#' Extract Coefficient Information from rxLinMod Models
#' 
#' Gets the coefficient values and standard errors, and variable names from an rxLinMod model.
#' 
#' @author Jared P. Lander
#' @aliases extract.coef.rxLinMod
#' @inheritParams extract.coef.default
#' @param \dots Further arguments
#' @S3method extract.coef rxLinMod
#' @method extract.coef rxLinMod
#' @return A \code{\link{data.frame}} containing the coefficient, the standard error and the variable name.
#' @examples
#' \dontrun{
#' require(ggplot2)
#' data(diamonds)
#' mod3 <- rxLinMod(price ~ carat + cut + x, data=diamonds)
#' extract.coef(mod3)
#' }
#' 
extract.coef.rxLinMod <- function(model, ...)
{
    # get summary
    theSumm <- summary(model)[[1]]
    # get coefs
    info <- as.data.frame(theSumm$coefficients[, 1:2])
    # give good names
    names(info) <- c("Value", "SE")
    # get variable names
    info$Coefficient <- rownames(info)
    
    return(info)
}


#' extract.coef.rxGlm
#' 
#' Extract Coefficient Information from rxGlm Models
#' 
#' Gets the coefficient values and standard errors, and variable names from an rxGlm model.
#' 
#' @author Jared P. Lander
#' @aliases extract.coef.rxGlm
#' @inheritParams extract.coef.default
#' @param \dots Further arguments
#' @S3method extract.coef rxGlm
#' @method extract.coef rxGlm
#' @return A \code{\link{data.frame}} containing the coefficient, the standard error and the variable name.
#' @examples
#' \dontrun{
#' require(ggplot2)
#' data(diamonds)
#' mod4 <- rxGlm(price ~ carat + cut + x, data=diamonds)
#' mod5 <- rxGlm(price > 10000 ~ carat + cut + x, data=diamonds, fmaily="binomial")
#' extract.coef(mod4)
#' extract.coef(mod5)
#' }
#' 
extract.coef.rxGlm <- function(model, ...)
{
    extract.coef.default(model=model, ...)
}


#' extract.coef.rxLogit
#' 
#' Extract Coefficient Information from rxLogit Models
#' 
#' Gets the coefficient values and standard errors, and variable names from an rxLogit model.
#' 
#' @author Jared P. Lander
#' @aliases extract.coef.rxLogit
#' @inheritParams extract.coef.default
#' @param \dots Further arguments
#' @S3method extract.coef rxLogit
#' @method extract.coef rxLogit
#' @return A \code{\link{data.frame}} containing the coefficient, the standard error and the variable name.
#' @examples
#' \dontrun{
#' require(ggplot2)
#' data(diamonds)
#' mod6 <- rxLogit(price > 10000 ~ carat + cut + x, data=diamonds)
#' extract.coef(mod6)
#' }
#' 
extract.coef.rxLogit <- function(model, ...)
{
    extract.coef.default(model=model, ...)
}
