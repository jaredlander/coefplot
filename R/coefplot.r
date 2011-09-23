## the generic function for coefplot
## this way it can be used for more than just the generalized family of linear models
coefplot <- function(model, ...)
{
    UseMethod(generic="coefplot", object=model)
}

## the lm method for coefplot
coefplot.lm <- function(model, ...)
{
    # get the information on the model
    modelInfo <- getModelInfo(model, reduced=factorsToShow)
}

## the glm method for coefplot
coefplot.glm <- function(model, ...)
{
    plot(coef(model))
}