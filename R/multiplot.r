### Functions for plotting multiple coefplots at once
#' Plot multiple coefplots
#'
#' Plot the coeffcients from multiple models
#'
#' Plots a graph similar to \code{\link{coefplot}} but for multiple plots at once
#'
#' @export multiplot
#' @seealso \code{link{coefplot}}
#' @param \dots Models to be plotted
#' @return A ggplot object
#' @examples
#'
#' data(diamonds)
#' model1 <- lm(price ~ carat + cut, data=diamonds)
#' model2 <- lm(price ~ carat + cut + color, data=diamonds)
#' multiplot(model1, model2)
multiplot <- function(..., outerCI=2, innerCI=1, intercept=TRUE, numeric=FALSE, sort="natural", decreasing=TRUE)
{
    # grab the models
    theDots <- list(...)
    
    # need to add arguments for buildModelCI
    # functionize modelMelt
    # need to change getModelInfo and buildModelCI and coefplot.lm so that shorten, factors and only are normal arguments and not part of ..., that way it will work better for this
    # get the modelCI for each model and make one big data.frame
    modelCI <- ldply(theDots, .fun=buildModelCI, outerCI=outerCI, innerCI=innerCI, intercept=intercept, numeric=numeric, sort=sort, decreasing=decreasing)
    
    # Turn the Call into a unique identifier for each model
    modelCI$Call <- as.numeric(factor(modelCI$Call, levels=unique(modelCI$Call)))
    
    return(modelCI)
}
