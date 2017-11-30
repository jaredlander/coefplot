#' @title coefpath
#' @description Visualize the coefficient path resulting from the elastic net
#' @details This is a replacement plot for visualizing the coefficient path resulting from the elastic net. This allows for interactively inspecting the plot so it is easier to disambiguate the coefficients.
#' @author Jared P. Lander
#' @export
#' @param model A \code{\link[glmnet]{glmnet}} model
#' @param \dots Arguments passed on to \code{\link{extractPath}}
#' @return A dygraphs object
#' @examples 
#' 
#' library(glmnet)
#' library(ggplot2)
#' library(useful)
#' data(diamonds)
#' diaX <- useful::build.x(price ~ carat + cut + x - 1, data=diamonds, contrasts = TRUE)
#' diaY <- useful::build.y(price ~ carat + cut + x - 1, data=diamonds)
#' modG1 <- glmnet(x=diaX, y=diaY)
#' coefpath(modG1)
#' 
#' modG2 <- cv.glmnet(x=diaX, y=diaY, nfolds=5)
#' coefpath(modG2)
#' 
#' x <- matrix(rnorm(100*20),100,20)
#' y <- rnorm(100)
#' fit1 <- glmnet(x, y)
#' coefpath(fit1)
#' 
coefpath <- function(model, ...)
{
    UseMethod('coefpath')
}

#' @rdname coefpath
#' @export
#' @importFrom magrittr "%>%"
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param showLegend When to display the legend. Specify "always" to always show the legend. Specify "onmouseover" to only display it when a user mouses over the chart. Specify "follow" to have the legend show as overlay to the chart which follows the mouse. The default behavior is "auto", which results in "always" when more than one series is plotted and "onmouseover" when only a single series is plotted.
#' @param annotate If \code{TRUE} (default) plot the name of the series
#' 
coefpath.glmnet <- function(model,
                            xlab='Log Lambda',
                            ylab='Coefficients',
                            showLegend=c('onmouseover', 'auto', 'always', 
                                         'follow' ,'never'),
                            annotate=TRUE,
                            ...)
{
    # figure out how to show the legend
    showLegend <- match.arg(showLegend)
    
    # get the coefficients in a nice data.frame
    pathDF <- extractPath(model, ...)
    
    # build the graph
    g <- dygraphs::dygraph(pathDF) %>% 
        # nice axis labels
        dygraphs::dyAxis(name='x', label=xlab) %>% 
        dygraphs::dyAxis(name='y', label=ylab) %>% 
        # control the legend
        dygraphs::dyLegend(show='onmouseover') %>% 
        # allow zooming
        dygraphs::dyRangeSelector() %>% 
        # allow unzooming
        dygraphs::dyUnzoom() %>% 
        dygraphs::dyHighlight(highlightCircleSize=3, 
                              highlightSeriesBackgroundAlpha=0.5,
                              highlightSeriesOpts=list(strokeWidth=3))
    
    if(annotate)
    {
        g <- purrr::reduce(.x=names(pathDF)[-1], 
                      .f=annotateSeries, 
                      .init=g, 
                      x=min(pathDF$lambda))
    }
    
    return(g)
}

#' @rdname coefpath
#' @export
#' @importFrom magrittr "%>%"
#' @param colorMin Color for line showing lambda.min
#' @param strokePatternMin Stroke pattern for line showing lambda.min
#' @param labelMin Label for line showing lambda.min
#' @param locMin Location for line showing lambda.min, can be 'bottom' or 'top'
#' @param color1se Color for line showing lambda.1se
#' @param strokePattern1se Stroke pattern for line showing lambda.1se
#' @param label1se Label for line showing lambda.1se
#' @param loc1se Location for line showing lambda.1se, can be 'bottom' or 'top'
#' 
coefpath.cv.glmnet <- function(model,
                            xlab='Log Lambda',
                            ylab='Coefficients',
                            showLegend=c('onmouseover', 'auto', 'always', 
                                         'follow' ,'never'),
                            annotate=TRUE,
                            colorMin='black', strokePatternMin='dotted',
                            labelMin='lambda.min', locMin=c('bottom', 'top'),
                            color1se='black', strokePattern1se='dotted',
                            label1se='lambda.1se', loc1se=c('bottom', 'top'),
                            ...)
{
    # figure out how to show the legend
    showLegend <- match.arg(showLegend)
    locMin <- match.arg(locMin)
    loc1se <- match.arg(loc1se)
    
    g <- coefpath(model$glmnet.fit, ...)
    
    g %>% 
        dygraphs::dyEvent(x=log(model$lambda.min), label=labelMin, 
                          color=colorMin,
                          labelLoc=locMin, strokePattern=strokePatternMin) %>% 
        dygraphs::dyEvent(x=log(model$lambda.1se), label=label1se, 
                          color=color1se,
                          labelLoc=loc1se, strokePattern=strokePattern1se)
}
