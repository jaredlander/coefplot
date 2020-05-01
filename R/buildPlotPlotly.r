#' @title buildPlottingPloty.default
#' @description Builds the plotting structure for interactive coefplots
#' @details Uses plotly to make an interactive version of coefplot. Still uses modelCI.
#' @author Jared P. Lander
#' @export
#' @seealso \code{\link{coefplot.default}} \code{\link{coefplot}} \code{\link{buildPlotting.default}}
#' @param modelCI An object created by \code{\link{buildModelCI}}
#' @param title The name of the plot, if NULL then no name is given
#' @param xlab The x label
#' @param ylab The y label
#' @param lwdInner The thickness of the inner confidence interval
#' @param lwdOuter The thickness of the outer confidence interval
#' @param color The color of the points and lines
#' @param shape The shape of the points
#' @param pointSize Size of coefficient point
#' @return a ggplot graph object
#' @examples
#'
#' data(diamonds)
#' mod1 <- lm(price ~ carat + cut, data=diamonds)
#' theCI1 <- coefplot:::buildModelCI(mod1)
#' coefplot:::buildPlottingPloty.default(theCI1)
#' coefplot(mod1, interactive=TRUE)
#' mod2 <- lm(mpg ~ cyl + qsec - 1, data=mtcars)
#' mod3 <- lm(mpg ~ cyl + qsec + disp - 1, data=mtcars)
#' theCI2 <- coefplot:::buildModelCI(mod2)
#' theCI3 <- coefplot:::buildModelCI(mod3)
#' coefplot::buildPlottingPloty.default(theCI2)
#' coefplot::buildPlottingPloty.default(theCI3)
#' coefplot(mod2, interactive=TRUE)
#' coefplot(mod3, interactive=TRUE)
#' 
#' mod4 <- glmnet::glmnet(
#' x=as.matrix(diamonds[, c('carat', 'x', 'y', 'z')]),
#' y=diamonds$price
#' )
#' coefplot(mod4, interactive=TRUE, lambda=0.65)
#' 
buildPlottingPloty.default <- function(
    modelCI,
    title="Coefficient Plot", 
    xlab="Value", ylab="Coefficient",
    lwdInner=3,
    lwdOuter=1,
    color="blue", shape='circle',
    pointSize=8
)
{
    # if there is only one model, use the hard coded version of color
    if(length(unique(modelCI$Model)) == 1)
    {
        outer_line_list <- list(color=color, width=lwdOuter)
        inner_line_list <- list(color=color, width=lwdInner)
        point_list <- list(symbol=shape, size=pointSize, color=color)
    } else
    {
        outer_line_list <- list(width=lwdOuter)
        inner_line_list <- list(width=lwdInner)
        point_list <- list(symbol=shape, size=pointSize)
    }
    
    p <- plotly::plot_ly(data=modelCI, y= ~ Coefficient)
    
    # if the entire column for a CI is not missing, add the segments
    # otherwise don't add the segments
    # because plotly does not fail gracefully when an entire column is NA
    if(!all(is.na(modelCI$LowInner)) && !all(is.na(modelCI$HighInner)))
    {
        p <- p %>% 
        # outer confidence interval
        plotly::add_segments(
            x=~LowOuter,
            xend=~HighOuter,
            y=~Coefficient,
            yend=~Coefficient,
            text=~Coefficient,
            # color=~Model,
            line=outer_line_list,
            # only showing legend for dots
            showlegend=FALSE, 
            # but all elements are connected
            # so clicking on a dot for a model, removes the bars too
            legendgroup=~Model,
            hoverinfo='text'
        ) %>%
        # inner confidence interval
        plotly::add_segments(
            x=~LowInner,
            xend=~HighInner,
            y=~Coefficient,
            yend=~Coefficient,
            text=~Coefficient,
            # color=~Model,
            line=inner_line_list,
            # only showing legend for dots
            showlegend=FALSE, 
            # but all elements are connected
            # so clicking on a dot for a model, removes the bars too
            legendgroup=~Model,
            hoverinfo='text'
        )
    }
    
    p %>% 
        # point estimates
        plotly::add_markers(
            x=~Value,
            text=~Coefficient,
            # color=~Model,
            marker=point_list,
            # only showing legend for dots
            # this will not show up if there is only one model
            showlegend=TRUE, 
            # but all elements are connected
            # so clicking on a dot for a model, removes the bars too
            legendgroup=~Model,
            hoverinfo='text+x'
        ) %>% 
        plotly::layout(
            title=title,
            xaxis=list(title=xlab),
            yaxis=list(title=ylab)
        )
}
