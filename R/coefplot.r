## the generic function for coefplot
## this way it can be used for more than just the generalized family of linear models

## Should there be a seperate help file for each coefplot method?

#' Dotplot for model coefficients
#'
#' A graphical display of the coefficients and standard errors from a fitted model
#'
#' \code{coefplot} is the S3 generic method for plotting the coefficients from a fitted model.
#'
#' This can be extended with new methods for other types of models not currently available.
#'
#' A future iteration of \code{coefplot.glm} will also allow for plotting the coefficients on the transformed scale.
#'
#' See \code{\link{coefplot.lm}} for specific documentation and the return value.
#'
#' @aliases coefplot plotcoef
#' @author Jared P. Lander
#' @seealso \code{\link{coefplot.lm}}
#' @param model The fitted model with coefficients to be plotted
#' @param \dots See \code{\link{coefplot.lm}} for argument details
#' @return A ggplot2 object or data.frame.  See details in \code{\link{coefplot.lm}} for more information
#' @export coefplot plotcoef
#' @keywords coefplot dotplot coefficient coefficients model lm glm rxLinMod linear
#' @import ggplot2 plyr reshape2 useful
#' @examples
#' 
#' data(diamonds)
#' head(diamonds)
#' model1 <- lm(price ~ carat + cut*color, data=diamonds)
#' model2 <- lm(price ~ carat*color, data=diamonds)
#' model3 <- glm(price > 10000 ~ carat*color, data=diamonds)
#' coefplot(model1)
#' coefplot(model2)
#' coefplot(model3)
#'
coefplot <- function(model, ...)
{
    UseMethod(generic="coefplot")
}



## the default method for coefplot
#' coefplot.default
#' 
#' Dotplot for coefficients
#'
#' A graphical display of the coefficients and standard errors from a fitted model
#'
#' \code{\link{coefplot}} is the S3 generic method for plotting the coefficients from a fitted model.
#'
#' This method also plots coefficients from glm (using coefplot.lm) and rxLinMod models (through a redirection from coefplot.rxLinMod)
#'
#' @aliases coefplot.default
#' @author Jared P. Lander
#' @param model The model to plot.
#' @param title The name of the plot, if NULL then no name is given
#' @param xlab The x label
#' @param ylab The y label
#' @param innerCI How wide the inner confidence interval should be, normally 1 standard deviation.  If 0, then there will be no inner confidence interval.
#' @param outerCI How wide the outer confidence interval should be, normally 2 standard deviations.  If 0, then there will be no outer confidence interval.
#' @param lwdInner The thickness of the inner confidence interval
#' @param lwdOuter The thickness of the outer confidence interval
#' @param pointSize Size of coefficient point
#' @param color The color of the points and lines
#' @param cex The text size multiplier, currently not used
#' @param textAngle The angle for the coefficient labels, 0 is horizontal
#' @param numberAngle The angle for the value labels, 0 is horizontal
#' @param zeroColor The color of the line indicating 0
#' @param zeroLWD The thickness of the 0 line
#' @param zeroType The type of 0 line, 0 will mean no line
#' @param facet logical; If the coefficients should be faceted by the variables, numeric coefficients (including the intercept) will be one facet.  Currently not available.
#' @param scales The way the axes should be treated in a faceted plot.  Can be c("fixed", "free", "free_x", "free_y").  Currently not available.
#' @param sort Determines the sort order of the coefficients.  Possible values are c("natural", "normal", "magnitude", "size", "alphabetical")
#' @param decreasing logical; Whether the coefficients should be ascending or descending
#' @param numeric logical; If true and factors has exactly one value, then it is displayed in a horizontal graph with constinuous confidence bounds.  Currently not available.
#' @param fillColor The color of the confidence bounds for a numeric factor.  Currently not available.
#' @param alpha The transparency level of the numeric factor's confidence bound.  Currently not available.
#' @param horizontal logical; If the plot should be displayed horizontally.  Currently not available.
#' @param intercept logical; Whether the Intercept coefficient should be plotted
#' @param interceptName Specifies name of intercept it case it is not the default of "(Intercept").
#' @param plot logical; If the plot should be drawn, if false then a data.frame of the values will be returned
#' @param variables A character vector specifying which variables to keep.  Each individual variable has to be specfied, so individual levels of factors must be specified.  We are working on making this easier to implement, but this is the only option for now.
##See Details for information on \code{factors}, \code{only} and \code{shorten}
#' @param newNames Named character vector of new names for coefficients
### non-listed arguments
#' @param factors Vector of factor variables that will be the only ones shown
#' @param only logical; If factors has a value this determines how interactions are treated.  True means just that variable will be shown and not its interactions.  False means interactions will be included.
#' @param shorten logical or character; If \code{FALSE} then coefficients for factor levels will include their variable name.  If \code{TRUE} coefficients for factor levels will be stripped of their variable names.  If a character vector of variables only coefficients for factor levels associated with those variables will the variable names stripped.  Currently not available.
#' @param \dots Arguments passed on to other functions
#' @return If \code{plot} is \code{TRUE} then a \code{\link{ggplot}} object is returned.  Otherwise a \code{\link{data.frame}} listing coeffcients and confidence bands is returned.
#' @seealso \code{\link{lm}} \code{\link{glm}} \code{\link{ggplot}} \code{\link{coefplot}} \code{\link{plotcoef}}
#' @export coefplot.default
#' @method coefplot default
#' @S3method coefplot default
#' @examples
#' 
#' data(diamonds)
#' head(diamonds)
#' model1 <- lm(price ~ carat + cut*color, data=diamonds)
#' model2 <- lm(price ~ carat*color, data=diamonds)
#' coefplot(model1)
#' coefplot(model2)
#'
coefplot.default <- function(model, title="Coefficient Plot", xlab="Value", ylab="Coefficient", 
						innerCI=1, outerCI=2, lwdInner=1, lwdOuter=0, pointSize=3,  color="blue",
						cex=.8, textAngle=0, numberAngle=0,
						zeroColor="grey", zeroLWD=1, zeroType=2,
						facet=FALSE, scales="free",
						sort=c("natural", "magnitude", "alphabetical"), decreasing=FALSE,
						numeric=FALSE, fillColor="grey", alpha=1/2,
						horizontal=FALSE, factors=NULL, only=NULL, shorten=TRUE,
						intercept=TRUE, interceptName="(Intercept)", variables=NULL, newNames=NULL, plot=TRUE, ...)
{
	theDots <- list(...)
	
    # get variables that have multiple options
    sort <- match.arg(sort)
    
    # construct a data.frame containing confidence interval information
    modelCI <- buildModelCI(model, outerCI=outerCI, innerCI=innerCI, intercept=intercept, variables=variables, newNames=newNames,
                            numeric=numeric, sort=sort, 
                            decreasing=decreasing, factors=factors, only=only, shorten=shorten, ...)

    # if not plotting just return the modelCI data.frame
    if(!plot)
    {
        return(modelCI)
    }
    
    # which columns will be kept in the melted data.frame
#    keepCols <- c("LowOuter", "HighOuter", "LowInner", "HighInner", "Coefficient", "Model")

#     modelMelting <- meltModelCI(modelCI=modelCI, keepCols=keepCols, id.vars=c("Coefficient", "Model"), 
#                                 variable.name="Type", value.name="Value", 
#                                 outerCols=c("LowOuter", "HighOuter"), 
#                                 innerCols=c("LowInner", "HighInner")) 

    #modelMelt <- modelMelting$modelMelt 
    #modelMeltInner <- modelMelting$modelMeltInner 
    #modelMeltOuter <- modelMelting$modelMeltOuter 
    #rm(modelMelting);      # housekeeping
    
    p <- buildPlotting.default(modelCI=modelCI,
                        #modelMeltInner=modelMeltInner, modelMeltOuter=modelMeltOuter,
                       title=title, xlab=xlab, ylab=ylab,
                       lwdInner=lwdInner, lwdOuter=lwdOuter, pointSize=pointSize, color=color, cex=cex, textAngle=textAngle, 
                       numberAngle=numberAngle, zeroColor=zeroColor, zeroLWD=zeroLWD, outerCI=outerCI, innerCI=innerCI, multi=FALSE,
                       zeroType=zeroType, numeric=numeric, fillColor=fillColor, alpha=alpha, 
                       horizontal=horizontal, facet=facet, scales=scales)
    
    #rm(modelCI);    	# housekeeping
	return(p)		# return the ggplot object
}

#' coefplot.lm
#' 
#' Dotplot for lm coefficients
#'
#' A graphical display of the coefficients and standard errors from a fitted lm model
#'
#' \code{\link{coefplot}} is the S3 generic method for plotting the coefficients from a fitted model.
#'
#' For more information on this function and it's arguments see \code{\link{coefplot.default}}
#'
#' @aliases coefplot.lm
#' @export coefplot.lm
#' @method coefplot lm
#' @S3method coefplot lm
#' @author Jared P. Lander
#' @param \dots All arguments are passed on to \code{\link{coefplot.default}}.  Please see that function for argument information.
#' @return A ggplot object.  See \code{\link{coefplot.lm}} for more information.
#' @examples
#' 
#' model1 <- lm(price ~ carat + cut*color, data=diamonds)
#' coefplot(model1)
coefplot.lm <- function(...)
{
    coefplot.default(...)
}

#' coefplot.glm
#' 
#' Dotplot for glm coefficients
#'
#' A graphical display of the coefficients and standard errors from a fitted glm model
#'
#' \code{\link{coefplot}} is the S3 generic method for plotting the coefficients from a fitted model.
#'
#' For more information on this function and it's arguments see \code{\link{coefplot.default}}
#'
#' @aliases coefplot.glm
#' @export coefplot.glm
#' @method coefplot glm
#' @S3method coefplot glm
#' @author Jared P. Lander
#' @param \dots All arguments are passed on to \code{\link{coefplot.default}}.  Please see that function for argument information.
#' @return A ggplot object.  See \code{\link{coefplot.lm}} for more information.
#' @examples
#' 
#' model2 <- glm(price > 10000 ~ carat + cut*color, data=diamonds, family=binomial(link="logit"))
#' coefplot(model2)
coefplot.glm <- function(...)
{
    coefplot.default(...)
}


#' coefplot.rxGlm
#' 
#' Dotplot for rxGlm coefficients
#'
#' A graphical display of the coefficients and standard errors from a fitted rxGlm model
#'
#' \code{\link{coefplot}} is the S3 generic method for plotting the coefficients from a fitted model.
#'
#' For more information on this function and it's arguments see \code{\link{coefplot.default}}
#'
#' @aliases coefplot.rxGlm
#' @export coefplot.rxGlm
#' @method coefplot rxGlm
#' @S3method coefplot rxGlm
#' @author Jared P. Lander
#' @param \dots All arguments are passed on to \code{\link{coefplot.default}}.  Please see that function for argument information.
#' @return A ggplot object.  See \code{\link{coefplot.lm}} for more information.
#' @examples
#' 
#' \dontrun{
#' mod4 <- rxGlm(price ~ carat + cut + x, data=diamonds)
#' mod5 <- rxGlm(price > 10000 ~ carat + cut + x, data=diamonds, fmaily="binomial")
#' coefplot(mod4)
#' coefplot(mod5)
#' }
#' 
coefplot.rxGlm <- function(...)
{
    coefplot.default(...)
}

## just simply call coefplot.lm which will work just fine
#' coefplot.rxLinMod
#' 
#' Dotplot for rxLinMod coefficients
#'
#' A graphical display of the coefficients and standard errors from a fitted rxLinMod model
#'
#' \code{\link{coefplot}} is the S3 generic method for plotting the coefficients from a fitted model.
#'
#' For more information on this function and it's arguments see \code{\link{coefplot.lm}}
#'
#' @aliases coefplot.rxLinMod
#' @export coefplot.rxLinMod
#' @method coefplot rxLinMod
#' @S3method coefplot rxLinMod
#' @author Jared P. Lander www.jaredlander.com
#' @param \dots All arguments are passed on to \code{\link{coefplot.lm}}.  Please see that function for argument information.
#' @return A ggplot object.  See \code{\link{coefplot.lm}} for more information.
#' @examples
#' 
#' \dontrun{
#' data(diamonds)
#' mod3 <- rxLinMod(price ~ carat + cut + x, data=diamonds)
#' coefplot(mod3)
#' }
coefplot.rxLinMod <- function(...)
{
    coefplot.default(...)
}


## just simply call coefplot.lm which will work just fine
#' coefplot.rxLogit
#' 
#' Dotplot for rxLogit coefficients
#'
#' A graphical display of the coefficients and standard errors from a fitted rxLogit model
#'
#' \code{\link{coefplot}} is the S3 generic method for plotting the coefficients from a fitted model.
#'
#' For more information on this function and it's arguments see \code{\link{coefplot.lm}}
#'
#' @aliases coefplot.rxLogit
#' @export coefplot.rxLogit
#' @method coefplot rxLogit
#' @S3method coefplot rxLogit
#' @author Jared P. Lander www.jaredlander.com
#' @param \dots All arguments are passed on to \code{\link{coefplot.lm}}.  Please see that function for argument information.
#' @return A ggplot object.  See \code{\link{coefplot.lm}} for more information.
#' @examples
#' 
#' \dontrun{
#' data(diamonds)
#' mod6 <- rxLogit(price > 10000 ~ carat + cut + x, data=diamonds)
#' coefplot(mod6)
#' }
coefplot.rxLogit <- function(...)
{
    coefplot.default(...)
}


# just another name for it
plotcoef <- function(...)
{
    coefplot(...)
}