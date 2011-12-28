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
#' www.jaredlander.com
#' @export coefplot plotcoef
#' @keywords coefplot dotplot coefficient coefficients model lm glm rxLinMod linear
#' @import ggplot2 plyr reshape2 useful
#' @examples
#' 
#' data(diamonds)
#' corner(diamonds)
#' model1 <- lm(price ~ carat + cut*color, data=diamonds)
#' model2 <- lm(price ~ carat*color, data=diamonds)
#' coefplot(model1)
#' coefplot(model1, shorten=FALSE)
#' coefplot(model1, shorten=c("cut"))
#' coefplot(model1, shorten=c("cut"), intercept=FALSE)
#' coefplot(model1, factors="cut")
#' coefplot(model1, factors="cut", only=TRUE)
#' coefplot(model1, facet=TRUE)
#' coefplot(model2)
#'
coefplot <- function(model, ...)
{
    UseMethod(generic="coefplot")
}



## the lm method for coefplot
#' Dotplot for lm coefficients
#'
#' A graphical display of the coefficients and standard errors from a fitted lm model
#'
#' \code{\link{coefplot}} is the S3 generic method for plotting the coefficients from a fitted model.
#'
#' This method also plots coefficients from glm (using coefplot.lm) and rxLinMod models (through a redirection from coefplot.rxLinMod)
#'
#' @aliases coefplot.lm
#' @author Jared P. Lander www.jaredlander.com
## @usage coefplot.lm(model, title="Coefficient Plot", xlab="Value", ylab="Coefficient", innerCI=1, outerCI=2, lwdInner=1, lwdOuter=0, color="blue", cex=.8, textAngle=0, numberAngle=0, zeroColor="grey", zeroLWD=1, zeroType=2, facet=FALSE, scales="free", sort="natural", decreasing=FALSE, numeric=FALSE, fillColor="grey", alpha=1/2, horizontal=FALSE, intercept=TRUE, plot=TRUE, ...)
#' @param model The model we are graphing
#' @param title The name of the plot, if NULL then no name is given
#' @param xlab The x label
#' @param ylab The y label
#' @param innerCI How wide the inner confidence interval should be, normally 1 standard deviation.  If 0, then there will be no inner confidence interval.
#' @param outerCI How wide the outer confidence interval should be, normally 2 standard deviations.  If 0, then there will be no outer confidence interval.
#' @param lwdInner The thickness of the inner confidence interval
#' @param lwdOuter The thickness of the outer confidence interval
#' @param color The color of the points and lines
#' @param cex The text size multiplier, currently not used
#' @param textAngle The angle for the coefficient labels, 0 is horizontal
#' @param numberAngle The angle for the value labels, 0 is horizontal
#' @param zeroColor The color of the line indicating 0
#' @param zeroLWD The thickness of the 0 line
#' @param zeroType The type of 0 line, 0 will mean no line
#' @param facet logical; If the coefficients should be faceted by the variables, numeric coefficients (including the intercept) will be one facet
#' @param scales The way the axes should be treated in a faceted plot.  Can be c("fixed", "free", "free_x", "free_y")
#' @param sort Determines the sort order of the coefficients.  Possible values are c("natural", "normal", "magnitude", "size", "alphabetical")
#' @param decreasing logical; Whether the coefficients should be ascending or descending
#' @param numeric logical; If true and factors has exactly one value, then it is displayed in a horizontal graph with constinuous confidence bounds.
#' @param fillColor The color of the confidence bounds for a numeric factor
#' @param alpha The transparency level of the numeric factor's confidence bound
#' @param horizontal logical; If the plot should be displayed horizontally
#' @param intercept logical; Whether the Intercept coefficient should be plotted
#' @param plot logical; If the plot should be drawn, if false then a data.frame of the values will be returned
##See Details for information on \code{factors}, \code{only} and \code{shorten}
### non-listed arguments
#' @param factors Vector of factor variables that will be the only ones shown
#' @param only logical; If factors has a value this determines how interactions are treated.  True means just that variable will be shown and not its interactions.  False means interactions will be included.
#' @param shorten logical or character; If \code{FALSE} then coefficients for factor levels will include their variable name.  If \code{TRUE} coefficients for factor levels will be stripped of their variable names.  If a character vector of variables only coefficients for factor levels associated with those variables will the variable names stripped.
#' @param \dots Arguments passed on to other functions
#' @return If \code{plot} is \code{TRUE} then a \code{\link{ggplot}} object is returned.  Otherwise a \code{\link{data.frame}} listing coeffcients and confidence bands is returned.
#' @seealso \code{\link{lm}} \code{\link{glm}} \code{\link{ggplot}} \code{\link{coefplot}} \code{\link{plotcoef}}
#' @export coefplot.lm
#' @method coefplot lm
#' @S3method coefplot lm
#' @examples
#' 
#' data(diamonds)
#' corner(diamonds)
#' model1 <- lm(price ~ carat + cut*color, data=diamonds)
#' model2 <- lm(price ~ carat*color, data=diamonds)
#' coefplot(model1)
#' coefplot(model1, shorten=FALSE)
#' coefplot(model1, shorten=c("cut"))
#' coefplot(model1, shorten=c("cut"), intercept=FALSE)
#' coefplot(model1, factors="cut")
#' coefplot(model1, factors="cut", only=TRUE)
#' coefplot(model1, facet=TRUE)
#' coefplot(model2)
#'
coefplot.lm <- function(model, title="Coefficient Plot", xlab="Value", ylab="Coefficient", 
						innerCI=1, outerCI=2, lwdInner=1, lwdOuter=0,  color="blue",
						cex=.8, textAngle=0, numberAngle=0,
						zeroColor="grey", zeroLWD=1, zeroType=2,
						facet=FALSE, scales="free",
						sort=c("natural", "normal", "magnitude", "size", "alphabetical"), decreasing=FALSE,
						numeric=FALSE, fillColor="grey", alpha=1/2,
						horizontal=FALSE, factors=NULL, only=NULL, shorten=TRUE,
						intercept=TRUE, plot=TRUE, ...)
{
	theDots <- list(...)
	
    # get variables that have multiple options
    sort <- match.arg(sort)
 
 	## if they are treating a factor as numeric, then they must specify exactly one factor
 	## hopefully this will soon expand to listing multiple factors
	if(numeric & length(factors)!=1)
	{
		stop("When treating a factor variable as numeric, the specific factor must be specified using \"factors\"")
	}else if(numeric)
	{
		# if we are treating it as numeric, then the sorting should be numeric
		sort="alpha"
	}
    
    if(length(factors) > 0)
    {
        factors <- subSpecials(factors)
    }

    modelCI <- buildModelCI(model, outerCI=outerCI, innerCI=innerCI, intercept=intercept, numeric=numeric, sort=sort, decreasing=decreasing, factors=factors, only=only, shorten=shorten, ...)

    if(numeric)
    {
        modelCI$CoefShort <- as.numeric(as.character(modelCI$CoefShort))
    }
    
    # which columns will be kept in the melted data.frame
    keepCols <- c("LowOuter", "HighOuter", "LowInner", "HighInner", "Coef", "Checkers", "CoefShort")

    modelMelting <- meltModelCI(modelCI=modelCI, keepCols=keepCols, id.vars=c("CoefShort", "Checkers"), 
                                variable_name="Type", outerCols=c("LowOuter", "HighOuter"), innerCols=c("LowInner", "HighInner")) 


    modelMelt <- modelMelting$modelMelt 
    modelMeltInner <- modelMelting$modelMeltInner 
    modelMeltOuter <- modelMelting$modelMeltOuter 
    rm(modelMelting); gc()      # housekeeping

	## if we are to make the plot
	if(plot)
	{
        p <- buildPlotting.lm(modelCI=modelCI,
                            modelMeltInner=modelMeltInner, modelMeltOuter=modelMeltOuter,
                           title=title, xlab=xlab, ylab=ylab,
                           lwdInner=lwdInner, lwdOuter=lwdOuter, color=color, cex=cex, textAngle=textAngle, 
                           numberAngle=numberAngle, zeroColor=zeroColor, zeroLWD=zeroLWD, outerCI=outerCI, innerCI=innerCI, multi=FALSE,
                           zeroType=zeroType, numeric=numeric, fillColor=fillColor, alpha=alpha, 
                           horizontal=horizontal, facet=facet, scales=scales)
        
        rm(modelCI); gc()    	# housekeeping
		return(p)		# return the ggplot object
	}else
	{
		#rm(modelMeltOuter, modelMeltInner); gc()		# housekeeping
		return(modelCI)
	}
}


## just simply call coefplot.lm which will work just fine
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
#' # See coefplot.lm for examples
coefplot.rxLinMod <- function(...)
{
    coefplot.lm(...)
}

# no need, glm defaults to lm
## the glm method for coefplot
# coefplot.glm <- function(model, ...)
# {
#     plot(coef(model))
# }


# just another name for it
plotcoef <- function(...)
{
    coefplot(...)
}