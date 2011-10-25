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
#' Dotplot for model coefficients
#'
#' A graphical display of the coefficients and standard errors from a fitted model
#'
#' \code{coefplot} is the S3 generic method for plotting the coefficients from a fitted model.
#'
#' @aliases coefplot.lm coefplot.rxLinMod
#' @author Jared P. Lander
#' @param model The model we are graphing
#' @param title  The name of the plot, if NULL then no name is given
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
#' @param \dots other arguments
### non-listed arguments
#' @param factors Vector of factor variables that will be the only ones shown
#' @param only logical; If factors has a value this determines how interactions are treated.  True means just that variable will be shown and not its interactions.  False means interactions will be included.
#' @param shorten logical or character; If \code{FALSE} then coefficients for factor levels will include their variable name.  If \code{TRUE} coefficients for factor levels will be stripped of their variable names.  If a character vector of variables only coefficients for factor levels associated with those variables will the variable names stripped.
#' @return If \code{plot} is \code{TRUE} then a \code{\link{ggplot}} object is returned.  Otherwise a \code{\link{data.frame}} listing coeffcients and confidence bands is returned.
#' @seealso \code{\link{lm}} \code{\link{glm}} \code{\link{rxLinMod}} \code{\link{ggplot}} \code{\link{coefplot}} \code{\link{plotcoef}}
#' @export coefplot.lm coefplot.rxLinMod
#' @usage coefplot.lm(model, title="Coefficient Plot", xlab="Value", ylab="Coefficient", innerCI=1, outerCI=2, lwdInner=1, lwdOuter=0,  color="blue", cex=.8, textAngle=0, numberAngle=0, zeroColor="grey", zeroLWD=1, zeroType=2, facet=FALSE, scales="free", sort="natural", decreasing=FALSE, numeric=FALSE, fillColor="grey", alpha=1/2, horizontal=FALSE, intercept=TRUE, plot=TRUE, ...)
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
						sort="natural", decreasing=FALSE,
						numeric=FALSE, fillColor="grey", alpha=1/2,
						horizontal=FALSE,
						intercept=TRUE, plot=TRUE, ...)
{
	theDots <- list(...)
	
 
 	## if they are treating a factor as numeric, then they must specify exactly one factor
 	## hopefully this will soon expand to listing multiple factors
	if(numeric & length(grep("factors", names(theDots)))!=1)
	{
		stop("When treating a factor variable as numeric, the specific factor must be specified using \"factors\"")
	}else if(numeric)
	{
		# if we are treating it as numeric, then the sorting should be numeric
		sort="mag"
	}

    # get the information on the model
    modelInfo <- getModelInfo(model, ...)
	
	# get the coef and SE from modelInfo
	modelCoef <- modelInfo$coef# the coefficients
	modelSE <- modelInfo$SE# the standard errors
 	modelMatched <- modelInfo$matchedVars# the data.frame matching coefficients to variables

	# all the info about the coefficients
	modelCI <- data.frame(LowOuter=modelCoef - outerCI*modelSE, HighOuter=modelCoef + outerCI*modelSE, LowInner=modelCoef - innerCI*modelSE, HighInner=modelCoef + innerCI*modelSE, Coef=modelCoef) # build a data.frame of the confidence bounds and original coefficients
    names(modelCI) <- c("LowOuter", "HighOuter", "LowInner", "HighInner", "Coef")

	modelCI$Name <- rownames(modelCI)	## grab the coefficient names into the data.frame
 
	## join the factor coefficient info to the data.frame holding the coefficient info
	modelMatcher <- modelMatched[, c("Checkers", "Coef", "CoefShort")]
	names(modelMatcher)[2] <- "Name"
	modelMatcher$Name <- as.character(modelMatcher$Name)
	modelCI <- join(modelCI, modelMatcher, by="Name")
	
	rm(modelMatcher); gc()		# housekeeping
	
	# since we will be using coef short for the coefficient labels the numeric variables need to be given CoefShort elements which will be taken from the Name column
	modelCI$CoefShort <- ifelse(is.na(modelCI$CoefShort), modelCI$Name, modelCI$CoefShort)
	
	# Similar for the Checkers column

	modelCI$Checkers <- ifelse(is.na(modelCI$Checkers), "Numeric", modelCI$Checkers)

	## if the intercept is not to be shown, then remove it
	if(intercept == FALSE | numeric)		## remove the intercept if so desired
	{
		theIntercept <- which(modelCI$Name == "(Intercept)")	# find the variable that is the intercept
		# make sure the intercept is actually present, if so, remove it
		if(length(theIntercept) > 0)
		{
			# remove the intercept
			modelCI <- modelCI[-theIntercept, ]
		}
  		rm(theIntercept); gc()		# housekeeping
	}
	
	# if there are no good coefficients, then stop
	if(nrow(modelCI) == 0)
	{
		stop("There are no valid coeficients to plot", call.=FALSE)
	}
  
	## possible orderings of the coefficients
	ordering <- switch(sort,
							natural=order(1:nrow(modelCI), decreasing=decreasing), 	# the way the data came in
							normal=order(1:nrow(modelCI), decreasing=decreasing),	# the way the data came in
							nat=order(1:nrow(modelCI), decreasing=decreasing), 			# the way the data came in
							magnitude=order(modelCI$Coef, decreasing=decreasing), 		#  size order
							mag=order(modelCI$Coef, decreasing=decreasing), 			# size order
							size=order(modelCI$Coef, decreasing=decreasing),			# size order
							alphabetical=order(modelCI$Name, decreasing=decreasing), 	# alphabetical order
							alpha=order(modelCI$Name, decreasing=decreasing),			# alphabetical order
							order(1:nrow(modelCI))		# default, the way it came in
					)
	
	# implement the ordering
	modelCI <- modelCI[ordering, ]
	#return(modelCI)
	#return(modelCI$Name)
	modelCI$CoefShort <- factor(modelCI$CoefShort, levels=modelCI$CoefShort)
	#print(modelCI$CoefShort)
	# which columns will be kept in the melted data.frame
	keepCols <- c("LowOuter", "HighOuter", "LowInner", "HighInner", "Coef", "Checkers", "CoefShort")

	# melt the data frame so it is suitable for ggplot
	modelMelt <- reshape2::melt(data=modelCI[ ,keepCols], id.vars=c("CoefShort", "Checkers"), variable_name="Type")
	
	# just the outerCI info
	modelMeltOuter <- modelMelt[modelMelt$Type %in% c("LowOuter", "HighOuter"), ]	# pull out the 95% CI
	
	# just the innerCI info
	modelMeltInner <- modelMelt[modelMelt$Type %in% c("LowInner", "HighInner"), ]	# pull out the 68% CI
	
	## build the layer infos
	# outerCI layer
	outerCIGeom <- list(
						Display=geom_line(aes(x=value, group=CoefShort), data=modelMeltOuter, colour=color, lwd=lwdOuter), None=NULL)
	# innerCI layer
	innerCIGeom <- list(Display=geom_line(aes(x=value, group=CoefShort), data=modelMeltInner, colour=color, lwd=lwdInner), None=NULL)
	# ribbon layer
	ribbonGeom <- list(None=NULL, geom_ribbon(aes(ymin=LowOuter, ymax=HighOuter, group=Checkers), data=modelCI, fill=fillColor, alpha=alpha, lwd=lwdOuter))

	# faceting info
	faceting <- list(None=NULL, Display=facet_wrap(~Checkers, scales=scales))

	## if we are to make the plot
	if(plot)
	{
		if(numeric)
		{
			p <- ggplot(data=modelCI, aes(y=Coef, x=CoefShort))			# the basics of the plot
			p <- p + geom_hline(yintercept=0, colour=zeroColor, linetype=zeroType, lwd=zeroLWD)		# the zero line
			p <- p + ribbonGeom[[numeric + 1]]		# the ribbon
			p <- p + geom_point(colour=color)						# the points
			p <- p + geom_line(data=modelCI, aes(y=HighOuter, x=CoefShort, group=Checkers), colour=color) +
				geom_line(data=modelCI, aes(y=LowOuter, x=CoefShort, group=Checkers), colour=color)
			return(p)
		}
		p <- ggplot(data=modelCI, aes(x=Coef, y=CoefShort))			# the basics of the plot
		p <- p + geom_vline(xintercept=0, colour=zeroColor, linetype=zeroType, lwd=zeroLWD)		# the zero line
		p <- p + outerCIGeom[[(outerCI/outerCI)]] +					# the outer CI bars
			innerCIGeom[[innerCI/innerCI]]						# the inner CI bars
  		p <- p + geom_point(colour=color)						# the points
		p <- p + opts(title=title, axis.text.y=theme_text(angle=textAngle), axis.text.x=theme_text(angle=numberAngle)) + labs(x=xlab, y=ylab)	# labeling and text info
		p <- p + faceting[[facet + 1]]		# faceting
		p <- p + if(horizontal) coord_flip()
  
		rm(modelMelt, modelMeltOuter, modelMeltInner); gc()		# housekeeping
		
		return(p)		# return the ggplot object
	}else
	{
		rm(modelMelt, modelMeltOuter, modelMeltInner); gc()		# housekeeping
		return(modelCI)
	}
}


## just simply call coefplot.lm which will work just fine
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