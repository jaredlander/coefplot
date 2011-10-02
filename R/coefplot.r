## the generic function for coefplot
## this way it can be used for more than just the generalized family of linear models
coefplot <- function(model, ...)
{
    UseMethod(generic="coefplot", object=model)
}


## the lm method for coefplot
## @model (lm object) the model we are graphing
## @title  (character) the name of the plot, if NULL then no name is given
## @xlab (character) the x label
## @ylab (character) the y label
## @innerCI (numeric) how wide the inner confidence interval should be, normally 1 standard deviation, if 0, then there will be no inner confidence interval
## @outerCI (numeric) how wide the outer confidence interval should be, normally 2 standard deviation, if 0, then there will be no outer confidence interval
## @lwdInner (numeric) the thickness of the inner confidence interval
## @lwdouter (numeric) the thickness of the outer confidence interval
## @color (character) the color of the points and lines
## @cex (numeric) the text size multiplier, currently not used
## @textAngle (numeric) the angle for the coefficient labels, 0 is horizontal
## @numericAngle (numeric) the angle for the value labels, 0 is horizontal
## @zeroColor (character) the color of the line indicating 0
## @zeroLWD (numeric) the thickness of the 0 line
## @zeroType (numeric) the type of 0 line, 0 will mean no line
## @facet (logical) if the coefficients should be faceted by the variables, numeric coefficients will be one facet
## @scales (character) the way the axes should be treated in a faceted plot
## @intercept (logical) whether the Intercept coefficient should be plotted
## @plot (logical) if the plot should be drawn, if false then a data.frame of the values will be returned
## @... other arguments
#####
### non-listed arguments
## @factors (character vector) list of factor vars that will be the only ones shown
## @only: (logical) if factors restricts what we are looking at then decide if we want just that variable or the stuff it interacts with too
coefplot.lm <- function(model, title="Coefficient Plot", xlab="Value", ylab="Coefficient", 
						innerCI=1, outerCI=2, lwdInner=1, lwdOuter=0,  color="blue",
						cex=.8, textAngle=0, numberAngle=0,
						zeroColor="grey", zeroLWD=1, zeroType=2,
						facet=FALSE, scales="free",
						sort="natural", decreasing=FALSE,
						intercept=TRUE, plot=TRUE, ...)
{
    # get the information on the model
    modelInfo <- getModelInfo(model, ...)
	
	# get the coef and SE from modelInfo
	modelCoef <- modelInfo$coef# the coefficients
	modelSE <- modelInfo$SE# the standard errors
	modelMatched <- modelInfo$matchedVars# the data.frame matching coefficients to variables
	
	# all the info about the coefficients
	modelCI <- data.frame(LowOuter=modelCoef - outerCI*modelSE, HighOuter=modelCoef + outerCI*modelSE, LowInner=modelCoef - innerCI*modelSE, HighInner=modelCoef + innerCI*modelSE, Coef=modelCoef) # build a data.frame of the confidence bounds and original coefficients
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
	if(intercept == FALSE)		## remove the intercept if so desired
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
	outerCIGeom <- list(Display=geom_line(aes(x=value, group=CoefShort), data=modelMeltOuter, colour=color, lwd=lwdOuter), None=NULL)
	# innerCI layer
	innerCIGeom <- list(Display=geom_line(aes(x=value, group=CoefShort), data=modelMeltInner, colour=color, lwd=lwdInner), None=NULL)
	
	# faceting info
	faceting <- list(None=NULL, Display=facet_wrap(~Checkers, scales=scales))

	## if we are to make the plot
	if(plot)
	{
		p <- ggplot(data=modelCI, aes(x=Coef, y=CoefShort))			# the basics of the plot
		p <- p + geom_vline(xintercept=0, colour=zeroColor, linetype=zeroType, lwd=zeroLWD)		# the zero line
		p <- p + outerCIGeom[[outerCI/outerCI]] +					# the outer CI bars
			innerCIGeom[[innerCI/innerCI]]						# the inner CI bars
  		p <- p + geom_point(colour=color)						# the points
		p <- p + opts(title=title, axis.text.y=theme_text(angle=textAngle), axis.text.x=theme_text(angle=numberAngle)) + labs(x=xlab, y=ylab)	# labeling and text info
		p <- p + faceting[[facet + 1]]		# faceting
		
		rm(modelMelt, modelMeltOuter, modelMeltInner); gc()		# housekeeping
		
		return(p)		# return the ggplot object
	}else
	{
		rm(modelMelt, modelMeltOuter, modelMeltInner); gc()		# housekeeping
		return(modelCI)
	}
}

## the glm method for coefplot
# coefplot.glm <- function(model, ...)
# {
#     plot(coef(model))
# }