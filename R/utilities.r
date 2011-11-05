## Utilities
## Written by Jared P. Lander
## See LISCENSE for copyright information


## @modelFactorVars: (character vector) names of variables that are factors
## @modelModel: (model.matrix) the model.matrix from the model, I would like this to be changed to accept a smaller set of the model.matrix so there is less data being passed arounf
## @modelCoefs: (character vector) the coefficient names that will be matched and shortened
## @shorten:  (logical or character vector) if true all variables will be shortened, if false, none will be (to save computation), if a character vector, only the ones listed will be shortened, the other will remain (this may get VERY complicated
## @factors: (character vector) a list of vectors to work with if we are only interested in a few
## @only: (logical) if factors restricts what we are looking at then decide if we want just that variable or the stuff it interacts with too
## have to finish dealing with only showing some factors while also shortening some, all or none
##      should be done! Yay!
buildFactorDF <- function(modelFactorVars, modelModel, modelCoefs, shorten=TRUE, factors=NULL, only=NULL)
{
    # if we are only looking for some factors, just check those saving time on the rest
    # needs to be changed to work with exclude
#      if(!is.null(factors))
#      {
#          modelFactorVars <- factors
#      }
    
    # build a data.frame that matches each factor variable with it's levels
    varDFTemp <- adply(modelFactorVars, 1, function(x, modelD) { expand.grid(x, extractLevels(x, modelD), stringsAsFactors=FALSE) }, modelModel)	## Build a frame of the variables and the coefficient names for the factor variables
	names(varDFTemp)[2:3] <- c("Var", "Pivot")		## give good names to the frame

    # match each level to every coefficient (factor or numeric)
	varDF <- expand.grid(varDFTemp$Pivot, modelCoefs, stringsAsFactors=FALSE)
	names(varDF)[1:2] <- c("Pivot", "Coef")		## give good names to the frame

    # join the two data.frames so we have variable name, levels name and coefficient names
    varDF <- join(varDF, varDFTemp, by="Pivot")

    rm(varDFTemp); gc() # housekeeping
	
    ## create columns to hold altered versions of the variable and pivot, it replaces special characters with their excaped versions
    varDF$PivotAlter <- varDF$Pivot
	varDF$VarAlter <- varDF$Var
	
    ## the special characters and their escaped equivalents
	specials <- c("!", "(", ")", "-", "=", ".")
	
    ## go through and do the replacing
	alterList <- subSpecials(varDF$VarAlter, varDF$PivotAlter, specialChars=specials)
    
    ## put the subbed values back
    varDF$VarAlter <- alterList[[1]]
    varDF$PivotAlter <- alterList[[2]]
    
    rm(alterList); gc() # housekeeping
    

    # set up a column to keep track of which combinations are good
	varDF$Valid<- NA
	
    # the short names of the coefficients
	varDF$CoefShort <- NA
    
    # see if the coefficient is equivalent to the variable
    varDF <- ddply(.data=varDF, .variables="PivotAlter", .fun=function(DF) { DF$Valid=regexpr(unique(DF$PivotAlter), DF$Coef, ignore.case=FALSE); return(DF) })

    # just take the ones that match
    varDF <- varDF[varDF$Valid > 0, ]
    
    varDF$VarCheck <- varDF$VarAlter
    
    ## if a list of variables to shorten was given, then make sure that the other variable names won't be subbed out
    if(identical(class(shorten), "character"))      # maybe this should be class(shorten) == "character" instead to make things safer?
    {
        # make the ones that aren't listed "" so that they won't be subbed out
        varDF[!varDF$Var %in% shorten, "VarAlter"] <- ""
    }
    
    ## group the variable names to sub out with a "|" between each so any will be subbed out
    ## this now creates two variables like that
    ## Subbers is used for the coefficient shortening
    ## Checkers is used for narrowing down the the variables
    varDF <- ddply(varDF, .(Coef), function(vect, namer, checker, collapse, keepers) { vect$Subbers <- paste(vect[, c(namer)], collapse=collapse); vect$Checkers <- paste(vect[, c(checker)], collapse=collapse); return(vect[1, keepers]) }, namer="VarAlter", checker="VarCheck", collapse="|", keepers=c("Var", "Coef", "Subbers", "Checkers", "CoefShort"))

    ## if only certain factors are to be shown, narrow down the list to them
    if(!is.null(factors))
    {
    	theCheckers <- strsplit(x=varDF$Checkers, split="|", fixed=TRUE)
	 
    	## if they only want that variable and not it's interactions
		if(identical(only, TRUE))
		{
			varDF <- varDF[varDF$Checkers %in% factors, ]
		}else
		{
			## if any of the variables are in the factors to keep then keep it
            ######################
            ## need to adjust it so the user can specify just the interaction of factors, and not the factors themselves
			theKeepers <- laply(theCheckers, function(x, toCheck) { any(x %in% toCheck) }, toCheck=factors)
			varDF <- varDF[theKeepers, ]
			rm(theKeepers); gc()
		}
		
		rm(theCheckers); gc()
    }
    
    # if we are not supposed to shorten the coefficients at all (shorten==FALSE) then just swap Coef into CoefShort
    # this can be done so that the caller function just grabs Coef instead of CoefShort
    # would be nice if this can be done higher up so not as much processing needs to be done
    if(identical(shorten, FALSE))
    {
        varDF$CoefShort <- varDF$Coef
        return(varDF[, c("Var", "Checkers", "Coef", "CoefShort")])
    }
    
    # now sub out the subbers from the coef to make coef short
    varDF <- ddply(varDF, .(Subbers), function(DF) { DF$CoefShort <- gsub(paste("(^|:)", "(", unique(DF$Subbers), ")", sep=""), "\\1", DF$Coef); return(DF) } )

    # return the results
	return(varDF[, c("Var", "Checkers", "Coef", "CoefShort")])
}


## get the levels of factor variables
## @varName:  the variable to pay attention to
## @modelModel:  the model of interest
extractLevels <- function(varName, modelModel)
{
    # put the variable name in front of the level
    paste(varName, levels(factor(modelModel[[varName]])), sep="")
}

# have to get the levels from the coef names
# factors and only not needed because that is already taken care of previously
## ALMOST IDENTICAL to buildFactorDF except in the way it does expand.grid
##          would be awesome to combine the two
rxVarMatcher <- function(modelFactorVars, modelCoefNames, modelCoefs, shorten=TRUE, factors=NULL, only=NULL)
{
    ## match a factor var to those that are just that var or interaction with a numeric
    ## do it for each factor var
    # then will find combinations
    # put each factor var with each coefficient
    varDF <- expand.grid(modelFactorVars, modelCoefNames, stringsAsFactors=FALSE)
    names(varDF) <- c("Var", "Coef")
    
    ## check if the variable matches the coef it is paired with
    # create columns to hold altered versions of the variable and pivot, it replaces special characters with their excaped versions
    varDF$VarAlter <- varDF$Var
	
    ## the special characters and their escaped equivalents
	specials <- c("!", "(", ")", "-", "=", ".")
	
    # go through and do the replacing
	alterList <- subSpecials(varDF$VarAlter, specialChars=specials)
    
    # put the subbed values back
    varDF$VarAlter <- alterList[[1]]
    varDF$VarCheck <- varDF$VarAlter
    varDF$CoefShort <- NA
    
    rm(alterList); gc() # housekeeping

    # now check VarAlter against coef
    varDF <- ddply(varDF, .variables="Var", .fun=function(DF) { DF$Valid <- regexpr(pattern=paste("(^| for |, )(", unique(DF$VarAlter), ")=", sep=""), text=DF$Coef); return(DF) })

    # only keep the valid ones
    varDF <- varDF[varDF$Valid != -1, ]
    
    ## if a list of variables to shorten was given, then make sure that the other variable names won't be subbed out
    if(identical(class(shorten), "character"))      # maybe this should be class(shorten) == "character" instead to make things safer?
    {
        # make the ones that aren't listed "" so that they won't be subbed out
        varDF[!varDF$Var %in% shorten, "VarAlter"] <- ""
    }
    
    ## group the variable names to sub out with a "|" between each so any will be subbed out
    ## this now creates two variables like that
    ## Subbers is used for the coefficient shortening
    ## Checkers is used for narrowing down the the variables
    varDF <- ddply(varDF, .(Coef), function(vect, namer, checker, collapse, keepers) { vect$Subbers <- paste(vect[, c(namer)], collapse=collapse); vect$Checkers <- paste(vect[, c(checker)], collapse=collapse); return(vect[1, keepers]) }, namer="VarAlter", checker="VarCheck", collapse="|", keepers=c("Var", "Coef", "Subbers", "Checkers", "CoefShort"))
    
    # if we are not supposed to shorten the coefficients at all (shorten==FALSE) then just swap Coef into CoefShort
    # this can be done so that the caller function just grabs Coef instead of CoefShort
    # would be nice if this can be done higher up so not as much processing needs to be done
    if(identical(shorten, FALSE))
    {
        varDF$CoefShort <- varDF$Coef
        return(varDF[, c("Var", "Checkers", "Coef", "CoefShort")])
    }

    # do the shortening
    varDF <- ddply(varDF, .(Subbers), function(DF) { DF$CoefShort <- gsub(paste("(^|, | for )", "(", unique(DF$Subbers), ")=", sep=""), "\\1", DF$Coef); return(DF) } )

    # return the results
    return(varDF[, c("Var", "Checkers", "Coef", "CoefShort")])
    
    #return(varDF)
}

#' Build data.frame for plotting
#'
#' Builds a data.frame that is appropriate for plotting coefficients
#'
#' This is the workhorse for coefplot, it get's the data all prepared
#'
#' \code{factors} Vector of factor variables that will be the only ones shown
#'
#' \code{only} logical; If factors has a value this determines how interactions are treated.  True means just that variable will be shown and not its interactions.  False means interactions will be included.
#'
#' \code{shorten} logical or character; If \code{FALSE} then coefficients for factor levels will include their variable name.  If \code{TRUE} coefficients for factor levels will be stripped of their variable names.  If a character vector of variables only coefficients for factor levels associated with those variables will the variable names stripped.
#'
#' @aliases buildModelCI
#' @author Jared P. Lander www.jaredlander.com
#' @param model The fitted model to build information on
#' @param innerCI How wide the inner confidence interval should be, normally 1 standard deviation.  If 0, then there will be no inner confidence interval.
#' @param outerCI How wide the outer confidence interval should be, normally 2 standard deviations.  If 0, then there will be no outer confidence interval.
#' @param sort Determines the sort order of the coefficients.  Possible values are c("natural", "normal", "magnitude", "size", "alphabetical")
#' @param decreasing logical; Whether the coefficients should be ascending or descending
#' @param numeric logical; If true and factors has exactly one value, then it is displayed in a horizontal graph with constinuous confidence bounds.
#' @param intercept logical; Whether the Intercept coefficient should be plotted
#' @param \dots See Details for information on \code{factors}, \code{only} and \code{shorten}
#' @param name A name for the model, if NULL the call will be used
## @param multi logical, If \code{TRUE} a column is added denoting which model the modelCI is for
## @param factors Vector of factor variables that will be the only ones shown
## @param only logical; If factors has a value this determines how interactions are treated.  True means just that variable will be shown and not its interactions.  False means interactions will be included.
## @param shorten logical or character; If \code{FALSE} then coefficients for factor levels will include their variable name.  If \code{TRUE} coefficients for factor levels will be stripped of their variable names.  If a character vector of variables only coefficients for factor levels associated with those variables will the variable names stripped.
#' @return Otherwise a \code{\link{data.frame}} listing coeffcients and confidence bands is returned.
#' @seealso \code{\link{coefplot}}
#' @examples
#'
#' data(diamonds)
#' model1 <- lm(price ~ carat + cut, data=diamonds)
#' model2 <- lm(price ~ carat, data=diamonds)
#' model3 <- lm(price ~ carat + cut + color, data=diamonds)
#' coefplot:::buildModelCI(model1)
#' #coefplot(model1)
#' #coefplot(model2)
#' #coefplot(model3)
#' #coefplot(model3, factors="cut")
#' #coefplot(model3, factors="cut", numeric=T)
#' #coefplot(model3, shorten="cut")
#'
buildModelCI <- function(model, outerCI=2, innerCI=1, intercept=TRUE, numeric=FALSE, sort="natural", decreasing=TRUE, name=NULL, ...)
{
    # get the information on the model
    modelInfo <- getModelInfo(model, ...)
    
	# get the coef and SE from modelInfo
	modelCoef <- modelInfo$coef             # the coefficients
	modelSE <- modelInfo$SE                 # the standard errors
 	modelMatched <- modelInfo$matchedVars   # the data.frame matching coefficients to variables

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
    
    # if a name for the model is provided, use it, otherwise use the call
    if(is.null(name))
    {
        modelCI$Name <- as.character(model$call)[2]
    }else
    {
        modelCI$Name <- name
    }
    
    # convert the pipe in Checkers to a * for better display
    modelCI$Checkers <- gsub("\\|", ":", modelCI$Checkers)
    
    # return the data.frame
    return(modelCI)
}


#' Melt the modelCI
#'
#' Melt a modelCI into a form suitable for plotting
#'
#' \code{\link{buildModelCI}} builds a data.frame for plotting.  This function melts it into plottable form and seperates the coefficient data from the SE data into seprate data.frames
#'
#' @author Jared P. Lander www.jaredlander.com
#' @aliases meltModelCI
#' @seealso \code{\link{coefplot}} \code{\link{buildModelCI}}
#' @param modelCI A \code{\link{data.frame}} as built by \code{\link{buildModelCI}}
#' @param keepCols The columns in modelCI that should be kept as there can be extras
#' @param id.vars The columns to use as ID variables in \code{\link{melt}}
#' @param variable_name Used in \code{\link{melt}} for naming the column that stores the melted values
#' @param innerCols The columns to be included in the \code{\link{data.frame}} of inner standard errors
#' @param outerCols The columns to be included in the \code{\link{data.frame}} of outer standard errors
#' @return A list consisting of
#' \item{modelMelt}{Melted modelCI with all values}
#' \item{modelMeltOuter}{modelMelt with only values associated with the outer standard errors}
#' \item{modelMeltInner}{modelMelt with only values associated with the inner standard errors}
#' @examples
#'
#' data(diamonds)
#' model1 <- lm(price ~ carat + cut, data=diamonds)
#' modeled <- coefplot:::buildModelCI(model1)
#' coefplot:::meltModelCI(modeled)
#'
meltModelCI <- function(modelCI, keepCols=c("LowOuter", "HighOuter", "LowInner", "HighInner", "Coef", "Checkers", "CoefShort"), 
                        id.vars=c("CoefShort", "Checkers"), variable_name="Type", outerCols=c("LowOuter", "HighOuter"), 
                        innerCols=c("LowInner", "HighInner"))
{
    # melt the data frame so it is suitable for ggplot
    modelMelt <- reshape2::melt(data=modelCI[ ,keepCols], id.vars=id.vars, variable_name=variable_name)
	
	# just the outerCI info
	modelMeltOuter <- modelMelt[modelMelt$Type %in% outerCols, ]	# pull out the 95% CI
	
	# just the innerCI info
	modelMeltInner <- modelMelt[modelMelt$Type %in% innerCols, ]	# pull out the 68% CI
    
    # return the data.frames
    return(list(modelMelt=modelMelt, modelMeltOuter=modelMeltOuter, modelMeltInner=modelMeltInner))
}

#buildPlotting <-function()

#' Coefplot plotting
#'
#' Build ggplot object for coefplot
#'
#' This function builds up the ggplot layer by layer for \code{\link{coefplot.lm}}
#'
#' @author Jared P. Lander www.jaredlander.com
#' @seealso \code{\link{coefplot.lm}} \code{\link{coefplot}}
#' @aliases buildPlotting.lm
#' @param modelCI An object created by \code{\link{buildModelCI}}
## @param modelMeltInner The inner SE part of the object built by \code{\link{meltModelCI}}
## @param modelMeltOuter The outer SE part of the object built by \code{\link{meltModelCI}}
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
#' @param numeric logical; If true and factors has exactly one value, then it is displayed in a horizontal graph with constinuous confidence bounds.
#' @param fillColor The color of the confidence bounds for a numeric factor
#' @param alpha The transparency level of the numeric factor's confidence bound
#' @param horizontal logical; If the plot should be displayed horizontally
#' @return a ggplot graph object
#' @examples
#'
#' data(diamonds)
#' model1 <- lm(price ~ carat + cut, data=diamonds)
#' theCI <- coefplot:::buildModelCI(model1)
#' theCIMelt <- coefplot:::meltModelCI(theCI)
#' coefplot:::buildPlotting.lm(theCI, theCIMelt$modelMeltInner, theCIMelt$modelMeltInner)
#'
buildPlotting.lm <- function(modelCI, 
                        #modelMeltInner, modelMeltOuter, 
                        title="Coefficient Plot", xlab="Value", ylab="Coefficient",
                        lwdInner=1, lwdOuter=0, color="blue",
    					cex=.8, textAngle=0, numberAngle=0, outerCI=2, innerCI=1,
						zeroColor="grey", zeroLWD=1, zeroType=2, numeric=FALSE, fillColor="grey", alpha=1/2,
    					horizontal=FALSE, facet=FALSE, scales="free")
{
    ## build the layer infos
    # outerCI layer
	outerCIGeom <- list(Display=geom_linerange(aes(ymin=LowOuter, ymax=HighOuter), data=modelCI, colour=color, lwd=lwdOuter), None=NULL)
	# innerCI layer
	innerCIGeom <- list(Display=geom_linerange(aes(ymin=LowInner, ymax=HighInner), data=modelCI, colour=color, lwd=lwdInner), None=NULL)
	# ribbon layer
	ribbonGeom <- list(None=NULL, geom_ribbon(aes(ymin=LowOuter, ymax=HighOuter, group=Checkers), data=modelCI, fill=fillColor, alpha=alpha, lwd=lwdOuter))

	# faceting info
	faceting <- list(None=NULL, Display=facet_wrap(~Checkers, scales=scales))
    
    if(numeric)
	{
		p <- ggplot(data=modelCI, aes(y=Coef, x=CoefShort))			# the basics of the plot
		p <- p + geom_hline(yintercept=0, colour=zeroColor, linetype=zeroType, lwd=zeroLWD)		# the zero line
		p <- p + ribbonGeom[[numeric + 1]]		# the ribbon
		p <- p + geom_point(colour=color)						# the points
		p <- p + geom_line(data=modelCI, aes(y=HighOuter, x=CoefShort, group=Checkers), colour=color) +
			geom_line(data=modelCI, aes(y=LowOuter, x=CoefShort, group=Checkers), colour=color)
	}else
    {
    	p <- ggplot(data=modelCI, aes(y=Coef, x=CoefShort))			# the basics of the plot
    	p <- p + geom_hline(yintercept=0, colour=zeroColor, linetype=zeroType, lwd=zeroLWD)		# the zero line
    	p <- p + outerCIGeom[[(outerCI/outerCI)]] +					# the outer CI bars
    		innerCIGeom[[innerCI/innerCI]]						# the inner CI bars
       	p <- p + geom_point(colour=color)						# the points
        p <- p + scale_x_discrete()
     	p <- p + opts(title=title, axis.text.x=theme_text(angle=textAngle), axis.text.y=theme_text(angle=numberAngle)) + labs(y=xlab, x=ylab)	# labeling and text info
     	p <- p + faceting[[facet + 1]]		# faceting
     	p <- p + if(!horizontal) coord_flip()
    }

	rm(modelCI); gc()		# housekeeping
    
	return(p)		# return the ggplot object
}


# ggplot(data=holder, aes(y=Coef, x=CoefShort, colour=factor(Name))) + geom_point(position=position_dodge(width=.8), aes(ymax=Coef)) + labs(x="Value", y="Coefficient") + scale_colour_discrete("Model") + geom_line(data=holdMelting$modelMeltOuter, aes(y=value, group=Grouper, ymax=value, ymin=value), position=position_dodge(width=.8)) + coord_flip()
# 
# ggplot(data=holder[!holder$CoefShort %in% c("(Intercept)", "carat") & holder$Name != 3, ], aes(y=Coef, x=CoefShort, colour=factor(Name))) + labs(x="Value", y="Coefficient") + scale_colour_discrete("Model") + geom_errorbar(data=holdMelting$modelMeltOuter[!holdMelting$modelMeltOuter$CoefShort %in% c("(Intercept)", "carat") & holdMelting$modelMeltOuter$Name != 3, ], aes(y=value, group=Grouper, ymax=value+100, ymin=value-100), position=position_dodge(width=.8)) + coord_flip()
# 
# mod5 <- lm(price ~ cut, data=diamonds)
# mod6 <- lm(price ~ cut + color, data=diamonds)
# mod7 <- lm(price ~ cut + color + clarity, data=diamonds)
# holder2 <- multiplot(mod5, mod6, mod7, plot=F)
# ggplot(holder2, aes(y=Coef, x=CoefShort, colour=factor(Name))) + geom_linerange(aes(ymin=LowOuter, ymax=HighOuter), position=position_dodge(width=1), lwd=0) + geom_linerange(aes(ymin=LowInner, ymax=HighInner), position=position_dodge(width=1), lwd=1) + geom_point(position=position_dodge(width=1), aes(ymax=Coef)) + coord_flip()