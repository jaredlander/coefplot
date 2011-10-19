\name{coefplot.lm}
\alias{coefplot.lm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
coefplot.lm(model, title = "Coefficient Plot", xlab = "Value", ylab = "Coefficient", innerCI = 1, outerCI = 2, lwdInner = 1, lwdOuter = 0, color = "blue", cex = 0.8, textAngle = 0, numberAngle = 0, zeroColor = "grey", zeroLWD = 1, zeroType = 2, facet = FALSE, scales = "free", sort = "natural", decreasing = FALSE, numeric = FALSE, fillColor = "grey", alpha = 1/2, horizontal = FALSE, intercept = TRUE, plot = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{innerCI}{
%%     ~~Describe \code{innerCI} here~~
}
  \item{outerCI}{
%%     ~~Describe \code{outerCI} here~~
}
  \item{lwdInner}{
%%     ~~Describe \code{lwdInner} here~~
}
  \item{lwdOuter}{
%%     ~~Describe \code{lwdOuter} here~~
}
  \item{color}{
%%     ~~Describe \code{color} here~~
}
  \item{cex}{
%%     ~~Describe \code{cex} here~~
}
  \item{textAngle}{
%%     ~~Describe \code{textAngle} here~~
}
  \item{numberAngle}{
%%     ~~Describe \code{numberAngle} here~~
}
  \item{zeroColor}{
%%     ~~Describe \code{zeroColor} here~~
}
  \item{zeroLWD}{
%%     ~~Describe \code{zeroLWD} here~~
}
  \item{zeroType}{
%%     ~~Describe \code{zeroType} here~~
}
  \item{facet}{
%%     ~~Describe \code{facet} here~~
}
  \item{scales}{
%%     ~~Describe \code{scales} here~~
}
  \item{sort}{
%%     ~~Describe \code{sort} here~~
}
  \item{decreasing}{
%%     ~~Describe \code{decreasing} here~~
}
  \item{numeric}{
%%     ~~Describe \code{numeric} here~~
}
  \item{fillColor}{
%%     ~~Describe \code{fillColor} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{horizontal}{
%%     ~~Describe \code{horizontal} here~~
}
  \item{intercept}{
%%     ~~Describe \code{intercept} here~~
}
  \item{plot}{
%%     ~~Describe \code{plot} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(model, title="Coefficient Plot", xlab="Value", ylab="Coefficient", 
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
	modelMeltOuter <- modelMelt[modelMelt$Type \%in\% c("LowOuter", "HighOuter"), ]	# pull out the 95\% CI
	
	# just the innerCI info
	modelMeltInner <- modelMelt[modelMelt$Type \%in\% c("LowInner", "HighInner"), ]	# pull out the 68\% CI
	
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
