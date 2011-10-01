## the generic function for coefplot
## this way it can be used for more than just the generalized family of linear models
coefplot <- function(model, ...)
{
    UseMethod(generic="coefplot", object=model)
}

## the lm method for coefplot
coefplot.lm <- function(model, innerCI=1, outerCI=2, ...)
{
    # get the information on the model
    modelInfo <- getModelInfo(model, reduced=factorsToShow)
	
	# get the coef and SE from modelInfo
	modelCoef <- modelInfo$coef
	modelSE <- modelInfo$SE
	modelMatched <- modelInfo$matchedVars
	
	modelCI <- data.frame(LowOuter=modelCoef - outerCI*modelSE, HighOuter=modelCoef + outerCI*modelSE, LowInner=modelCoef - innerCI*modelSE, HighInner=modelCoef + innerCI*modelSE, Coef=modelCoef) # build a data.frame of the confidence bounds and original coefficients
	modelCI$Name <- rownames(modelCI)	## grab the coefficient names into the data.frame
	
	modelMatcher <- modelMatched[, c("Var", "Coef")]
	names(modelMatcher)[2] <- "Name"
	modelMatcher$Name <- as.character(modelMatcher$Name)
	
	modelCI <- join(modelCI, modelMatcher, by="Name")
	
	rm(modelMatcher); gc()
	
	return(modelCI)
}

## the glm method for coefplot
coefplot.glm <- function(model, ...)
{
    plot(coef(model))
}