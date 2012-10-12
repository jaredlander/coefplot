coef.mer <- function(object, ...)
{
    ## get random and fixed effects
    theRan <- ranef(object)
    theFix <- fixef(object)
    
    # loop through and calculate combine fixed and random effects
    lapply(theRan, add.ran.fix, fixed=theFix)
}

add.ran.fix <- function(ranGroup, fixed)
{
    matrix(fixed[names(ranGroup)], byrow=TRUE, nrow=nrow(ranGroup), ncol=ncol(ranGroup)) + ranGroup
}

#' getModelInfo.mer
#' 
#' Title
#' 
#' Details
#' @aliases getModelInfo.met
#' @inheritParams getModelInfo
# @S3method getModelInfo mer
#' @param effects Indicates if we are plotting fixed (individual level) or random (group) effects
#' @param \dots Further arguments
#' @return List of coefficients and standard errors
getModelInfo.mer.ranef <- function(model, grouping=1, ...)
{
    # figure out which type of effects we are plotting
    #effects <- match.arg(effects)
    
    # get random effects
    theRan <- ranef(model, postVar=TRUE)
    theFix <- fixef(model)
        
    # get the coefficients
    #coef <- coef.mer(model)
    coef <- add.ran.fix(ranGroup=theRan[[grouping]], fixed=theFix)
    
    # get standard error for random
    #SE <- lapply(theRan, get.ran.se)
    SE <- get.ran.se(x=theRan[[grouping]])
    
    list(coef=coef, SE=SE)
}

get.ran.se <- function(x)
{
    result <- t(apply(attr(x, "postVar"), 3, function(x) sqrt(diag(x))))
    colnames(result) <- names(x)
    rownames(result) <- rownames(x)
    return(result)
}

#' @import reshape2
buildModelCI.mer.ranef <- function(modelInfo, outerCI=2, innerCI=1)
{
    modelCoef <- modelInfo$coef             # the coefficients
    modelSE <- as.data.frame(modelInfo$SE)                 # the standard errors
    
    # build the level names into the data.frame
    modelCoef$Level <- rownames(modelCoef)
    modelSE$Level <- rownames(modelSE)
    
    coefMelt <- melt(modelCoef, id.vars="Level", variable.name="Coefficient", value.name="Value")
    seMelt <- melt(modelSE, id.vars="Level", variable.name="Coefficient", value.name="Value")
    
    # build a data.frame of the confidence bounds and original coefficients
    modelCI <- data.frame(Value=coefMelt$Value,
                          LowOuter=coefMelt$Value - outerCI*seMelt$Value, HighOuter=coefMelt$Value + outerCI*seMelt$Value, 
                          LowInner=coefMelt$Value - innerCI*seMelt$Value, HighInner=coefMelt$Value + innerCI*seMelt$Value, 
                          Level=coefMelt$Level, Coefficient=coefMelt$Coefficient)
    #names(modelCI) <- c("LowOuter", "HighOuter", "LowInner", "HighInner", "Coef")
    return(modelCI)
}

prepare.data.mer.ranef <- function(model, grouping=1, outerCI=2, innerCI=1, ...)
{
    # get information for model
    theInfo <- getModelInfo.mer.ranef(model=model, grouping=grouping)
    
    # build appropriate data.frame
    modelCI <- buildModelCI.mer.ranef(modelInfo=theInfo, outerCI=outerCI, innerCI=innerCI)
    
    # melt it down
    modelMelt <- meltModelCI(modelCI=modelCI, keepCols=names(modelCI), id.vars=c("Coefficient", "Level"), variable.name="Type", value.name="Value")
    
    return(list(modelCI=modelCI, modelMelt=modelMelt))
}

coefplot.mer.ranef <- function(model, grouping=1, outerCI=2, innerCI=1, ...)
{
    # get all the info needed for plotting
    modelInfo <- prepare.data.mer.ranef(model=model, grouping=grouping, outerCI=outerCI, innerCI=innerCI, ...)
    
    buildPlotting.mer.ranef(modelCI=modelInfo$modelCI, 
                            modelMeltInner=modelInfo$modelMelt$modelMeltInner, modelMeltOuter=modelInfo$modelMelt$modelMeltOuter, ...)
}

#' coefplot.mer
#' 
#' Plot Coefficients from lmer
#' 
#' Details to follow
#' 
#' @aliases coefplot.mer
#' @author Jared P. Lander
#' @export coefplot.mer
#' @S3method coefplot mer
#' @method coefplot mer
#' @param model Model to be plotted
#' @param effects Which type of effects to plot, only ranef implemented now
#' @param grouping Which grouping factor t0 plot
#' @param \dots Further arguments
#' @return A coefficient plot
coefplot.mer <- function(model, effects=c("ranef", "fixef"), grouping=1, ...)
{
    effects <- match.arg(effects)
    
    do.call(sprintf("coefplot.mer.%s", effects), args=list(model=model, grouping=grouping, ...=...))
}

buildPlotting.mer.ranef <- function(modelCI,
                                    modelMeltInner, modelMeltOuter,
                                    color="blue", scales=c("free_x", "free_y", "free", "fixed"),
                                    title="Coefficient Plot", xlab="Value", ylab="Level",
                                    lwdInner=1, lwdOuter=0, 
                                    cex=.8, textAngle=0, numberAngle=0, 
                                    multi=FALSE,
                                    zeroColor="grey", zeroLWD=1, zeroType=2, ...
                                    )
{
    scales <- match.arg(scales)
    
    g <- ggplot(modelCI, aes(x=Value, y=Level))
    g <- g + geom_vline(xintercept=0, colour=zeroColor, lwd=zeroLWD, linetype=zeroType)
    g <- g + geom_point(colour=color)
    g <- g + geom_line(data=modelMeltOuter, aes(group=Level), lwd=lwdOuter, colour=color) + 
        geom_line(data=modelMeltInner, aes(group=Level), lwd=lwdInner, colour=color)
    g <- g + facet_wrap(~Coefficient, scales=scales) + labs(x=xlab, y=ylab, title=title)
    g
}



# given a set of ranef, add it to it's fixed effect and add the SEs together
