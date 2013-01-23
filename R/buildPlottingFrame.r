# make data plotable

buildModelCI <- function(model, outerCI=2, innerCI=1, intercept=TRUE, numeric=FALSE, 
                         sort=c("natural", "normal", "magnitude", "size", "alphabetical"), 
                         decreasing=TRUE, name=NULL, ...)
{
    # get model information
    modelCI <- extract.coef(model)
    
    # build confidence bounds columns
    modelCI <- within(modelCI, {LowOuter <- Coefficient - outerCI*SE;
                  HighOuter <- Coefficient + outerCI*SE;
                  LowInner <- Coefficient - innerCI*SE;
                  HighInner <- Coefficient - innerCI*SE})
    
    # get rid of SE column
    modelCI$SE <- NULL
    
    # make column for model name
    # if a name for the model is provided, use it, otherwise use the call
    if(is.null(name))
    {
        modelCI$Model <- as.character(paste(model$call, collapse="_"))
    }else
    {
        modelCI$Model <- name
    }
    
    return(modelCI)
}