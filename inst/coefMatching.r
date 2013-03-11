require(reshape2)
require(plyr)
data("tips", package="reshape2")
mod1 <- lm(tip ~ total_bill * sex + day, tips)
mod2 <- lm(tip ~ total_bill * sex + day - 1, tips)
mod3 <- glm(tip ~ total_bill * sex + day, tips, family=gaussian(link="identity"))
mod4 <- lm(tip ~ (total_bill + sex + day)^3, tips)
mod5 <- lm(tip ~ total_bill * sex + day + I(total_bill^2), tips)
matchCoefs(mod1)
matchCoefs(mod2)
matchCoefs(mod3)
matchCoefs(mod4)
matchCoefs(mod5)


baseball <- data.frame(Bat=sample(1:100, 20, replace=T), Batter=sample(c("David", "Batley", "Bob", "Ace"), 20, replace=T), Hits=sample(1:20, 20, replace=T))
bMod <- lm(Hits ~ Bat*Batter, baseball)
matchCoefs(bMod)

matchCoefs <- function(model)
{
    # get the terms
    theTerms <- model$terms
    # get the assignment position
    thePos <- model$assign
    # get intercept indicator
    inter <- attr(theTerms, "intercept")
    # get coef names
    coefNames <- names(coef(model))
    # get pred names
    predNames <- attr(theTerms, "term.labels")
    # expand out pred names to match coefficient names
    predNames <- predNames[thePos]
    # if there's an intercept term add it to the pred names
    if(inter == 1)
    {
        predNames <- c("(Intercept)", predNames)
    }
    
    # build data.frame linking term to coefficient name
    matching <- data.frame(Term=predNames, Coefficient=coefNames)
    
    ## now match individual predictor to term
    # get matrix as data.frame
    factorMat <- as.data.frame(attr(theTerms, "factor"))
    # add column from rownames as identifier
    factorMat$.Pred <- rownames(factorMat)
    factorMat$.Type <- attr(theTerms, "dataClasses")
    # melt it down for comparison
    factorMelt <- melt(factorMat, id.vars=c(".Pred", ".Type"), variable.name="Term", )
    # only keep rows where there's a match
    factorMelt <- factorMelt[factorMelt$value == 1, ]
    # again, bring in coefficient if needed
    if(inter == 1)
    {
        factorMelt <- rbind(data.frame(.Pred="(Intercept)", .Type="(Intercept)", Term="(Intercept)", value=1), factorMelt)
    }
    # join into the matching data.frame
    matching <- join(matching, factorMelt, by="Term")
    
    return(matching)
}

# theTerms <- mod1$terms
# thePos <- mod1$assign
# inter <- attr(theTerms, "intercept")
# coefNames <- names(coef(mod1))
# coefNames
# predNames <- attr(theTerms, "term.labels")
# predNames <- predNames[thePos]
# factorMat <- as.data.frame(attr(theTerms, "factor"))
# factorMat$.Pred <- rownames(factorMat)
# factorMelt <- melt(factorMat, id.vars=".Pred")
# factorMelt <- factorMelt[factorMelt$value == 1, ]
# 
# 
