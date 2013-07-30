baseball <- data.frame(Batter=sample(1:100, 10000, replace=T), Bat=sample(c("David", "Batley", "Bob", "Ace"), 10000, replace=T), Hits=sample(1:100, 10000, replace=T), Team=sample(c("Braves", "Red Sox", "Yankees", "Phillies"), size=10000, replace=T))
bLinMod <- rxLinMod(Hits ~ Bat*Batter + Bat*Team, baseball)
b1 <- lm(Hits ~ Bat*Batter + Bat*Team, baseball)

theCoef <- coef(bLinMod)
names(theCoef)
theForm <- as.character(bLinMod$formula[3])
require(stringr)
length(str_split(string=theForm, pattern=" . ")[[1]])


matchCoefsForRevo <- function(model, ...)
{
    # get the formula
    theFormula <- model$formula
    theForm <- as.character(theFormula[3])
    
    # find unique terms in the formula
    formPreds <- unique(strsplit(x=theForm, split=" .*? ", )[[1]])
    
    # get the terms
    theTerms <- terms(theFormula)
    
    # get intercept indicator
    inter <- attr(theTerms, "intercept")
    # get coef names
    coefNames <- names(coef(model))
    # get pred names
    predNames <- attr(theTerms, "term.labels")
}


matchCoefsForRevo <- function(model, ...)
{
    ## find the unique predictor variables
    # build the terms
    theTerms <- terms(model$formula)
    termMat <- attr(theTerms, "factors")
    # get the rownames from the terms
    predNames <- rownames(termMat)[-1]
    
    # get intercept indicator
    inter <- attr(theTerms, "intercept")
    # get coef names
    coefNames <- names(coef(model))
    
    if(inter == 1)
    {
        predNames <- c("(Intercept)", predNames)
    }
    predNames
}
matchCoefsForRevo(bLinMod)

predNames <- matchCoefsForRevo(bLinMod)
coefNames <- names(coef(bLinMod))

str_detect(string=coefNames, pattern="(^| )Bat($|,|=)")
grep(pattern="(^| )Bat($|,|=)", x=coefNames)
doRegex <- function(x, matchAgainst, pattern="(^| )%s($|,|=)")
{
    grep(pattern=sprintf(pattern, x), x=matchAgainst)
}

mine <- subSpecials(predNames)[[1]]
names(mine) <- predNames
# not strict
dude1 <- lapply(mine, FUN=doRegex, matchAgainst=coefNames, pattern="(^| )%s($|,|=| )")
ldply(dude1, function(x) data.frame(Target=x))
# strict
dude2 <- lapply(mine, FUN=doRegex, matchAgainst=coefNames, pattern="^%s(=+[^,]*)*$")
ldply(dude2, function(x) data.frame(Target=x))

dude3 <- lapply(list(Bat="Bat"), FUN=doRegex, matchAgainst=coefNames, pattern="^%s(=+[^,]*)*$")
ldply(dude3, function(x) data.frame(Target=x))

dude4 <- lapply(list(Batter="Batter"), FUN=doRegex, matchAgainst=coefNames, pattern="^%s(=+[^,]*)*$")
ldply(dude4, function(x) data.frame(Target=x))
dude5 <- lapply(list(Batter="Batter"), FUN=doRegex, matchAgainst=coefNames, pattern="(^| )%s($|,|=| )")
ldply(dude5, function(x) data.frame(Target=x))

grep(pattern="^Batter($|=[^,]*$)", x=coefNames)
grep(pattern="^Team(=+[^,]*)*$", x=coefNames)