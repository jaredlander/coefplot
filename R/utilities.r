## Utilities
## Written by Jared P. Lander
## See LISCENSE for copyright information

## Converts special characters in escaped special characters
## Meant to help out when doing regular expressions
## loops through all the special characters, subbing them out one by one with their escaped equivalent
## @toAlter:  vector of words to have their special characters subbed out
## @specialChars: the characters to be replaced
## @modChars: new version of characters
## returns the modified vector
# subOut <- function(toAlter, specialChars=c("\\!", "\\(", "\\)", "\\-", "\\=", "\\*", "\\."), 
#                         modChars=c("\\\\!", "\\\\(", "\\\\)", "\\\\-", "\\\\=", "\\\\*", "\\\\."))
# {
#   # make sure the special characters has the same length as the replacement characters
#   if(length(specialChars) != length(modChars))
#   {
#     stop("specialChars and modChars must be the same length", call.=FALSE)
#   }
#   
#   # loop through the special characters and sub in the replacements
#   for(i in 1:length(specialChars))
#     {
# 		toAlter <- gsub(specialChars[i], modChars[i], toAlter)    # do the subbing
#   }
#   
#   return(toAlter)
# }


## Converts special characters in escaped special characters
## Meant to help out when doing regular expressions
## @...: 1 to n vectors to be subbed on
## @specialChars: the characters to be replaced
## @modChars: new version of characters
## calls .subOut to do the actual work
## returns list of the modified vectors
# subSpecials <- function(..., specialChars=c("\\!", "\\(", "\\)", "\\-", "\\=", "\\*"), 
#                         modChars=c("\\\\!", "\\\\(", "\\\\)", "\\\\-", "\\\\=", "\\\\*"), simplify=FALSE)
# {
#   # make sure the special characters has the same length as the replacement characters
#   if(length(specialChars) != length(modChars))
#   {
#     stop("specialChars and modChars must be the same length", call.=FALSE)
#   }
#     
#     return(llply(list(...), subOut, specialChars=specialChars, modChars))  # run .subOut on each vector, returning the resulting list
#}


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
#return(varDFTemp)	
    # match each level to every coefficient (factor or numeric)
	varDF <- expand.grid(varDFTemp$Pivot, modelCoefs, stringsAsFactors=FALSE)
	names(varDF)[1:2] <- c("Pivot", "Coef")		## give good names to the frame
#return(varDF)	
    # join the two data.frames so we have variable name, levels name and coefficient names
    varDF <- join(varDF, varDFTemp, by="Pivot")
#return(varDF)
    rm(varDFTemp); gc() # housekeeping
	
    ## create columns to hold altered versions of the variable and pivot, it replaces special characters with their excaped versions
    varDF$PivotAlter <- varDF$Pivot
	varDF$VarAlter <- varDF$Var
	
    ## the special characters and their escaped equivalents
	specials <- c("!", "(", ")", "-", "=", ".")
#    specials <- c("\\!", "\\(", "\\)", "\\-", "\\=", "\\.")
#	specialsSub <- c("\\\\!", "\\\\(", "\\\\)", "\\\\-", "\\\\=", "\\\\.")
	
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
#return(varDF)
    
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
#return(varDF)

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
#    specials <- c("\\!", "\\(", "\\)", "\\-", "\\=", "\\.")
#	specialsSub <- c("\\\\!", "\\\\(", "\\\\)", "\\\\-", "\\\\=", "\\\\.")
	
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
#return(varDF)
    # do the shortening
    varDF <- ddply(varDF, .(Subbers), function(DF) { DF$CoefShort <- gsub(paste("(^|, | for )", "(", unique(DF$Subbers), ")=", sep=""), "\\1", DF$Coef); return(DF) } )
#return(varDF)

    # return the results
    return(varDF[, c("Var", "Checkers", "Coef", "CoefShort")])
    
    #return(varDF)
}
#getModelInfo(rxModel6, shorten=T )
#buildFactorDF <- compiler::cmpfun(buildFactorDF)