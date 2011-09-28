##Utilities

## Converts special characters in escaped special characters
## Meant to help out when doing regular expressions
## loops through all the special characters, subbing them out one by one with their escaped equivalent
## @toAlter:  vector of words to have their special characters subbed out
## @specialChars: the characters to be replaced
## @modChars: new version of characters
## returns the modified vector
.subOut <- function(toAlter, specialChars=c("\\!", "\\(", "\\)", "\\-", "\\=", "\\*"), 
                        modChars=c("\\\\!", "\\\\(", "\\\\)", "\\\\-", "\\\\=", "\\\\*"))
{
  # make sure the special characters has the same length as the replacement characters
  if(length(specialChars) != length(modChars))
  {
    stop("specialChars and modChars must be the same length", call.=FALSE)
  }
  
  # loop through the special characters and sub in the replacements
  for(i in 1:length(specialChars))
    {
		toAlter <- gsub(specialChars[i], modChars[i], toAlter)    # do the subbing
  }
  
  return(toAlter)
}

## Converts special characters in escaped special characters
## Meant to help out when doing regular expressions
## @...: 1 to n vectors to be subbed on
## @specialChars: the characters to be replaced
## @modChars: new version of characters
## calls .subOut to do the actual work
## returns list of the modified vectors
subSpecials <- function(..., specialChars=c("\\!", "\\(", "\\)", "\\-", "\\=", "\\*"), 
                        modChars=c("\\\\!", "\\\\(", "\\\\)", "\\\\-", "\\\\=", "\\\\*"), simplify=FALSE)
{
  # make sure the special characters has the same length as the replacement characters
  #if(length(specialChars) != length(modChars))
  #{
    #stop("specialChars and modChars must be the same length", call.=FALSE)
  #}
    
    return(llply(list(...), .subOut, specialChars=specialChars, modChars))  # run .subOut on each vector, returning the resulting list
}

#bob <- subSpecials(c("Jared", "Ben"), c("Aimee"))
# length(bob[[2]])
# length(bob[[1]])

buildFactorDF <- function(modelFactorVars, modelModel, modelCoefs)
{
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
#    varDF$CoefAlter <- varDF$Coef
	
    ## the special characters and their escaped equivalents
	specials <- c("\\!", "\\(", "\\)", "\\-", "\\=")
	specialsSub <- c("\\\\!", "\\\\(", "\\\\)", "\\\\-", "\\\\=")
	
    ## go through and do the replacing
	alterList <- subSpecials(varDF$VarAlter, varDF$PivotAlter, specialChars=specials, modChars=specialsSub)
    
    ## put the subbed values back
    varDF$VarAlter <- alterList[[1]]
    varDF$PivotAlter <- alterList[[2]]
    
    rm(alterList); gc() # housekeeping
    
#    return(varDF)
#    for(i in 1:length(specials))
# 	{
# 		varDF$VarAlter <- gsub(specials[i], specialsSub[i], varDF$VarAlter)
# 		varDF$PivotAlter <- gsub(specials[i], specialsSub[i], varDF$PivotAlter)
# #        varDF$CoefAlter <- gsub(specials[i], specialsSub[i], varDF$Coef)
#     }

    # set up a column to keep track of which combinations are good
	varDF$Valid<- NA
	
    # the short names of the coefficients
	varDF$CoefShort <- NA
    
    # convert to idata.frame to make ddply faster
#   varIDF <- idata.frame(varDF)

    # see if the coefficient is equivalent to the variable
    varDF <- ddply(.data=varDF, .variables="PivotAlter", .fun=function(DF) { DF$Valid=regexpr(unique(DF$PivotAlter), DF$Coef, ignore.case=FALSE); return(DF) })
#    hold <- ddply(.data=varIDF, .variables="PivotAlter", .fun=function(DF) { data.frame(Coef=DF$Coef, Valid=regexpr(unique(DF$PivotAlter), DF$Coef, ignore.case=FALSE), stringsAsFactors=FALSE) })
#return(hold)
#    # join the validity table back to the varDF
#    varDF <- join(varDF, hold, by=c("PivotAlter", "Coef"))
    
#    rm(hold); gc()  # housekeeping

    # just take the ones that match
    varDF <- varDF[varDF$Valid > 0, ]
    
#	varDF <- ddply(varDF, .(Coef), function(vect, namer, collapse, keepers) { vect$Subbers <- paste(vect[, c(namer)], collapse=collapse); return(vect[1, keepers]) }, namer="Var", collapse="|", keepers=c("Var", "Coef", "Subbers", "CoefAlter", "CoefShort"))
    varDF <- ddply(varDF, .(Coef), function(vect, namer, collapse, keepers) { vect$Subbers <- paste(vect[, c(namer)], collapse=collapse); return(vect[1, keepers]) }, namer="VarAlter", collapse="|", keepers=c("Var", "Coef", "Subbers", "CoefShort"))
#return(varDF)
 #   varDF$SubbersAlter <- gsub("[!.()=-]", "99", varDF$Subbers)					## alter punctuation in thing to be replaced also
	
	#subbers <- strsplit(varDF$VarAlter, ":", fixed=TRUE)		## if there are interaction terms split them up
	
    # now sub out the subbers from the coef to make coef short
    varDF <- ddply(varDF, .(Subbers), function(DF) { DF$CoefShort <- gsub(unique(DF$Subbers), "", DF$Coef); return(DF) } )
#    varDF$CoefShort <- ddply(idata.frame(varDF), .(Subbers), function(DF) { CoefShort <- gsub(unique(DF$Subbers), "", DF$Coef); return(CoefShort) } )

# 	for(i in 1:nrow(varDF))
# 	{
# 		varDF$CoefShort[i] <- gsub(paste(varDF$SubbersAlter[i], collapse="|"), "", varDF$CoefAlter[i])
# 	}
	
# 	varDF$CoefBack<- gsub(":", ":|", varDF$CoefShort)
# 	
# 	for(i in 1:nrow(varDF))
# 	{
# 		varDF$Var[i] <- gsub(paste(varDF$CoefBack[i], "$", sep=""), ":", varDF$Coef[i])
# 	}
	
	varDF$Var <- gsub(":$", "", varDF$Var)
	
	return(varDF[, c("Var", "Coef", "CoefShort")])
}


## get the levels of factor variables
## @varName:  the variable to pay attention to
## @modelModel:  the model of interest
extractLevels <- function(varName, modelModel)
{
    # put the variable name in front of the level
    paste(varName, levels(factor(modelModel[[varName]])), sep="")
}

#buildFactorDF <- compiler::cmpfun(buildFactorDF)