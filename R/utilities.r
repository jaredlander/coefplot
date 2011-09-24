##Utilities

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
    varDF$CoefAlter <- varDF$Coef
	
    ## the special characters and their escaped equivalents
	specials <- c("\\!", "\\(", "\\)", "\\-", "\\=")
	specialsSub <- c("\\\\!", "\\\\(", "\\\\)", "\\\\-", "\\\\=")
	
    ## go through and do the replacing
	for(i in 1:length(specials))
	{
		varDF$VarAlter <- gsub(specials[i], specialsSub[i], varDF$VarAlter)
		varDF$PivotAlter <- gsub(specials[i], specialsSub[i], varDF$PivotAlter)
        varDF$CoefAlter <- gsub(specials[i], specialsSub[i], varDF$Coef)
    }

    # set up a column to keep track of which combinations are good
	varDF$Valid<- NA
	
    # the short names of the coefficients
	varDF$CoefShort <- NA
    
    # convert to idata.frame to make ddply faster
    varIDF <- idata.frame(varDF)

    # see if the coefficient is equivalent to the variable
    varDF[, c("PivotAlter", "Valid")] <- ddply(.data=varIDF, .variables="PivotAlter", .fun=function(DF) { data.frame(Valid=regexpr(unique(DF$PivotAlter), DF$Coef, ignore.case=FALSE)) })

    ## go through each row and 
# 	for(i in 1:nrow(varDF))
# 	{
# 		varDF$Valid[i] <- with(varDF, regexpr(paste("", PivotAlter[i], "", sep=""), Coef[i], ignore.case=FALSE))
# 	}
	
	varDF <- varDF[varDF$Valid > 0, ]
	return(varDF)
	varDF <- ddply(varDF, .(Coef), function(vect, namer, collapse, keepers) { vect$Subbers <- paste(vect[, c(namer)], collapse=collapse); return(vect[1, keepers]) }, namer="Var", collapse="|", keepers=c("Var", "Coef", "Subbers", "CoefAlter", "CoefShort"))
	varDF$SubbersAlter <- gsub("[!.()=-]", "99", varDF$Subbers)					## alter punctuation in thing to be replaced also
	
	#subbers <- strsplit(varDF$VarAlter, ":", fixed=TRUE)		## if there are interaction terms split them up
	
	for(i in 1:nrow(varDF))
	{
		varDF$CoefShort[i] <- gsub(paste(varDF$SubbersAlter[i], collapse="|"), "", varDF$CoefAlter[i])
	}
	
	varDF$CoefBack<- gsub(":", ":|", varDF$CoefShort)
	
	for(i in 1:nrow(varDF))
	{
		varDF$Var[i] <- gsub(paste(varDF$CoefBack[i], "$", sep=""), ":", varDF$Coef[i])
	}
	
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