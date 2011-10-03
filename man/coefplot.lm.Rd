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
function (model, title = "Coefficient Plot", xlab = "Value", 
    ylab = "Coefficient", innerCI = 1, outerCI = 2, lwdInner = 1, 
    lwdOuter = 0, color = "blue", cex = 0.8, textAngle = 0, numberAngle = 0, 
    zeroColor = "grey", zeroLWD = 1, zeroType = 2, facet = FALSE, 
    scales = "free", sort = "natural", decreasing = FALSE, numeric = FALSE, 
    fillColor = "grey", alpha = 1/2, horizontal = FALSE, intercept = TRUE, 
    plot = TRUE, ...) 
{
    theDots <- list(...)
    if (numeric & length(grep("factors", names(theDots))) != 
        1) {
        stop("When treating a factor variable as numeric, the specific factor must be specified using \"factors\"")
    }
    else if (numeric) {
        sort = "mag"
    }
    modelInfo <- getModelInfo(model, ...)
    modelCoef <- modelInfo$coef
    modelSE <- modelInfo$SE
    modelMatched <- modelInfo$matchedVars
    modelCI <- data.frame(LowOuter = modelCoef - outerCI * modelSE, 
        HighOuter = modelCoef + outerCI * modelSE, LowInner = modelCoef - 
            innerCI * modelSE, HighInner = modelCoef + innerCI * 
            modelSE, Coef = modelCoef)
    modelCI$Name <- rownames(modelCI)
    modelMatcher <- modelMatched[, c("Checkers", "Coef", "CoefShort")]
    names(modelMatcher)[2] <- "Name"
    modelMatcher$Name <- as.character(modelMatcher$Name)
    modelCI <- join(modelCI, modelMatcher, by = "Name")
    rm(modelMatcher)
    gc()
    modelCI$CoefShort <- ifelse(is.na(modelCI$CoefShort), modelCI$Name, 
        modelCI$CoefShort)
    modelCI$Checkers <- ifelse(is.na(modelCI$Checkers), "Numeric", 
        modelCI$Checkers)
    if (intercept == FALSE | numeric) {
        theIntercept <- which(modelCI$Name == "(Intercept)")
        if (length(theIntercept) > 0) {
            modelCI <- modelCI[-theIntercept, ]
        }
        rm(theIntercept)
        gc()
    }
    if (nrow(modelCI) == 0) {
        stop("There are no valid coeficients to plot", call. = FALSE)
    }
    ordering <- switch(sort, natural = order(1:nrow(modelCI), 
        decreasing = decreasing), normal = order(1:nrow(modelCI), 
        decreasing = decreasing), nat = order(1:nrow(modelCI), 
        decreasing = decreasing), magnitude = order(modelCI$Coef, 
        decreasing = decreasing), mag = order(modelCI$Coef, decreasing = decreasing), 
        size = order(modelCI$Coef, decreasing = decreasing), 
        alphabetical = order(modelCI$Name, decreasing = decreasing), 
        alpha = order(modelCI$Name, decreasing = decreasing), 
        order(1:nrow(modelCI)))
    modelCI <- modelCI[ordering, ]
    modelCI$CoefShort <- factor(modelCI$CoefShort, levels = modelCI$CoefShort)
    keepCols <- c("LowOuter", "HighOuter", "LowInner", "HighInner", 
        "Coef", "Checkers", "CoefShort")
    modelMelt <- reshape2::melt(data = modelCI[, keepCols], id.vars = c("CoefShort", 
        "Checkers"), variable_name = "Type")
    modelMeltOuter <- modelMelt[modelMelt$Type \%in\% c("LowOuter", 
        "HighOuter"), ]
    modelMeltInner <- modelMelt[modelMelt$Type \%in\% c("LowInner", 
        "HighInner"), ]
    outerCIGeom <- list(Display = geom_line(aes(x = value, group = CoefShort), 
        data = modelMeltOuter, colour = color, lwd = lwdOuter), 
        None = NULL)
    innerCIGeom <- list(Display = geom_line(aes(x = value, group = CoefShort), 
        data = modelMeltInner, colour = color, lwd = lwdInner), 
        None = NULL)
    ribbonGeom <- list(None = NULL, geom_ribbon(aes(ymin = LowOuter, 
        ymax = HighOuter, group = Checkers), data = modelCI, 
        fill = fillColor, alpha = alpha, lwd = lwdOuter))
    faceting <- list(None = NULL, Display = facet_wrap(~Checkers, 
        scales = scales))
    if (plot) {
        if (numeric) {
            p <- ggplot(data = modelCI, aes(y = Coef, x = CoefShort))
            p <- p + geom_hline(yintercept = 0, colour = zeroColor, 
                linetype = zeroType, lwd = zeroLWD)
            p <- p + ribbonGeom[[numeric + 1]]
            p <- p + geom_point(colour = color)
            p <- p + geom_line(data = modelCI, aes(y = HighOuter, 
                x = CoefShort, group = Checkers), colour = color) + 
                geom_line(data = modelCI, aes(y = LowOuter, x = CoefShort, 
                  group = Checkers), colour = color)
            return(p)
        }
        p <- ggplot(data = modelCI, aes(x = Coef, y = CoefShort))
        p <- p + geom_vline(xintercept = 0, colour = zeroColor, 
            linetype = zeroType, lwd = zeroLWD)
        p <- p + outerCIGeom[[(outerCI/outerCI)]] + innerCIGeom[[innerCI/innerCI]]
        p <- p + geom_point(colour = color)
        p <- p + opts(title = title, axis.text.y = theme_text(angle = textAngle), 
            axis.text.x = theme_text(angle = numberAngle)) + 
            labs(x = xlab, y = ylab)
        p <- p + faceting[[facet + 1]]
        p <- p + if (horizontal) 
            coord_flip()
        rm(modelMelt, modelMeltOuter, modelMeltInner)
        gc()
        return(p)
    }
    else {
        rm(modelMelt, modelMeltOuter, modelMeltInner)
        gc()
        return(modelCI)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
