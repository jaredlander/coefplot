#' @keywords internal 
"_PACKAGE"
#' Plotting Model Coefficients
#' @description
#' A short description...
#'  Provides an S3 generic method for plotting coefficients from a model so it can be extended to other model types.
#' @name coefplot
# @aliases coefplot-package

# @import plyr ggplot2 reshape2
NULL


## quiets concerns of R CMD check re: the .data's that appear in pipelines 
## because dplyr doesn't offer a way to use arrange with Standard evaluation

if(getRversion() >= "2.15.1")  utils::globalVariables(c(".data"))
