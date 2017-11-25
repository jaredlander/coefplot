#' @title annotateSeries
#' @description Annotate a series
#' @details A helper function that changes the order of some options for \code{link[dygraphs]{dyAnnotation}} so it is easier to use with \code{\link[purrr]{reduce}}.
#' @author Jared P. Lander
#' @inheritParams dygraphs::dyAnnotation
#' @param \dots Further arguments passed to \code{link[dygraphs]{dyAnnotation}}
annotateSeries <- function(dygraph, series, x=0, 
                           text=series, tooltip=series, 
                           width=50, 
                           ...)
{
    dygraphs::dyAnnotation(dygraph=dygraph,
                           x=x, 
                           text=text,
                           series=series,
                           width=width,
                           tooltip=tooltip,
                           ...)
}
