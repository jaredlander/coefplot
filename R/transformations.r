#' @title invlogit
#' @description Calculates the inverse logit
#' @details Maps the real line to [0, 1]
#' @author Jared P. Lander
#' @export invlogit
#' @rdname invlogit
#' @param x Vector of numbers
#' @return \code{x} mapped to [0, 1]
#' @examples 
#' invlogit(3)
#' invlogit(-6:6)
#' invlogit(c(-1, 1, 2))
#' 
invlogit <- function(x)
{
    1/(1 + exp(-x))
}
