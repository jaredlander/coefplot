#' @title extractPath
#' @description Extracts the coefficient path of the elastic net
#' @details This is a replacement plot for visualizing the coefficient path resulting from the elastic net.
#' @author Jared P. Lander
#' @export
#' @param model A \code{\link[glmnet]{glmnet}} model
#' @param \dots Further arguments
#' @return A \code{link[tibble]{tibble}} holding the coefficients for various lambdas
#' @examples 
#' 
#' library(glmnet)
#' data(diamonds, package='ggplot2')
#' diaX <- useful::build.x(price ~ carat + cut + x - 1, data=diamonds, contrasts = TRUE)
#' diaY <- useful::build.y(price ~ carat + cut + x - 1, data=diamonds)
#' modG1 <- glmnet(x=diaX, y=diaY)
#' extractPath(modG1)
#' 
#' modG2 <- cv.glmnet(x=diaX, y=diaY, nfolds=5)
#' extractPath(modG2)
#' 
extractPath <- function(model, ...)
{
    UseMethod('extractPath')
}

#' @rdname extractPath
#' @export
#' @importFrom magrittr "%>%"
#' @param intercept If \code{FALSE} (the default), no intercept will be provided
extractPath.glmnet <- function(model, intercept=FALSE, ...)
{
    thePath <- dplyr::bind_cols(
        tibble::tibble(lambda=log(model$lambda)),
        model %>% 
            glmnet::coef.glmnet() %>% 
            as.matrix() %>% t() %>% 
            tibble::as_tibble()
    ) %>% 
        dplyr::arrange(.data$lambda)
    
    if(!intercept)
    {
        thePath <- thePath %>% 
            dplyr::select(-dplyr::matches('(Intercept)'))
    }
    
    return(thePath)
}


#' @rdname extractPath
#' @export
#' @importFrom magrittr "%>%"
extractPath.cv.glmnet <- function(model, ...)
{
    extractPath(model$glmnet.fit, ...)
}
