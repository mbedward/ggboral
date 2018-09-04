#' Prepare data to plot variance components for a boral model
#'
#' This function calls the boral function \code{\link[boral]{calc.varpart}} to
#' estimate the proportion of variance accounted for by explanatory variables,
#' latent variables and any row effects. It returns the results as a data frame
#' suitable for plotting with ggplot. It is used by \code{\link{gg_varpart}} but
#' you can also use it directly if you are constructing plots 'manually'.
#'
#' This function requires that you fitted the model with explanatory variables
#' and specified \code{save.model = TRUE} when calling boral so that MCMC samples
#' were stored.
#'
#' @param model A boral model fitted with one or more latent variables.
#'
#' @return A data frame to use with ggplot that has the following columns:
#'   \describe{
#'     \item{varpart}{Variance component: one of X (explanatory variables),
#'       lv (latent variables), row (row effects).}
#'     \item{value}{Estimated proportion of variance.}
#'     \item{label}{Name of the response (e.g. species).}
#'   }
#'
#' @seealso \code{\link[boral]{calc.varpart}} \code{\link{gg_varpart}}
#'
#' @examples
#' #' library(boral)
#' library(ggboral)
#'
#' data(spider, package = "mvabund")
#' y <- spider$abun
#' X <- scale(spider$x)
#'
#' # Warning - these settings are only to make the example run quickly.
#' # Don't use them for a real analysis!
#' example.control <- list(n.burnin = 10, n.iteration = 100, n.thin = 1)
#'
#' # Note that we specify save.model = TRUE when calling boral
#' #
#' spiderfit_nb <- boral(y, X,
#'                       family = "negative.binomial",
#'                       num.lv = 2,
#'                       row.eff = "fixed",
#'                       mcmc.control = example.control,
#'                       save.model = TRUE)
#'
#' dat.varpart <- gg_varpart_data(spiderfit_nb)
#' head(dat.varpart)
#'
#' @importFrom dplyr %>% bind_rows mutate rename
#' @export
#'
gg_varpart_data <- function(model) {

  if (model$num.X == 0) stop("Model was fitted with no explanatory variables")

  v <- boral::calc.varpart(model)

  dat <- do.call(cbind, v) %>%
    as.data.frame() %>%

    mutate(label = colnames(model$y)) %>%

    # re-order responses by X var part
    mutate(label = factor(label, levels = label[order(varpart.X)])) %>%

    # re-shape for plotting
    tidyr::gather(varpart, value, -label) %>%
    mutate(varpart = stringr::str_replace(varpart, "^varpart\\.", ""))
}


#' Plot variance components as estimated by calc.varpart
#'
#' @param model A boral model fitted with one or more latent variables.
#'
#' @return A ggplot object.
#'
#' @seealso \code{\link[boral]{calc.varpart}}  \code{\link{gg_varpart_data}}
#'
#' @examples
#' #' library(boral)
#' library(ggboral)
#'
#' data(spider, package = "mvabund")
#' y <- spider$abun
#' X <- scale(spider$x)
#'
#' # Warning - these settings are only to make the example run quickly.
#' # Don't use them for a real analysis!
#' example.control <- list(n.burnin = 10, n.iteration = 100, n.thin = 1)
#'
#' # Note that we specify save.model = TRUE when calling boral
#' #
#' spiderfit_nb <- boral(y, X,
#'                       family = "negative.binomial",
#'                       num.lv = 2,
#'                       row.eff = "fixed",
#'                       mcmc.control = example.control,
#'                       save.model = TRUE)
#'
#' gg_varpart(spiderfit_nb) + theme_bw()
#'
#' @export
#'
gg_varpart <- function(model) {
  dat <- gg_varpart_data(model)

  ggplot(data = dat) +
    geom_bar(aes(x = label, y = value, fill = varpart),
             stat = "identity") +

    scale_fill_discrete(name = "Variance component",
                        breaks = c("X", "row", "lv"),
                        labels = c("Predictors", "Row effects", "Latent variables")) +

    labs(x = "", y = "Proportion of variance") +

    coord_flip() +

    theme(legend.position = "right")
}
