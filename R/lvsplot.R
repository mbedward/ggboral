#' Prepare data to plot latent variables from a boral model
#'
#' This function takes a fitted boral model and returns a data frame to use
#' with ggplot for an ordination-style bi-plot of latent variables and
#' coefficients. It is used by function \code{\link{gg_lvsplot}} but you
#' can also use it directly.
#'
#' @param model A boral model fitted with one or more latent variables.
#'
#' @param scaling A numeric value between 0.8 and 1.2 that controls the
#'   relative scaling of latent variables (e.g. sites) and latent variable
#'   coefficients (e.g. species).
#'
#' @return A data frame with \code{n} column(s) for the values of the \code{n}
#'   latent variables; a column \code{var} specifying whether a row is for
#'   latent variable (\code{'lv'}) or coefficient (\code{'lvcoef'}) values;
#'   and a column \code{label} giving object (e.g. site) name for 'lv' rows
#'   and attribute (e.g. species) name for 'lvcoef' rows.
#'
#' @examples
#' \dontrun{
#' dat.gg <- gg_lvsplot_data(mymodel)
#'
#' # Draw 'manually' with ggplot as an alternative to using
#' # the gg_lvsplot function
#' library(ggplot2)
#' library(ggrepel)
#'
#' ggplot(data = dat.gg, aes(x = lv1, y = lv2, colour = var)) +
#'   geom_point() +
#'   geom_text_repel(aes(label = label)) +
#'   theme(legend.position = "none")
#' }
#'
#' @seealso \code{\link{gg_lvsplot}}
#'
#' @export
#'
gg_lvsplot_data <- function (model, scaling = 1.0) {

  Nlv <- model$num.lv
  if (Nlv == 0) stop("No latent variables in the fitted model")

  scaling <- scaling[1]
  if (scaling < 0.8) {
    warning("Adjusting scaling to minimum value of 0.8")
    scaling <- 0.8
  } else if (scaling > 1.2) {
    warning("Adjusting scaling to maximum value of 1.2")
    scaling <- 1.2
  }

  alpha <- scaling / 2

  if (Nlv == 1) {
    scaled.lvs <- model$lv.median
    scaled.lvs.coefs <- model$lv.coefs.median[, 2]
  }
  else if (Nlv > 1) {
    x.cross <- tcrossprod(model$lv.median,
                          model$lv.coefs.median[, 2:(Nlv + 1)])

    x.svd <- svd(x.cross, Nlv, Nlv)

    m1 <- matrix(x.svd$d[1:Nlv]^alpha,
                 nrow = model$n,
                 ncol = Nlv,
                 byrow = TRUE)

    scaled.lvs <- scale(x.svd$u * m1, center = TRUE, scale = FALSE)

    m2 <- matrix(x.svd$d[1:Nlv]^(1-alpha),
                 nrow = model$p,
                 ncol = Nlv,
                 byrow = TRUE)

    scaled.lv.coefs <- scale(x.svd$v * m2, center = TRUE, scale = FALSE)
  }

  colnames(scaled.lvs) <- paste0("lv", 1:Nlv)

  scaled.lvs <- data.frame(scaled.lvs,
                           var = "lv",
                           label = rownames(model$lv.median),
                           stringsAsFactors = FALSE)

  colnames(scaled.lv.coefs) <- paste0("lv", 1:Nlv)

  scaled.lv.coefs <- data.frame(scaled.lv.coefs,
                                var = "lvcoef",
                                label = rownames(model$lv.coefs.median),
                                stringsAsFactors = FALSE)

  rbind(scaled.lvs, scaled.lv.coefs)
}



#' Plot the latent variables from a boral model
#'
#' This is a ggplot version of the boral function \code{\link[boral]{lvsplot}}
#' to draw an ordination-style graph of latent variables (sites or objects)
#' and their corresponding coefficients (species or attributes).
#'
#' @param model A boral model fitted with one or more latent variables.
#'
#' @param scaling A numeric value between 0.8 and 1.2 that controls the
#'   relative scaling of latent variables (e.g. sites) and latent variable
#'   coefficients (e.g. species).
#'
#' @param lvs Integer indices of the latent variables to plot.
#'
#' @return A ggplot object for the latent variable graph.
#'
#' @note Presently this function only graphs two latent variables.
#'
#' @importFrom ggrepel geom_text_repel
#'
#' @seealso \code{\link{gg_lvsplot_data}}
#'
#' @examples
#' # This example is based on that for the lvsplot function
#' # in the boral package and requires an example dataset from the
#' # mvabund package.
#' library(boral)
#' library(ggboral)
#'
#' data(spider, package = "mvabund")
#' y <- spider$abun
#'
#' # Warning - these settings are only to make the example run quickly.
#' # Don't use them for a real analysis!
#' example.control <- list(n.burnin = 10, n.iteration = 100, n.thin = 1)
#'
#' spiderfit_nb <- boral(y, family = "negative.binomial",
#'                       num.lv = 2,
#'                       row.eff = "fixed",
#'                       mcmc.control = example.control)
#'
#' gg_lvsplot(spiderfit_nb)
#'
#' # Since the function returns a ggplot object you can tweak it further.
#' # Here we add labels and a white background.
#' last_plot() +
#'   labs(x = "Latent variable 1", y = "Latent variable 2",
#'        title = "Example unconstrained model of spider abundance") +
#'
#'   theme_bw()
#'
#' @export
#'
gg_lvsplot <- function(model, scaling = 1.0, lvs = c(1,2)) {
  if (length(lvs) != 2 | model$num.lv < 2)
    stop("Presently this function only works for two latent variables")

  lv.names <- paste0("lv", lvs)

  dat <- gg_lvsplot_data(model, scaling)
  dat <- dat[, c(lv.names, "var", "label")]

  ggplot(data = dat, aes_string(x = lv.names[1], y = lv.names[2])) +
    geom_point(aes(colour = var), show.legend = FALSE) +
    geom_text_repel(aes(colour = var, label = label), show.legend = FALSE)
}

