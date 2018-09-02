#' Prepare data for caterpillar plots of boral regression coefficients
#'
#' This function takes a fitted boral model and returns a data frame to use
#' with ggplot to draw 'caterpillar plots' of regression coefficients.
#' It is used by function \code{\link{gg_coefsplot}} but you can also use it
#' directly.
#'
#' @param model A boral model fitted with one or more latent variables.
#'
#' @return A data frame to use with ggplot that has the following columns:
#'   \describe{
#'     \item{label}{y variable labels (e.g. species names)}
#'     \item{var}{X (explanatory) variable names}
#'     \item{lower95}{lower bound of the 95% highest posterior density interval
#'       for coefficient value}
#'     \item{upper95}{upper bound of the 95% highest posterior density interval
#'       for coefficient value}
#'     \item{median}{median coefficent value}
#'     \item{revlabel}{Factor variable based on label with levels in reverse order
#'       so that graphs will show, for example, species names in A-Z order from
#'       top to bottom.}
#'   }
#'
#' \dontrun{
#' dat.gg <- gg_coefsplot_data(mymodel)
#'
#' # Draw 'manually' with ggplot as an alternative to using
#' # the gg_coefsplot function. First we add a variable
#' # for how central zero is in the intervals which we will
#' # use to colour the intervals.
#' #
#' library(dplyr)
#'
#' dat.gg <- dat.gg %>%
#'   mutate(zeroness = ifelse(lower95 > 0 | upper95 < 0, 0,
#'                            min(abs(lo), hi) / max(abs(lo), hi)))
#'
#' ggplot(data = dat.gg, aes(y = revlabel)) +
#'   geom_segment(aes(yend = revlabel, x = lower95, y = upper95,
#'                    colour = zeroness)) +
#'
#'   scale_colour_grey(start = 1.0, end = 0.2) +
#'
#'   geom_point(aes(x = median)) +
#'   geom_vline(xintercept = 0, linetype = "dashed")
#'   facet_wrap(~ var)
#' }
#'
#' @seealso \code{\link{gg_lvsplot}}
#'
#' @importFrom dplyr %>% mutate
#' @importFrom tidyr gather spread
#'
#' @export
#'
gg_coefsplot_data <- function(model) {
  if (model$num.X == 0) stop("The model was fitted with no explanatory variables")

  dat <- data.frame(
    model$X.coefs.median,
    label = rownames(model$X.coefs.median),
    stat = "median",
    stringsAsFactors = FALSE)

  lo <- data.frame(
    model$hpdintervals$X.coefs[ , , 1],
    label = rownames(model$X.coefs.median),
    stat = "lower95",
    stringsAsFactors = FALSE)

  hi <- data.frame(
    model$hpdintervals$X.coefs[ , , 2],
    label = rownames(model$X.coefs.median),
    stat = "upper95",
    stringsAsFactors = FALSE)

  dat <- rbind(dat, lo, hi) %>%
    gather(var, value, -c(label, stat)) %>%
    spread(stat, value) %>%

    mutate(revlabel = factor(label, levels = rev(sort(unique(label)))))

  dat
}


#' Caterpillar plots of regression coefficients from a boral model
#'
#' Constructs horizontal line plot showing 95% highest posterior density
#' intervals, otherwise known as a caterpillar plot, for the column-specific
#' regression coefficients corresponding to the covariates in X fitted
#' in the boral model. Median coefficient values are shown as a point on
#' each interval.
#'
#' @param model A boral model fitted with one or more latent variables.
#'
#' @param X.labels An optional vector of text strings to use as labels
#'   for the X variables (graph facet labels).
#'   If a named vector is provided (safest), the names will
#'   be used to match labels to the X variable names in the fitted model.
#'   If \code{NULL} (default), X variable names are taken from the model.
#'
#' @param linesize Thickness of the line segments drawn for intervals (passed
#'   to ggplot as the \code{size} argument for \code{\link[ggplot2]{geom_segment}}).
#'
#' @param pointsize Size of points representing median values (passed to
#'   ggplot as the \code{size} argument for \code{\link[ggplot2]{geom_point}}).
#'   Set to zero if you do not want to display points.
#'
#' @param palette The name of the colour palette to use when indicating where
#'   intervals lie in relation to zero. This should be one of the sequential
#'   palettes recognized by \code{\link[ggplot2]{scale_colour_distiller}}.
#'   The default is "Greys". Set to \code{NULL} or \code{""} to turn off
#'   colouring and draw black lines for all intervals.
#'
#' @importFrom dplyr %>% mutate
#'
#' @examples
#' # This example is based on that for the coefsplot function
#' # in the boral package and requires an example dataset from the
#' # mvabund package.
#' library(boral)
#' library(ggboral)
#'
#' data(spider, package = "mvabund")
#' y <- spider$abun
#' X <- scale( spider$x )
#'
#' # Warning - these settings are only to make the example run quickly.
#' # Don't use them for a real analysis!
#' example.control <- list(n.burnin = 10, n.iteration = 100, n.thin = 1)
#'
#' spiderfit_nb <- boral(y, X = X,
#'                       family = "negative.binomial",
#'                       num.lv = 2,
#'                       mcmc.control = example.control)
#'
#' gg_coefsplot(spiderfit_nb)
#'
#' # Display plot again with a white background
#' last_plot() + theme_bw()
#'
#' @export
#'
gg_coefsplot <- function(model, X.labels = NULL,
                         linesize = 1.5, pointsize = 3,
                         palette = "Greys") {

  dat <- gg_coefsplot_data(model)

  if (is.null(X.labels)) {
    labeller = "label_value"

  } else {
    if (length(X.labels) != model$num.X)
      stop("Expected ", model$num.X, " X variable labels")

    varnames <- sort(unique(dat$var))

    if (!is.null(names(X.labels))) {
      ii <- pmatch(names(X.labels), varnames)
      if (anyNA(ii)) stop("Not all label vector names match X variable names")

      facet.labels <- X.labels
      names(facet.labels) <- varnames[ii]

    } else {
      # No names in label vector so just assume order is correct
      facet.labels <- X.labels
      names(facet.labels) <- varnames
    }

    labeller <- as_labeller(facet.labels)
  }


  dat <- mutate(dat, linecolour = 0.2 + 0.8 * nonzeroness(lower95, upper95))


  gg <- ggplot(data = dat, aes(y = revlabel)) +
    geom_segment(aes(yend = revlabel,
                     x = lower95, xend = upper95,
                     colour = linecolour),
                 size = linesize) +

    geom_point(aes(x = median, colour = linecolour),
               size = pointsize)

  if (!is.null(palette) && palette != "") {
    gg <- gg + scale_color_distiller(palette = palette, direction = 1)
  } else {
    gg <- gg + scale_colour_gradient(low = "black", high = "black")
  }

  gg + geom_vline(xintercept = 0, linetype = "dashed") +

    labs(x = "", y = "") +

    facet_wrap(~ var, scales = "free_x", labeller = labeller) +

    theme(legend.position = "none")
}

