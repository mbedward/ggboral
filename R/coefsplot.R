#' Prepare data for caterpillar plots of boral regression coefficients
#'
#' @param model A boral model fitted with one or more latent variables.
#'
#' @return A data frame for use with ggplot.
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
#' @param model A boral model fitted with one or more latent variables.
#'
#' @param X.labels An optional vector of text strings to use as labels
#'   for the X variables (graph facet labels).
#'   If a named vector is provided (safest), the names will
#'   be used to match labels to the X variable names in the fitted model.
#'   If \code{NULL} (default), X variable names are taken from the model.
#'
#' @importFrom dplyr %>% mutate
#'
#' @export
#'
gg_coefsplot <- function(model, X.labels = NULL) {
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

  ggplot(data = dat, aes(y = revlabel)) +
    geom_point(aes(x = median)) +
    geom_segment(aes(yend = revlabel, x = lower95, xend = upper95)) +

    geom_vline(xintercept = 0, linetype = "dashed") +

    labs(x = "", y = "") +

    facet_wrap(~ var, scales = "free_x", labeller = labeller)
}

