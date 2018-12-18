#' Describe where intervals lie in relation to zero
#'
#' This is a helper function used by \code{\link{gg_coefsplot}} to colour
#' intervals based on where they lie in relation to zero. It returns a
#' value of 0.0 for intervals centred on zero; values between
#' 0.0 and 1.0 for intervals containing zero away from their centre;
#' and 1.0 for intervals that exclude zero. This assumes that the intervals
#' represent symmetrical distributions, which often will not be the case.
#'
#' @param lowers A vector of lower bounds.
#' @param uppers A vector of upper bounds the same length as \code{lowers}.
#'
#' @return A vector of nonzeroness values.
#'
#' @export
#'
nonzeroness <- function(lowers, uppers) {
  stopifnot(length(lowers) > 0,
            length(uppers) == length(lowers),
            all(lowers <= uppers))

  1 - ifelse(lowers > 0 | uppers < 0,
             0.0,
             pmin(abs(lowers), uppers) / pmax(abs(lowers), uppers))
}


#' Gets the number of latent variables from a boral model object
#'
#' Starting with version 1.7 of the boral package, the number of latent
#' variables is specified via the \code{lv.control} list argument to the
#' \code{boral} function. Prior to this it was specified using the \code{num.lv}
#' argument. This function detects which has been used.
#'
#' This function is not strictly necessary, at least not with boral version 1.7.
#' Although the \code{boral} function ignores the \code{num.lv} argument when
#' called, it sets the \code{num.lv} element in the returned model object to
#' have the same value as \code{lv.control$num.lv}. But it seemed like a good
#' idea to have this helper function in case that ever changes in the future.
#'
#' @param model A fitted \code{boral} object.
#' @return Number of latent variables.
#'
#' @export
#'
get_num_lvs <- function(model) {
  if (!inherits(model, "boral")) stop("model argument must be a boral object")

  if ("lv.control" %in% names(model)) {
    # Version >= 1.7
    model$lv.control$num.lv
  } else {
    # Version <= 1.6
    model$num.lv
  }
}
