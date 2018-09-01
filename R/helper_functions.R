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
