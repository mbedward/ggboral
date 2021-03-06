% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/variance_plots.R
\name{gg_varpart}
\alias{gg_varpart}
\title{Plot variance components as estimated by calc.varpart}
\usage{
gg_varpart(model, as.percent = FALSE, label.means = FALSE)
}
\arguments{
\item{model}{A boral model fitted with one or more latent variables.}

\item{as.percent}{If \code{TRUE}, format variance axis tick marks and label
as percentages. If \code{FALSE} (default), format as proportions.}

\item{label.means}{If \code{TRUE}, append the mean value for each variance
component to the legend label. Default is \code{FALSE}.}
}
\value{
A ggplot object.
}
\description{
Plots estimates of the proportion of variance in each response accounted for
by explanatory variables, latent variables and any row effects, as estimated
by the boral function \code{calc.varpart}.
}
\details{
Proceed with caution! Read the caveats and warnings detailed in the help page
for \code{calc.varpart} when interpreting the plot drawn by this function.
Also keep in mind that the results can be influenced by data artefacts, e.g.
when modelling species occurrence data with a binomial model, the estimate
of variance explained by predictor variables will tend to be higher for species
with few presences than for more common species.
}
\examples{
library(boral)
library(ggboral)

data(spider, package = "mvabund")
y <- spider$abun
X <- scale(spider$x)

# Warning - these settings are only to make the example run quickly.
# Don't use them for a real analysis!
example.control <- list(n.burnin = 10, n.iteration = 100, n.thin = 1)

# Note that we specify save.model = TRUE when calling boral
#
spiderfit_nb <- boral(y, X,
                      family = "negative.binomial",
                      lv.control = list(num.lv = 2),
                      row.eff = "fixed",
                      mcmc.control = example.control,
                      save.model = TRUE)

gg_varpart(spiderfit_nb,
           as.percent = TRUE,
           label.means = TRUE) + theme_bw()

}
\seealso{
\code{\link[boral]{calc.varpart}}  \code{\link{gg_varpart_data}}
}
