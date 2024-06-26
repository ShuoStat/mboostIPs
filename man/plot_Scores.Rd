% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_Scores.R
\name{plot_Scores}
\alias{plot_Scores}
\title{Plot the Influential Scores}
\usage{
plot_Scores(
  obj,
  aims = c("variableSelection", "prediction"),
  topN = 5,
  ylim = NULL,
  refline = 0,
  ylab = "",
  xlab = "Observations",
  ...
)
}
\arguments{
\item{obj}{obj An object resulting from a leave-one-out analysis performed
by \code{glmboostDrop1}.}

\item{aims}{A character vector indicating which influential scores to be
plotted: "variableSelection" or "prediction" or both.}

\item{topN}{An integer specifying the number of top scores to highlight on
the plot. Default is 5.}

\item{ylim}{Numeric vector of length 2, providing the range for the y-axis.
If \code{NULL}, the function automatically calculates a suitable range.
Default is \code{NULL}.}

\item{refline}{A numeric value specifying the position of a horizontal
reference line on the plot. Default is 0.}

\item{ylab}{A character string specifying the label for the y-axis.
Default is an empty string.}

\item{xlab}{A character string specifying the label for the x-axis.
The default label is "Observations".}

\item{...}{Other arguments.}
}
\description{
This function plots a set of scores against their observation indices,
highlighting the top N scores. It allows for customization of the y-axis
limits, reference line, and axis labels. Observations with the highest
absolute scores are highlighted and labeled on the plot.
}
\details{
The function creates a Lollipop plot for the influential scores. The input
should be the outputs of the glmboostDrop1 funciton. The \code{topN}
highest scores are highlighted by labeling their observation indices directly
on the plot. The y-axis range can be customized via the \code{ylim} parameter.
If not provided, the function calculates a range based on the provided scores,
extending slightly above the highest score to ensure visibility.
Both x-axis and y-axis labels can be customized through the \code{xlab} and \code{ylab}
parameters, respectively.
}
\examples{

library(mboost)
data(golub99)
X <- golub99$X
X <- scale(X)
y <- golub99$y
set.seed(1) 
foldid <- sample(rep(1:10, length = nrow(X)), nrow(X)) 
cv <- sapply(1:max(foldid), function(x) as.numeric(foldid != x))
obj <- glmboost(X, y,
                family = Binomial(),
                control = boost_control(mstop = 140,
                                        nu = 0.1,
                                        risk = "inbag"), 
                                        center = FALSE)
drop1obj <- glmboostDrop1(obj,
                          nCores = 1,
                          fixMstop = NULL,
                          aims = c("variableSelection", "prediction"),
                          ref = "mean",
                          folds = cv)
plot_Scores(drop1obj)
plot_Path(drop1obj, ref = "mean")
}
