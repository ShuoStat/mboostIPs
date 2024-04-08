
#' Plot the Influential Scores
#'
#' This function plots a set of scores against their observation indices, 
#' highlighting the top N scores. It allows for customization of the y-axis 
#' limits, reference line, and axis labels. Observations with the highest 
#' absolute scores are highlighted and labeled on the plot.
#'
#' @param obj obj An object resulting from a leave-one-out analysis performed 
#' by `glmboostDrop1`. 
#' @param aims A character vector indicating which influential scores to be 
#' plotted: "variableSelection" or "prediction" or both.
#' @param topN An integer specifying the number of top scores to highlight on 
#' the plot. Default is 5.
#' @param ylim Numeric vector of length 2, providing the range for the y-axis. 
#' If `NULL`, the function automatically calculates a suitable range. 
#' Default is `NULL`.
#' @param refline A numeric value specifying the position of a horizontal 
#' reference line on the plot. Default is 0.
#' @param ylab A character string specifying the label for the y-axis. 
#' Default is an empty string.
#' @param xlab A character string specifying the label for the x-axis. 
#' The default label is "Observations".
#'
#' @details
#' The function creates a Lollipop plot for the influential scores. The input 
#' should be the outputs of the glmboostDrop1 funciton. The `topN`
#' highest scores are highlighted by labeling their observation indices directly 
#' on the plot. The y-axis range can be customized via the `ylim` parameter. 
#' If not provided, the function calculates a range based on the provided scores, 
#' extending slightly above the highest score to ensure visibility. 
#' Both x-axis and y-axis labels can be customized through the `xlab` and `ylab` 
#' parameters, respectively.
#'
#' @examples
#' 
#'
#' @export plot_Scores

plot_Scores <- function(obj,
                        aims = c("variableSelection", "prediction"),
                        topN = 5,
                        ylim = NULL,
                        refline = 0,
                        ylab = "",
                        xlab = "Observations",
                        ...){
  
  #$ argument checking
  if (!inherits(obj, "glmboostDrop1"))
      stop("obj should be object from glmboostDrop1")
      
  ## plot for variableSelection
  if ("variableSelection" %in% aims){
    
    x <- obj$vsScore
    if (is.null(x)) stop("NULL vsScore in obj")
    
    lollipop(x, 
             decreasing = T, 
             topN = topN, 
             ylim = ylim, 
             refline = refline, 
             ylab = ylab, 
             xlab = xlab)
  }
  
  ## plot for variableSelection
  if ("prediction" %in% aims){
    
    x <- obj$PredScores
    if (is.null(x)) stop("NULL PredScores in obj")
    
    lollipop(x, 
             decreasing = T, 
             topN = topN, 
             ylim = ylim, 
             refline = refline, 
             ylab = ylab, 
             xlab = xlab)
  }
}


