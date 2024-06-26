
#' Visualize Influential Scores Across Boosting Iterations
#'
#' This function visualizes the influential score changes across different 
#' boosting iterations. It depicts the influence of excluding each observation 
#' on variable selection and prediction. 
#'
#' @param obj An object resulting from a leave-one-out analysis performed 
#' by `glmboostDrop1`. Specifically, `obj` should be a list containing `LooObj`, 
#' which holds the leave-one-out objects.
#' @param aims A character vector indicating the aim of the influential 
#' observation detection: "variableSelection" for IPs detection for the aim of 
#' variable selection, "prediction" for the aim of prediction, or both.
#' @param ref A character string specifying the reference for comparison. Valid 
#' options are "mean" for the average similarity across all leave-one-out 
#' iterations, or "orig" for the similarity compared to the original model's 
#' selected variables. This parameter influences how the similarity scores are 
#' calculated and displayed in the plot.
#' @param topN An integer specifying the number of topN to emphasize as the 
#' most influential, based on their distance from the reference. Default is 3. 
#' These paths are highlighted in the plot, facilitating the identification of 
#' observations with the most influence on either variable selection and 
#' prediction.
#' @details The plot generated by this function displays the number of boosting 
#' iterations on the x-axis and the influential scores on the y-axis. Each line 
#' represents one leave-out observation, illustrating the influence of a case on 
#' either variable selection and prediction evolves with additional boosting 
#' iterations. The `topN` most influential observations are emphasized in a 
#' different color, and the selected variables at their optimal boosting 
#' iteration are marked with a point. The path of the reference observation is 
#' depicted with a dashed vertical line.
#'
#' @examples 
#' library(mboost)
#' data(golub99)
#' X <- golub99$X
#' X <- scale(X)
#' y <- golub99$y
#' set.seed(1) 
#' foldid <- sample(rep(1:10, length = nrow(X)), nrow(X)) 
#' cv <- sapply(1:max(foldid), function(x) as.numeric(foldid != x))
#' obj <- mboost::glmboost(X, y,
#'                 family = Binomial(),
#'                 control = boost_control(mstop = 140,
#'                                         nu = 0.1,
#'                                         risk = "inbag"), 
#'                                         center = FALSE)
#' drop1obj <- glmboostDrop1(obj,
#'                           nCores = 1,
#'                           fixMstop = NULL,
#'                           aims = c("variableSelection", "prediction"),
#'                           ref = "mean",
#'                           folds = cv)
#' plot_Scores(drop1obj)
#' plot_Path(drop1obj, ref = "mean")
#' @importFrom graphics abline axis box lines mtext plot.new plot.window points text
#' @export plot_Path

plot_Path <- function(obj, 
                      aims = c("variableSelection", "prediction"),
                      ref = c("mean", "orig."), 
                      topN = 3) {
  
  # argument checking
  stopifnot(inherits(obj, "glmboostDrop1"))
  
  obj <- obj$LooObj
  ## get the mstop
  
  ## plot 
  
  plotPathfun <- function(scores, ylab, topN) {
    
    mat <- scores
    plot.new()
    plot.window(xlim = c(1, maxstop), ylim = c(min(mat), max(mat)))
    axis(1)
    axis(2)
    box()
    
    x <- 1:maxstop
    n <- ncol(mat)
    apply(mat, 2, function(y) lines(x, y, col = "grey"))
    
    # get mstops
    mstops <- unlist(lapply(obj, `[[`, "mstop"))
    scores_mstops <- mapply(function(case, m){
      return(mat[m, case])
    }, case = seq_len(ncol(mat)), m = mstops[-1])
    
    orig <- mstops[1]
    score_fix <- mat[orig, ]
    
    ordTopN <- order(score_fix, decreasing = T)[seq_len(topN)]
    
    for(i in ordTopN){
      lines(x, mat[,i])
    }
    
    # add top points
    points(mstops[ordTopN + 1], scores_mstops[ordTopN], col = "red", pch = 19)
    #points(rep(org, 2), score.fix[top2], pch = 19)
    
    #- add orig line
    abline(v = mstops[1], lty = "dashed")
    mtext("orig.", line = 0.2, side = 3, at = mstops[length(mstops)])
    
    #- add the other points
    y = setdiff(1:n, ordTopN)
    points(mstops[y + 1], scores_mstops[y], cex = 0.7, pch = 19)
    
    #- add line nunbers
    at <- mat[nrow(mat), ordTopN]
    mtext(ordTopN, side = 4, at = at, line = 0, las = 2, cex = 0.7)
    axis(side = 3, at = mstops[topN + 1], tick = T, cex = 0.6,  tcl = 0.3, labels = F)
    mtext(ordTopN, side = 3, at = mstops[ordTopN + 1], line = 0, cex = 0.7)
    
    #- add labels
    mtext("Boosting iterations", line = 2.2, side = 1)
    mtext(ylab, line = 2.2, side = 2)
  }
  
  for(aim in aims) {
    
    if (aim == "variableSelection") {
      
      maxstop <- length(obj[[1]]$vs)
      ## get xselect list
      vslist <- lapply(obj, `[[`, "vs")
      
      scores <- c()
      for(i in seq_len(maxstop)){
        
        mod <- lapply(vslist, function(x){
          unique(as.character(x[seq_len(i)]))
        })
        
        if (ref == "orig.") {
          tmpScore <- mapply(ham, sj = mod[-1], 
                             MoreArgs = list(si = mod[[1]]))
          scores   <- rbind(scores, tmpScore)
        }
        
        if (ref == "mean") {
          tmpScore <- moddist(mod = mod[-1], std = F, L = 1)
          scores <- rbind(scores, tmpScore)
        }
      }
    } 
    
    if (aim == "prediction") {
      
      maxstop <- length(obj[[1]]$cvm)
      ## get xselect list
      cvmlist <- lapply(obj, `[[`, "cvm")
      scores <- Reduce(rbind, cvmlist[-1])
      
      if (ref == "orig.") {
        refScore <- cvmlist[[1]]
      }
      
      if (ref == "mean") {
        refScore <- colMeans(scores)
      }
      
      scores   <- sweep(scores, 2, refScore)
      scores   <- t(abs(scores))
    }
    
    # generate lab
    if (aim == "prediction")
      ylab = expression("Delta CVM")
    if (aim == "variableSelection")
      ylab = "Inflential Score"
    
    plotPathfun(scores, ylab, topN) 
  }
  
  # end
}









