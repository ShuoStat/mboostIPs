
moddist <- function(mod, std = F, L = 1) {

  #- std, whether p1 need to be standardized
  #- L, norm, 1, manhattan distance; 2, Euclidean distance
  
  n = length(mod)
  allvar <- as.character(unique(unlist(mod)))
  f <- table(unlist(mod))[allvar]
  p1 <-  f / n
  p2 <- 1 - p1

  score <- mapply(function(x) {
    
    s <-  as.numeric(allvar %in% x)
    sel <- p1 < 1
    s  <- s[sel]
    q1 <- p1[sel]
    q2 <- p2[sel]
    ifelse(std, sum(abs((s - q1)^L)) / (n * q1 * q2),
           sum(abs((s - q1)^L)))
  }, mod)
  return(score)
}


ham <- function(si, sj){

  if(!is.list(sj)) sj = list(sj)
  ham <- mapply(function(sj){
    s1 <- length(setdiff(si, sj))
    s2 <- length(setdiff(sj, si))
    s1 + s2
  }, sj = sj)
  return(mean(ham))
}


# lollipop function, used in plot_Scores

lollipop <- function(x, 
                     decreasing = T, 
                     topN = 5, 
                     ylim = NULL, 
                     refline = 0, 
                     ylab = "", 
                     xlab = "Observations",
                     ...){
  
  # x, influential scores
  y = x
  x = seq_along(x)
  
  if (is.null(ylim))
    ylim = c(min(y), max(y) * 1.1)
  
  if (is.null(refline))
    refline = 0
  
  plot(x, y, xlab = "", ylab = "", cex = 0.8, ylim = ylim, pch = 19,
       yaxs = "i", ... )
  abline(h = refline, lty = "dashed")
  
  for(i in x){
    lines(c(x[i], x[i]), c(0, y[i]), col = "grey")
  }
  
  #- points
  points(x, y, cex = 0.8, ylim = ylim, pch = 19, ...)
  
  ord <- order(abs(y), decreasing = T)[1:topN]
  text(x[ord], y[ord], pos = 3, cex = 0.9, labels = ord)
  mtext(xlab, side = 1, line = 2.2)
  
  mtext(ylab, side = 2, line = 2.2)
  abline(h = 0)
  
}
