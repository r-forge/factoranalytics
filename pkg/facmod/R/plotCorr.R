plotCorr <- function (cor1, cor2=NULL, labels1="", labels2="", method1="", method2="", 
  ndigits = 4,lab.cex=1, col1=4,col2=2, ...) 
{
  doEllipses <- function (acov, pos, ...) 
  {
    acov = cov2cor(acov)
    cov.svd <- svd(acov, nv = 0)
    r <- cov.svd[["u"]] %*% diag(sqrt(cov.svd[["d"]]))
    m <- 100
    alphamd <- c(1/3)
    e1md <- cos(c(0:m)/m * 2 * pi) * alphamd
    e2md <- sin(c(0:m)/m * 2 * pi) * alphamd
    emd <- cbind(e1md, e2md)
    ttmd <- t(r %*% t(emd)) + rep(1, m + 1)
    lines(ttmd[, 1] + pos[1], ttmd[, 2] + pos[2], ...)
  }
  plot.new()
  #oldmar = par("mar")
  #par(mar = rep(1, 4))
  p = ncol(cor1)
  lim = c(-1, p + 1)
  plot.window(xlim = lim, ylim = lim, xaxs = "i", yaxs = "i")
  if( nchar(labels1) == 0 ) {
    labels1 <- colnames(cor1)
  }
  if( nchar(labels2) == 0 ) {
    labels2 <- rownames(cor1)
  }
  for (i in 1:p) {
    text(i - 0.5, p + 0.5, labels1[i], srt = 90,cex=lab.cex)
    text(-0.5, p - i + 0.5, labels2[i],cex=lab.cex)
  }
  for (i in 2:p) {
    for (j in 1:(i - 1)) {
      doEllipses(cor1[c(i, j), c(i, j)], pos = c(i - 1.5, p - j - 0.5), lwd = 1,col=col1, ...)
      text(j - 0.5, p - i + 0.5, round(cor1[i, j], ndigits), adj = c(0.5, -0.1),col=col1, ...)
      if( !is.null(cor2) ) {
        doEllipses(cor2[c(i, j), c(i, j)], pos = c(i - 1.5, p - j - 0.5), lwd = 1,col=col2, ...)
        text(j - 0.5, p - i + 0.5, round(cor2[i, j], ndigits), adj = c(0.5, 1.1),col=col2, ...)
      }
    }
  }
  lines(c(0.5, p - 0.5), c(p - 0.5, 0.5), lwd = 3)
  if( nchar(method1) > 0 ) {
    lines(lim[2] - 2 + c(-0.5, 0.3), c(-0.3, -0.3),col=col1)
    text(lim[2] - 2 - 0.7, -0.275, method1, pos = 2,cex=lab.cex,col=col1)
  }  
  if( nchar(method2) > 0 ) {
    lines(lim[2] - 2 + c(-0.5, 0.3), c(-0.7, -0.7),col=col2)
    text(lim[2] - 2 - 0.7, -0.675, method2, pos = 2,cex=lab.cex,col=col2)
  }
  #par(mar = oldmar)
}
