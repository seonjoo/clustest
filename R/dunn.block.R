#' Compute Dunn index usign blockwise computation
#'
#' @param clusters
#' @param Data
#' @param method
#' @param seg.length (=default=100)
#' @param n.cores
#'
#' @return
#' @export
#'
#' @examples
#' require(MASS)
#' n=1000
#' sigma=matrix(0.4, 2, 2);diag(sigma)=1
#' x = rbind(mvrnorm(n=n, rep(0,2),sigma),
#' mvrnorm(n=n, rep(5,2),sigma),
#' mvrnorm(n=n, rep(-5,2),sigma))
#' cfit = lapply(2:5,function(k)kmeans(x,k,nstart=10))
#' dunn.score<-unlist(mclapply(cfit, function(obj) dunn.block( obj$cluster,Data=x), mc.cores=4))
dunn.block<-function (clusters, Data = NULL, method = "euclidean",
                         seg.length=min(100,nrow(Data),n.cores=4)
{
  nc <- max(clusters)
  interClust <- matrix(NA, nc, nc)
  intraClust <- rep(NA, nc)
  for (i in 1:nc) {
    c1 <- which(clusters == i)
    for (j in i:nc) {
      if (j == i)
        intraClust[i] <- find.maxdist.block(Data,c1,seg.length=seg.length)
      if (j > i) {
        c2 <- which(clusters == j)
        interClust[i, j] <- find.mindist.block(Data,c1,c2,seg.length=seg.length)
      }
    }
  }
  dunn <- min(interClust, na.rm = TRUE)/max(intraClust)
  return(dunn)
}
