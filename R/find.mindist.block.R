#' Compute the minimum distance between data block.
#'
#' @param dat
#' @param indx1
#' @param indx2
#' @param seg.length
#' @param n.cores
#'
#' @return
#' @export
#'
#' @examples
find.mindist.block<-function(dat,indx1,indx2,seg.length=100, n.cores=4){
  n1=length(indx1)
  n2=length(indx2)
  num.seg1=ceiling(n1 /seg.length)
  num.seg2=ceiling(n2 /seg.length)

  #print(num.seg)
  gs1  <- split(indx1, ceiling( seq(n1) /seg.length))
  gs2  <- split(indx2, ceiling( seq(n2) /seg.length))

  aa<-min(unlist(
    mclapply(1:num.seg1,
             function(ss){
               min(unlist(lapply(1:num.seg2,
                                 function(tt){
                                   dd=min(pdist(dat[gs1[[ss]],],dat[gs2[[tt]],])@dist)
                                   # print(c(ss,tt,dd))
                                   return(dd)})))}, mc.cores=4)))
  return(aa)}
