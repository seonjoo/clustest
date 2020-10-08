#' Find the maximum distance between data block
#'
#' @param dat
#' @param indx
#' @param seg.length
#'
#' @return
#' @export
#'
#' @examples
find.maxdist.block<-function(dat,indx, seg.length=1000){
  if(seg.length>nrow(indx)){seg.length = min(seg.length, floor(nrow/4))}
  n=length(indx)
  num.seg=ceiling(n /seg.length)
  #print(num.seg)
  gs  <- split(indx, ceiling( seq(n) /seg.length))
  aa<-max(unlist(
    mclapply(2:num.seg,
             function(ss){
               max(unlist(lapply(1:ss,
                                 function(tt){
                                   dd=max(pdist(dat,indices.A=gs[[ss]],indices.B=gs[[tt]])@dist)
                                   # print(c(ss,tt,dd))
                                   return(dd)})))}, mc.cores=4)))
  return(aa)
}
