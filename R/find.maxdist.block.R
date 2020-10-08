#' Find the maximum distance between data block
#'
#' @param dat :n x p matrix with rows are observations, columns are variables
#' @param indx 1 < length(indx) < n index vector
#' @param seg.length segmentation length (default=1000).
#' If seg.length > length(indx), floor(nrow(dat)/4) will be used.
#'
#' @return aa : maximum distance between specific indiced data block
#' @import pdist
#' @import parallel
#' @export
#'
#' @examples
find.maxdist.block<-function(dat,indx, seg.length=1000){
  if(seg.length>length(indx)){seg.length = floor(nrow(dat)/4)}
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
