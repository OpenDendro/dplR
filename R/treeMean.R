treeMean <- function(rwl,ids){
  rwl2 <- as.matrix(rwl)
  trees <- ids$tree
  uTrees <- unique(trees)
  res <- matrix(NA, nrow=nrow(rwl2), ncol=length(uTrees))
  for (i in seq(along=uTrees)) {
    res[,i] <- rowMeans(rwl2[,trees == uTrees[i], drop=FALSE], na.rm=TRUE)
  }
  res[is.nan(res)] <- NA
  res <- as.data.frame(res)
  colnames(res) <- uTrees
  rownames(res) <- rownames(rwl)
  res
}
