bakker <- function (rwl, ancillary) { 
  if (!is.data.frame(rwl)) 
    stop("'rwl' must be a data.frame")
  
  if (ncol(rwl) != nrow(ancillary)) 
    stop("dimension problem: ", "'ncol(rw)' != 'nrow(ancillary)'")
  
  if (ncol(ancillary) != 4) 
    stop("dimension problem: ", "'ncol(ancillary)' != 4")
  if (!all(ancillary[, 1] == names(rwl))) {
    print(data.frame(rwlNames = names(rwl), seriesID = ancillary[, 1], test = ancillary[, 1] == names(rwl)))
    stop("series ids in 'ancillary' and 'rwl' do not match exactly.")
  }
  if (!is.data.frame(ancillary)) {
    stop("'ancillary' must be a data.frame")
  }
  rwl_d2pith <- ancillary[, "d2pith"]
  rwl_diam <- ancillary[, "diam"]
  rwl_po<-ancillary[,"PO"]
  
  #filling in the missing rings towards the pith is recommended when there are multiple cores per tree. 
  #average multiple BAI time series of a tree, not raw ring width measurements.
  #Otherwise, one can end up with an artifact, i.e. one negative increment when two radii 
  #have different growth rates and the faster growing sample has more rings.
  #The filled rings will be deleted at the end.
  
  rwl_filled<-NULL
  for (i in 1:length(rwl)){
    if(rwl_d2pith[i] !=0){
      temp<-data.frame(rep(rwl_d2pith[i]/rwl_po[i],times=rwl_po[i])) #create a dummy time series with the number of estimated rings missing 
      temp2<-na.omit(rwl[,i])
      select_remove<-c(attr(temp2,"na.action"))
      if(!is.null(select_remove)){
        temp2<-data.frame(temp2,row.names=rownames(rwl)[-select_remove]) 
      }else{
        temp2<- data.frame(temp2,row.names=rownames(rwl))
      }
      rownames(temp)<-c((as.numeric(rownames(temp2))[1]-rwl_po[i]):(as.numeric(rownames(temp2))[1]-1)) 
      colnames(temp)<-colnames(temp2)
      rwl_filled[[i]]<-ts(rbind(temp,temp2),start=as.numeric(rownames(temp))[1]) #paste the dummy time series and ring width measurements
    } else { 
      temp3<-data.frame(rwl[,i],row.names=rownames(rwl))
      rwl_filled[[i]]<-ts(temp3,start=as.numeric(rownames(temp3))[1])
    }
    
    
  }
  names(rwl_filled)<-colnames(rwl)
  rwl_filled<-do.call(cbind,rwl_filled)
  rownames(rwl_filled)<-c(tsp(rwl_filled)[1]:tsp(rwl_filled)[2])
  
  cum.na <- function(x) { #cumulative sum that changes NAs to zeros. helper function
    x[which(is.na(x))] <- 0
    return(cumsum(x))
  }
  
  IP <- apply(rwl_filled, 2, sum,na.rm=T) #how long is your measured radius including missing distance to pith?
  IH <- rwl_filled
  G <- IH
  for(i in 1:ncol(rwl_filled)){IH[,i] <-cum.na(rwl_filled[,i])}  #cumulative sum
  for(i in 1:ncol(IH)){G[,i]<-(IP[i]-(IH[,i]))/IP[i]} #proportional cumulative sum, reversed
  
  DBHhist<-G
  colnames(DBHhist)<-colnames(rwl)
  for (i in 1:ncol(DBHhist)){
    DBHhist[,i]<-rwl_diam[i]-(G[,i] *rwl_diam[i]) #subtract proportional DBH from measured DBH, the final proportional historic DBH
  }
  
  for(i in 1:ncol(DBHhist)){  #clean zeros, fill with NAs
    temp<-which(is.na(rwl_filled[,i]))
    if(any(which(diff(temp)>1))){
      DBHhist[which(is.na(rwl_filled[,i]))[-which(diff(temp)>1)],i]<-NA
    }else{
      DBHhist[rev(which(is.na(rwl_filled[,i]))),i]<-NA 
    }
  }
  
  if(nrow(rwl_filled)-nrow(rwl)==0){
    DBHhist2<- DBHhist[1:nrow(DBHhist),]
    DBHhist2[is.na(rwl)]<-NA
  }else{
    DBHhist2<- DBHhist[-c(1:(nrow(rwl_filled)-nrow(rwl))),]
    DBHhist2[is.na(rwl)]<-NA
  }
  
  
  out <- DBHhist
  n.vec <- seq_len(nrow(DBHhist))
  for (i in seq_len(ncol(rwl))) {
    dat <- DBHhist[,i]
    dat2 <- na.omit(dat)
    
    
    r0 <- dat2/2*10 #if dbh in cm, if dbh in mm, then remove *10; /2 to convert to radius
    bai <- pi * (diff(r0 * r0))
    na <- attributes(dat2)$na.action
    no.na <- n.vec[!n.vec %in% na]
    out[no.na[-1], i] <-  bai
  }
  
  if(nrow(rwl_filled)-nrow(rwl)==0){
    out2<- out[1:nrow(out),]
    out2[is.na(rwl)]<-NA
  }else{
    out2<- out[-c(1:(nrow(rwl_filled)-nrow(rwl))),]
    out2[is.na(rwl)]<-NA #this removes all the filled dummy values from rwl_filled above
  }
  
  
  output<-list(DBHhist_raw=DBHhist2,baiBakker_raw=out2)
  #I termed it raw, because at some point back in time I wanted to expand the function
  #to fill the innermost rings of the sample with less rings towards the center with data from the other core(s).
  #Never got to it, though.
  
}



