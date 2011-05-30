`read.crn` <-
function(fname, header=NULL)
{
  ## Open the data file for reading
  dat=file(fname,"r")
  if(is.null(header)){
    ## Try to determine if the file has a header. This is failable.
    ## Find out if an ITRDB header (3 lines) in file
    hdr1=readLines(dat,n=1)
    if(length(hdr1)==0){
      close(dat)
      stop("File is empty")
    }
    if(nchar(hdr1) < 10){
      close(dat)
      stop("First line in the crn file ends before col 10")
    }
    yrcheck=suppressWarnings(as.numeric(substr(hdr1,7,10)))
    if(is.null(yrcheck) | length(yrcheck)!=1 | is.na(yrcheck) |
       yrcheck < -1e04 | yrcheck > 1e04) {
      cat("There appears to be a header in the crn file\n")
      is.head=TRUE
    }
    else {
      cat("There does not appear to be a header in the crn file\n")
     	is.head=FALSE # No header lines
    }
    seek(dat, where=0, origin="start")
  } else if(!is.logical(header)){
    close(dat)
    stop("header must be NULL or logical")
  } else{
    is.head = header
  }
  if(is.head){
    ## Read 4th line - should be first data line
    dat1=readLines(dat,n=4)
    if(length(dat1)<4){
      close(dat)
      stop("File has under 4 lines")
    }
    dat1=dat1[4]
  } else{
    dat1=readLines(dat,n=1)
    if(length(dat1)==0){
      close(dat)
      stop("File is empty")
    }
  }
  if(nchar(dat1) < 10){
    close(dat)
    stop("First data line ends before col 10")
  }
  yrcheck=as.numeric(substr(dat1,7,10))
  if(is.null(yrcheck) | length(yrcheck)!=1 | is.na(yrcheck) |
     yrcheck < -1e04 | yrcheck > 1e04) {
    close(dat)
    stop("Cols 7-10 of first data line not a year")
  }
  ## Look at last line to determine if Chronology Statistics are present
  ## if nchar <=63 then there is a stats line
  lastline=readLines(dat,n=-1)
  close(dat)
  nlines=length(lastline)
  ## Read file
  skip.lines=ifelse(is.head,3,0)
  ## Get chron stats if needed
  chron.stats=read.fwf(fname,c(6,4,6,6,6,7,9,9,10),
    skip=nlines+skip.lines,strip.white=TRUE)
  ## Really read file
  dat=read.fwf(fname,c(6,4,rep(c(4,3),10)),skip=skip.lines,strip.white=TRUE)
  ## If columns 3 in chron.stats is an integer then there is no statistics line
  ## Function to check if x (a single number) is equivalent to
  ## its integer representation.
  ## Note: Returns FALSE for values that fall outside
  ## the range of the integer type.
  is.int = function(x) {
    x >= -.Machine$integer.max &&
    x <= .Machine$integer.max &&
    x == as.integer(x)
  }
###  is.int=function(x,tol=.Machine$double.eps) {
###    if(is.numeric(x)) ans=(x - floor(x)) < tol
###    else ans=FALSE
###    ans
###  }

  if(is.numeric(chron.stats[,3]) & !is.int(as.numeric(chron.stats[,3]))){
    names(chron.stats)=c("SiteID","nYears","AC[1]","StdDev","MeanSens","MeanRWI",
      "IndicesSum","IndicesSS","MaxSeries")
    cat("Embedded chronology statistics\n")
    print(chron.stats)
    ## Chop off last row of dat
    dat=dat[-nrow(dat),]
  }

  series=dat[,1]
  series.ids=unique(series)
  nseries=length(series.ids)
  cat("There are ",nseries," series\n",sep="")
  series.index=match(series,series.ids)
  min.year=(min(dat[,2]) %/% 10) * 10
  max.year=((max(dat[,2])+10) %/% 10) * 10
  span=max.year - min.year + 1
  ncol.crn.mat = nseries + 1
  crn.mat=matrix(NA,ncol=ncol.crn.mat,nrow=span)
  colnames(crn.mat)=c(as.character(series.ids),"samp.depth")
  rownames(crn.mat)=min.year:max.year
  for(i in 1:nseries){
    id=unique(dat[series.index==i,1])
    decade.yr=dat[series.index==i,2]
    first.yr=dat[series.index==i,2][1]
    ## RWI
    x=dat[series.index==i,-c(1,2,seq(from=4, to=22, by=2))]
    ## All sample depths
    y=dat[series.index==i,-c(1,2,seq(from=3, to=21, by=2))]
    for(j in 1:nrow(x)) {
      yr=decade.yr[j]
      if(j==1) yr=min.year
      for(k in 1:ncol(x)){
        if(is.na(x[j,k])) break
        crn.mat[as.character(yr),i]=x[j,k]
        ## If i is one then make samp depth
        if(i == 1) {
          crn.mat[as.character(yr),ncol.crn.mat]=y[j,k]
        }
        yr=yr + 1
      }
    }
  }
  ## Clean up NAs
  crn.mat[which(crn.mat[,-ncol.crn.mat]==9990)]=NA # rely on column-major order
  crn.mat=crn.mat[!apply(as.matrix(is.na(crn.mat[,-ncol.crn.mat])),1,all),]
  ## If samp depth is all 1 then dump it
  sd.one=all(crn.mat[,ncol.crn.mat]==1)
  if(sd.one) {
    save.names=colnames(crn.mat)[-ncol.crn.mat]
    crn.mat=crn.mat[,-ncol.crn.mat]
    crn.mat=crn.mat/1000
    crn.df=as.data.frame(crn.mat)
    colnames(crn.df)=save.names
    cat("All embedded sample depths are one...Dumping from matrix\n")
  }
  else {
    crn.mat[,1:nseries]=crn.mat[,1:nseries]/1000
    crn.df=as.data.frame(crn.mat)
  }
  crn.df
}
