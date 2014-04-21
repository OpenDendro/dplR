xskel.plot <- function(rwl,series,series.yrs = as.numeric(names(series)),
         win.start, win.end=win.start+100, n = NULL, prewhiten = TRUE, 
         biweight = TRUE) {      
  
  ## Handle different types of 'series'
  tmp <- pick.rwl.series(rwl, series, series.yrs)
  rwl <- tmp[[1]]
  series <- tmp[[2]]
  
  master.yrs <- as.numeric(rownames(rwl))
  series.yrs <- as.numeric(names(series))
  yrs <- seq(from=win.start,to=win.end)
  nyrs <- length(yrs)
  
  if(nyrs > 101){
    warning("These plots get crowded with windows longer than 100 years.")
  }
  # check window overlap with master and series yrs
  if (!all(yrs %in% series.yrs)) {
    cat("Window Years: ", min(yrs), "-", max(yrs)," & ",
        "Series Years: ", min(series.yrs), "-", max(series.yrs),
        "\n", sep="")
    stop("Fix window overlap")
  }
  if (!all(yrs %in% master.yrs)) {
    cat("Window Years: ", min(yrs), "-", max(yrs)," & ",
        "Master Years: ", min(master.yrs), "-", max(master.yrs),
        "\n", sep="")
    stop("Fix window overlap")
  }
  
  # normalize.
  names(series) <- series.yrs
  tmp <- normalize.xdate(rwl, series, n, prewhiten, biweight)
  
  # master
  master <- tmp$master
  master.yrs <- as.numeric(names(master))
  master <- master[master.yrs%in%yrs]
  master.yrs <- as.numeric(names(master))
  # series
  series <- tmp$series
  series.yrs <- as.numeric(names(series))
  series <- series[series.yrs%in%yrs]
  series.yrs <- as.numeric(names(series))
  
  
  # skeleton
  master.skel <- cbind(master.yrs,xskel.calc(master))
  master.skel <- master.skel[master.skel[,1]%in%yrs,]
  master.yrs.sig <- master.skel[!is.na(master.skel[,2]),1]
  series.skel <- cbind(series.yrs,xskel.calc(series))
  series.skel <- series.skel[series.skel[,1]%in%yrs,]
  series.yrs.sig <- series.skel[!is.na(series.skel[,2]),1]
  
  # cor and skel agreement
  overall.r <- round(cor(series,master),3)    
  overall.agree <- sum(series.yrs.sig%in%master.yrs.sig)/length(master.yrs.sig)
  overall.agree <- round(overall.agree*100,1)
  
  # build page for plotting
  grid.newpage()
  # 1.0 a bounding box for margins  
  bnd.vp <- plotViewport(margins=rep(0.5,4),name = "bnd.vp") # 1/2 line margin
  # go from bottom up.
  
  # 4.1 bounding box for skeleton plot. 55% of area  
  skel.bnd.vp <- viewport(x = 0, y = 0, width = 1, height = 0.95,
                          just = c("left", "bottom"), name = "skel.bnd.vp")
  # 4.2 plotting region for skeleton plot. 2 lines left and right. 
  # 3 lines on top and bottom
  skel.region.vp <- plotViewport(margins=c(3,2,3,2),
                                 xscale=c(min(yrs)-0.5,max(yrs)+0.5), 
                                 yscale=c(-10,10),
                                 name = "skel.region.vp")
  # 5.0 a box for overall text. 5%
  overall.txt.vp <- viewport(x = 0, y = 0.95, width = 1, height = 0.05,
                             just = c("left", "bottom"), 
                             name = "overall.txt.vp")
  
  # actual plotting
  pushViewport(bnd.vp) # inside margins
  pushViewport(skel.bnd.vp) # inside skel
  pushViewport(skel.region.vp) # inside margins
  grid.rect(gp = gpar(col="lightgreen", lwd=1))
  grid.grill(h = unit(seq(-10, 10, by=1), "native"),
             v = unit(yrs-0.5, "native"),
             gp = gpar(col="lightgreen", lineend = "square", 
                       linejoin = "round"))
  # rw plot
  master.tmp <- master*-2
  for(i in 1:length(yrs)){
    xx <- c(yrs[i]+0.5,yrs[i]-0.5,yrs[i]-0.5,yrs[i]+0.5)
    yy <- c(0,0,master.tmp[i],master.tmp[i])
    grid.polygon(xx,yy,default.units="native", 
                 gp=gpar(fill='lightgreen',col='darkgreen')) 
  }
  series.tmp <- series*2
  for(i in 1:length(yrs)){
    xx <- c(yrs[i]+0.5,yrs[i]-0.5,yrs[i]-0.5,yrs[i]+0.5)
    yy <- c(0,0,series.tmp[i],series.tmp[i])
    grid.polygon(xx,yy,default.units="native", 
                 gp=gpar(fill='lightgreen',col='darkgreen')) 
  }
  
  #master
  grid.segments(x0=master.yrs.sig,y0=0,
                x1=master.yrs.sig,y1=-10,
                default.units="native",
                gp=gpar(lwd=1,col='black',lineend="butt"))
  grid.segments(x0=master.skel[,1],y0=0,
                x1=master.skel[,1],y1=master.skel[,2]*-1,
                default.units="native",
                gp=gpar(lwd=5,col='black',lineend="butt"))
  #series
  grid.segments(x0=series.yrs.sig,y0=0,
                x1=series.yrs.sig,y1=10,
                default.units="native",
                gp=gpar(lwd=1,col='black',lineend="butt"))
  grid.segments(x0=series.skel[,1],y0=0,
                x1=series.skel[,1],y1=series.skel[,2],
                default.units="native",
                gp=gpar(lwd=5,col='black',lineend="butt"))
  
  # text  
  grid.text(master.yrs.sig, x=unit(master.yrs.sig,"native"), 
            y = unit(0, "npc"), rot = 90,just="right",
            gp=gpar(fontsize=12))
  grid.text(series.yrs.sig, x=unit(series.yrs.sig,"native"), 
            y = unit(1, "npc"), rot = 90,just="left",
            gp= gpar(fontsize = 12))
  grid.text("Master",x=unit(0,"npc"),
            y=unit(0,"npc"),hjust = 0,vjust = 0,rot=90,
            gp= gpar(fontsize = 12))
  grid.text("Series",x=unit(0,"npc"),
            y=unit(1,"npc"),hjust=1,vjust=0,rot=90,
            gp= gpar(fontsize = 12))
  
  upViewport(3) # back to bnd
  pushViewport(overall.txt.vp) # description
  tmp.txt <- paste("Period: ",min(yrs),"-",max(yrs), 
                   ", r(lag0)= ", overall.r, 
                   ". Skeleton Agreement ", overall.agree, "%",sep="")
  tmp.txt <- bquote("Period:" ~ .(min(yrs)) * "-" * 
                      .(max(yrs)) * ","~r[lag0] * "=" * .(overall.r)*
                      ","~"Skeleton Agreement"~.(overall.agree)*"%")
  grid.rect(gp=gpar(col=NA,fill="white"))
  grid.text(tmp.txt,y=unit(0.5,"npc"),x=unit(0.5,"npc"),
            just = "center",
            gp= gpar(fontsize = 12))
  
}