skel.ccf <- function(rwl,series,series.yrs = as.numeric(names(series)),
                       win.start, win.width=50, n = NULL, prewhiten = TRUE, 
                       biweight = TRUE) {      
  yrs <- seq(from=win.start,to=win.start+win.width)
  cen.win <- win.width/2
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
  master.skel <- skel.plot(master,yr.vec=master.yrs,dat.out=TRUE,plot=FALSE)
  master.skel <- master.skel[master.skel[,1]%in%yrs,]
  master.yrs.sig <- master.skel[!is.na(master.skel[,2]),1]
  series.skel <- skel.plot(series,yr.vec=series.yrs,dat.out=TRUE,plot=FALSE)
  series.skel <- series.skel[series.skel[,1]%in%yrs,]
  series.yrs.sig <- series.skel[!is.na(series.skel[,2]),1]

  # divide in half
  first.half <- 1:cen.win
  second.half <- (cen.win + 1):win.width
  first.yrs <- yrs[first.half]
  second.yrs <- yrs[second.half]
  master.early <- master[first.half]
  series.early <- series[first.half]
  master.late <- master[second.half]
  series.late <- series[second.half]

  # subset skel data
  early.series.skel <- series.skel[series.skel[,1]%in%first.yrs,]
  early.series.yrs.sig <- early.series.skel[!is.na(early.series.skel[,2]),1]
  
  early.master.skel <- master.skel[master.skel[,1]%in%first.yrs,]
  early.master.yrs.sig <- early.master.skel[!is.na(early.master.skel[,2]),1]
  
  late.series.skel <- series.skel[series.skel[,1]%in%second.yrs,]
  late.series.yrs.sig <- late.series.skel[!is.na(late.series.skel[,2]),1]
  
  late.master.skel <- master.skel[master.skel[,1]%in%second.yrs,]
  late.master.yrs.sig <- late.master.skel[!is.na(late.master.skel[,2]),1]
  
  
  # ccf
  ccf.early <- as.vector(ccf(x=series.early,y=master.early,lag.max=5,plot=FALSE)$acf)
  ccf.late <- as.vector(ccf(x=series.late,y=master.late,lag.max=5,plot=FALSE)$acf)
  pcrit=0.05
  sig <- qnorm(1 - pcrit / 2) / sqrt(length(master.early))
  sig <- c(-sig, sig)
  
  # cor and skel agreement
  overall.r <- round(cor(series,master),3)  
  early.r <- round(cor(series.early,master.early),3)
  late.r <- round(cor(series.late,master.late),3)

  # aggreement btwn series skel and master skel 
  overall.agree <- sum(series.yrs.sig%in%master.yrs.sig)/length(master.yrs.sig)
  overall.agree <- round(overall.agree*100,1)
  
  early.agree <- sum(early.series.yrs.sig%in%early.master.yrs.sig)/length(early.master.yrs.sig)
  early.agree <- round(early.agree*100,1)
    
  late.agree <- sum(late.series.yrs.sig%in%late.master.yrs.sig)/length(late.master.yrs.sig)
  late.agree <- round(late.agree*100,1)
  
  # plotting, finally..
  grid.newpage()
  # bounding box for margins  
  bnd.vp <- plotViewport(margins=rep(0.5,4),name = "bnd.vp") # 1/2 line margin
  # a box for text
  overall.txt.vp <- viewport(x = 0, y = 0.95, width = 1, height = 0.05,
                          just = c("left", "bottom"), 
                          name = "overall.txt.vp")
  # bounding box for skeleton plot  
  skel.bnd.vp <- viewport(x = 0, y = 0.5, width = 1, height = 0.5,
                          just = c("left", "bottom"), name = "skel.bnd.vp")
  # plotting region for skeleton plot
  skel.region.vp <- plotViewport(margins=c(2.5,2,2.5,2),
                                 xscale=c(min(yrs)-1,max(yrs)+1), 
                                 yscale=c(-10,10),
                                 name = "skel.region.vp")
  # box for text comparing early and late periods
  text.bnd.vp <- viewport(x = 0, y = 0.4, width = 1, height = 0.1,
                          just = c("left", "bottom"), name = "text.bnd.vp")
  
  # bounding box for ccf early
  ccf.early.bnd.vp <- viewport(x = 0, y = 0, width = 0.5, height = 0.4,
                               just = c("left", "bottom"), name = "ccf.early.bnd.vp")
  # plotting region for ccf early
  ccf.early.region.vp <- plotViewport(margins=c(2,2,0,0),
                                      xscale=c(0,12), 
                                      yscale=c(-1,1),
                                      name = "ccf.early.region.vp")
  # bounding box for ccf late
  ccf.late.bnd.vp <- viewport(x = 0.5, y = 0, width = 0.5, height = 0.4,
                              just = c("left", "bottom"), name = "ccf2.late.vp")
  # plotting region for ccf late
  ccf.late.region.vp <- plotViewport(margins=c(2, 0, 0, 2),
                                     xscale=c(0,12), 
                                     yscale=c(-1,1),
                                     name = "ccf.late.region.vp")
  # actual plotting
  pushViewport(bnd.vp) # inside margins
    pushViewport(skel.bnd.vp) # inside skel
        pushViewport(skel.region.vp) # inside margins
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
                    y = unit(-13, "native"), rot = 90,
                    gp=gpar(fontsize=14))
          grid.text(series.yrs.sig, x=unit(series.yrs.sig,"native"), 
                    y = unit(13, "native"), rot = 90,
                    gp= gpar(fontsize = 14))
          grid.text("Master",x=unit(min(yrs)-2,"native"),
                    y=unit(-10,"native"),just = "left",rot=90,
                    gp= gpar(fontsize = 14))
          grid.text("Series",x=unit(min(yrs)-2,"native"),
                    y=unit(10,"native"),just = "right",rot=90,
                    gp= gpar(fontsize = 14))
  
  upViewport(3) # back to bnd
  pushViewport(ccf.early.bnd.vp) #into early ccf
    pushViewport(ccf.early.region.vp) # inside margins
      grid.grill(v = unit(seq(1, 11, by=1), "native"),
                 h=NA,
                 gp = gpar(col="lightblue", lineend = "square", 
                           linejoin = "round"))
      grid.segments(x0=unit(0, "native"),y0=unit(sig[1], "native"),
                    x1=unit(12, "native"),y1=unit(sig[1], "native"),
                    gp=gpar(col="darkblue", lty="dashed",lwd=2))

      grid.segments(x0=unit(0, "native"),y0=unit(sig[2], "native"),
                    x1=unit(12, "native"),y1=unit(sig[2], "native"),
                    gp=gpar(col="darkblue", lty="dashed",lwd=2))

      grid.segments(x0=unit(0, "native"),y0=unit(sig[2], "native"),
                    x1=unit(12, "native"),y1=unit(sig[2], "native"),
                    gp=gpar(col="darkblue", lty="dashed",lwd=2))

      grid.segments(x0=unit(0, "native"),y0=unit(0, "native"),
                    x1=unit(12, "native"),y1=unit(0, "native"),
                    gp=gpar(col="black", lty="solid",lwd=1))

      grid.segments(x0=unit(6, "native"),y0=unit(-1, "native"),
                    x1=unit(6, "native"),y1=unit(1, "native"),
                    gp=gpar(col="black", lty="solid",lwd=1))


      grid.segments(x0=1:11,y0=0,x1=1:11,y1=ccf.early,
                    default.units="native",
                    gp=gpar(lwd=2,lend="butt", col="darkblue"))
      grid.points(x=1:11,y=ccf.early,pch=21,
                  default.units="native",
                  gp=gpar(fill="lightblue",col="darkblue"))
      grid.text("(Negative)",y=unit(-1,"lines"),x=unit(1,"native"),
                default.units="native",just = "left",
                gp= gpar(fontsize = 14))
      grid.text("(Positive)",y=unit(-1,"lines"),x=unit(11,"native"),
                just = "right",
                gp= gpar(fontsize = 14))

  upViewport(2)
  pushViewport(ccf.late.bnd.vp) #into late ccf
    pushViewport(ccf.late.region.vp) # inside margins
      grid.grill(v = unit(seq(1, 11, by=1), "native"),
                 h=NA,
                 gp = gpar(col="lightblue", lineend = "square", 
                           linejoin = "round"))
      grid.segments(x0=unit(0, "native"),y0=unit(sig[1], "native"),
                    x1=unit(12, "native"),y1=unit(sig[1], "native"),
                    gp=gpar(col="darkblue", lty="dashed",lwd=2))
      
      grid.segments(x0=unit(0, "native"),y0=unit(sig[2], "native"),
                    x1=unit(12, "native"),y1=unit(sig[2], "native"),
                    gp=gpar(col="darkblue", lty="dashed",lwd=2))
      
      grid.segments(x0=unit(0, "native"),y0=unit(sig[2], "native"),
                    x1=unit(12, "native"),y1=unit(sig[2], "native"),
                    gp=gpar(col="darkblue", lty="dashed",lwd=2))
      
      grid.segments(x0=unit(0, "native"),y0=unit(0, "native"),
                    x1=unit(12, "native"),y1=unit(0, "native"),
                    gp=gpar(col="black", lty="solid",lwd=1))
      
      grid.segments(x0=unit(6, "native"),y0=unit(-1, "native"),
                    x1=unit(6, "native"),y1=unit(1, "native"),
                    gp=gpar(col="black", lty="solid",lwd=1))
      
      
      grid.segments(x0=1:11,y0=0,x1=1:11,y1=ccf.late,
                    default.units="native",
                    gp=gpar(lwd=2,lend="butt", col="darkblue"))
      grid.points(x=1:11,y=ccf.late,pch=21,
                  default.units="native",
                  gp=gpar(fill="lightblue",col="darkblue"))
      grid.text("(Negative)",y=unit(-1,"lines"),x=unit(1,"native"),
                default.units="native",just = "left",
                gp= gpar(fontsize = 14))
      grid.text("(Positive)",y=unit(-1,"lines"),x=unit(11,"native"),
                just = "right",
                gp= gpar(fontsize = 14))
  popViewport(2) # to top
  grid.segments(x0=0.5,y0=0,x1=0.5,y1=1,
                default.units="npc",
                gp=gpar(lwd=2,lend="butt", col="black"))
  pushViewport(text.bnd.vp) # description
    tmp.txt <- paste("Period: ",min(first.yrs),"-",max(first.yrs), 
                     ", r(lag0)= ", early.r, sep="")
    grid.text(tmp.txt,y=unit(0.75,"npc"),x=unit(0.25,"npc"),
              just = "center",
              gp= gpar(fontsize = 14))

    tmp.txt <- paste("Skeleton Agreement ", early.agree, "%",sep="")
    grid.text(tmp.txt,y=unit(0.25,"npc"),x=unit(0.25,"npc"),
              just = "center",
              gp= gpar(fontsize = 14))


    tmp.txt <- paste("Period: ",min(second.yrs),"-",max(second.yrs), 
                     ", r(lag0)= ", late.r, sep="")
    grid.text(tmp.txt,y=unit(0.75,"npc"),x=unit(0.75,"npc"),
              just = "center",
              gp= gpar(fontsize = 14))

    tmp.txt <- paste("Skeleton Agreement ", late.agree, "%",sep="")
    grid.text(tmp.txt,y=unit(0.25,"npc"),x=unit(0.75,"npc"),
              just = "center",
              gp= gpar(fontsize = 14))

  upViewport(1) # back to bnd

  pushViewport(overall.txt.vp) # description
  tmp.txt <- paste("Period: ",min(yrs),"-",max(yrs), 
                   ", r(lag0)= ", overall.r, 
                   ". Skeleton Agreement ", overall.agree, "%",sep="")
  grid.rect(gp=gpar(col=NA))
  grid.text(tmp.txt,y=unit(0.5,"npc"),x=unit(0.5,"npc"),
            just = "center",
            gp= gpar(fontsize = 14))
  
}
