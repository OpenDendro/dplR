xskel.ccf.plot <- function(rwl,series,series.yrs = as.numeric(names(series)),
         win.start, win.width=50, n = NULL, prewhiten = TRUE,
         biweight = TRUE, series.x = FALSE) {
  
  ## check to see that win.width is even
  if(as.logical(win.width %% 2)) stop("'win.width' must be even")
  if (win.width > 100) {
    warning("win.width should be < 100 unless your plotting is very wide!")
  }

  ## Handle different types of 'series'
  tmp <- pick.rwl.series(rwl, series, series.yrs)
  rwl2 <- tmp[[1]]
  series2 <- tmp[[2]]

  master.yrs <- as.numeric(rownames(rwl2))
  series.yrs2 <- as.numeric(names(series2))
  yrs <- seq(from=win.start,to=win.start+win.width)
  ## nyrs <- length(yrs)
  cen.win <- win.width/2

  ## check window overlap with master and series yrs
  if (!all(yrs %in% series.yrs2)) {
    cat("Window Years: ", min(yrs), "-", max(yrs)," & ",
        "Series Years: ", min(series.yrs2), "-", max(series.yrs2),
        "\n", sep="")
    stop("Fix window overlap")
  }
  if (!all(yrs %in% master.yrs)) {
    cat("Window Years: ", min(yrs), "-", max(yrs)," & ",
        "Master Years: ", min(master.yrs), "-", max(master.yrs),
        "\n", sep="")
    stop("Fix window overlap")
  }

  ## normalize.
  names(series2) <- series.yrs2
  tmp <- normalize.xdate(rwl2, series2, n, prewhiten, biweight)

  ## master
  master <- tmp$master
  master.yrs <- as.numeric(names(master))
  master <- master[master.yrs%in%yrs]
  master.yrs <- as.numeric(names(master))
  ## series
  series2 <- tmp$series
  series.yrs2 <- as.numeric(names(series2))
  series2 <- series2[series.yrs2%in%yrs]
  series.yrs2 <- as.numeric(names(series2))


  ## skeleton
  master.skel <- cbind(master.yrs,xskel.calc(master))
  master.skel <- master.skel[master.skel[,1]%in%yrs,]
  master.yrs.sig <- master.skel[!is.na(master.skel[,2]),1]
  series.skel <- cbind(series.yrs2,xskel.calc(series2))
  series.skel <- series.skel[series.skel[,1]%in%yrs,]
  series.yrs.sig <- series.skel[!is.na(series.skel[,2]),1]

  ## divide in half
  first.half <- 1:cen.win
  second.half <- (cen.win + 1):win.width
  first.yrs <- yrs[first.half]
  second.yrs <- yrs[second.half]
  master.early <- master[first.half]
  series.early <- series2[first.half]
  master.late <- master[second.half]
  series.late <- series2[second.half]

  ## subset skel data
  early.series.skel <- series.skel[series.skel[,1]%in%first.yrs,]
  early.series.yrs.sig <- early.series.skel[!is.na(early.series.skel[,2]),1]

  early.master.skel <- master.skel[master.skel[,1]%in%first.yrs,]
  early.master.yrs.sig <- early.master.skel[!is.na(early.master.skel[,2]),1]

  late.series.skel <- series.skel[series.skel[,1]%in%second.yrs,]
  late.series.yrs.sig <- late.series.skel[!is.na(late.series.skel[,2]),1]

  late.master.skel <- master.skel[master.skel[,1]%in%second.yrs,]
  late.master.yrs.sig <- late.master.skel[!is.na(late.master.skel[,2]),1]


  ## ccf
  if(series.x){
    ccf.early <- as.vector(ccf(x=series.early,y=master.early,lag.max=5,plot=FALSE)$acf)
    ccf.late <- as.vector(ccf(x=series.late,y=master.late,lag.max=5,plot=FALSE)$acf)
  }
  else{
    ccf.early <- as.vector(ccf(x=master.early,y=series.early,lag.max=5,plot=FALSE)$acf)
    ccf.late <- as.vector(ccf(x=master.late,y=series.late,lag.max=5,plot=FALSE)$acf)
  }
  pcrit=0.05
  sig <- qnorm(1 - pcrit / 2) / sqrt(length(master.early))
  sig <- c(-sig, sig)

  ## cor and skel agreement
  overall.r <- round(cor(series2,master),3)
  early.r <- round(cor(series.early,master.early),3)
  late.r <- round(cor(series.late,master.late),3)

  ## aggreement btwn series skel and master skel
  overall.agree <- sum(series.yrs.sig%in%master.yrs.sig)/length(master.yrs.sig)
  overall.agree <- round(overall.agree*100,1)

  early.agree <- sum(early.series.yrs.sig%in%early.master.yrs.sig)/length(early.master.yrs.sig)
  early.agree <- round(early.agree*100,1)

  late.agree <- sum(late.series.yrs.sig%in%late.master.yrs.sig)/length(late.master.yrs.sig)
  late.agree <- round(late.agree*100,1)

  ## build page for plotting
  grid.newpage()
  fontsize <- 12       # fontsize for all text
  pointsize <- 12      # fontsize for grid.points()
  textJust <- "center" # justification for horizontal text elements
  col1light <- "lightgreen"
  col1dark <- "darkgreen"
  col2light <- "lightblue"
  col2dark <- "darkblue"
  ## 1.0 a bounding box for margins
  bnd.vp <- plotViewport(margins=rep(0.5,4), # 1/2 line margin
                         name = "bnd.vp",
                         gp = gpar(fontsize = fontsize))
  ## go from bottom up.

  ## 2.1 bounding box for ccf early: 30% of area height inside bnd.vp
  ccf.early.bnd.vp <- viewport(x = 0, y = 0, width = 0.5, height = 0.3,
                               just = c("left", "bottom"),
                               name = "ccf.early.bnd.vp")
  ## 2.12 plotting region for ccf early. 1 line margin bottom. 2 lines left
  ccf.early.region.vp <- plotViewport(margins=c(1,2,0,0),
                                      xscale=c(0,12),
                                      yscale=c(-1,1),
                                      name = "ccf.early.region.vp")
  ## 2.2 bounding box for ccf late: 30% of area height inside bnd.vp
  ccf.late.bnd.vp <- viewport(x = 0.5, y = 0, width = 0.5, height = 0.3,
                              just = c("left", "bottom"),
                              name = "ccf.late.bnd.vp")
  ## 2.22 plotting region for ccf late. 1 line margin bottom. 2 lines right
  ccf.late.region.vp <- plotViewport(margins=c(1, 0, 0, 2),
                                     xscale=c(0,12),
                                     yscale=c(-1,1),
                                     name = "ccf.late.region.vp")

  ## 3.0 box for text comparing early and late periods. 10% area height
  text.bnd.vp <- viewport(x = 0, y = 0.3, width = 1, height = 0.1,
                          just = c("left", "bottom"), name = "text.bnd.vp")

  ## 4.1 bounding box for skeleton plot. 55% of area
  skel.bnd.vp <- viewport(x = 0, y = 0.4, width = 1, height = 0.55,
                          just = c("left", "bottom"), name = "skel.bnd.vp")
  ## 4.2 plotting region for skeleton plot. 2 lines left and right.
  ## 3 lines on top and bottom
  skel.region.vp <- plotViewport(margins=c(3,2,3,2),
                                 xscale=c(min(yrs)-0.5,max(yrs)+0.5),
                                 yscale=c(-10,10),
                                 name = "skel.region.vp")
  ## 5.0 a box for overall text. 5%
  overall.txt.vp <- viewport(x = 0, y = 0.95, width = 1, height = 0.05,
                             just = c("left", "bottom"),
                             name = "overall.txt.vp")



  ## actual plotting
  dev.hold()
  on.exit(dev.flush())
  pushViewport(bnd.vp) # inside margins
  pushViewport(skel.bnd.vp) # inside skel
  pushViewport(skel.region.vp) # inside margins
  grid.rect(gp = gpar(col=col1light, lwd=1))
  grid.grill(h = unit(seq(-10, 10, by=1), "native"),
             v = unit(yrs-0.5, "native"),
             gp = gpar(col=col1light, lineend = "square",
                       linejoin = "round"))
  ## rw plot
  grid.rect(x = yrs, y = 0, width = 1, height = 2 * master,
            hjust = 0.5, vjust = 1, default.units = "native",
            gp=gpar(fill=col1light,col=col1dark))
  grid.rect(x = yrs, y = 0, width = 1, height = 2 * series2,
            hjust = 0.5, vjust = 0, default.units = "native",
            gp=gpar(fill=col1light,col=col1dark))

  ## master
  grid.segments(x0=master.yrs.sig,y0=0,
                x1=master.yrs.sig,y1=-10,
                default.units="native",
                gp=gpar(lwd=1,col='black',lineend="butt"))
  grid.segments(x0=master.skel[,1],y0=0,
                x1=master.skel[,1],y1=master.skel[,2]*-1,
                default.units="native",
                gp=gpar(lwd=5,col='black',lineend="butt"))
  ## series
  grid.segments(x0=series.yrs.sig,y0=0,
                x1=series.yrs.sig,y1=10,
                default.units="native",
                gp=gpar(lwd=1,col='black',lineend="butt"))
  grid.segments(x0=series.skel[,1],y0=0,
                x1=series.skel[,1],y1=series.skel[,2],
                default.units="native",
                gp=gpar(lwd=5,col='black',lineend="butt"))

  ## text
  grid.text(master.yrs.sig, x=unit(master.yrs.sig,"native"),
            y = unit(0, "npc"), rot = 90,just="right")
  grid.text(series.yrs.sig, x=unit(series.yrs.sig,"native"),
            y = unit(1, "npc"), rot = 90,just="left")
  grid.text(gettext("Master", domain="R-dplR"),x=unit(0,"npc"),
            y=unit(0,"npc"),hjust = 0,vjust = 0,rot=90)
  grid.text(gettext("Series", domain="R-dplR"),x=unit(0,"npc"),
            y=unit(1,"npc"),hjust=1,vjust=0,rot=90)

  popViewport(2) # back to bnd

  negText <- textGrob(gettext("(Negative)", domain="R-dplR"),
                      y=unit(0.25,"lines"),x=unit(3,"native"),
                      just = textJust)
  posText <- textGrob(gettext("(Positive)", domain="R-dplR"),
                      y=unit(0.25,"lines"),x=unit(9,"native"),
                      just = textJust)
  

  for (period in c("early", "late")) {
      if (period == "early") {
          vp1 <- ccf.early.bnd.vp
          vp2 <- ccf.early.region.vp
          ccf.period <- ccf.early
      } else {
          vp1 <- ccf.late.bnd.vp
          vp2 <- ccf.late.region.vp
          ccf.period <- ccf.late
      }
      pushViewport(vp1) # into ccf
      pushViewport(vp2) # inside margins
      grid.grill(v = unit(seq(1, 11, by=1), "native"),
                 h=NA,
                 gp = gpar(col=col2light, lineend = "square",
                 linejoin = "round"))
      grid.segments(x0=unit(c(0, 0), "native"),y0=unit(sig, "native"),
                    x1=unit(c(12, 12), "native"),y1=unit(sig, "native"),
                    gp=gpar(col=col2dark, lty="dashed",lwd=2))
      grid.segments(x0=unit(c(0, 6), "native"),y0=unit(c(0, -1), "native"),
                    x1=unit(c(12, 6), "native"),y1=unit(c(0, 1), "native"),
                    gp=gpar(col="black", lty="solid",lwd=1))
      grid.segments(x0=1:11,y0=0,x1=1:11,y1=ccf.period,
                    default.units="native",
                    gp=gpar(lwd=2,lend="butt", col=col2dark))
      grid.points(x=1:11, y=ccf.period, pch=21,
                  default.units="native",
                  gp=gpar(fill=col2light, col=col2dark,
                  fontsize=pointsize))
      grid.draw(negText)
      grid.draw(posText)
      popViewport(2) # back to bnd
  }

  periodPattern <- gettext("Period: %d-%d", domain = "R-dplR")
  agreePattern <- gettext("Skeleton Agreement %s%%", domain = "R-dplR")

  grid.segments(x0=0.5,y0=0.05,x1=0.5,y1=0.95,
                default.units="npc",
                gp=gpar(lwd=2,lend="butt", col="black"))
  pushViewport(text.bnd.vp) # description
  tmp.txt <- substitute(period * ", " * r[lag0] == corr,
                        list(period = sprintf(periodPattern,
                             min(first.yrs), max(first.yrs)),
                             corr = early.r))
  
  grid.text(tmp.txt,y=unit(0.65,"npc"),x=unit(0.25,"npc"),
            just = textJust)

  grid.text(sprintf(agreePattern, early.agree),
            y=unit(0.35,"npc"), x=unit(0.25,"npc"),
            just = textJust)


  tmp.txt <- substitute(period * ", " * r[lag0] == corr,
                        list(period = sprintf(periodPattern,
                             min(second.yrs), max(second.yrs)),
                             corr = late.r))
  grid.text(tmp.txt,y=unit(0.65,"npc"),x=unit(0.75,"npc"),
            just = textJust)

  grid.text(sprintf(agreePattern, late.agree),
            y=unit(0.35,"npc"), x=unit(0.75,"npc"),
            just = textJust)

  popViewport(1) # back to bnd

  pushViewport(overall.txt.vp) # description
  tmp.txt <- substitute(period * ", " * r[lag0] == corr * ", " * agree,
                        list(period = sprintf(periodPattern,
                             min(yrs), max(yrs)),
                             corr = overall.r,
                             agree = sprintf(agreePattern, overall.agree)))
  grid.text(tmp.txt,y=unit(0.5,"npc"),x=unit(0.5,"npc"),
            just = textJust)
  
  
  popViewport(2)
  series.x.txt <- ifelse(series.x,
                         "NB: With series.x = TRUE, postive lags indicate missing rings in series",
                         "NB: With series.x = FALSE (default), negative lags indicate missing rings in series")
  grid.text(series.x.txt,y=unit(0.015,"npc"),x=unit(0.5,"npc"),
            just = textJust)
  
}
