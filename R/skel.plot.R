`skel.plot` <-
function(rw.vec, yr.vec = NULL, sname = '', filt.weight = 9, dat.out = FALSE,
  master=FALSE,plot=TRUE)
{
  if(nchar(sname) > 7) stop('sname must be a character vector less than 8 characters long')

  # what about NA. Internal NA?
  na.mask <- is.na(rw.vec)
  rw.vec = rw.vec[!na.mask]
  yr.vec = yr.vec[!na.mask]

  if(length(rw.vec) > 840) {
    cat('Input series has length of', length(rw.vec), '\n')
    stop('Long series should be split into multiple plots')
  }

  # should wrap this into a function called skel.calc that returns the
  # dates and skel

  # if no yr then....
  if(is.null(yr.vec)) {
    yr.vec=1:length(rw.vec) - 1
  }
  # pad down to the nearst 10 if not already there
  pad0 = floor(min(yr.vec)/10)*10
  if(pad0 != min(yr.vec)){
    pad.length = min(yr.vec) - pad0
    rw.df = data.frame(rw = rep(NA,pad.length), yr = pad0:(pad0+pad.length-1))
    rw.df = rbind(rw.df,data.frame(rw = rw.vec, yr = yr.vec))
  }
  else {
    pad.length = 0
    rw.df = data.frame(rw = rw.vec, yr = yr.vec)
  }

  # detrend and pad
  rw.dt=hanning(rw.df$rw,filt.weight)
  skel.tmp=rep(NA,length(rw.df$rw))
  # calc rel growth
  for(i in 2:(length(rw.df$rw)-1)) {
    bck=(rw.df$rw[i]-rw.df$rw[i-1])/rw.dt[i]
    fwd=(rw.df$rw[i]-rw.df$rw[i+1])/rw.dt[i]
    skel.tmp[i]=mean(c(bck,fwd))
  }
  skel.tmp[skel.tmp > 0]=NA
  # rescale from 0 to 10
  skel.range=range(skel.tmp[!is.na(skel.tmp)])
  newrange=c(10,1)
  mult.scalar=(newrange[2] - newrange[1])/(skel.range[2] - skel.range[1])
  skel=newrange[1] + (skel.tmp - skel.range[1]) * mult.scalar
  skel[skel < 3]=NA
  skel=ceiling(skel)


  # Variables for plotting
  # page width
  pw = 254
  # page height
  ph = 178
  # row height
  rh = 22
  # row width
  rw = 240
  # spacer for text and dashed cutting lines
  spcr = 5

  # break series into sections of 120 years with an index
  yrs.col = rw/2 # n years per row
  n = length(skel)
  m = 1:ceiling(n/yrs.col)
  row.index = rep(m,each = yrs.col)[1:n]
  n.rows = max(m)
  # bind row.index to data
  skel.df = data.frame(yr=rw.df$yr, skel, row.index)
  if(plot){
    #master page
    grid.newpage()
    tree = vpTree(viewport(width=unit(pw,"mm"), height=unit(ph,"mm"), name="page"),
                   vpList(viewport(x=unit(3,"mm"), y=unit(ph-(rh*1)-(spcr*1),"mm"),
                                           width=unit(246,"mm"), height=unit(rh,"mm"),
                                           just=c("left", "bottom"), name="A"),
                          viewport(x=unit(3,"mm"), y=unit(ph-(rh*2)-(spcr*2),"mm"),
                                           width=unit(246,"mm"), height=unit(rh,"mm"),
                                           just=c("left", "bottom"), name="B"),
                          viewport(x=unit(3,"mm"), y=unit(ph-(rh*3)-(spcr*3),"mm"),
                                           width=unit(246,"mm"), height=unit(rh,"mm"),
                                           just=c("left", "bottom"), name="C"),
                          viewport(x=unit(3,"mm"), y=unit(ph-(rh*4)-(spcr*4),"mm"),
                                           width=unit(246,"mm"), height=unit(rh,"mm"),
                                           just=c("left", "bottom"), name="D"),
                          viewport(x=unit(3,"mm"), y=unit(ph-(rh*5)-(spcr*5),"mm"),
                                           width=unit(246,"mm"), height=unit(rh,"mm"),
                                           just=c("left", "bottom"), name="E"),
                          viewport(x=unit(3,"mm"), y=unit(ph-(rh*6)-(spcr*6),"mm"),
                                           width=unit(246,"mm"), height=unit(rh,"mm"),
                                           just=c("left", "bottom"), name="F"),
                          viewport(x=unit(3,"mm"), y=unit(ph-(rh*7)-(spcr*7),"mm"),
                                           width=unit(246,"mm"), height=unit(rh,"mm"),
                                           just=c("left", "bottom"), name="G")
                                  ))

    # set up page with the right number of rows
    pushViewport(tree)
    for (i in 1:n.rows) {

      seekViewport(LETTERS[i])
      # working code goes here - e.g., skelplot!
      # seq for 0 to plot width by 2mm
      grid.segments(x0=unit(seq(0,rw,by=2),'mm'),y0=unit(0,'mm'),
                    x1=unit(seq(0,rw,by=2),'mm'),y1=unit(rh,'mm'),
                    gp = gpar(col='green', lineend = 'square', linejoin = 'round'))
      grid.segments(x0=unit(0,'mm'),y0=unit(seq(0,rh,by=2),'mm'),
                    x1=unit(rw,'mm'),y1=unit(seq(0,rh,by=2),'mm'),
                    gp = gpar(col='green', lineend = 'square', linejoin = 'round'))

      # decadal lines
      grid.segments(x0=unit(seq(0,rw,by=20),'mm'),y0=unit(0,'mm'),
                    x1=unit(seq(0,rw,by=20),'mm'),y1=unit(rh,'mm'),
                    gp = gpar(col = 'black',lwd = 1.5, lty = 'dashed',
                    lineend = 'square', linejoin = 'round'))

      # lines on top and bottom of plot
      grid.lines(x=unit(c(0,rw),'mm'),
                 y=unit(c(rh,rh),'mm'),
                 gp=gpar(lwd = 2, lineend = 'square', linejoin = 'round'))
      grid.lines(x=unit(c(0,rw),'mm'),
                 y=unit(c(0,0),'mm'),
                 gp=gpar(lwd = 2, lineend = 'square', linejoin = 'round'))
      # plot x axis
      # get this row's data
      skel.sub = skel.df[skel.df$row.index == i,1:2]
      end.yr = length(skel.sub$yr)
      ticks = seq(0,rw/2,by=10)
      init.lab = min(skel.sub$yr)
      x.labs = seq(init.lab, length.out = length(ticks), by=10)
      for(j in 1:length(ticks)){
        if(!master){
          grid.text(label = x.labs[j],
                    x=unit(ticks[j]*2,'mm'),
                    y=unit(rh+0.5,'mm'),
                    just = c('center','bottom'),
                    gp = gpar(fontsize=10))
        }
        else {
          grid.text(label = x.labs[j],
                    x=unit(ticks[j]*2,'mm'),
                    y=unit(rh-22.5,'mm'),
                    just = c('center','top'),
                    gp = gpar(fontsize=10))
        }

      }
      # plot data
      for(j in 1:length(skel.sub$yr)){
        if(!is.na(skel.sub$skel[j])){
          if(!master){
            grid.lines(x=unit(c((j-1)*2,(j-1)*2),'mm'),
              y=unit(c(0,skel.sub$skel[j]*2),'mm'),
              gp = gpar(col = 'black',lwd = 2, lineend = 'square',
              linejoin = 'round'))
            }
          else{
            grid.lines(x=unit(c((j-1)*2,(j-1)*2),'mm'),
              y=unit(c(22,22-skel.sub$skel[j]*2),'mm'),
              gp = gpar(col = 'black',lwd = 2, lineend = 'square',
              linejoin = 'round'))
            }

        }
        # end arrow
        if(i == max(n.rows) & j == end.yr){
          end.mm = (j-1)*2
          grid.lines(x=unit(c(end.mm,end.mm),'mm'),y=unit(c(rh,0),'mm'),
                     gp = gpar(lwd = 2, lineend = 'square', linejoin = 'round'))
          if(!master){
            grid.polygon(x=unit(c(end.mm,end.mm,end.mm+2), 'mm'),
                         y=unit(c(0, 6, 6), 'mm'),
                         gp=gpar(fill = 'black', lineend = 'square', linejoin = 'round'))
          }
          else{
            grid.polygon(x=unit(c(end.mm,end.mm,end.mm+2), 'mm'),
                         y=unit(c(rh, 16, 16), 'mm'),
                         gp=gpar(fill = 'black', lineend = 'square', linejoin = 'round'))
          }
        }
      }
      # start arrow and sample id
      if(i == 1){
        start.mm = pad.length*2
        grid.lines(x=unit(c(start.mm,start.mm),'mm'),y=unit(c(rh,0),'mm'),
                   gp = gpar(lwd = 2, lineend = 'square', linejoin = 'round'))
        if(!master){
          grid.polygon(x=unit(c(start.mm, start.mm, start.mm-2), 'mm'),
                       y=unit(c(0, 6, 6), 'mm'),
                       gp=gpar(fill = 'black', lineend = 'square', linejoin = 'round'))
          # sample id
          fontsize.sname = ifelse(nchar(sname) > 6, 9, 10)
          grid.text(label = sname,
                    x=unit(start.mm-1,'mm'),
                    y=unit(rh-1,'mm'),
                    just = c('right','bottom'),
                    rot = 90,
                    gp = gpar(fontsize=fontsize.sname))
        }
        else{
          grid.polygon(x=unit(c(start.mm, start.mm, start.mm-2), 'mm'),
                       y=unit(c(rh, 16, 16), 'mm'),
                       gp=gpar(fill = 'black', lineend = 'square', linejoin = 'round'))
          # sample id
          fontsize.sname = ifelse(nchar(sname) > 6, 9, 10)
          grid.text(label = sname,
                    x=unit(start.mm-1,'mm'),
                    y=unit(1,'mm'),
                    just = c('left','bottom'),
                    rot = 90,
                    gp = gpar(fontsize=fontsize.sname))
        }

      }

    }
  }
  if(dat.out) return(skel.df[,-3])
}

