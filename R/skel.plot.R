`skel.plot` <-
function(y, sname = '')
{
  if(nchar(sname) > 7) stop('sname must be a character vector less than 8 characters long')

  # should wrap this into a function called skel.calc that returns the
  # dates and z_rel
  y=y[!is.na(y)]
  yr.vec=1:length(y)

  if(!is.null(names(y))) yr.vec=as.numeric(names(y))
  y.dt=hanning(y,9)

  z=rep(NA,length(y))
  for(i in 2:(length(y)-1)) {
    bck_ration=(y[i]-y[i-1])/y.dt[i]
    fwd_ration=(y[i]-y[i+1])/y.dt[i]
    z[i]=mean(c(bck_ration,fwd_ration))
  }
  z[z > 0]=NA
  # rescale from 0 to 10
  zrange=range(z[!is.na(z)])
  newrange=c(10,1)
  mfac=(newrange[2] - newrange[1])/(zrange[2] - zrange[1])
  z_rel=newrange[1] + (z - zrange[1]) * mfac
  z_rel[z_rel < 3]=NA
  z_rel=ceiling(z_rel)

  # plot width in mm
  series.length <- length(yr.vec)*2
  plot.width=series.length%/%10*10+10
  plot.height=11*2

  grid.newpage()
  pushViewport(viewport(layout=grid.layout(nrow=1,ncol=1,
    widths=unit(plot.width,"mm"),heights=unit(plot.height,"mm"))))
  pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1))
  # green grid

  # seq for 0 to plot width by 2mm
  grid.segments(x0=unit(seq(0,plot.width,by=2),'mm'),y0=unit(0,'mm'),
                x1=unit(seq(0,plot.width,by=2),'mm'),y1=unit(plot.height,'mm'),
                gp = gpar(col='green', lineend = 'square', linejoin = 'round'))
  grid.segments(x0=unit(0,'mm'),y0=unit(seq(0,plot.height,by=2),'mm'),
                x1=unit(plot.width,'mm'),y1=unit(seq(0,plot.height,by=2),'mm'),
                gp = gpar(col='green', lineend = 'square', linejoin = 'round'))

  # decadal lines
  grid.segments(x0=unit(seq(0,plot.width,by=20),'mm'),y0=unit(0,'mm'),
                x1=unit(seq(0,plot.width,by=20),'mm'),y1=unit(plot.height,'mm'),
                gp = gpar(col = 'black',lwd = 1.5, lty = 'dashed',
                lineend = 'square', linejoin = 'round'))

  # start and finish arrows
  grid.lines(x=unit(c(0,0),'mm'),y=unit(c(plot.height,0),'mm'),
             gp = gpar(lwd = 2, lineend = 'square', linejoin = 'round'))
  grid.polygon(x=unit(c(0, 0, -2), 'mm'),
               y=unit(c(0, 6, 6), 'mm'),
               gp=gpar(fill = 'black', lineend = 'square', linejoin = 'round'))
  grid.lines(x=unit(c(series.length,series.length),'mm'),y=unit(c(plot.height,0),'mm'),
             gp = gpar(lwd = 2, lineend = 'square', linejoin = 'round'))
  grid.polygon(x=unit(c(series.length, series.length, series.length+2), 'mm'),
               y=unit(c(0, 6, 6), 'mm'),
               gp=gpar(fill = 'black', lineend = 'square', linejoin = 'round'))

  # lines on top and bottom of plot
  grid.lines(x=unit(c(0,plot.width),'mm'),
             y=unit(c(plot.height,plot.height),'mm'),
             gp=gpar(lwd = 2, lineend = 'square', linejoin = 'round'))
  grid.lines(x=unit(c(0,plot.width),'mm'),
             y=unit(c(0,0),'mm'),
             gp=gpar(lwd = 2, lineend = 'square', linejoin = 'round'))
  # plot x axis
  ticks <- seq(0,plot.width/2,by=10)
  for(i in 1:length(ticks)){
    grid.text(label = as.character(ticks[i]),
              x=unit(ticks[i]*2,'mm'),
              y=unit(plot.height+1,'mm'),
              just = c('center','bottom'),
              gp = gpar(fontsize=10))
  }
  # plot sample id
  grid.text(label = sname,
            x=unit(-1,'mm'),
            y=unit(plot.height,'mm'),
            just = c('right','bottom'),
            rot = 90,
            gp = gpar(fontsize=10))

  # plot data
  for(i in 1:length(yr.vec)){
    if(!is.na(z_rel[i])){
      grid.lines(x=unit(c((i-1)*2,(i-1)*2),'mm'),y=unit(c(0,z_rel[i]*2),'mm'),
        gp = gpar(col = 'black',lwd = 2, lineend = 'square',
        linejoin = 'round'))
    }
  }
}

