plotRings <- function(year, trwN, trwS = NA_real_,
                      trwE = NA_real_, trwW = NA_real_,
                      length.unit = "mm",
                      animation = FALSE, 
                      sys.sleep = 0.2, 
                      year.labels = FALSE, 
                      d2pith = NA,
                      col.inrings = "grey", col.outring = "black", 
                      x.rings = "none", col.x.rings = "red",
                      xy.lim = NULL,
                      species.name = NA,
                      saveGIF=FALSE, fname="GIF_plotRings.gif") {
  
  ## Creating a data.frame
  TRW <- data.frame(row.names = year, trwN = trwN, 
                    trwS = if (exists("trwS") == TRUE) 
                      trwS
                    else NA, trwE = if (exists("trwE") == TRUE) 
                      trwE
                    else NA, trwW = if (exists("trwW") == TRUE) 
                      trwW
                    else NA)
  
  ## Setting the length unit of ring measurement
  if(length.unit == "mm") 
    TRW[, 1:4] <- TRW[, 1:4]    
  else if(length.unit == "1/10 mm") 
    TRW[, 1:4] <- TRW[, 1:4]/10 
  else if(length.unit == "1/100 mm") 
    TRW[, 1:4] <- TRW[, 1:4]/100
  else if(length.unit == "1/1000 mm") 
    TRW[, 1:4] <- TRW[, 1:4]/1000 
  
  ## Setting the unit of d2pith
  if(length.unit == "mm") 
    d2pith <- d2pith
  else if(length.unit == "1/10 mm") 
    d2pith <- d2pith/10 
  else if(length.unit == "1/100 mm") 
    d2pith <- d2pith/100
  else if(length.unit == "1/1000 mm") 
    d2pith <- d2pith/1000
  
  
  TRW <- TRW[as.logical((rowSums(is.na(TRW))-length(TRW))),] # It is to remove rows with NAs across all rows
  
  # trw means
  TRW$trw.means <- rowMeans(TRW, na.rm = TRUE)
  
  # Distance to pith (d2pith)
  # Add a new row with the d2pith value at the first position if this argument is assigned
  if (!is.na(mean(d2pith, na.rm = T))) {
    TRW.d2pith <- TRW[1, 1:4]
    TRW.d2pith[1,] <- NA
    rownames(TRW.d2pith)[1] <- as.numeric(rownames(TRW))[1]-1 
    TRW.d2pith$trw.means[1] <-  d2pith 
    TRW <- rbind(TRW.d2pith, TRW)
  }
  
  # Accumulative trw.means
  TRW$trw.acc <- cumsum(TRW$trw.means)
  
  # Eccentricity
  y <- TRW$trwN - TRW$trwS   # eccentricity2
  y[is.na(y)] <- 0
  if(exists("y")==TRUE) TRW$N_S <- y  # add to the TRW data.frame
  x <- TRW$trwE - TRW$trwW   # eccentricity1
  x[is.na(x)] <- 0
  if(exists("x")==TRUE) TRW$E_W <- x  # add to the TRW data.frame
  z <- TRW$trw.acc   # accumulative rings
  
  
  # Getting and coloring the narrow and wider rings
  q2 <- as.numeric(quantile(TRW[,5])[2]) # quantile 25% of trw.means
  col.narrow.rings <- ifelse(TRW[,5] <= q2, col.x.rings, col.inrings) 
  q4 <- as.numeric(quantile(TRW[,5])[4]) # quantile 75% of trw.means
  col.wider.rings <- ifelse(TRW[,5] >= q4, col.x.rings, col.inrings) 
  
  
  
  ## AREA calculation: pi(radius)^2    ||  pi(z)^2
  
  # Acummulative BAI (inside out)
  TRW$bai.acc <- pi*(TRW$trw.acc)^2  
  # Individual BAI (inside out)
  TRW$bai.ind  <-c(TRW$bai.acc[1], TRW$bai.acc[2:nrow(TRW)] - TRW$bai.acc[1:nrow(TRW)-1]) 
  
  # # # # # # # # # # # # # # # # # # # ## # # # # # # # # # # # # # # # # # # #
  # set plotting parameters for all the plots that might follow
  op <- par(no.readonly=TRUE)
  on.exit(par(op), add=TRUE)
  par(mar=c(4,4,4,1)+0.1,xaxs="i",yaxs="i",pty="s",mgp=c(1.5,0.5,0))
  dev.hold()
  on.exit(dev.flush(), add=TRUE)
  ## Plotting  
  if (animation == TRUE) {
    
    # With animation
    for (i in 1:length(x)) {
      # Rings
      cols <-  c(rep(col.inrings, i-1), col.outring) 
      narrow.cols <- c(col.narrow.rings[1:i-1], col.outring) # colors when is selected "narrow.rings"
      wider.cols <- c(col.wider.rings[1:i-1], col.outring) # colors when is selected "wider.rings"
      #auto.lim <- max(z, na.rm = TRUE) * 2.0
      if(is.null(xy.lim)) xy.lim <- max(z, na.rm = TRUE) * 1.05
      
      symbols(y = y[1:i], x = if(length(x) > 0) y[1:i] else x[1:i],
              circles=z[1:i], inches=FALSE, xlim = c(-xy.lim, xy.lim), ylim = c(-xy.lim, xy.lim), 
              xlab='Width [mm]', ylab='Width [mm]', main=mtext(bquote(~bold(.("Annual tree growth"))),
                                                     line=1.5,adj=0.5, side=3, cex=1.5), 
              sub=if(!is.na(species.name)) mtext(bquote(~plain(.("(")) ~italic(.(species.name)) ~plain(.(")"))),
                                                 line=0.5,adj=0.5, side=3, cex=1), 
              fg=  if(x.rings == "narrow.rings") narrow.cols 
              else if(x.rings == "wider.rings") wider.cols 
              else if(x.rings == "none") cols) 
      
      # year labels
      #if(year.labels == TRUE) legend('topright', legend=year[i], bty="n", inset = 0.01, cex=2)
      if(year.labels == TRUE) title(sub=year[i])
      
      Sys.sleep(sys.sleep)
    }
  }  
  
  # Without animation
  else {
    cols <- c(rep(col.inrings, length(x)-1), col.outring)
    narrow.cols <- c(col.narrow.rings[1:length(x)-1], col.outring) # colors when is selected "narrow.rings"
    wider.cols <- c(col.wider.rings[1:length(x)-1], col.outring) # colors when is selected "wider.rings"
    rings.lwd <- c(rep(1, length(x)), 3)
    #auto.lim <- max(z, na.rm = TRUE) * 2.0
    if(is.null(xy.lim)) xy.lim <- max(z, na.rm = TRUE) * 1.05
    
    symbols( y = y, x = if(length(x) > 0) y else x,
             circles=z, inches=FALSE, xlim = c(-xy.lim, xy.lim), ylim = c(-xy.lim, xy.lim), 
             xlab='Width [mm]', ylab='Width [mm]', main=mtext(bquote(~bold(.("Annual tree growth"))), line=1.5,adj=0.5, 
                                                    side=3, cex=1.5), sub= if(!is.na(species.name)) mtext(bquote(~plain(.("(")) ~italic(.(species.name)) ~plain(.(")"))),
                                                                                                          line=0.5,adj=0.5, side=3, cex=1), 
             fg=  if(x.rings == "narrow.rings") narrow.cols 
             else if(x.rings == "wider.rings") wider.cols 
             else if(x.rings == "none") cols)
    
    # year labels
    #if(year.labels == TRUE) legend('topright', legend=paste(range(year)[1], "-", range(year)[2]), bty="n", inset = 0.01, cex=1.2)
    if(year.labels == TRUE) title(sub=paste(range(year)[1], "-", range(year)[2]))
    
  }
  
  # saveGIF
  
  if (saveGIF == TRUE) {
    
    saveGIF({
      ani.options(interval = sys.sleep, nmax = 50, ani.width = 1000, ani.height = 1000)
      
      par(bg="white")
      
      # With animation
      for (i in 1:length(x)) {
        # Rings
        cols <-  c(rep(col.inrings, i-1), col.outring) 
        narrow.cols <- c(col.narrow.rings[1:i-1], col.outring) # colors when is selected "narrow.rings"
        wider.cols <- c(col.wider.rings[1:i-1], col.outring) # colors when is selected "wider.rings"
        #auto.lim <- max(z, na.rm = TRUE) * 2.0
        if(is.null(xy.lim)) xy.lim <- max(z, na.rm = TRUE) * 1.05
        
        symbols(y = y[1:i], x = if(length(x) > 0) y[1:i] else x[1:i],
                circles=z[1:i], inches=FALSE, xlim = c(-xy.lim, xy.lim), ylim = c(-xy.lim, xy.lim), 
                xlab='Width [mm]', ylab='Width [mm]', main=mtext(bquote(~bold(.("Annual tree growth"))),
                                                       line=1.5,adj=0.5, side=3, cex=1.5), 
                sub=if(!is.na(species.name)) mtext(bquote(~plain(.("(")) ~italic(.(species.name)) ~plain(.(")"))),
                                                   line=0.5,adj=0.5, side=3, cex=1), 
                fg=  if(x.rings == "narrow.rings") narrow.cols 
                else if(x.rings == "wider.rings") wider.cols 
                else if(x.rings == "none") cols) 
        
        # year labels
        #if(year.labels == TRUE) legend('topright', legend=year[i], bty="n", inset = 0.01, cex=2)
        if(year.labels == TRUE) title(sub=year[i])
      }
      # AGB changed interval to delay which works under mac. Not sure about windows.
    }, movie.name = fname)
  }
  
  # Without saving the GIF
  else {
    cols <- c(rep(col.inrings, length(x)-1), col.outring)
    narrow.cols <- c(col.narrow.rings[1:length(x)-1], col.outring) # colors when is selected "narrow.rings"
    wider.cols <- c(col.wider.rings[1:length(x)-1], col.outring) # colors when is selected "wider.rings"
    rings.lwd <- c(rep(1, length(x)), 3)
    # auto.lim <- max(z, na.rm = TRUE) * 2.0
    if(is.null(xy.lim)) xy.lim <- max(z, na.rm = TRUE) * 1.05
    
    symbols( y = y, x = if(length(x) > 0) y else x,
             circles=z, inches=FALSE, xlim = c(-xy.lim, xy.lim), ylim = c(-xy.lim, xy.lim), 
             xlab='Width [mm]', ylab='Width [mm]', main=mtext(bquote(~bold(.("Annual tree growth"))), line=1.5,adj=0.5, 
                                                    side=3, cex=1.5), sub= if(!is.na(species.name)) mtext(bquote(~plain(.("(")) ~italic(.(species.name)) ~plain(.(")"))),
                                                                                                          line=0.5,adj=0.5, side=3, cex=1), 
             fg=  if(x.rings == "narrow.rings") narrow.cols 
             else if(x.rings == "wider.rings") wider.cols 
             else if(x.rings == "none") cols)
    
    # year labels
    #if(year.labels == TRUE) legend('topright', legend=paste(range(year)[1], "-", range(year)[2]), bty="n", inset = 0.01, cex=1.2)
    if(year.labels == TRUE) title(sub=paste(range(year)[1], "-", range(year)[2]))
  }
  
  
  ## Print Report:  
  print("Output data:")
  # print(TRW)  # all data.frame
  if  ((sum(TRW$trwN, na.rm = TRUE) > 0) & is.na(d2pith[1]))
    print(paste("Length Radius N:  ", round(sum(TRW$trwN, na.rm = TRUE), digits = 2), sep = " ", "mm"))
  else if  ((sum(TRW$trwN, na.rm = TRUE) > 0) & (d2pith[1] > 0))
    print(paste("Length Radius N:  ", round(sum(TRW$trwN, d2pith, na.rm = TRUE), digits = 2), sep = " ", "mm"))
  
  if  ((sum(TRW$trwS, na.rm = TRUE) > 0) & is.na(d2pith[2]))
    print(paste("Length Radius S:  ", round(sum(TRW$trwS, na.rm = TRUE), digits = 2), sep = " ", "mm"))
  else if  ((sum(TRW$trwS, na.rm = TRUE) > 0) & (d2pith[2] > 0))
    print(paste("Length Radius S:  ", round(sum(TRW$trwS, d2pith, na.rm = TRUE), digits = 2), sep = " ", "mm"))
  
  if  ((sum(TRW$trwE, na.rm = TRUE) > 0) & is.na(d2pith[3]))
    print(paste("Length Radius E:  ", round(sum(TRW$trwE, na.rm = TRUE), digits = 2), sep = " ", "mm"))
  else if  ((sum(TRW$trwE, na.rm = TRUE) > 0) & (d2pith[3] > 0))
    print(paste("Length Radius E:  ", round(sum(TRW$trwE, d2pith, na.rm = TRUE), digits = 2), sep = " ", "mm"))
  
  if  ((sum(TRW$trwW, na.rm = TRUE) > 0) & is.na(d2pith[4]))
    print(paste("Length Radius W:  ", round(sum(TRW$trwW, na.rm = TRUE), digits = 2), sep = " ", "mm"))
  else if  ((sum(TRW$trwW, na.rm = TRUE) > 0) & (d2pith[4] > 0))
    print(paste("Length Radius W:  ", round(sum(TRW$trwW, d2pith, na.rm = TRUE), digits = 2), sep = " ", "mm"))
  
  if (sum(TRW$trw.means, na.rm = TRUE) > 0) 
    print(paste("Length Diameter:  ", round(sum(TRW$trw.means, na.rm = TRUE) * 2/10, digits = 6), sep = " ", "cm"))
  
  if (sum(TRW$bai.ind, na.rm = TRUE) > 0) 
    print(paste("Basal Area of the disc:  ", round(sum(TRW$bai.ind, na.rm = TRUE)/10^6, digits = 6), sep = " ", "m2"))
  
  TRW  
}

