`write.crn` <-
function(crn, fname, header=NULL, append=FALSE)
{
  if(ncol(crn) != 2) stop("input should only have 2 cols")

  if(any(is.na(crn))) {
    crn[is.na(crn[,1]),2] = 0
    crn[is.na(crn[,1]),1] = 9.99
    print(head(crn))
  }

  if(append) {
    if(!file.exists(fname)) stop("fname does not exist, can\'t append")
    if(length(header)>0) stop("bad idea to append with header")
  }
  if(length(header)>0){
    if(!is.list(header)) stop("header must be a list")
    header.names = c("site.id","site.name","spp.code","state.country","spp",
      "elev","lat","long","first.yr","last.yr","lead.invs","comp.date")
    if(!all(header.names %in% names(header))) {
      stop(paste("header must be a list with names",
      paste(header.names,collapse = ", ")))
    }
    # Record #1: 1-6 Site ID, 10-61 Site Name, 62-65 Species Code, optional ID#s
    # Record #2: 1-6 Site ID, 10-22 State/Country, 23-40 Species, 41-45
    # Elevation, 48-57 Lat-Long, 68-76 1st & last Year
    # Note: lat-lons are in degrees and minutes, ddmm or dddmm
    # Record #3: 1-6 Site ID, 10-72 Lead Investigator, 73-80 comp. date
    header = lapply(header,as.character)
    site.id = header$site.id[1]
    site.name = header$site.name[1]
    spp.code = header$spp.code[1]
    state.country = header$state.country[1]
    spp = header$spp[1]
    elev = header$elev[1]
    lat = header$lat[1]
    long = header$long[1]
    first.yr = header$first.yr[1]
    last.yr = header$last.yr[1]
    lead.invs = header$lead.invs[1]
    comp.date = header$comp.date[1]

    site.id = ifelse(nchar(site.id) > 6, substr(site.id, 1, 6),site.id)
    site.id = ifelse(nchar(site.id) < 6, encodeString(site.id, width = 6),site.id)
    site.name = ifelse(nchar(site.name) > 52, substr(site.name, 1, 52),site.name)
    site.name = ifelse(nchar(site.name) < 52, encodeString(site.name, width = 52),site.name)
    spp.code = ifelse(nchar(spp.code) > 4, substr(spp.code, 1, 4),spp.code)
    spp.code = ifelse(nchar(spp.code) < 4, encodeString(spp.code, width = 4),spp.code)
    state.country = ifelse(nchar(state.country) > 13, substr(state.country, 1, 13),
      state.country)
    state.country = ifelse(nchar(state.country) < 13, encodeString(state.country, width = 13),state.country)
    spp = ifelse(nchar(spp) > 18, substr(spp, 1, 18),spp)
    spp = ifelse(nchar(spp) < 18, encodeString(spp, width = 18),spp)
    elev = ifelse(nchar(elev) > 5, substr(elev, 1, 5),elev)
    elev = ifelse(nchar(elev) < 5, encodeString(elev, width = 5),elev)
    lat.long = ifelse(nchar(long) > 5, paste(lat,long,sep=""),
     paste(lat,long,sep=" "))
    lat.long = ifelse(nchar(lat.long) > 10, substr(lat.long, 1, 10),lat.long)
    lat.long = ifelse(nchar(lat.long) < 10, encodeString(lat.long, width = 10),lat.long)
    yrs = paste(first.yr,last.yr,sep=" ")
    yrs = ifelse(nchar(yrs) > 9, substr(yrs, 1, 9),yrs)
    yrs = ifelse(nchar(yrs) < 9, encodeString(yrs, width = 9),yrs)
    lead.invs = ifelse(nchar(lead.invs) > 63, substr(lead.invs, 1, 63),lead.invs)
    lead.invs = ifelse(nchar(lead.invs) < 63, encodeString(lead.invs, width = 63),lead.invs)
    comp.date = ifelse(nchar(comp.date) > 8, substr(comp.date, 1, 8),comp.date)
    comp.date = ifelse(nchar(comp.date) < 8, encodeString(comp.date, width = 8),comp.date)

    hdr1 = paste(site.id, "   ", site.name, spp.code, sep="")
    hdr2 = paste(site.id, "   ", state.country,spp," ",elev,"  ",
      lat.long, "          ", yrs, sep="")
    hdr3 = paste(site.id, "   ", lead.invs, comp.date, sep="")
    hdr = c(hdr1,hdr2,hdr3)
  }

  yrs = as.numeric(rownames(crn))
  min.year = min(yrs)
  max.year = max(yrs)
  span = max.year - min.year + 1
  decades.vec = yrs%/%10 * 10
  decades = unique(decades.vec)
  n.decades = length(decades)
  # 1-6
  crn.name = colnames(crn)[1]
  crn.width = nchar(crn.name)
  ## If crn.width > 6, truncate
  if(crn.width > 6)
    crn.name = substr(crn.name, 1, 6)
  ## Pad to nchar 6 (no leading zero)
  else if(crn.width < 6)
    crn.name = formatC(crn.name, wid = 6, format = "f")

  dec.str <- character(n.decades)
  for(i in 1:n.decades){
    # 7-10 decade column
    dec = decades[i]
    dec.idx = which(decades.vec%in%dec)
    n.yrs = length(dec.idx)
    dec.yrs = yrs[dec.idx]
    # Pad to nchar 4 (no leading zero)
    dec.yrs = formatC(dec.yrs, dig = 0, wid = 4, format = "f")

    dec.rwi = crn[dec.idx,1]
    # Pad to nchar 4 (no leading zero)
    dec.rwi = round(dec.rwi,3) * 1000
    dec.rwi = formatC(dec.rwi, dig = 0, wid = 4, format = "f")

    # Pad to nchar 3 (no leading zero)
    dec.samp.depth = crn[dec.idx,2]
    dec.samp.depth = formatC(dec.samp.depth, dig = 0, wid = 3, format = "f")
    # Pad front end
    if(i == 1){
      yrs2start <- 10-n.yrs
      if(yrs2start !=0){
        tmp <- paste("9990","  0",sep="")
        if(yrs2start > 1){
          for(k in 2:yrs2start){
            tmp = paste(tmp,"9990","  0",sep="")
          }
        }
        dec.str[i] = paste(crn.name,dec.yrs[1],tmp,dec.rwi[1],dec.samp.depth[1],sep="")
      }
      else {
        dec.str[i] = paste(crn.name,dec.yrs[1],dec.rwi[1],dec.samp.depth[1],sep="")
      }
    }
    else {
      dec.str[i] = paste(crn.name,dec.yrs[1],dec.rwi[1],dec.samp.depth[1],sep="")
    }
    for(j in inc(2,n.yrs)){
      j.rwi = dec.rwi[j]
      j.samp.depth = dec.samp.depth[j]
      dec.str[i] = paste(dec.str[i],j.rwi,j.samp.depth,sep="")
    }
  }
  # Finish last decade with 9990 as NA and 0 as samp depth.
  yrs.left <- 10-n.yrs
  for(k in inc(1,yrs.left)){
    dec.str[i] = paste(dec.str[i],"9990","  0",sep="")
  }
  if(length(header)>0) dec.str = c(hdr,dec.str)
  cat(dec.str , file = fname, sep = "\n", append=append)
}

