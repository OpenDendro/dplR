plot.rwl <- function(rwl,type=c("seg","spag"),...){
  switch(match.arg(type),
         seg = seg.plot(rwl,...),
         spag = spag.plot(rwl,...))
}
