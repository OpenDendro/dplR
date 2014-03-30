plot.rwl <- function(x, plot.type=c("seg","spag"),...){
  if (!inherits(x, "rwl")) {
    stop('use only with "rwl" objects')
  }
  switch(match.arg(plot.type),
         seg = seg.plot(x,...),
         spag = spag.plot(x,...))
}
