`gini.coef` <-
function(x)
{
  x=x[!is.na(x)]
  n = length(x)
  n.seq = 1:n/n
  x = x[order(x)]
  y = cumsum(n.seq[1]*x)
  y = y/y[n]
  sum(y[-1]*n.seq[-n])-sum(y[-n]*n.seq[-1])
}
