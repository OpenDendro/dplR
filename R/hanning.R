`hanning` <-
    function (x, n=7)
{
    j <- 0:(n - 1)
    win <- 1 - cos(2 * pi / (n - 1) * j)
    win <- win / sum(win)
    as.vector(filter(x, win))
}
