cii <-
function(n, a,  conf.level=0.95, ci=T, verbose=F) {
        N. <- 1 - ((1 - conf.level)/2)
        b <- n - a
        p <- a/n
        a. <- ifelse(a == 0, a + 1, a)
        b. <- ifelse(b == 0, b + 1, b)
        low <- a./(a. + (b. + 1) * (1/qf(1 - N., 2 * a., 2 * 
            b. + 2)))
        up <- (a. + 1)/(a. + 1 + b./(1/qf(1 - N., 2 * b., 2 * 
            a. + 2)))
        low <- ifelse(a == 0, 0, low)
        up <- ifelse(a == n, 1, up)
		if (ci) {
        rval <- ifelse(verbose, paste(format(a/n,digits=3),"(", format(low,digits=3),"-",format(up,digits=3),")",sep=""),
					paste(format(low,digits=3),"-",format(up,digits=3),sep="")) 
				} else { rval <- format(a/n,digits=3)}
        rval
    }

