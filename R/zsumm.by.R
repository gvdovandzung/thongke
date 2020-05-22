zsumm.by <-
function (data = .data, var = "", by = "")
{
		if (var=="") {var<-setdiff(names(data),by)}
    t1 <- tapply(data[, var], data[, by], FUN = zsumm)
    for (i in 1:length(t1)) {
        ti <- t1[i]
        ti <- data.frame(names(ti), ti)
        ti[,2]<-var
        names(ti) <- c(by, "Variable",  "mean","sd", "N", "ci", "median.iqr", "minmax")
        if (i == 1) t2 <- ti else t2 <- rbind(t2, ti)
    }
    class(t2) <- c("z", "data.frame")
    return(t2)
}

