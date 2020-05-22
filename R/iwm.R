iwm<-function (var1, data = .data, value = NULL, id="id") 
{
    if (is.null(attr(data, id))) {
        keyid <- names(data)[1]
    }
    else {
        keyid <- attr(data,id)
    }
    if (length(keyid) == 1) {
        if (is.null(value)) {
            t1 <- (data[is.na(data[, var1]), keyid])
        }
        else {
            t1 <- data[!is.na(data[, var1]) & data[, var1] == 
                value, keyid]
			
        }
        if (length(t1) > 0) {
			# t1<-	t1[sort.list(t1)]
            cat(paste0(var1, ":", Hmisc::label(data[var1]), " : "))
			if (length(t1)<=20) {cat(paste0(t1, collapse = ", "),"\n")} else
			{t1<-t1[1:15];cat(paste(paste0(t1, collapse = ", "), ", and other variables ..."),"\n")}
        }
    }
    else {
        if (is.null(value)) {
            t1 <- (data[is.na(data[, var1]), keyid])
        }
        else {
            t1 <- data[!is.na(data[, var1]) & data[, var1] == 
                value, keyid]
        }
        t1 <- t1[sort.list(t1[, 1]), ]
        if (nrow(t1) > 0) {
            cat(paste0(pkey(t1, mvar = paste0(var1, "-", label(data[var1]))), 
                "\n"))
        }
    }
}
