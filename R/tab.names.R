tab.names <-
function(tab,rev = c("neither", "rows", "columns", "both")){
    if ((ncol(tab)>= 2) & (nrow(tab)>=2)) {
        N<-nrow(tab)
            reverse <- match.arg(rev)
            if (reverse == "columns")  tab <- tab[N:1, ]
            if (reverse == "rows")     tab <- tab[, 2:1]
            if (reverse == "neither")  tab <- tab[N:1, 2:1]
            if (reverse == "both")     tab <- tab
    return(colnames(tab))
}
}

