
tab.tab <- function(tab,rev = c("rows", "columns")){
            reverse <- match.arg(rev)
switch(reverse,
	rows=rownames(tab),
	columns=colnames(tab))
}
