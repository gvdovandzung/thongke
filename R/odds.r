odds <-
function (disease, population, conf.level = 0.95) 
{
    N. <- 1 - ((1 - conf.level)/2)
    if ((length(disease)>1) & (length(population)==1)) {population<-rep(population,length(disease))}
	b<-(population-disease)
    logit<-log(disease/b)
	se<-sqrt(1/disease+1/b)
	ul<-exp(logit+qnorm(N.)*se)
	ll<-exp(logit-qnorm(N.)*se)
    data.frame.a <- data.frame(disease = disease, population = population, 
        odds = disease/b, se = se, ll = ll, ul = ul)
    names(data.frame.a)[5] <- paste("exact.lower", 100 * conf.level, "ci", sep = "")
    names(data.frame.a)[6] <- paste("exact.upper", 100 * conf.level, "ci", sep = "")
    if (nrow(data.frame.a) == 1) {
        rownames(data.frame.a) <- ""
	}
data.frame.a
}

