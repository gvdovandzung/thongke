prevalence <-
function (disease, population, precision, alpha = 0.05, ...) 
{
    if ((length(disease)>1) & (length(population)==1)) {population<-rep(population,length(disease))}
    reverse <- rep(FALSE, length(disease))
    reverse[disease/population > 0.5] <- TRUE
    disease[reverse] <- population[reverse] - disease[reverse]
    if (missing(precision)) {
        precision <- disease/population/10000
    }
    precision[disease == 0 | disease == population] <- 0.01/population[disease == 
        0 | disease == population]
    probab <- disease/population
    disease1 <- disease
    disease1[disease > 0] <- disease[disease > 0] - 1
    for (i in 1:length(disease)) {
        while (pbinom(disease1[i], population[i], probab[i], lower.tail = FALSE) > 
            alpha/2) {
            probab[i] <- probab[i] - precision[i]
        }
    }
    estimate <- disease/population
    se <- sqrt(estimate * (1 - estimate)/population)
    ll <- probab
    probab <- disease/population
    for (i in 1:length(disease)) {
        while (pbinom(disease[i], population[i], probab[i], lower.tail = TRUE) >  alpha/2) {
            probab[i] <- probab[i] + precision[i]
        }
    }
    ul <- probab
    data.frame.a <- data.frame(disease = disease, population = population, 
        prevalence = estimate, se = se, ll = ll, ul = ul)
    data.frame.a[reverse, ] <- data.frame(disease = population[reverse] - 
        disease[reverse], population = population[reverse], prevalence = 1 - 
        estimate[reverse], se = se[reverse], ll = 1 - ul[reverse], 
        ul = 1 - ll[reverse])
    names(data.frame.a)[5] <- paste("exact.lower", 100 * (1 - 
        alpha), "ci", sep = "")
    names(data.frame.a)[6] <- paste("exact.upper", 100 * (1 - 
        alpha), "ci", sep = "")
    if (nrow(data.frame.a) == 1) {
        rownames(data.frame.a) <- ""
    }
    data.frame.a
}

