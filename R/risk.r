risk <-
function (disease, population, precision, alpha = 0.05, ...) {
data.frame.a<-prevalence(disease, population, precision, alpha)
names(data.frame.a)[1]<-"#event"
names(data.frame.a)[2]<-"Pop.at.risk"
names(data.frame.a)[3]<-"risk"
data.frame.a
}

