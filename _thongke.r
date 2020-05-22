require(Hmisc)
PackageName="thongke"

writedescription  <- function() {
fileConn<-file(paste0("F:/dung/Rtemp/",PackageName,"/Description"))
writeLines(c(
paste("Package:",PackageName),
      "Type:    Package",
      "Title:   What the package does (short line)",
paste("Version:",version),
paste("Date:   ",Sys.Date()),
      "Author:  Do Van Dzung",
      "Maintainer: Who to complain to <dovandzung@gmail.com>",
      "Description:More about what it does (maybe more than one line)",
      "License:    What license is it under?",
      "LazyLoad:   yes"),fileConn)
close(fileConn)
}

desFolder<-"F:\\dung\\Rtemp\\"
desPath<-paste0(desFolder,PackageName)
version<-paste0(R.Version()$major,".",paste0(R.Version()$minor))
sourceFolder<-paste0("E:/CDMedStat/R package/R package ",version,"/")
sourcePath<-paste0(sourceFolder,PackageName)
# setwd(paste0(sourceFolder,PackageName,"/R"))
# filelist<-dir()

setwd(paste0(sourceFolder,PackageName,"/R"))
dirlist<-list.dirs(path = ".", full.names = TRUE, recursive = FALSE)

filelist<-list.files(pattern='.\\.r', recursive=TRUE,ignore.case=T)
skeleton1<-sub(".r","",filelist,fixed=T)
skeleton1<-sub(".R","",skeleton1,fixed=T)

for (file1 in filelist){
cat("\n",file1)
source(file1)
}
package.skeleton(list=skeleton1,name=PackageName, path = desFolder, force = TRUE)
setwd(sourceFolder)
file.copy(from=paste0(sourceFolder,"/",PackageName,"/man"), to=desPath, 
		overwrite = TRUE, recursive = TRUE,
          copy.mode = TRUE)
writedescription()
cmd1<-paste0("\"C:\\Program Files\\R\\R-",version,"\\bin\\i386\\Rcmd\" INSTALL --build ",desPath)
system(cmd1)
# writeClipboard(paste0("\"C:\\Program Files\\R\\R-",version,"\\bin\\i386\\Rcmd\" INSTALL --build ",desPath))
# "C:\Program Files\R\R-3.0.1\bin\i386\Rcmd" INSTALL --build  d:\dung\Rtemp\thongke
# package duoc tao ra o C:\Users\Dzung

# file.copy(from=paste0(desFolder,PackageName,"_",version,".zip"), to=sourceFolder, overwrite = TRUE, recursive = FALSE, copy.mode = TRUE)



p1<-match(paste0("package:",PackageName),search())
if (!is.na(p1)) {detach(pos=p1, unload=TRUE)}
install.packages(paste0(sourceFolder,PackageName,"_",version,".zip"))




