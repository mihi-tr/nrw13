
zscore <- function (d) {
  return ((d-mean(d))/sd(d))
  }

d <- read.csv("Cleaned-20131016-1525.csv")
parties<-colnames(d)[c(6,8,9,10,11,12,13,14,15,16)]
parties<-c(parties,"Sonstige")
sonstige<-colnames(d)[c(17:21)]
d[["Sonstige"]]<-Reduce(function(x,y) { i=d[[y]]; 
  i[is.na(i)]<-0 ; 
  return (x+i) }, sonstige, 0);

normalized<-sapply(parties,function(x) { return (zscore(d[[x]]/d[["Abgegebene"]])) })
c<-cmdscale(dist(normalized))
r<-d[,c(1:16,23)]
r[["x"]]<-c[,1]
r[["y"]]<-c[,2]
write.csv(r,"mds.csv")
