
# Formulas for Z scores
zscore <- function (d) {
  return ((d-mean(d))/sd(d))
  }

# Read in data
d <- read.csv("Cleaned-20131016-1525.csv")

# The parties we want to use
parties<-colnames(d)[c(6,8,9,10,11,12,13,14,15,16)]

# Create the "Other party column"

parties<-c(parties,"Sonstige")

# The parties in Other Parties
sonstige<-colnames(d)[c(17:21)]

# Calculate votes for "Other Parties" - trouble due to NA's in the data...
d[["Sonstige"]]<-Reduce(function(x,y) { i=d[[y]]; 
  i[is.na(i)]<-0 ; 
  return (x+i) }, sonstige, 0);

# Normalize votes using Z-Scores of the percentage of votes 
normalized<-sapply(parties,function(x) { return (zscore(d[[x]]/d[["Abgegebene"]])) })

# This is where linear algebra magic happens:
# Perform Multidimensional Scaling to get 2 dimensional coordinates
c<-cmdscale(dist(normalized))

# Build the return object...
r<-d[,c(1:16,23)]

# Add the coordinates in two columns...
r[["x"]]<-c[,1]
r[["y"]]<-c[,2]

# Save the file!
write.csv(r,"mds.csv")
