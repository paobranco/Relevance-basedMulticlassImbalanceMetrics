source("MetricsImpl.R")
library(igraph)

##################################################################################################
## case1
##################################################################################################
beta <- 1
rel <-c(1,0.9,0.1)
graphP <-c("C", "A", "C", "B")
graphT <- c("C", "B", "B", "A")

mat1 <- matrix(c(5,0,0,0,10,0,0,300,0), ncol=3, nrow=3, byrow=TRUE)
rownames(mat1) <- colnames(mat1) <- c("A", "B", "C")
res1 <- runMeasures(beta, mat1, rel, graphP, graphT)

for(i in 1:33){
  cat(met.names[i],"&", round(res1[i], 3), " \\\\ \n" )
}



##################################################################################################
## case2
##################################################################################################
beta <- 1
rel <-c(1,0.2,0.1)

graphP <-c("C", "A", "B", "A")
graphT <- c("C", "B", "B", "A")

mat2 <- matrix(c(1,0,3,0,100,0,0,0,200), ncol=3, nrow=3, byrow=TRUE)
rownames(mat2) <- colnames(mat2) <- c("A", "B", "C")
res2 <- runMeasures(beta, mat2, rel, graphP, graphT)

for(i in 1:33){
  cat(met.names[i],"&", round(res2[i], 3), " \\\\ \n" )
}




##################################################################################################
## case3 changed for having near 300 examples
##################################################################################################
beta <- 1
rel <-c(1,0.9,0.2, 0.1)

graphP <- c("D", "A", "C", "A", "D", "B")
graphT <- c("D", "C", "C","B", "B", "A")

mat3 <- matrix(c(1,3,0,0,9,1,0,0,0,0,100,0,0,0,0,200), ncol=4, nrow=4, byrow=TRUE)
rownames(mat3) <- colnames(mat3) <- c("A", "B", "C", "D")
res3 <- runMeasures(beta, mat3, rel, graphP, graphT)

for(i in 1:33){
  cat(met.names[i],"&", round(res3[i], 3), " \\\\ \n" )
}


# all cases raw results in a table
# for(i in 1:33){
#   cat(met.names[i],"&", round(res1[i], 3)*100,"&", round(res1[i], 3),"&", round(res2[i], 3)*100,"&", round(res2[i], 3),"&", round(res3[i], 3)*100,"&", round(res3[i], 3), " \\\\ \n" )
# }
