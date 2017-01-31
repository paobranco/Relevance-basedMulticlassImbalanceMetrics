source("MetricsImpl.R")
library(igraph)
###############################################################################
## multi-minority scenario
###############################################################################
# minority classes dim: {2,3} and {4,5}
# majority classes dim: {15,16} and {17,18}

beta <- 1

c1 <- rep(c(2,3), 4)
c2 <- rep(c(4,5,5,4), 2)
m.min <- cbind(c1, c2, c3=c(rep(15,4), rep(16,4)))

rel.m.min <- c(1, 0.8, 0.1)


graphP <- c("C", "A", "C", "B")
graphT <- c("C", "B", "B", "A")

## generate all possible matrices for a given problem
ListRes <- list()

for (prob in 1:8){ # prob is the problem nr in m.min
  p <- m.min[prob,]
  rel <- rel.m.min

  l1 <-GetPosLines(p[1],0,length(p)) # all possible distributions of class1
  l2 <-GetPosLines(p[2],0,length(p)) # all possible distributions of class2
  l3 <-GetPosLines(p[3],0,length(p)) # all possible distributions of class3
  
  resA <- matrix(NA, ncol=0,nrow=33)
  rownames(resA) <- met.names
  for(cl1 in 1:nrow(l1)){
    for(cl2 in 1:nrow(l2)){
      for(cl3 in 1:nrow(l3)){
        mat <- matrix(c(l1[cl1,],l2[cl2,],l3[cl3,]), nrow=3, ncol=3, byrow=TRUE)
        colnames(mat) <- rownames(mat) <- c("A", "B", "C")
        resA <-cbind(resA, runMeasures(beta, mat, rel, graphP, graphT))
      }
    }
  }

ListRes[[prob]] <- resA
}


save(ListRes, file="MMinRes.RData")



###############################################################################
## multi-majority scenario
###############################################################################

beta <- 1

c1 <- rep(c(2,3), 4)
c2 <- rep(c(15,16,16,15), 2)
m.maj <- cbind(c1, c2, c(rep(17,4), rep(18,4)))

rel.m.maj <- c(1, 0.2, 0.1)

graphP <- c("C", "A", "B", "A")
graphT <- c("C", "B", "B", "A")


## generate all possible matrices for a given problem
ListRes <- list()

for (prob in 1:8){ # prob is the problem nr in m.maj
  p <- m.maj[prob,]
  rel <- rel.m.maj
  
  l1 <-GetPosLines(p[1],0,length(p)) # all possible distributions of class1
  l2 <-GetPosLines(p[2],0,length(p)) # all possible distributions of class2
  l3 <-GetPosLines(p[3],0,length(p)) # all possible distributions of class3
  
  resA <- matrix(NA, ncol=0,nrow=33)
  rownames(resA) <- met.names
  for(cl1 in 1:nrow(l1)){
    for(cl2 in 1:nrow(l2)){
      for(cl3 in 1:nrow(l3)){
        mat <- matrix(c(l1[cl1,],l2[cl2,],l3[cl3,]), nrow=3, ncol=3, byrow=TRUE)
        colnames(mat) <- rownames(mat) <- c("A", "B", "C")
        resA <-cbind(resA, runMeasures(beta, mat, rel, graphP, graphT))
      }
    }
  }
  
  ListRes[[prob]] <- resA
}


save(ListRes, file="MMajRes.RData")


###############################################################################
## complete scenario
###############################################################################


# 2 problems: {2,3,9,10} {2,3,9,11}
#m.comp <- rbind(c(2,3,9,10), c(2,3,9,11))
m.comp <- rbind(c(2,3,9,10))
rel.comp <- c(1,0.9,0.4,0.2)

beta <- 1

graphP <- c("C", "A", "D", "B", "D", "A" )
graphT <- c("D", "C", "C", "B", "B", "A")

# evaluate the problems
ListRes <- list()

for (prob in 1){
  p <- m.comp[prob,]
  rel <- rel.comp
  
  l1 <-GetPosLines(p[1],0,length(p)) # all possible distributions of class1
  l2 <-GetPosLines(p[2],0,length(p)) # all possible distributions of class2
  l3 <-GetPosLines(p[3],0,length(p)) # all possible distributions of class3
  l4 <-GetPosLines(p[4],0,length(p)) # all possible distributions of class3
  
  
  resA1 <- matrix(NA, ncol=0,nrow=33)
  rownames(resA1) <- met.names
  for(cl1 in 1:nrow(l1)){
    for(cl2 in 1:nrow(l2)){
      for(cl3 in 1:nrow(l3)){
        for(cl4 in 1:nrow(l4)){
          mat <- matrix(c(l1[cl1,],l2[cl2,],l3[cl3,],l4[cl4,]), nrow=4, ncol=4, byrow=TRUE)
          colnames(mat) <- rownames(mat) <- c("A", "B", "C", "D")
          resA1 <-cbind(resA1, runMeasures(beta, mat, rel, graphP, graphT))
        }
      }
    }
  }  
}

ListRes[[1]] <- resA1


m.comp <- rbind(c(2,3,9,11))
rel.comp <- c(1,0.9,0.4,0.2)

beta <- 1

graphP <- c("C", "A", "D", "B", "D", "A" )
graphT <- c("D", "C", "C", "B", "B", "A")

# evaluate the problems

for (prob in 1){
  p <- m.comp[prob,]
  rel <- rel.comp
  
  l1 <-GetPosLines(p[1],0,length(p)) # all possible distributions of class1
  l2 <-GetPosLines(p[2],0,length(p)) # all possible distributions of class2
  l3 <-GetPosLines(p[3],0,length(p)) # all possible distributions of class3
  l4 <-GetPosLines(p[4],0,length(p)) # all possible distributions of class3
  
  
  resA1 <- matrix(NA, ncol=0,nrow=33)
  rownames(resA1) <- met.names
  for(cl1 in 1:nrow(l1)){
    for(cl2 in 1:nrow(l2)){
      for(cl3 in 1:nrow(l3)){
        for(cl4 in 1:nrow(l4)){
          mat <- matrix(c(l1[cl1,],l2[cl2,],l3[cl3,],l4[cl4,]), nrow=4, ncol=4, byrow=TRUE)
          colnames(mat) <- rownames(mat) <- c("A", "B", "C", "D")
          resA1 <-cbind(resA1, runMeasures(beta, mat, rel, graphP, graphT))
        }
      }
    }
  }  
}

ListRes[[2]] <- resA1
save(res, file="CompRes.RData")



###############################################################################
## Analysis of results, i.e., count number of different results obtained by metric
###############################################################################

# load("MMinRes.RData")
 
for(i in 1:33){
  cat(rownames(ListRes[[1]])[i], "&")
  for (j in 1:8){ # j nr of problems
    cat(length(unique(ListRes[[j]][i,])), "&")
  }
  cat("\\tabularnewline", "\n")
}

# results in percentage
for(i in 1:33){
  cat(rownames(ListRes[[1]])[i], "&")
  for (j in 1:8){
    cat(round((length(unique(ListRes[[j]][i,]))*100)/length(ListRes[[j]][i,]), 1), "&")
  }
  cat("\\\\", "\n")
}


#analysis in the multi-majority scenario
#setwd("~/Desktop/MetricsPOT/MMaj")
#load("MMajRes.RData")
for(i in 1:33){
  cat(rownames(ListRes[[1]])[i], "&")
  for (j in 1:8){
    cat(length(unique(ListRes[[j]][i,])), "&")
  }
  cat("\\tabularnewline", "\n")
}

# results in percentage
for(i in 1:33){
  cat(rownames(ListRes[[1]])[i], "&")
  for (j in 1:8){
    cat(round((length(unique(ListRes[[j]][i,]))*100)/length(ListRes[[j]][i,]), 1), "&")
  }
  cat("\\\\", "\n")
}

#analysis in the complete scenario

for(i in 1:33){
  cat(rownames(newres)[i], "&")
  cat(length(unique(newres[i,])), "&")
  cat("\\\\", "\n")
}

#resuls in percentage
for(i in 1:33){
  cat(rownames(newres)[i], "&")
  cat(round((length(unique(newres[i,]))*100)/length(newres[i,]),1),"\\\\", "\n")
}

