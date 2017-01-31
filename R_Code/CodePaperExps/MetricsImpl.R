# Always consider square confusion matrices as follows:
#             preds
#           c1 c2 c3
#       c1  5   2   0
# trues c2 10  200  1
#       c3  3   5  300
#   

# 
# mat <- matrix(c(5,2,0,10,200,1,3,5, 300), nrow=3, ncol=3, byrow=TRUE)
# colnames(mat) <- c("c1", "c2", "c3")
# rownames(mat) <- c("c1", "c2", "c3")




## Average Accuracy

AvACC <- function(z) {
  corr <- diag(z)
  t <- rowSums(z)
  p <- colSums(z)
  res <- 0
  for(i in 1:length(corr)){
    res <- res+(sum(corr)/(sum(corr)+t[i]+p[i]-2*corr[i]))
  }
  res <- as.numeric(res/length(corr))
  res
  
}


## Macro Average Geometric (geometric average of each class recall)
MAvG <- function(z) {
  corr <- diag(z)
  t <- rowSums(z)
  #corr: correclty classified
  #t: total trues of each class
  res <- 1
  for (i in 1:length(corr)){
    res <- res*corr[i]/t[i]
  }
  res <- as.numeric(res^(1/length(corr)))
  res
}

## Macro Average Arithmetic / Recall macro
MAvA <- function(z) {
  corr <- diag(z)
  t <- rowSums(z)
  res <- 0
  for (i in 1:length(corr)){
    res <- res+corr[i]/t[i]
  }
  res <- as.numeric(res/length(corr))
  res
}

## Precision macro
precM <- function(z) {
  corr <- diag(z)
  p <- colSums(z)  
  res <- 0
  idx <- which(p==0)
  if (length(idx)==length(p)){
    res <-NaN
  } else{
    for(i in setdiff(1:length(p), idx)){
      res <- res+corr[i]/p[i]
    }
  }
  res <- as.numeric(res/length(corr))
  res
}

## Recall micro
recMiu <- function(z) {
  corr <- diag(z)
  t <- rowSums(z)
  res <- sum(corr)/sum(t)
  res
}


## Precision micro
precMiu <- function(z) {
  corr <- diag(z)
  p <- colSums(z)  
  res <- sum(corr)/sum(p)
  res
}

## MFb (Alejo and Ferri 2009 extended for any beta)
MFb <- function(beta, z) {
  corr <- diag(z)
  t <- rowSums(z)
  p <- colSums(z)  
  res <- 0
  for (i in 1:length(corr)){
    res <- res+(((1+beta^2)*corr[i])/(beta^2*t[i]+p[i]))  # robusteness for indefined precision or recall in one class
  }
  res <- as.numeric(res/length(corr))
  res
}


## MFbM (sokolova 2009)
FbM <- function(beta, z) {
  rec <- MAvA(z)
  prec <- precM(z)
  res <- as.numeric(((1+beta^2)*prec*rec)/(beta^2*prec+rec))
  res
}


## MFbMiu (sokolova 2009)
FbMiu <- function(beta, z) {
  rec <- recMiu(z)
  prec <- precMiu(z)
  res <- ((1+beta^2)*prec*rec)/(beta^2*prec+rec)
  res
}


### CBA
cba <- function(z) {
  n <- nrow(z)
  across <- function(u, v, t) {
    if(sum(u) == 0 & sum(v) == 0){ return(0)}
    else{
      t/max(sum(u), sum(v))
    }
  }
  xyacross <- array(NA, c(n, n))
  for (i in 1:n) {
    for (j in 1:n) {
      xyacross[i, j] <- across(z[i, ], z[, j], z[i, j])
    }
  }
  return(mean(diag(xyacross)))
}


MCC <- function(z) {
  n <- nrow(z)
  nom <- 0
  for(k in 1:n){
    for(l in 1:n){
      for(m in 1:n){
        nom <- nom+(z[k,k]*z[m,l]-z[l,k]*z[k,m])
      }
    }
  }
  den1 <- 0
  den2 <- 0
  for(k in 1:n){
    term1 <- 0
    term2 <- 0
    term3 <- 0
    term4 <- 0

    for(l in 1:n){
      term1 <- term1+z[l,k]
      term3 <- term3+z[k,l]
    }
    for(f in setdiff(1:n,k)){
      for(g in 1:n){
        term2 <- term2+z[g,f]
        term4 <- term4+z[f,g]
      }
    }
    den1 <- den1+term1*term2
    den2 <- den2+term3*term4
  }
  res <- nom/(sqrt(den1)*sqrt(den2))
  res
}


RCI <- function(z) {
    total.sum <- sum(z)
    row.sums <- rowSums(z)
    col.sums <- colSums(z)
    
    probs.in <- row.sums / total.sum
    probs.out <- col.sums / total.sum
    H.in <- sum(-probs.in*log(probs.in))
    
    likelihood <- apply(z, 2, function(x) x / sum(x))
    H.likelihood <-
      apply(likelihood, 2, function(x) sum(-x*log(x), na.rm=TRUE))
    
    H.out <- sum(probs.out * H.likelihood)
    
    res <- (H.in - H.out)/H.in
    res    
}



CEN <- function(z) {
  n <- nrow(z)
  row.sums <- rowSums(z)
  col.sums <- colSums(z)
  res <- 0
  for(j in 1:n) {
    P <- (row.sums[j]+col.sums[j])/(2*sum(z))
    res <- res + P*CEN.Class(z, j)
  }
  res <- as.numeric(res)
  res
}

CEN.Class <- function(z, j) {
  n <- nrow(z)
  row.sums <- rowSums(z)
  col.sums <- colSums(z)
  probs <- array(NA, c(n, n)) # probs(i,k) = prob of classifying class i to class k subject to class j.
  for(k in 1:n) {
    if(k != j){
      #p_jk
      probs[j,k] <- z[j,k]/(sum(row.sums[j]+col.sums[j]))
      #p_kj
      probs[k,j] <- z[k,j]/(sum(row.sums[j]+col.sums[j]))
    } else {
      #p_jj
      probs[j,j] <- 0
    }
  }
  
  if(sum(row.sums[j]+col.sums[j]) == 0){
    res <- NA
    return(res)
  } else {
    res <- 0
    for(k in setdiff(1:n, j)){
      l1 <- 0
      l2 <- 0
      if(probs[j,k] != 0) {
        l1 <- probs[j,k]*log(probs[j,k], base=2*(n-1)) 
      }
      if (probs[k,j] != 0) {
        l2 <- probs[k,j]*log(probs[k,j], base=2*(n-1))
      }
      res <- res+l1+l2
    }
  }
  return(-res)
}
###############################################################################
## Weighted measures: the relevance is AUTOMATICALLY determined 
## using the classes frequency
## strategy labeled as PRE in the paper; here appended with W
###############################################################################

wEvt <- function(t){
  # input: t: vector with original frequencies (trues) of each class
  # output: vector with normalized weights for each class
  res1 <- 1/t
  res <- res1/sum(res1)
  res
}

wEvp <- function(p){
  # input: p: vector with predicted frequencies (preds) of each class
  # output: vector with normalized weights for each class
  res1 <- 1/p
  res <- res1/sum(res1)
  res
}

WMRec <- function(z) {
  corr <- diag(z)
  t <- rowSums(z)
  wi <- wEvt(t)
  res <- 0
  for(i in 1:length(t)){
    res <- res+wi[i]*corr[i]/t[i]
  }
  res <- as.numeric(res)
  res
}

WMPrec <- function(z) {
  corr <- diag(z)
  t <- rowSums(z)
  p <- colSums(z)
  wi <- wEvt(t)
  res <- 0
  idx <- which(p==0)
  if (length(idx)==length(p)){
    res <-NaN
  } else{
    k <- sum(wi[setdiff((1:length(p)),idx)])
    wi <- wi/k
    for(i in setdiff(1:length(p), idx)){
      res <- res+wi[i]*corr[i]/p[i]
    }
  }
  res <- as.numeric(res)
  res
}

# weighted Fmeasure with previous calculated weighted rec and prec
WFM <- function(beta, z) {
  prec <-WMPrec(z)
  rec <-WMRec(z)
  res <-as.numeric(((1+beta^2)*prec*rec)/(beta^2*prec+rec))
  res
}

# weighted averaged FM
WAvFM <- function(beta, z) {
  corr <- diag(z)
  t <- rowSums(z)
  p <- colSums(z)
  res <- 0
  wit <- wEvt(t)
  for(i in 1:length(corr)){
    res <- res + (wit[i]*(1+beta^2)*corr[i])/(beta^2*t[i]+p[i]) # robust solution for undefined recall or precision
  }
  res <- as.numeric(res)
  res
}


# cba with phi weighting automatic
Wcba <- function(z){
  tr <- rowSums(z)
  wi <- wEvt(tr)
  n <- nrow(z)
  across <- function(u, v, t) {
    if(sum(u) == 0 & sum(v) == 0){ return(0)}
    else{
      t/max(sum(u), sum(v))
    }
  }
  xyacross <- array(NA, c(n, n))
  for (i in 1:n) {
    for (j in 1:n) {
      xyacross[i, j] <- across(z[i, ], z[, j], z[i, j])
    }
  }
  return(sum(wi*diag(xyacross)))  
}

###############################################################################
## Relevance sensitive measures: the user specifies the relevance of each class
## the user provides the phi values
## strategy labeled as \phi in the paper; here appended with Rel
###############################################################################

# The user must provide a sorted importance vector for the classes
# The importance/relevance ranges between 0 and 1
# example for a three class problem: rel <- c(0.1, 0.8, 1)
RelMRec <- function(z, rel) {
  # rel: relevance of each class
  corr <- diag(z)
  t <- rowSums(z)
  res <- 0
  for(i in 1:length(t)){
    res <- res+rel[i]*corr[i]/t[i]
  }
  res <- as.numeric(res/sum(rel))
  res
}

RelMPrec <- function(z, rel) {
  corr <- diag(z)
  p <- colSums(z)
  res <- 0
  idx <- which(p==0)
  if (length(idx)==length(p)){
    res <-NaN
  } else{
    rel <- rel/sum(rel)
    k <- sum(rel[setdiff((1:length(p)),idx)])
    rel <- rel/k
    for(i in setdiff(1:length(p), idx)){
      res <- res+rel[i]*corr[i]/p[i]
    }
  }
  res <- as.numeric(res)
  res
}

# weighted Fmeasure with previous claculated weighted rec and prec
RelFM <- function(beta, z, rel) {
  prec <-RelMPrec(z, rel)
  rec <-RelMRec(z, rel)
  res <-as.numeric(((1+beta^2)*prec*rec)/(beta^2*prec+rec))
  res
}

# weighted averaged FM
RelAvFM <- function(beta, z, rel) {
  corr <- diag(z)
  t <- rowSums(z)
  p <- colSums(z)
  res <- 0
  for(i in 1: length(corr)){
    res <- res + (rel[i]*(1+ beta^2)*corr[i])/(beta^2*t[i]+p[i]) # robust solution for recall or precision undefined
  }
  res <- as.numeric(res/sum(rel))
  res
}



Relcba <- function(z, rel){
  n <- nrow(z)
  across <- function(u, v, t) {
    if(sum(u) == 0 & sum(v) == 0){ return(0)}
    else{
      t/max(sum(u), sum(v))
    }
  }
  xyacross <- array(NA, c(n, n))
  for (i in 1:n) {
    for (j in 1:n) {
      xyacross[i, j] <- across(z[i, ], z[, j], z[i, j])
    }
  }
  return(sum(rel*diag(xyacross))/sum(rel))
  
}



###############################################################################
## Partial/Total order relevance measures: the user specifies a class order, 
## that can be partial or total, and a minimum threshold
## strategies POS and TOS: here appended with O
## the function GetGraphRel returns the relevance estimation given a partial or total order.
###############################################################################

ORec <- function(z, graph) {
  # check if all problem classes are provided
  if (length(unique(graph)) < length(rownames(z))) {
    stop("Classes are missing in the graph!")
  }
  if (length(unique(graph)) > length(rownames(z))) {
    stop("Provided more classes than those existing in the confusion matrix!")
  }
  rel <- GetGraphRel(z, graph)
  corr <- diag(z)
  t <- rowSums(z)
  res <- 0
  for(i in 1:length(t)){
    res <- res+rel[i]*corr[i]/t[i]
  }
  res <- as.numeric(res/sum(rel))
  res
}

OPrec <- function(z, graph) {
  # check if all problem classes are provided
  if (length(unique(graph)) < length(rownames(z))) {
    stop("Classes are missing in the graph!")
  }
  if (length(unique(graph)) > length(rownames(z))) {
    stop("Provided more classes than those existing in the confusion matrix!")
  }
  rel <- GetGraphRel(z, graph)
  corr <- diag(z)
  p <- colSums(z)
  res <- 0
  idx <- which(p==0)
  if (length(idx)==length(p)){
    res <-NaN
  } else{
    rel <- rel/sum(rel)
    k <- sum(rel[setdiff((1:length(p)),idx)])
    rel <- rel/k
    for(i in setdiff(1:length(p), idx)){
      res <- res+rel[i]*corr[i]/p[i]
    }
  }
  res <- as.numeric(res)
  res
}

# weighted Fmeasure with previous claculated weighted rec and prec
OFM <- function(beta, z, graph) {
  prec <-OPrec(z, graph)
  rec <-ORec(z, graph)
  res <-as.numeric(((1+beta^2)*prec*rec)/(beta^2*prec+rec))
  res
}

# weighted averaged FM
OAvFM <- function(beta, z, graph) {
  # check if all problem classes are provided
  if (length(unique(graph)) < length(rownames(z))) {
    stop("Classes are missing in the graph!")
  }
  if (length(unique(graph)) > length(rownames(z))) {
    stop("Provided more classes than those existing in the confusion matrix!")
  }
  rel <- GetGraphRel(z, graph)
  corr <- diag(z)
  t <- rowSums(z)
  p <- colSums(z)
  res <- 0
  for(i in 1: length(corr)){
    res <- res + (rel[i]*(1+ beta^2)*corr[i])/(beta^2*t[i]+p[i]) # robust solution for recall or precision undefined
  }
  res <- as.numeric(res/sum(rel))
  res
}



Ocba <- function(z, graph){
  # check if all problem classes are provided
  if (length(unique(graph)) < length(rownames(z))) {
    stop("Classes are missing in the graph!")
  }
  if (length(unique(graph)) > length(rownames(z))) {
    stop("Provided more classes than those existing in the confusion matrix!")
  }
  rel <- GetGraphRel(z, graph)
  
  n <- nrow(z)
  across <- function(u, v, t) {
    if(sum(u) == 0 & sum(v) == 0){ return(0)}
    else{
      t/max(sum(u), sum(v))
    }
  }
  xyacross <- array(NA, c(n, n))
  for (i in 1:n) {
    for (j in 1:n) {
      xyacross[i, j] <- across(z[i, ], z[, j], z[i, j])
    }
  }
  return(sum(rel*diag(xyacross))/sum(rel))
  
}


# aux function for relevance estimation given a partial or total order
#graph <- c("A", "B", "A", "C", "C", "D", "D", "E")

GetGraphRel <- function(z, graph){
  # z: confusion matrix
  # graph: a vector with all (directed) edges between two nodes 
  #"A", "B" represents an edge from node A to node B. 
  # All the edges are provided consecutivelly
  # an edge from A to B represents: A < B
  # r a named vector with the classes names and the rankings that should be updated
  
  r <- setNames(rep(NA, nrow(z)), rownames(z))
  g <- make_graph(graph, directed =TRUE)
  d <- distances(g, mode="out")
  tot <- length(r) # total number of nodes
  for(i in names(r)){
    min <- length(which(d[,i]<Inf)) # S+1
    max <- min+length(intersect(names(which(d[i,]==Inf)), names(which(d[,i]==Inf)))) # S+1+U
    r[i] <- mean(c(min, max))
  }
  maxR <- max(r)
  rel <- c()
  for(i in names(r)){
     rel <- c(rel,r[i]/maxR)
  }
  rel
}








###############################################################################
### run all the measures
###############################################################################


runMeasures <- function(beta, z, rel, graphP, graphT) {  
  res <- c(AvACC(z), MAvG(z), MAvA(z), precM(z),
           recMiu(z), precMiu(z), MFb(beta,z), FbM(beta,z), FbMiu(beta, z),
           cba(z), MCC(z), RCI(z), CEN(z), WMRec(z),  WMPrec(z), WFM(beta,z), WAvFM(beta,z), Wcba(z),
           RelMRec(z,rel), RelMPrec(z,rel), RelFM(beta,z,rel),RelAvFM(beta,z,rel), Relcba(z,rel), 
           ORec(z,graphP), OPrec(z,graphP), OFM(beta,z,graphP), OAvFM(beta,z,graphP), Ocba(z,graphP),
           ORec(z,graphT), OPrec(z,graphT), OFM(beta,z,graphT), OAvFM(beta,z,graphT), Ocba(z,graphT))
  
  res
}


## metrics names for help constructing latex tables
met.names <- c("$AvAcc$", "$MAvG$","$Rec_M$",
               "$Prec_M$", "$Rec_mu$","$Prec_mu$","$MF_beta$", "$MF_{beta M}$","$MF_{beta mu}$",
               "$CBA$", "$MCC$", "$RCI$", "$CEN$","$WMRec$","$WMPrec$","$WFM_beta$",
               "$WAvFM_beta$", "$WCBA$", "$RelMRec$","$RelMPrec$","$RelFM_beta$",
               "$RelAvFM_beta$", "$RelCBA$", "$ORec_P$", "$OPrec_P$", "$OFM_{beta P}$", "$OAvFM_{beta P}$",
               "$OCBA_P$","$ORec_T$", "$OPrec_T$", "$OFM_{beta T}$", "$OAvFM_{beta T}$",
               "$OCBA_T$")






###############################################################################
### auxs functions for discrimination tests
###############################################################################
# target<-3  #the target sum of the line (tpi)
# lowest<-0   #the smallest integer I allow (allways zero!)
# size<-3 #the number of integers I wish to use to sum (target number of classes in preds)


GetPosLines <- function(target, lowest, size) {
  # given a matrix with all possible lines for a given class i with tpi=target in
  # a problem with "size" total number of classes
  maxposs <- target - ((size-1) * lowest) #max possible integer.
  m <- combn(rep(lowest:maxposs,size), size)
  m1<- m[,colSums(m)==target]
  m1 <- unique(m1, MARGIN=2) 
  res <- t(m1)
  res
}

