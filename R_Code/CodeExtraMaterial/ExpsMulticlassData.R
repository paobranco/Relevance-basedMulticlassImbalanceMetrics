library(DMwR)
library(rpart)
library(e1071)
library(igraph)
source("MetricsImpl.R")
source("Auxs.R")
load("MulticlassDataSets.RData")
# data set 15(yeast): removed SequenceName feature
# data set 6 (ecoli): removed SequenceName feature
# data set 16(zoo): removed animalName feature

rel <- list(c(1,0.6, 0.4), 
            c(0, 0.5, 1, 0.8, 0.6),
            c(0.8, 1, 0.2, 1),
            c(0.3,1,0.5),
            c(0.2,0.5,0.4,0.7,0.7,1),
            c(0,0.4,1,1,0.8,0.9,1,0.6),
            c(0.2,0.1,0.8,0.9,1,0.7),
            c(0.1,0.95,1),
            c(0,0.4,0.8,0.8,0.9,1,1,1),
            c(0.1, 0.9, 1,1,1,1),
            c(0.1,1,1),
            c(1,1,0.3),
            c(1,0.9,0.2,0.2,0.9,1),
            c(1,0.9,0.2,0.1,0.6,0.9,1),
            c(0,1,0.9,0.8,0.8,0.6,0.5,0.1,0.95,0.9),
            c(0.1,0.6,1,0.8,1,0.9,0.8)
            )

graphP <- list(c("L", "B", "R", "B"), 
               c("4", "3", "5", "3", "2", "4", "2", "5", "1", "2"),
               c("acc", "good", "acc", "vgood", "unacc", "vgood"),
               c("1", "2", "3", "2"),
               c("1", "4", "3","2","4", "6","5", "6","2", "6"),
               c("cp", "im", "im","imU","pp", "imU", "pp", "om", "om", "omL",
                 "imU","imL","imU","imS"),
               c("2","7","2","5","1","5","7", "3","3", "6", "5", "6"),
               c("1", "2", "1", "3"),
               c("0","1","1","2","1","3","3","5","2","4","4","5","4","6","5","8"),
               c("0","1","0","2", "1","3","1","4","1","5"),
               c("0", "1", "0", "2"),
               c("N", "EI", "N", "IE"),
               c("5","4","5","7","6","7","4","3","7","8"),
               c("6","7","5","7","5","4","4","3","7","8","8","9"),
               c("CYT", "MIT", "MIT", "ME1", "NUC", "ME3", "ME3", "ME2", "ME2",
                 "EXC", "ME1", "EXC", "ME1", "POX", "POX", "ERL", "EXC", "VAC",
                 "VAC", "ERL"),
               c("1","7","2","4","4","6","7","6","7","3","3","5")
               )

graphT <- list(c("R","L", "L","B"), 
               c("1", "2", "2", "5", "5", "4", "4", "3"),
               c("unacc", "acc", "acc", "good", "good", "vgood"),
               c("1", "3", "3", "2"),
               c("1", "3", "3", "2", "2", "4", "4", "5", "5", "6"),
               c("cp", "im", "im","pp", "pp","imU", "imU", "om", "om", "omL",
                 "omL","imS","imS","imL"),
               c("2", "1", "1", "7", "7", "3", "3", "5", "5", "6"),
               c("1", "2", "2", "3"),
               c("0", "1","1", "2","2","3","3","4","4","5","5","6","6","8"),
               c("0", "1","1", "2","2","3","3","4","4","5"),
               c("0", "1","1", "2"),
               c("N", "IE", "IE", "EI"),
               c("8", "3", "3", "4", "4", "7", "7", "6", "6", "5"),
               c("9","3","3","4","4","8","8","7","7","5","5","6"),
               c("CYT", "NUC", "NUC", "MIT", "MIT", "ME3", "ME3", "ME2", "ME2",
                 "ME1", "ME1", "EXC", "EXC", "VAC", "VAC", "POX", "POX", "ERL"),
               c("1","2","2","4","4","7","7","6","6","3","3","5")
               )

CVsetts <- cvSettings(2,10,1234,TRUE) #stratified sampling

TODO <- c('rpart','svm','naiveBayes')

for (d in 1:16){ # for each data set in DSs
  for(td in TODO) {
    assign(td,
          try(
           experimentalComparison(
             DSs[d],         
               do.call('variants',
                       c(list('allData',learner=td,
                         nr=d),
                         varsRootName=paste('allData',td,sep='.'))),
             CVsetts)
           )
         )
         # save the results
         if (class(get(td)) != 'try-error') save(list=td,file=paste(DSs[[d]]@name,td,'Rdata',sep='.'))
}
}

# 
# DSs[[1]]@name
# summary(DSs[[1]]@data)
# 
# sp <- sample(1:nrow(DSs[[9]]@data), 0.7*nrow(DSs[[9]]@data))
# train <- DSs[[9]]@data[sp,]
# test <- DSs[[9]]@data[-sp,]
# 
# #rpart
# model <- rpart(DSs[[9]]@formula, train)
# preds <- predict(model, test, type="class")
# 
# # naiveBayes (e1071)
# model <- naiveBayes(DSs[[1]]@formula, DSs[[1]]@data)
# preds <- predict(model, DSs[[1]]@data)
# 
# #svm (e1071)
# model <- svm(DSs[[1]]@formula, DSs[[1]]@data)
# preds <- predict(model, DSs[[1]]@data)
# 


## get estimated relevance of data sets:
# for(d in 1:2){
#   form <- DSs[[d]]@formula
#   ds <- DSs[[d]]@data
#   tgt <- which(names(ds) == as.character(form[[2]]))
#   res <- wEvt(table(DSs[[d]]@data[,tgt]))
#   cat("Data set: ", DSs[[d]]@name, "\nautomatic relevance: ")
#   print(res)
#   cat("\n \n")
# }



# just some plots of the graphs:
# library(igraph)
 # plot(make_graph(graphP[[2]], directed=TRUE))
 # 
 # g <- make_graph(graphP[[2]], directed=TRUE)
 # comps <- components(g)$membership
 # colbar <- rainbow(length(comps)+1)
 # V(g)$color <- colbar[comps+1]
 # plot(g, vertex.size=10, vertex.color=rainbow(length(comps)+1))
 # tkplot(g, vertex.size=10,vertex.color=rainbow(length(comps)))
 # 
