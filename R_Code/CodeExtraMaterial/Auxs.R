eval.stats <- function(form,train,test,preds,rel,graphP,graphT) {
  trues <- resp(form,test)
z <- table(trues, preds)
  #  z <- confusion matrix
  beta <- 1
  AvACC <- AvACC(z)
  MAvG <- MAvG(z)
  MAvA <- MAvA(z)
  precM <- precM(z)
  recMiu <- recMiu(z)
  precMiu <- precMiu(z)
  MFb <- MFb(beta,z)
  FbM <- FbM(beta,z)
  FbMiu <- FbMiu(beta, z)
  cba <- cba(z)
  MCC <- MCC(z)
  RCI <- RCI(z)
  CEN <- CEN(z)
  WMRec <- WMRec(z)
  WMPrec <- WMPrec(z)
  WFM <- WFM(beta,z)
  WAvFM <- WAvFM(beta,z)
  Wcba <- Wcba(z)
  RelMRec <- RelMRec(z,rel)
  RelMPrec <- RelMPrec(z,rel)
  RelFM <- RelFM(beta,z,rel)
  RelAvFM <- RelAvFM(beta,z,rel)
  Relcba <- Relcba(z,rel)
  OPRec <- ORec(z,graphP)
  OPPrec <- OPrec(z,graphP)
  OPFM <- OFM(beta,z,graphP)
  OPAvFM <- OAvFM(beta,z,graphP)
  OPcba <- Ocba(z,graphP)
  OTRec <- ORec(z,graphT)
  OTPrec <- OPrec(z,graphT)
  OTFM <- OFM(beta,z,graphT)
  OTAvFM <- OAvFM(beta,z,graphT)
  OTcba <- Ocba(z,graphT)
  
  
    c(AvACC = AvACC, MAvG = MAvG, MAvA = MAvA, precM = precM,
      recMiu = recMiu, precMiu = precMiu, MFb = MFb, FbM = FbM, 
      FbMiu = FbMiu, cba = cba, MCC = MCC, RCI = RCI, CEN = CEN, 
      WMRec = WMRec,  WMPrec = WMPrec, WFM = WFM, WAvFM = WAvFM, 
      Wcba = Wcba, RelMRec = RelMRec, RelMPrec = RelMPrec,
      RelFM = RelFM, RelAvFM = RelAvFM, Relcba = Relcba, 
      OPRec = OPRec, OPPrec= OPPrec, OPFM = OPFM, OPAvFM = OPAvFM,
      OPcba = OPcba, OTRec = OTRec, OTPrec= OTPrec, OTFM = OTFM, 
      OTAvFM = OTAvFM, OTcba = OTcba)
}



# ============================================================
# Training a model using the standard data set,
# i.e. all given data
# ============================================================
allData <- function(form,train,test,learner, nr, ...){
  preds <- do.call(paste('cv',learner,sep='.'),
                   list(form,train,test, ...)
  )
  rel <- rel[[nr]]
  graphP <- graphP[[nr]]
  graphT <- graphT[[nr]]
  eval.stats(form,train,test,preds,rel, graphP, graphT)
}

# ==============================
# the learn/test functions for the different systems 
cv.svm <- function(form,train,test,...) {
  m <- svm(form,train,...)
  predict(m,test)
}
cv.naiveBayes <- function(form,train,test,...) {
  m <- naiveBayes(form,train,...)
  predict(m,test)
}
cv.rpart <- function(form,train,test,...) {
  m <- rpart(form,train,...)
  predict(m,test, type="class")
}

