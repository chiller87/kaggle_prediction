


# R XGBOOST starter script

# The readr library is the best way to read and write CSV files in R
library(readr)
library(reshape2)
library(xgboost)



boost_method_CV <- function() {
    
    
    
    # The competition datafiles are in the directory ../input
    # Read competition data files:
    data <- read.csv("../data/train.csv")
    
    # CV method
    # specify strata
    stratum_count <- 2
    stratum_size <- nrow(data) %/% stratum_count
    considered_row_count <- stratum_count * stratum_size
    curr.strat <- 0
    
    tcost <- data$Hazard
    ecost <- rep(0,nrow(data))
    
    while ( curr.strat < stratum_count ) {
        
        ind <- (curr.strat*stratum_size + 1):((curr.strat+1)*stratum_size) 
        
        X <- data[-ind,]
        X.test <- data[ind,]
        
        target <- X.test$Hazard
        X.test$Hazard <- NULL
        
        # extract id
        id.test <- X.test$Id
        X.test$Id <- NULL
        X$Id <- NULL
        n <- nrow(X)
        
        # extarct target
        y <- X$Hazard
        X$Hazard <- NULL
        
        # replace factors with level mean hazard
        for (i in 1:ncol(X)) {
            if (class(X[,i])=="factor") {
                mm <- aggregate(y~X[,i], data=X, mean)
                levels(X[,i]) <- as.numeric(mm[,2]) 
                levels(X.test[,i]) <- mm[,2] 
                X[,i] <- as.numeric(as.character(X[,i]))  
                X.test[,i] <- as.numeric(as.character(X.test[,i]))
            }
        }
        X <- as.matrix(X)
        X.test <- as.matrix(X.test)
        
        # train & tune --skipped--
        logfile <- data.frame(shrinkage=c(0.04, 0.03, 0.03, 0.03, 0.02),
                              rounds = c(140, 160, 170, 140, 180),
                              depth = c(8, 7, 9, 10, 10),
                              gamma = c(0, 0, 0, 0, 0),
                              min.child = c(5, 5, 5, 5, 5),
                              colsample.bytree = c(0.7, 0.6, 0.65, 0.6, 0.85),
                              subsample = c(1, 0.9, 0.95, 1, 0.6))
        
        # generate final prediction -- bag of 50 models --
        models <- 5
        repeats <- 10
        yhat.test  <- rep(0,nrow(X.test))
        for (j in 1:repeats) {
            for (i in 1:models){
                set.seed(j*1000 + i*100)
                xgboost.mod <- xgboost(data = X, label = y, max.depth = logfile$depth[i], eta = logfile$shrinkage[i],
                                       nround = logfile$rounds[i], nthread = 4, objective = "reg:linear", subsample=logfile$subsample[i],
                                       colsample_bytree=logfile$colsample.bytree[i], gamma=logfile$gamma[i], min.child.weight=logfile$min.child[i])
                yhat.test  <- yhat.test + predict(xgboost.mod, X.test)  
            }
        }
        yhat.test <-  yhat.test/(models*repeats)
        write.csv(data.frame(Id=id.test, Hazard=target, Pred=yhat.test), paste("R_xgboost_benchmark_", curr.strat, ".csv", sep=""),row.names=F, quote=FALSE)
        
        
        ecost[ind] <- yhat.test
        
        curr.strat <- curr.strat + 1
        
    }
    
    return(
        list(
            hazard=data.frame(true=tcost,predicted=ecost), 
            params=NULL, 
            test=FALSE, 
            submission=data.frame(Id=data$Id, Hazard=ecost), 
            solution=data.frame(Id=data$Id, Hazard=tcost)
        )
    )
    
    
}







boost_method_test <- function() {
    # R XGBOOST starter script
    
    # The readr library is the best way to read and write CSV files in R
    library(readr)
    library(reshape2)
    library(xgboost)
    
    # The competition datafiles are in the directory ../input
    # Read competition data files:
    X <- read.csv("../data/train.csv")
    X.test <- read.csv("../data/test.csv")
    
    tcost <- rep(0,nrow(X.test))
    ecost <- tcost
    
    # extract id
    id.test <- X.test$Id
    X.test$Id <- NULL
    X$Id <- NULL
    n <- nrow(X)
    
    # extarct target
    y <- X$Hazard
    X$Hazard <- NULL
    
    # replace factors with level mean hazard
    for (i in 1:ncol(X)) {
        if (class(X[,i])=="factor") {
            mm <- aggregate(y~X[,i], data=X, mean)
            levels(X[,i]) <- as.numeric(mm[,2]) 
            levels(X.test[,i]) <- mm[,2] 
            X[,i] <- as.numeric(as.character(X[,i]))  
            X.test[,i] <- as.numeric(as.character(X.test[,i]))
        }
    }
    X <- as.matrix(X)
    X.test <- as.matrix(X.test)
    
    # train & tune --skipped--
    logfile <- data.frame(shrinkage=c(0.04, 0.03, 0.03, 0.03, 0.02),
                          rounds = c(140, 160, 170, 140, 180),
                          depth = c(8, 7, 9, 10, 10),
                          gamma = c(0, 0, 0, 0, 0),
                          min.child = c(5, 5, 5, 5, 5),
                          colsample.bytree = c(0.7, 0.6, 0.65, 0.6, 0.85),
                          subsample = c(1, 0.9, 0.95, 1, 0.6))
    
    # generate final prediction -- bag of 50 models --
    models <- 5
    repeats <- 10
    yhat.test  <- rep(0,nrow(X.test))
    for (j in 1:repeats) {
        for (i in 1:models){
            set.seed(j*1000 + i*100)
            xgboost.mod <- xgboost(data = X, label = y, max.depth = logfile$depth[i], eta = logfile$shrinkage[i],
                                   nround = logfile$rounds[i], nthread = 4, objective = "reg:linear", subsample=logfile$subsample[i],
                                   colsample_bytree=logfile$colsample.bytree[i], gamma=logfile$gamma[i], min.child.weight=logfile$min.child[i])
            yhat.test  <- yhat.test + predict(xgboost.mod, X.test)  
        }
    }
    yhat.test <-  yhat.test/(models*repeats)
    write.csv(data.frame(Id=id.test, Hazard=yhat.test),"R_xgboost_submission.csv",row.names=F, quote=FALSE)
    
    ecost <- yhat.test
    
    return(
        list(
            hazard=data.frame(true=tcost,predicted=ecost), 
            params=NULL, 
            test=TRUE, 
            submission=data.frame(Id=id.test, Hazard=ecost), 
            solution=data.frame(Id=id.test, Hazard=tcost)
        )
    )
}








