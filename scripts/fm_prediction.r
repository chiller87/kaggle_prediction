



fm_execute <- function(trainfile, testfile, outfile, params) {
    # init parts for libfm execution
    libfm.cmd <- "libfm -task r"
    libfm.train <- paste("-train ", trainfile, sep="")
    libfm.test <- paste("-test ", testfile, sep="")
    libfm.out <- paste("-out ", outfile, sep="")
    libfm.dim <- paste("-dim 1,1,", params["k"], sep="")
    libfm.iter <- paste("-iter ", params["iter"], sep="")
    libfm.method <- paste("-method ", params["method"], sep="")
    libfm.stdev <- paste("-init_stdev ", params["stdev"], sep="")
    libfm.reg <- paste("-regular ", params["reg"], sep="")
    libfm.lr <- paste("-learn_rate ", params["lr"], sep="")
    libfm.rlog <- "" #paste("-rlog ", logname, sep="")
    
    cmd <- paste(libfm.cmd, libfm.train, libfm.test, libfm.out, libfm.dim, libfm.iter, libfm.method, libfm.stdev, libfm.reg, libfm.lr, libfm.rlog, sep=" ")
    print(paste("command to execute: ", cmd, sep=""))
    # execute libfm
    system(cmd)
}



fm_prediction <- function(data, data.test, some.params) {
    # initialize default params
    params <- list(
        # method for FM parameter learning
        method="mcmc",
        # Anzahl Faktoren
        k=8,
        # Anzahl Iterationen
        iter=400,
        # output filename
        stdev=0.1,
        # regulation (only affect ALS and SGD)
        reg=0,
        # learning rate (only affect SGD)
        lr=0.001,
        # data representation
        data.rep=NA
    )
    
    # override default params with those, that are specified in some.params
    if (!identical(some.params,NA)) {
        for (n in names(some.params)) {
            if (!(n %in% names(params))) {
                stop("unknown parameter: ", n, "\n  allowed parameters: ", paste(names(params),collapse=",")) 
            }
            params[n] <- some.params[n]
        }
    }
    
    # initialize output filename
    outfile = "tmp/prediction.libfm"
    
    #data <- data[,!(names(data) == "Id")]
    # convert data to matrix
    data.matrix <- as.matrix(data)
    

    if(!identical(data.test, NA)) {
        
        trainfile <- "tmp/train.libfm"
        testfile <- "tmp/test.libfm"
        
        #test.matrix <- test[,!(names(data) == "Id")]
        # convert test data to matrix
        test <- as.matrix(data.test)
        
        tcost <- c(rep(0, times=nrow(test)))
        ecost <- vector('numeric',length(tcost))
        
        # create test and training sets
        train <- data.matrix
        
        # define train data
        # 1: Id, 2: Hazard, 3-ncol: properties
        x.train <- train[,3:(ncol(train))]
        y.train <- train[,2]
        
        # write the training data as file in sparse libsvm format
        if(!file.exists(trainfile))
        {
            print("write train data in libfm format to file ...")
            write.matrix.csr(x=x.train, y=y.train, file=trainfile) 
        }
        else
        {
            print("skip writing train file in libfm format: already exist!")
        }
        
        # define test data
        # 1: Id, 2-ncol: properties   =>   no hazard in kaggle-test data
        x.test <- test[,2:(ncol(test))]
        y.test <- tcost
        
        # write the test data as file in sparse libsvm format
        if(!file.exists(testfile))
        {
            print("write test data in libfm format to file ...")
            write.matrix.csr(x=x.test, y=y.test, file=testfile)
        }
        else
        {
            print("skip writing test file in libfm format: already exist!")
        }
        
        fm_execute(trainfile=trainfile, testfile=testfile, outfile=outfile, params=params)
        
        prediction <- read.table(outfile)
        ecost <- as.numeric(prediction[,1])
        
        # build submission with Id and Hazard
        ecost.ids <- data.test[["Id"]]
        submission <- cbind(ecost.ids, ecost)
        colnames(submission) <- c("Id", "Hazard")
        
        # solution is not known
        solution <- NA
        
    }
    else {
        
        tcost <- data[["Hazard"]]
        ecost <- vector('numeric',length(tcost))
    
        # specify strata
        stratum_count <- 2
        stratum_size <- nrow(data) %/% stratum_count
        considered_row_count <- stratum_count * stratum_size
        i <- 0	
        while ( i < stratum_count ) {
            
            ind <- (i*stratum_size + 1):((i+1)*stratum_size)
            #ind <- c(1)
            
            trainfile <- paste("tmp/train_s", i, ".libfm", sep="")
            testfile <- paste("tmp/test_s", i, ".libfm", sep="")
            
            # cat("Test indices: ", ind[1], "-", ind[length(ind)], "...")
            
            # create test and training sets
            train <- data.matrix[-ind,]
            test <- data.matrix[ind,]
            
            # define train data
            # 1: Id, 2: Hazard, 3-ncol: properties
            x.train <- train[,3:(ncol(train))]
            y.train <- train[,2]
    
            # write the training data as file in sparse libsvm format
            if(!file.exists(trainfile))
            {
                print("write train data in libfm format to file ...")
                write.matrix.csr(x=x.train, y=y.train, file=trainfile) 
            }
            else
            {
                print("skip writing train file in libfm format: already exist!")
            }
            
            # define test data
            # 1: Id, 2: Hazard, 3-ncol: properties
            x.test <- test[,3:(ncol(test))]
            y.test <- test[,2]
    
            # write the test data as file in sparse libsvm format
            if(!file.exists(testfile))
            {
                print("write test data in libfm format to file ...")
                write.matrix.csr(x=x.test, y=y.test, file=testfile)
            }
            else
            {
                print("skip writing test file in libfm format: already exist!")
            }
    
            fm_execute(trainfile=trainfile, testfile=testfile, outfile=outfile, params=params)
            
            prediction <- read.table(outfile)
            ecost[ind] <- as.numeric(prediction[,1])
            i <- i + 1
        }
        
        # build submission with Id and Hazard
        ecost.ids <- data[["Id"]]
        submission <- cbind(ecost.ids, ecost)
        colnames(submission) <- c("Id", "Hazard")
        
        # build solution with Id and Hazard
        tcost.ids <- data[["Id"]]
        solution <- cbind(tcost.ids, tcost)
        colnames(solution) <- c("Id", "Hazard")
    }
    
    
    return(
        list(
            hazard=data.frame(true=tcost,predicted=ecost), 
            params=params, 
            test=!identical(data.test, NA), 
            submission=submission, 
            solution=solution
            )
        )
}


























