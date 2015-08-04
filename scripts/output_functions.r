


save_result <- function(result) {
    
    res = result$hazard
    params = result$params
    submission = result$submission
    solution = result$solution
    
    ae <- abs( res$predicted - res$true )
    re <- ae/res$true

    mae <- mean(ae)
    medae <- median(ae)
    mre <- mean(re)
    medre <- median(re)
    
    if(!identical(solution, NA))
    {
        gini <- NormalizedGini(solution[,2], submission[,2])
    }
    else 
    {
        gini <- NA
    }
    
    params$data.rep <- paste(params$data.rep, collapse=" ")
    data_f <- list(params, test=result$test, mae=mae, medae=medae, mre=mre, medre=medre, gini=gini)
    print_result(data_f)
    print("save data to csv ...")
    write.table(data_f,file="results.csv", sep=";", append=TRUE, col.names=FALSE, row.names=FALSE)
    
    print("save submission ...")
    write.table(result$submission, file="submission.csv", sep=",", append=FALSE, col.names=TRUE, row.names=FALSE)
    
    return(data.frame(data_f))
}


# gibt Kennzahlen aus
print_result <- function(result) {
    
    cat(sprintf("AE: mean=%.2f, median=%.2f,", result$mae, result$medae))
    cat("\nAE (CSV): ")
    cat(result$mae, result$medae, sep=";")
    cat("\n")
    cat(sprintf("RE: mean=%.4f, median=%.4f,", result$mre, result$medre))
    cat("\nRE (CSV): ")
    cat(result$mre, result$medre, sep=";")
    cat("\n")
    cat(sprintf("gini=%.4f,", result$gini))
    cat("\n")
    
#     cat(build_paramstr(result$params))
#     cat("\n")
}

# bereitet Parameterliste als String auf
build_paramstr <- function(params) {
    paramstr <- paste(names(params)[1],"=",params[[1]], sep="")
    for (i in 2:length(params)) {
        paramstr <- paste(paramstr, ",", names(params)[i], "=", params[[i]], sep="")
    }
    return(paramstr)
}


