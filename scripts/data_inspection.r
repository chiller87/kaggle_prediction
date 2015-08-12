

source("prepare_data.r")

data.train <- read.table("../data/train.csv", sep = ",", header = TRUE)
data.test <- read.table("../data/test.csv", sep = ",", header = TRUE)



non.numeric.columns <- c("T1_V4", "T1_V5", "T1_V6", "T1_V7", "T1_V8", "T1_V9", 
                         "T1_V11", "T1_V12", "T1_V15", "T1_V16", "T1_V17", "T2_V3", 
                         "T2_V5", "T2_V11", "T2_V12", "T2_V13")

all.columns <- names(data.train)
numeric.columns <- all.columns[!(all.columns %in% non.numeric.columns)]
numeric.columns <- numeric.columns[3:length(numeric.columns)]

target <- data.train[["Hazard"]]


what <- c("frequency") # c("correlation", "ranges", "frequency")



if("frequency" %in% what) {
    
    column.names <- c("Hazard")
    
    # compute frequencies of all observed hazard values
    freq <- as.data.frame(table(target))
    column.names <- c(column.names, "Freq")
    colnames(freq) <- column.names
    
    # compute cumulative sums of the frequencies
    cumulative.sums <- cumsum(freq[["Freq"]])
    freq <- cbind(freq, cumulative.sums)
    column.names <- c(column.names, "cumsum")
    colnames(freq) <- column.names
    
    # compute percentage of cumsums of number of train data
    max.value <- max(freq[["cumsum"]])
    percentages <- sapply(freq[["cumsum"]], function(x){(x / max.value) * 100})
    freq <- cbind(freq, percentages)
    column.names <- c(column.names, "percentage")
    colnames(freq) <- column.names
    
    # write data to file
    write.table(freq, file = "tmp/hazard_frequencies.csv", sep = ";",
                row.names = FALSE, col.names = TRUE, append = FALSE)
}


if("correlation" %in% what) {
    # compute correlations with hazard for each numeric column
    correlations.list <- list()
    
    for(column in numeric.columns)
    {
        c <- cor(target, data.train[[column]]) 
        cat(sprintf("%s, %5.3f ", column, c ))
        correlations.list[[column]] <- c
    }
    
    correlations.matrix <- do.call("cbind", correlations.list)
    write.table(correlations.matrix, file = "tmp/data_correlation.csv", sep = ";",
                row.names = FALSE, col.names = TRUE, append = FALSE)
}




if("ranges" %in% what) {
    #all.columns <- c("T1_V4")
    column.ranges.list <- list()
    
    for(column in all.columns)
    {
        # ignore id column
        if(column == "Id")
            next
        
        # just use train data for hazard
        if(column == "Hazard")
        {
            different.elements.train <- extract_column_range(data.train, column)
            different.elements.train <- sort(different.elements.train, decreasing = FALSE)
            different.elements.matrix <- as.matrix(different.elements.train)
            colnames(different.elements.matrix) <- c(paste(column, "_train", sep=""))
            column.ranges.list[[column]] <- different.elements.matrix
            next
        }
    
        # compute ranges for both, train and test data
        different.elements.train <- extract_column_range(data.train, column)
        different.elements.test <- extract_column_range(data.test, column)
        different.elements.train <- sort(different.elements.train, decreasing = FALSE)
        different.elements.test <- sort(different.elements.test, decreasing = FALSE)
    
        if(column %in% non.numeric.columns)
        {
            different.elements.train <- as.character(different.elements.train)
            different.elements.test <- as.character(different.elements.test)
        }
        
        lengths <- max(c(length(different.elements.train), length(different.elements.test)))
        length(different.elements.train) <- lengths
        length(different.elements.test) <- lengths
        
        # convert both vectors to matrix
        different.elements.matrix <- cbind(different.elements.train, different.elements.test)
        colnames(different.elements.matrix) <- c(paste(column, "_train", sep=""), paste(column, "_test", sep=""))
        # append matrix to ranges list
        column.ranges.list[[column]] <- different.elements.matrix
    
    }
    
    # combine all list elements to one data frame
    max.length <- max(sapply(column.ranges.list, nrow))
    column.ranges.matrix <- do.call("cbind", lapply(column.ranges.list, function(x)
        rbind(x, matrix(, max.length - nrow(x), ncol(x)))))
    
    # write data to file
    write.table(column.ranges.matrix, file = paste("tmp/column_ranges.csv", sep=""), 
                sep = ";", row.names = FALSE, append = FALSE, col.names = TRUE, na = "")
}

