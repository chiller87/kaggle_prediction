

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


#all.columns <- c("T1_V4")
column.ranges.list <- list()
for(column in all.columns)
{
    if(column == "Id" || column == "Hazard")
        next

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
    
    different.elements.matrix <- cbind(different.elements.train, different.elements.test)
    colnames(different.elements.matrix) <- c(paste(column, "_train", sep=""), paste(column, "_test", sep=""))
    column.ranges.list[[column]] <- different.elements.matrix

}


max.length <- max(sapply(column.ranges.list, nrow))
column.ranges.matrix <- do.call("cbind", lapply(column.ranges.list, function(x)
    rbind(x, matrix(, max.length - nrow(x), ncol(x)))))

write.table(column.ranges.matrix, file = paste("tmp/column_ranges.csv", sep=""), 
            sep = ";", row.names = FALSE, append = FALSE, col.names = TRUE, na = "")


