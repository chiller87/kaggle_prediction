

data <- read.table("../data/train.csv", sep = ",", header = TRUE)
non.numeric.columns <- c("T1_V4", "T1_V5", "T1_V6", "T1_V7", "T1_V8", "T1_V9", 
                         "T1_V11", "T1_V12", "T1_V15", "T1_V16", "T1_V17", "T2_V3", 
                         "T2_V5", "T2_V11", "T2_V12", "T2_V13")
all.columns <- names(data)
numeric.columns <- all.columns[!(all.columns %in% non.numeric.columns)]


column.name <- "T1_V4"
unique.value.range <- unique(data[[column.name]])

#unique.value.range <- read.csv(paste("tmp/", column.name, ".data", sep=""), sep = ",", header = FALSE, stringsAsFactors = FALSE)

#print(unique.value.range)

#print(unique.value.range[column.name])
#unique.value.range <- c("B", "S", "L")
explanatory_variables <- c()
indicator.list <- list()
#print(unique.value.range)
for(unique.value in unique.value.range) {
    # create vector with zeros
    vec <- c(rep(0, times=nrow(data)))
    # replace all lines, where the current unique.value appears
    vec <- replace(vec, data[column.name] == unique.value, 1)
    # add appropriate column name
    curr.name <- paste(column.name, "_", unique.value, sep="")
    explanatory_variables <- append(explanatory_variables, curr.name)
    # append vector to indicator list
    indicator.list[[curr.name]] <- vec
}

indicator.data <- do.call("cbind", indicator.list)
idx <- which(names(data) == column.name)[1]
data <- cbind(data[,1:idx, drop=F], indicator.data, data[,(idx+1):length(data), drop=F])

filename <- paste("tmp/", column.name, ".csv", sep="")
unique.value.range <- read.csv(filename, sep = ",", header = FALSE, stringsAsFactors = FALSE)
unique.value.range <- as.vector(as.matrix(unique.value.range))
#unique.value <- (unique.value.range[,1])[1]
unique.value <- unique.value.range[1]
data[column.name] == unique.value
bla <- list()
bla[[column.name]] <- data[column.name] == unique.value

unlink("tmp/*.libfm")

foo <- seq(1,8,1)





data.train <- read.table("../data/train.csv", sep = ",", header = TRUE)
data.test <- read.table("../data/test.csv", sep = ",", header = TRUE)

column <- "T1_V4"

different.elements.train <- extract_column_range(data.train, column)
different.elements.test <- extract_column_range(data.test, column)
different.elements.train <- sort(different.elements.train, decreasing = FALSE)
different.elements.test <- sort(different.elements.test, decreasing = FALSE)

different.elements.test <- as.character(different.elements.test)

lengths <- max(c(length(different.elements.train), length(different.elements.test)))
length(different.elements.train) <- lengths
length(different.elements.test) <- lengths

different.elements.matrix <- cbind(different.elements.train, different.elements.test)
colnames(different.elements.matrix) <- c(paste(column, "_train", sep=""), paste(column, "_test", sep=""))





combinations <- combn(all.columns, 20, simplify = FALSE)



source("xgboost.r")

blubb <- boost_method()










