


prepare_data <- function(data, column.name.range, data.representation) {
    
    
    
    if("indicators" %in% data.representation)
    {
        data <- replace_with_indicators(data, column.name.range = column.name.range)
    }
    
    if("indicators_all" %in% data.representation)
    {
        all.columns <- names(data)
        feature.columns <- all.columns[!(all.columns %in% c("Id", "Hazard"))]
        data <- replace_with_indicators(data, column.name.range = feature.columns)
    }
    
    if("setindicators" %in% data.representation)
    {
        data <- replace_with_set_indicators(data, column.name.range = column.name.range)
    }
    
    if("ids" %in% data.representation)
    {
        data <- replace_with_ids(data, column.name.range = column.name.range)
    }
    
    if("mean" %in% data.representation)
    {
        data <- replace_with_mean(data, column.name.range = column.name.range)
    }
    
    if("clean" %in% data.representation)
    {
        data <- remove_unimportant_columns(data)
    }
    
    
    
    return(data)
}


remove_unimportant_columns <- function(data) {
    # columns to remove
    #unimportant.columns <- c("T1_V10", "T1_V11", "T2_V10", "T2_V11", "T2_V12")
    #print(paste("removing unimportant columns: ", unimportant.columns, sep=""))
    
    print(paste("removing unimportant columns: ", features.to.be.cleaned, sep=""))
    
    #data <- data[,!(names(data) %in% unimportant.columns)]
    
    for(column in features.to.be.cleaned) {
        data <- drop_columns_beginning_with(data, column)
    }
    
    return(data)
}


extract_column_range <- function(data, column.name) {
    different.elements <- unique(data[[column.name]])
    write.table(different.elements, file = paste("tmp/", column.name, ".csv", sep=""), sep = ";", row.names = FALSE, append = FALSE, col.names = FALSE)
    
    return(different.elements)
}


replace_with_indicators <- function(data, column.name.range) {
    columns.to.drop <- c()
    
    print("replacing non-numeric columns with indicators ...")
    for(column.name in column.name.range) {
        print(paste("curr column: ", column.name, sep=""))
        
        # get unique values of current column
        filename <- paste("tmp/", column.name, ".csv", sep="")
        if(!file.exists(filename))
        {
            print("extracting value range and write range to appropriate file ...")
            unique.value.range <- extract_column_range(data, column.name = column.name)
        }
        else
        {
            print("reading value range from file ...")
            unique.value.range <- read.csv(filename, sep = ",", header = FALSE, stringsAsFactors = FALSE)
            unique.value.range <- as.vector(as.matrix(unique.value.range))
        }
        
        # get index of current column.name
        idx <- which(names(data) == column.name)[1]
        
        # some initializations
        indicator.list <- list()
        
        
        print("parsing current column to indicators ... ")
        for(unique.value in unique.value.range) {
            # create vector with zeros
            vec <- c(rep(0, times=nrow(data)))
            # replace all lines, where the current unique.value appears
            vec <- replace(vec, data[column.name] == unique.value, 1)
            #print(vec)
            # add appropriate column name
            curr.name <- paste(column.name, "_", unique.value, sep="")
            # append vector to indicator list
            indicator.list[[curr.name]] <- vec
            #indicator.list <- c(indicator.list, vec)
        }
        
        indicator.data <- do.call("cbind", indicator.list)

        print("adding indicators to data table ... ")
        # join data and indicator.data
        if(idx == ncol(data)) {
            # if features s last column
            data <- cbind(data[,1:idx, drop=F], indicator.data)
        }
        else {
            data <- cbind(data[,1:idx, drop=F], indicator.data, data[,(idx+1):length(data), drop=F])
        }
        # remember column.name to drop later
        columns.to.drop <- c(columns.to.drop, column.name)
        
    }
    
    print("dropping non-numeric columns ...")
    data <- data[,!(names(data) %in% columns.to.drop)]
    
    
    return(data)
}




replace_with_ids <- function(data, column.name.range) {
    columns.to.drop <- c()
    
    print("replacing non-numeric columns with indicators ...")
    for(column.name in column.name.range) {
        print(paste("curr column: ", column.name, sep=""))
        
        # get unique values of current column
        filename <- paste("tmp/", column.name, ".csv", sep="")
        if(!file.exists(filename))
        {
            print("extracting value range and write range to appropriate file ...")
            unique.value.range <- extract_column_range(data, column.name = column.name)
            print("done!")
        }
        else
        {
            print("reading value range from file ...")
            unique.value.range <- read.csv(filename, sep = ",", header = FALSE, stringsAsFactors = FALSE)
            unique.value.range <- as.vector(as.matrix(unique.value.range))
            print("done!")
        }
        
        # get index of current column.name
        idx <- which(names(data) == column.name)[1]
        
        # some initializations
        id.list <- list()
        
        id <- 1
        # create vector with zeros
        vec <- c(rep(0, times=nrow(data)))
        print("parsing current column to indicators ... ")
        for(unique.value in unique.value.range) {
            
            # replace all lines, where the current unique.value appears
            vec <- replace(vec, data[column.name] == unique.value, id)
            id <- id + 1
        }
        
        curr.name <- paste(column.name, "_ids", sep="")
        id.list[[curr.name]] <- vec
        id.data <- do.call("cbind", id.list)
        print("adding indicators to data table ... ")
        # join data and indicator.data
        data <- cbind(data[,1:idx, drop=F], id.list, data[,(idx+1):length(data), drop=F])
        # remember column.name to drop later
        columns.to.drop <- c(columns.to.drop, column.name)
        
    }
    
    print("dropping non-numeric columns ...")
    data <- data[,!(names(data) %in% columns.to.drop)]
    
    print("done!")
    
    return(data)
}



replace_with_set_indicators <- function(data, column.name.range) {
    columns.to.drop <- c()
    
    print("replacing non-numeric columns with indicators ...")
    for(column.name in column.name.range) {
        print(paste("curr column: ", column.name, sep=""))
        
        # get unique values of current column
        filename <- paste("tmp/", column.name, ".csv", sep="")
        if(!file.exists(filename))
        {
            print("extracting value range and write range to appropriate file ...")
            unique.value.range <- extract_column_range(data, column.name = column.name)
        }
        else
        {
            print("reading value range from file ...")
            unique.value.range <- read.csv(filename, sep = ",", header = FALSE, stringsAsFactors = FALSE)
            unique.value.range <- as.vector(as.matrix(unique.value.range))
        }
        
        # get index of current column.name
        idx <- which(names(data) == column.name)[1]
        
        # some initializations
        indicator.list <- list()
        
        
        print("parsing current column to indicators ... ")
        for(unique.value in unique.value.range) {
            # create vector with zeros
            vec <- c(rep(0, times=nrow(data)))
            # replace all lines, where the current unique.value appears
            vec <- replace(vec, data[column.name] == unique.value, 1)
            #print(vec)
            # add appropriate column name
            curr.name <- paste(column.name, "_", unique.value, sep="")
            # append vector to indicator list
            indicator.list[[curr.name]] <- vec / length(unique.value.range)
            #indicator.list <- c(indicator.list, vec)
        }
        
        indicator.data <- do.call("cbind", indicator.list)
        
        print("adding indicators to data table ... ")
        # join data and indicator.data
        data <- cbind(data[,1:idx, drop=F], indicator.data, data[,(idx+1):length(data), drop=F])
        # remember column.name to drop later
        columns.to.drop <- c(columns.to.drop, column.name)
        
    }
    
    print("dropping non-numeric columns ...")
    data <- data[,!(names(data) %in% columns.to.drop)]
    
    
    return(data)
}





drop_columns_beginning_with <- function(data, column.prefix) {
    data.modified <- data[, !(grepl(column.prefix, names(data)))]
    return(data.modified)
}









