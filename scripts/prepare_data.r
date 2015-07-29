


prepare_data <- function() {
    
}


extract_column_ranges <- function(data, column.name.range) {
    for(column.name in column.nane.range) {
        extract_column_range(data, column.name = column.name)
    }
}

extract_column_range <- function(data, column.name) {
    different.elements <- unique(data[column.name])
    write.table(different.elements, file = paste("tmp/", column.name, ".data", sep=""), sep = ",", row.names = FALSE, append = FALSE, col.names = FALSE)
    
    return(different.elements)
}


replace_with_indicators <- function(data, column.name.range) {
    columns.to.drop <- c()
    
    for(column.name in column.name.range) {
        # get unique values of current column
        values.unique <- extract_column_range(data, column.name = column.name)
        
        
    }
    
    # TODO: replace non numeric values with indicator vectors
    param <- "ENGINE_TYPE_GROUP"
    if(param %in% params$metadata) {
        engine.types.range <- c("BENZIN", "DIESEL", "GAS", "NA")
        
        # append engine types (names) to regions matrix
        explanatory_variables <- append(explanatory_variables, param)
        regions <- cbind(regions, car.model[param])
        
        # engine.types.vectors <- list()
        for(engine.type in engine.types.range) {
            if(engine.type != "NA") {
                # create vector with zeros
                vec <- c(rep(0, times=nrow(regions)))
                # set appropriate indices to one
                vec <- replace(vec, regions[param]==engine.type, 1)
                # append vec as column to regions
                regions <- cbind(regions, vec)
                # add explanatory variable
                explanatory_variables <- append(explanatory_variables, paste("ET_", engine.type, sep=""))
            }
            else {
                # create vector with zeros
                vec <- c(rep(0, times=nrow(regions)))
                # set appropriate indices to one
                vec <- replace(vec, is.na(regions[param]), 1)
                # append vec as column to regions
                regions <- cbind(regions, vec)
                # add explanatory variable
                explanatory_variables <- append(explanatory_variables, paste("ET_", engine.type, sep=""))
            }
        }
        
        columns.to.drop <- c(columns.to.drop, param)
    }
}