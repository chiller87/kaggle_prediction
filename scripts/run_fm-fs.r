





source("prepare_data.r")
source("fm_prediction.r")
source("output_functions.r")
source("compute_score.r")
#source("data_inspection.r")
# ===================================================================================

data.representation <- c("indicators", "clean") # c("ids", "indicators", "setindicators", "clean")
features.to.be.cleaned <- c("T1_V11")

# reading data:
# id hazard T1_V1-T1_V17 T2_V1-T2_V15
# => 34 columns!
print("preparing data ...")
raw_data <- read.table("../data/train.csv", sep = ",", header = TRUE)
#print("done!")


non.numeric.columns <- c("T1_V4", "T1_V5", "T1_V6", "T1_V7", "T1_V8", "T1_V9", 
                         "T1_V11", "T1_V12", "T1_V15", "T1_V16", "T1_V17", "T2_V3", 
                         "T2_V5", "T2_V11", "T2_V12", "T2_V13")
all.columns <- names(raw_data)
numeric.columns <- all.columns[!(all.columns %in% non.numeric.columns)]
feature.columns <- all.columns[!(all.columns %in% c("Id", "Hazard"))]

#print("extracting values for non numeric columns ...")
data <- prepare_data(data = raw_data, column.name.range = non.numeric.columns, data.representation=data.representation)
write.table(data, file = "tmp/prepared_data.csv", sep = ",", row.names = FALSE, col.names = TRUE, append = FALSE)
#print("done!")

print("preparing test data ...")
test.data <- NA
#test.raw_data <- read.table("../data/test.csv", sep = ",", header = TRUE)
#test.data <- prepare_data(data = test.raw_data, column.name.range = non.numeric.columns, data.representation=data.representation)
#write.table(test.data, file = "tmp/testdata_with_indicators.csv", sep = ",", row.names = FALSE, col.names = TRUE, append = FALSE)
#print("done!")

data.representation <- c(data.representation, features.to.be.cleaned)


# initialize ranges (FM training)
fm.method.range <- c("mcmc")
k.range <- c(10) #seq(8, 12, 1)#c(8, 12, 16) 
stdev.range <- c(0.0)# seq(0.02, 0.1, 0.02)#c(0.0) 
iter.range <- c(400) #seq(200, 2000, 200) #seq(400, 1600, 200) 
reg.range <-  c(0)#seq(0, 2, 1)
lr.range <- c(0.0) 

feature.range <- feature.columns[!(feature.columns %in% features.to.be.cleaned)]

for (feature in feature.range) {
    
    data.modified <- drop_columns_beginning_with(data, feature)
    data.rep <- c(data.representation, feature)

for (fm.method in fm.method.range) {
for (stdev in stdev.range) {
for (reg in reg.range) {
for (lr in lr.range) {
for (k in k.range) {
for (iter in iter.range) {

    
    result <- fm_prediction(data=data, data.test=test.data, some.params=list(method=fm.method, iter=iter, k=k, stdev=stdev, reg=reg, lr=lr, data.rep=data.rep))
    statistics <- save_result(result)
    #print_result(statistics)
    

    
}
}
}
}
}
}

}

# clean up
print("cleaning up ...")
unlink("tmp/*.libfm")


print("all done!")
















