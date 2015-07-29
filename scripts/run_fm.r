





source("prepare_data.r")
source("fm_prediction.r")

# ===================================================================================


# reading data:
# id hazard T1_V1-T1_V17 T2_V1-T2_V15
# => 34 columns!
print("reading data ...")
raw_data <- read.table("../data/train.csv", sep = ",", header = TRUE)
print("done!")


non.numeric.columns <- c("T1_V4", "T1_V5", "T1_V6", "T1_V7", "T1_V8", "T1_V9", 
                         "T1_V11", "T1_V12", "T1_V15", "T1_V16", "T1_V17", "T2_V3", 
                         "T2_V5", "T2_V11", "T2_V12", "T2_V13")

print("extracting values for non numeric columns ...")
extract_column_ranges(data = raw_data, column.name.range = non.numeric.columns)
print("done!")







# initialize ranges (FM training)
fm.method.range <- c("mcmc")
k.range <- c(30) #seq(10, 40, 10)
stdev.range <- c(0.8) #seq(0.0, 1.0, 0.2) # seq(0.0, 0.5, 0.1)
iter.range <- c(200) #seq(400, 2000, 400)
reg.range <-  c(0.0) #seq(0.0, 1.0, 0.5) #seq(0.0, 1.0, 0.2)
lr.range <- c(0.0) # seq(1e-06, 1e-05, 2e-06)


for (fm.method in fm.method.range) {
for (iter in iter.range) {
for (k in k.range) {
for (stdev in stdev.range) {
for (reg in reg.range) {
for (lr in lr.range) {
    
    result <- fm_prediction()
    
}
}
}
}
}
}





















