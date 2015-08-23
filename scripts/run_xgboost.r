



source("xgboost.r")
source("output_functions.r")
source("compute_score.r")

res <- boost_method_CV()
#res <- boost_method_test()
statistics <- save_boost_result(res)





