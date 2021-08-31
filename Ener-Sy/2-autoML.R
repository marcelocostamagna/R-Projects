library(h2o)
library(tidyverse)
h2o.init()

model_data <- read_csv("Data/model_data.csv")

#remove NA's on target value
model_data_no_na <- model_data %>% filter(!is.na(retired)) 
write_csv(model_data_no_na, "Data/model_data_no_na.csv")

#train
train <- h2o.importFile("Data/model_data_no_na.csv")

aml <- h2o.automl(y = "retired",
                  training_frame = train,
                  max_runtime_secs = 300)

lb <- aml@leaderboard
lb

print(lb, n = nrow(lb))

lb <- h2o.get_leaderboard(object = aml, extra_columns = 'ALL')
lb
