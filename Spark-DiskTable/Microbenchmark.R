library(disk.frame)
library(here)
library(tidyverse)
library(sparklyr)
library(arrow)
library(microbenchmark)

## Setup Disk Frame
setup_disk.frame()
# This allows large datasets to be transferred between sessions
options(future.globals.maxSize = Inf)

## Setup Sparkly
#spark_install(version = "2.4.5")  # with java 8
sc <- spark_connect(master = "local", version = "2.4.5", config = list("sparklyr.shell.driver-memory" = "12G"))

# Quick Benchmark ---------------------------------------------------------


df_flights <- nycflights13::flights
flights_tbl <- sparklyr::copy_to(sc, df_flights, "df_flights") #Spark table from Data frame
flights.df <- as.disk.frame(nycflights13::flights)             #Disk.frame Table  

mbm <- microbenchmark("op_dplyr" = df_flights %>%
                        filter(year == 2013) %>%
                        mutate(origin_dest = paste0(origin, dest)) %>%
                        head(2),
                      "op_disk_table" = flights.df %>%
                        filter(year == 2013) %>%
                        mutate(origin_dest = paste0(origin, dest)) %>%
                        head(2),
                      "op_spark" = flights_tbl %>%
                        filter(year == 2013) %>%
                        mutate(origin_dest = paste0(origin, dest)) %>%
                        head(2)
)

mbm
autoplot(mbm)

#Disconnect Spark
spark_disconnect(sc)


# Service History Table ---------------------------------------------------------------

# Load RDS
df_service_history <- readRDS(file = here("data", "df_service_history.rds"))

# Disk.frame
service.df <- as.disk.frame(df_service_history)   
service.df %>%
  group_by(user_id) %>% 
  summarise(tot = n()) %>% 
  collect
# usando 8 workers demora 3 minutos aprox



# Spark
serv_hist_tbl <- sparklyr::copy_to(sc, df_service_history, "serv_hist")
serv_hist_tbl %>% 
  count(user_id, sort = TRUE) %>% 
  head(5)

spark_disconnect(sc)


# Read Globoplay VS 24 M rows -------------------------------------------------------

# Data read
df_globo_vs_table <-spark_read_csv(sc = sc , name = "df_gvs", path =  "/home/marcelus/Downloads/borrar/Globoplay_VideoSession.csv")

# Create reference to Spark data frame
df_globo_vs_table <- tbl(sc, "df_gvs")

# Bring data from spark into R memory
df_globo_vs_table %>% 
  select(num_interactions) %>% 
  collect()

# Save DF to parquet files, must specify a folder TO CREATE not a name. 
spark_write_parquet(x = df_globo_vs_table, path = "/home/marcelus/Downloads/borrar/globo_vs_parquet", mode = "append")

spark_disconnect(sc)

#  TMP --------------------------------------------------------------------

colnames(df_globo_vs_table)
spark_write_parquet(x = flights_tbl, path = "/home/marcelus/Downloads/borrar/flights", mode = "append")


