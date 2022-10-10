.libPaths("/data/hydro/R_libs35")
.libPaths()


library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(foreign)
library(tidyverse)


options(digits=9)

args = commandArgs(trailingOnly = TRUE)
# args[1] = 134

source_path = "/home/kraghavendra/hardiness/hardiness_core.R"
# source_path = "/home/rbarik/hardiness/hardiness_core.R"
source(source_path)


param_dir = "/home/kraghavendra/hardiness/parameters/"
# param_dir = "/home/rbarik/hardiness/parameters/"

# read parameters
input_params  = data.table(read.csv(paste0(param_dir, "input_parameters", ".csv")))
variety_params = data.table(read.csv(paste0(param_dir, "variety_parameters", ".csv")))

climate_model = data.table(read.csv(paste0(param_dir,"config_observed",".csv")))
climate_model <- climate_model[climate_model$ARRAYID == args[1],] # !! 'min-max'

hist = TRUE

# File path location for observed historical 
input_file_path = "/data/hydro/jennylabcommon2/metdata/historical/UI_historical/VIC_Binary_CONUS_to_2016/"
#input_file_path <- paste0(input_file_path,climate_model$Model,"/",climate_model$Scenario,"/")
input_file_path <- "/data/cahnrs/Hossein/hardiness/00_Kaushik/observed"
filename = paste0(climate_model$Location)

file_found <- list.files(path= input_file_path,pattern = filename)
file_found <- paste0(input_file_path,filename)
print(paste0('FOUND FILE: ', file_found))

meta_data <- data.table(read_binary(file_path = file_found, hist = hist , no_vars= 8))
# To calculate the jday, we combined the column of day, month and year and then converted it into date format, then into jday format and then to numeric
# It was converted to numeric because the "%j" format is from 1-366, we needed 0-365, hence subtracting 1 from the number obtained

meta_data <- meta_data %>% mutate(Date = paste0(meta_data$month,"/",meta_data$day,"/",meta_data$year))
meta_data <- meta_data %>% mutate(jday = as.numeric(format(as.Date(meta_data$Date,format = "%m/%d/%y"),"%j"))-1) 
meta_data <- meta_data %>% mutate(T_mean = (meta_data$tmax + meta_data$tmin)/2)
meta_data <- meta_data %>% select("Date", "year", "jday", "T_mean", "tmax", "tmin")
# print(head(meta_data))

#print(paste0('FIRST 6 ROWS OF META DATA: ', head(meta_data)))

sapply(meta_data,class)

start_time <- Sys.time()
output <- hardiness_model(data = meta_data, input_params = input_params, variety_params = variety_params)
end_time <- Sys.time()
time_taken <- end_time - start_time
print(paste0('COMPUTED IN: ', time_taken))
# print(time_taken)

output$hardiness_year <- ifelse(leap_year(output$year),ifelse(output$jday <=136,output$year-1,
                                                              ifelse(output$jday >=244 & output$jday < 367,output$year,0)),
                                ifelse(output$jday <=135,output$year-1,
                                       ifelse(output$jday >=243 & output$jday < 366,output$year,0)))

output_CDI = data.table(matrix(NA,nrow =1, ncol =38))
colnames(output_CDI) <- paste(min(output$hardiness_year):max(output$hardiness_year),sep =" ")
output_CDI
# find the count anamolies per hardiness year
for (i in colnames(output_CDI))
{
  output_CDI[[i]] = sum(subset(output, hardiness_year == i)$CDI)
}

output_CDI <- cbind(Time_elapsed = time_taken, output_CDI )
output_CDI <- cbind(Location= climate_model$Location,output_CDI)

######################################### OUTPUT
output_dir <- "/data/hydro/users/kraghavendra/hardiness/output_data/observed/"
output_dir <- "/data/cahnrs/Vlad/hardiness/output_data/observed/"

dir.create(file.path(output_dir))
write.csv(output, file = paste0(output_dir, "output_observed_historical_", filename,".csv"), row.names=FALSE)
# !! uncomment if needed?
# write.table(output_CDI, file = paste0(output_dir, "consolidated_observed_historical.csv"),sep = ",",
#             col.names = !file.exists(paste0(output_dir, "consolidated_observed_historical.csv", ".csv")),
#             row.names= FALSE,append = TRUE )


