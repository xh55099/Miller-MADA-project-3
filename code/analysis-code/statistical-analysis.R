###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#Call a bunch of libraries.
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(broom)) 
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(readxl)) 
suppressPackageStartupMessages(library(dplyr)) 
suppressPackageStartupMessages(library(tidyr)) 
suppressPackageStartupMessages(library(skimr))
suppressPackageStartupMessages(library(gt))
suppressPackageStartupMessages(library(dslabs))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(gapminder))
suppressPackageStartupMessages(library(kableExtra))
suppressPackageStartupMessages(library(openxlsx))
suppressPackageStartupMessages(library(xlsx))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(gt))

suppressPackageStartupMessages(library(parsnip))
suppressPackageStartupMessages(library(tune))
suppressPackageStartupMessages(library(recipes))
suppressPackageStartupMessages(library(workflows))
suppressPackageStartupMessages(library(yardstick))
suppressPackageStartupMessages(library(rsample))

#Load data
data_location <- here::here("data","processed-data","DATE_REGION.xlsx")
date_region <- readxl::read_excel(data_location)

#Narrow down to 6 specific WHO regions
date_region2 <- date_region %>% select(AFR, AMR, EMR, EUR, SEAR, WPR)

#AMR vs. ALL
lin_mod <- linear_reg() %>% set_engine("lm")
linfitA <- lin_mod %>% fit(AMR ~ ., data = date_region2)

tidy(linfitA)

# place results from fit into a data frame with the tidy function
lmtableA <- broom::tidy(linfitA)

#look at fit results
print(lmtableA)

# save fit results table  
table_fileA = here("results", "tables", "lmtableA.rds")
saveRDS(lmtableA, file = table_fileA)



#EUR vs. ALL
lin_mod <- linear_reg() %>% set_engine("lm")
linfitB <- lin_mod %>% fit(EUR ~ ., data = date_region2)

tidy(linfitB)

# place results from fit into a data frame with the tidy function
lmtableB <- broom::tidy(linfitB)

#look at fit results
print(lmtableB)

# save fit results table  
table_fileB = here("results", "tables", "lmtableB.rds")
saveRDS(lmtableB, file = table_fileB)



data_location <- here::here("data","processed-data","REGION_OFFSET.xlsx")
Region_offset <- readxl::read_excel(data_location)

print(Region_offset)

# fit the linear models with AMR as outcome 
# model has ALL OTHER REGIONS as predictors
lin_mod <- linear_reg() %>% set_engine("lm")
linfit1 <- lin_mod %>% fit(AMR ~ EMR + WPR, data = Region_offset)

#model 2 -- includes date variable
lin_mod <- linear_reg() %>% set_engine("lm")
linfit2 <- lin_mod %>% fit(EUR ~ EMR + WPR + AMR, data = Region_offset)

# Compute the RMSE and R squared for model 1
metrics_1 <- linfit1 %>% 
  predict(Region_offset) %>% 
  bind_cols(Region_offset) %>% 
  metrics(truth = AMR, estimate = .pred)

# Compute the RMSE and R squared for model 2
metrics_2 <- linfit2 %>% 
  predict(Region_offset) %>% 
  bind_cols(Region_offset) %>% 
  metrics(truth = EUR, estimate = .pred)

tidy(linfit1)
print(metrics_1)

# place results from fit into a data frame with the tidy function
lmtable1 <- broom::tidy(linfit1)

#look at fit results
print(lmtable1)

# save fit results table  
table_file1 = here("results", "tables", "lmtable1.rds")
saveRDS(lmtable1, file = table_file1)

tidy(linfit2)
print(metrics_2)

# place results from fit into a data frame with the tidy function
lmtable2 <- broom::tidy(linfit2)

#look at fit results
print(lmtable2)

# save fit results table  
table_file2 = here("results", "tables", "lmtable2.rds")
saveRDS(lmtable2, file = table_file2)


#model 3 -- includes date variable
lin_mod <- linear_reg() %>% set_engine("lm")
linfit3 <- lin_mod %>% fit(AMR ~ EMR +WPR + EUR, data = Region_offset)

# Compute the RMSE and R squared for model 3
metrics_3 <- linfit3 %>% 
  predict(Region_offset) %>% 
  bind_cols(Region_offset) %>% 
  metrics(truth = AMR, estimate = .pred)

tidy(linfit3)
print(metrics_3)

# place results from fit into a data frame with the tidy function
lmtable3 <- broom::tidy(linfit3)

#look at fit results
print(lmtable3)

# save fit results table  
table_file3 = here("results", "tables", "lmtable3.rds")
saveRDS(lmtable3, file = table_file3)



#OLD
#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed-data","processeddata.rds")

#load data. 
mydata <- readRDS(data_location)


######################################
#Data fitting/statistical analysis
######################################

############################
#### First model fit
# fit linear model using height as outcome, weight as predictor

lmfit1 <- lm(Height ~ Weight, mydata)  

# place results from fit into a data frame with the tidy function
lmtable1 <- broom::tidy(lmfit1)

#look at fit results
print(lmtable1)

# save fit results table  
table_file1 = here("results", "tables", "resulttable1.rds")
saveRDS(lmtable1, file = table_file1)

############################
#### Second model fit
# fit linear model using height as outcome, weight and gender as predictor

lmfit2 <- lm(Height ~ Weight + Gender, mydata)  

# place results from fit into a data frame with the tidy function
lmtable2 <- broom::tidy(lmfit2)

#look at fit results
print(lmtable2)

# save fit results table  
table_file2 = here("results", "tables", "resulttable2.rds")
saveRDS(lmtable2, file = table_file2)

  