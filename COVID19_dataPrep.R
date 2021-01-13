#COVID-19 Data Set Pre-Processing Steps
#---------------------------------------

#Install or load packages
library(tidyverse)
library(psych)
install.packages("skimr")
library(skimr)
install.packages("ISLR")
library(ISLR)
library(readxl)

#read excel COVID-19 data set
covid19_original <- read_excel("covid19_original.xlsx")
View(covid19_original)

str(covid19_original)

#read the starting few lines of data
head(covid19_original)
head(covid19_original,10)

#read the ending few lines of data
tail(covid19_original)
tail(covid19_original,10)

#to know the structure of your data set
glimpse(covid19_original)
summary(covid19_original)
describe(covid19_original)
skim(covid19_original)

#remove the dupicate values from the selected columns
corona_deduped <- covid19_original[!duplicated(covid19_original[c("reporting date","location","country","gender","age","symptom_onset","If_onset_approximated","hosp_visit_date","exposure_start","exposure_end","visiting Wuhan","from Wuhan","death","recovered")]),]

#reducing the number of dimensions(Redundant/irrelevant attributes eliminated)
library(dplyr)
corona_deduped <- select(corona_deduped, -1:-2, -4:-5, -10:-11, -19:-21)

#to export the dataframe to .csv file
write.csv(corona_deduped, "covid19_cleaned.csv")

#missing values(NA's) - for columns "death" and "recovered" were fixed manually in excel "covid19_cleaned.csv"

#to fix missing values in other columns from file "covid19_cleaned.csv"
#libraries
library(mice)

#loaded the file "covid19_cleaned.csv"
covid19_cleaned <- read.csv(file.choose(), header = T)
str(covid19_cleaned)
summary(covid19_cleaned)

#to identify the NA's in the "covid19_cleaned" data frame
which(is.na(covid19_cleaned))

#if you want to omit the rows that contain the NA
#in our case we will not be eliminating any rows with NA's to avoid losing useful information
#applied the rule just to check how many rows remain if we drop the NA rows
covid19_NAs_dropped <- na.omit(covid19_cleaned)
#it shows that only 60 observations remaining after deleting the rows with NA
#in our case, it will be a bad decision to drop rows with NA. We have to come up with a different strategy

#replacing the NA's in age column with mean value
covid19_cleaned$age[which(is.na(covid19_cleaned$age))] <- mean(covid19_cleaned$age,na.rm=TRUE)
summary(covid19_cleaned)

#kNN Imputation for remaining columns having missing values(NA)
install.packages("VIM")
library(VIM)

covid19_cleaned <- kNN(covid19_cleaned, variable = c("gender", "reporting.date", "hosp_visit_date", "exposure_start", "exposure_end", "from.Wuhan"), k=6)
summary(covid19_cleaned)

covid19_cleaned <- select(covid19_cleaned, -13:-18)

#writing or saving the cleaned complete dataframe to a csv file
write.csv(covid19_cleaned, "covid19_cleaned.csv")

#creating a random samples subset for analysis purpose
#ideal sample size taken as 10% of the population(approximately)
install.packages("dplyr")
library(dplyr)

covid19_sample <- sample_n(covid19_cleaned, 100)

#writing or saving the sampled dataframe to a csv file
write.csv(covid19_sample, "covid19_sample.csv")
