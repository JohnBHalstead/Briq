####################################################################################
#### MVP Model for Briq
#### John B. Halstead, Ph.D.
#### Nov 18 2019
####################################################################################

### My standard libraries (they give me the flexibility to use ML and SQL in case I need them)

library(stats)
library(dplyr)
library(data.table)
library(zoo)
library(forecast)
library(e1071)
library(quantmod)
library(quadprog)
library(randomForest)
library(car)
library(ggplot2)
library(caret)
library(pROC)
library(stringr)

### Upload Data frames

BaseData <- read.csv("~/data/CityCouncilPlanNotes.csv", header = TRUE, sep=",") # Converted xls file to csv and only kept the filtered workbook
head(BaseData)
length(BaseData)
nrow(BaseData)

CostData <- read.table("~/data/SFHousingCosts.txt", header = TRUE) # Data obtained from https://www.propertyshark.com/Real-Estate-Reports/2017/09/28/expensive-zip-codes-san-francisco/
# Will use data to estimate potential value and score for zipcodes that have higher marginal returns
head(CostData)
length(CostData)
nrow(CostData)

### The description column in CostData contains square foot information and non residential information
### We can use it screen out rows and then deal with the remaining rows
### Our contractor is only interested in buildings greater then 50K SqFeet and non residential, these are our screening criteria
### That space constraint eliminates residential and if no square feet available, we don't have sufficient information
### So, let's extract relevant square feet numbers from the BaseData, basically use SqFeetData as the operator to reduce and calculate

SqFeetData <- BaseData$description
SqFeetData <- as.character(SqFeetData)
head(SqFeetData)

## Only grab rows with square feet, sf, s/f, and sq. ft.

keeper <- grepl("square|sf|s/f|sq", SqFeetData) # logical statement if the row mentions square feet in any form
table(keeper)["TRUE"] # parsed file down to 151 rows

## update BaseData

BaseData <- BaseData[keeper,]
head(BaseData)
length(BaseData)
nrow(BaseData)

## update sqfeetdata

SqFeetData <- BaseData$description
SqFeetData <- as.character(SqFeetData)
head(SqFeetData)

## only grab non residential (logic is find residential and then keep the opposites)

keeper2 <- grepl("residential|Residential", SqFeetData) # logical statement if residential is mentioned
table(keeper2)["FALSE"] # parsed file down 98 rows
keeper2 <- !keeper2
table(keeper2)["TRUE"] # parsed file down 98 rows

## update BaseData

BaseData <- BaseData[keeper2,]
head(BaseData)
length(BaseData)
nrow(BaseData)

## update sqfeetdata

SqFeetData <- BaseData$description
SqFeetData <- as.character(SqFeetData)
head(SqFeetData)


## removed the commas and periods to capture all complete numbers

SqFeetData <- gsub("[,]", "", SqFeetData)
SqFeetData <- gsub("[.]", "", SqFeetData)
head(SqFeetData)

## extract approximate sum of square feet data and place in a vector x

x <- NULL
for (i in 1:length(SqFeetData)) {
  numbers <- as.numeric(str_extract_all(SqFeetData[i], "[0-9]+")[[1]])
  numbers <- sum(numbers)
  x <- append(x, numbers)
  
}

#### Append the x vector (approx sq. ft.) to the BaseData to add an approximate sq/ft estimate (not the best but it will do)

BaseData['ApproxSqFt'] <- x
head(BaseData)

#### Merge the data to form a MVP (Value Model)

MergedData <- merge(BaseData, CostData, by.x = "Zip", by.y = "ZipCode", all = TRUE)
MergedData = MergedData[complete.cases(MergedData), ] # remove NA rows
head(MergedData)

#### creat a value score (could spend hours developing a more complet way of doing this, but this will do for now)
#### Value score will use the ApproxSqFt, 2017PpSqFt, and the Change
#### ValueScore = ApproxSqFt*2017PpSqFt*(1+Change)

MergedData$ValueScore <- with(MergedData, ApproxSqFt*X2017PpSqFt*(1+Change))

colnames(MergedData)
head(MergedData)

#### Order data from highest to lowest value score

Preferences <- MergedData[order(-MergedData$ValueScore) , ]
head(Preferences)

#### Export to data folder

write.csv(Preferences, file = "~/data/preferences.csv")






