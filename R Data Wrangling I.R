# Data Wrangling Exercise 1: Basic Data Manipulation

# Import and inspect the refine csv file
#getwd()
setwd("/Users/vijaygopalakrishnan/R Programming")
refine <- read.csv("refine_original.csv")
refine
str(refine)
head(refine)

# 1) To clean up the first column 'company'. convert the field values into lower case
# using gsub clean up the column into standardied values philips, akzo, van houten and unilever
refine$company <- tolower(refine$company)
refine

library(dplyr)
refine[,1] <- gsub(".*\\ps", "philips", refine$company) 
refine[,1] <- gsub(".*ver", "unilever", refine$company) 
refine[,1] <- gsub("va.*\\s*", "van houten", refine$company)  
refine[,1] <- gsub("ak.*\\s*.*[0-9]*.*", "akzo", refine$company)
refine
#com <-refine$company
#com

#refine[,1] = tolower(refine[,1])
#refine[] <- lapply(refine, tolower)

# 2) Seperate product code and number from the second field
# Method1, extract values for individual columns using substr and use cbind to combine with refine dataframe
pnum <- refine$`Product code / number`
pnum
product <- substr(pnum, start =1, stop =1)
product
number <- substr(pnum, start =3, stop = 4)
number
refine2 <- cbind(refine[], product, number)
refine2

# Method2, use strsplit & rbind and a do.call to create df, name the columns and attach to the refine df
x <- do.call(rbind, strsplit(as.vector(refine$`Product code / number`), split = "-"))
x
colnames(x) <- c("product", "number")
refine2 <- cbind(refine[], x)
refine2

# 3), Add product categories using Recode function. Need to install "car" package
#install.packages("car")
library(car)
refine2$productcategory <- recode(refine2$product, "'p' = 'Smartphone'; 'v' = 'TV'; 'x' = 'Laptop'; 'q' ='Tablet'")
refine2

# 4) Add full address for geocoding

# method 1, using paste function
attach(refine2)               
refine2$full_address <- paste(address,city,country, sep=",")
refine2
detach(refine2)

# method 2, using within and paste
refine2 <- within(refine2, full_address <- paste(address,city,country,sep="-"))
refine2

# 5) Create dummy variables for company and product category

library(caret)
#?dummyVars
#dummies <- model.matrix( ~ company + productcategory , data = refine2 )
dummies <- dummyVars(~ company + productcategory, data = refine2, sep ="_")
head(predict(dummies, newdata = refine2), n=50)
refine_clean <- cbind(refine2[], predict(dummies, newdata = refine2))
refine_clean

names(refine_clean) [10:17] <- c("company_akzo", "company_philips", "company_unilever", "company_van_houten",
                                 "product_laptop","product_smartphone","product_tablet", "product_tv")

#refine_clean[, c(1,9:17)]

write.csv(refine_clean, file = "refine_clean.csv")

