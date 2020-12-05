###
# Title: Significance testing of data distribution for USA housing dataset using chi square test
# Problem Statement: Is there any statistically significant differences in the distribution of sales and houses 
#                    sold from 2006-2009 in the USA housing?
# Author: "Mousaidna Rosario"
# Dataset description : Dataset from Kaggle.com called Housing prices dataset employing sqldf library
# Import data > Data cleaning > Data Transformation > Data Visualization > Hypothesis testing > Results 
###


# Step 1. Import data 

housing <- read.csv("housingdataset.csv")

head(housing) # show the first 5 data of the data
dim(housing)  # summary dimension of data,(rows, columns)
summary(housing) # quick view of how the data looks, according to mean, median, min and max


# sTEP 2 . Data cleaning

# Display structure of data
str(housing) # Initially, 1460 observed data, 81 columns

# Pull important columns from the dataset and put it to another table for ease of use. 
# The dataset is big with 81 columns and it gets harder to process if we don't remove unnecessary data in the model.

housing <-housing[,c("LotArea","YearBuilt","YrSold","SalePrice")]

str(housing) # from 81 columns to 5 important variables to the Problem
summary(housing)
plot(housing )  # initial plot, will show you if the data will fit a linear model. 


# Identify rows with missing data
# Pull data that has no missing values
# Missing values can cause inaccurate data

housing<- housing[complete.cases(housing),]

str(housing) # no data is removed because all variables have values.

#sort data to Year sold

housing <- housing[order(-housing$YrSold),]

head(housing)


# STEP 3: Data Transformation

# count years in Yearbuilt

# I observed that the data  in 2010 is incomplete with 6 months worth of data
# Hence, it's not worth comparing it at this time, Otherwise, we can predict data for the next months
# However, this is not the scope of the data exploration; hence, we will eliminate data from 2010 in the dataset
# Otherwise it will affect data analysis due to incomplete data

length(unique(housing$YrSold))

library(sqldf)

#count data by year Easier to use sql for this using sqldf library


# count sold house per year

head(housing)


CntSold <- sqldf("SELECT YrSold
                   ,COUNT(*) as [CountHouseSold]
                    FROM housing
                    WHERE YrSold BETWEEN 2006 AND 2009
                    GROUP BY YrSold
                    ORDER BY YrSold ")
CntSold

# Step 4: Visualize the data

barplot(CntSold$CountHouseSold, beside= T, 
        names.arg = c(2006,2007,2008,2009)
        , ylim=c(0,400)
        , col=heat.colors(5)
        ,main='Sold houses from 2006-2009'
        ,xlab='Years'
        ,ylab='Count')


# STEP 5: Formulate a hypothesis 

# Test to determine if a statistically significant difference in the distribution of houses sold from 2006-2009.

#h0 :  No differences in the distribution of houses sold from 2006-2009
#ha :  Significant differences in  distribution of data from 2006-2009

# Step 6: Test the hypothesis

chisq.test(CntSold,rescale.p=TRUE)

chisq.test(CntSold,rescale.p=TRUE, simulate.p.value = TRUE,B = 1000)

# Step 7 : Result declaration

# The result of the test shows that the the p-val is 0.6076, higher than the significant
# value of 0.05, which means we do not reject the null. Therefore, there we accept the null that
# there is no difference in the distribution of houses sold from 2006-2009

# Sales per year

# statistical summary of sales from 2006-2009

aggregate(housing$SalePrice, list(YearSold=housing$YrSold), summary)[1:4,] # show from 2006-2009 only

YearSales <- sqldf("SELECT YrSold
                      ,SUM(SalePrice) as [TotalSales]
                          FROM housing
                          WHERE YrSold BETWEEN 2006 AND 2009
                          GROUP BY YrSold
                          ORDER BY YrSold")

YearSales


# plot sales
barplot(YearSales$TotalSales,beside= T,
        names.arg =  c(2006,2007,2008,2009)
        , col=heat.colors(5)
        ,main= 'Total sales from 2006-2010'
        ,xlab='Years'
        ,ylab='Sales')



# total sales per year


yr2006 <- subset(housing$SalePrice, housing$YrSold==2006)
yr2007 <- subset(housing$SalePrice, housing$YrSold==2007)
yr2008 <- subset(housing$SalePrice, housing$YrSold==2008)
yr2009 <- subset(housing$SalePrice, housing$YrSold==2009)

boxplot(cbind(yr2006,yr2007,yr2008,yr2009)
        ,main='Total house sales in the USA\nfrom 2006-2009'
        ,col=heat.colors(5))

abline(h=mean(housing$SalePrice),col = 'blue',
       lwd = 2, lty = 2)


# Test to determine if a statistically significant difference in the distribution of sales exists from 2006 and 2009

#h0 :  No differences in the distribution of sales from 2006-2009
#ha :  Significant differences in  distribution of sales from 2006-2009

chisq.test(YearSales, rescale.p=TRUE)

chisq.test(YearSales,rescale.p=TRUE, simulate.p.value = TRUE,B = 1000)


# The result of the testing shows that the the p-val is 0.0001213, lower than the significant
# value of 0.05, which means we do reject the null. Therefore, there we accept the null that
# there is a statistical significant difference in the distribution of houses sales from 2006-2009
