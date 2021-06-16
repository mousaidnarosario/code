###
# Title: Data insights for KPMG task 2 virtual internship program
# Problem Statement: Sprocket Central specializes in high quality bikes and biking accessories 
#                    Marketing team is looking to boost their business sales by analysing given dataset
#                    Identify/recommend 1000 customers that the company should target to grow sales.
# Author: Mousaidna Rosario
# Dataset description : Dataset from KPMG virtual internship program
# Tasks: 2. Data Exploration, 2. Modeling, 3. Interpretation
###



getwd()

setwd("C:/Users/mousa/Documents/apply/port folio/KPMG")


# Interpretation using Visualization : significant variables and co-efficient. 


CustomerAddress <- read.csv('CustomerAddress.csv')
CustomerDemographic <- read.csv('CustomerDemographic.csv')
Transactions <- read.csv('Transactions.csv')


# Identify and recommend top 1000 customers to target from datasets

# Data Analysis
## New and old customer age distribution
# customer orders whether online or instore
## Bike related purchases over the past 3 years by gender
## Understand consumers capability to purchase. Wealth classification by age, from the new and old customers.
#  Consumer classification ( RFM analysis and customer classification) (Table )


# 1. DATA EXPLORATION
# Questions to ask about the problem:
## Identify consumer:
## What are the new and old customer age distribution?
## What the consumer demographics Over the past 3 years? 
## age
## gender
## What are the location of consumers in AU?
## Job industry of consumers 
## Understand the customers' capability to purchase # wealth segmentation
# Identify the impact of choosing these targeted customers to predict sales for the rest of the customer base


#1. Data quality issue identification and mitigation

head(CustomerDemographic)
str(CustomerDemographic)

CustomerDemographic <- CustomerDemographic[complete.cases(CustomerDemographic),]

Transactions[,c("transaction_date","product_first_sold_date")]

str(CustomerAddress)
CustomerAddress <- CustomerAddress[complete.cases(CustomerAddress),]

head(Transactions)
str(Transactions)
Transactions <- Transactions[complete.cases(Transactions),]

#strftime(Transactions$transaction_date,"%m")
#Transactions$transaction_date<- as.Date(Transactions$transaction_date,"%m/%d/%y")

## What are the new and old customer age distribution gender?

# using recency column in transactions, we will understand the most recent transactions according to its quartile.

?any

quantile(Transactions$Recency) # recency quantile near to 0 is the most recent

recent0 <-  Transactions[which(Transactions$Recency  >=0 & Transactions$Recency  < 88),]
recent25 <- Transactions[which(Transactions$Recency  >=89 & Transactions$Recency  < 179),]
recent50 <- Transactions[which(Transactions$Recency  >=180 & Transactions$Recency  < 272),]
recent75 <- Transactions[which(Transactions$Recency  >=273 & Transactions$Recency  <= 363 ),]


#combine first 1st and 2nd quantile : Most recent
recent1 <- rbind(recent0,recent25)
#combine first 3rd and 4th quantile : Most recent
recent2 <- rbind(recent50,recent75)

# get the gender of recent 
#recent0

par(mfrow=c(1,2))

####
## NEW CONSUMERS
####
library(sqldf)
# new consumers by age group by gender 
nag <- sqldf("select cd.gender ,[Age]/10*10 as [AgeGroup]
              ,count(*) as orders,'New' as Status
            from recent1 t, CustomerDemographic cd
            where t.customer_id = cd.customer_id
            group by cd.gender, [Age]/10*10")



# get the statistical summary of new customers orders per gender
aggregate(nag$orders, list(AgeGroup=nag$AgeGroup), summary)
aggregate(nag$orders, list(Species=nag$gender), summary)

#difference in means between new and old

library(ggplot2)

#plot a bar graph according to age group , gender and orders 
ggplot(nag, aes(x = AgeGroup, y = orders,fill=gender)) +
  geom_bar(stat='identity',position = "dodge") + ggtitle ("New customers orders age and gender")

# new consumer by gender
nmg <- sqldf("select cd.gender, count(*) as orders,t.month
      from recent1 t, CustomerDemographic cd
      where t.customer_id = cd.customer_id
       group by  t.month,cd.gender")

####
## OLD CONSUMERS
####

# old consumers by age group by gender 
oag <- sqldf("select cd.gender ,[Age]/10*10 as [AgeGroup]
              ,count(*) as orders,'Old' as Status
            from recent2 t, CustomerDemographic cd
            where t.customer_id = cd.customer_id
            group by cd.gender, [Age]/10*10")

diff.mean <- aggregate(nag$orders, list(AgeGroup=nag$AgeGroup), mean)$x-
  aggregate(oag$orders, list(AgeGroup=oag$AgeGroup), mean)$x

names(diff.mean) <- c(10,20,30,40,50,60,70,80)


barplot(diff.mean,col=topo.colors(8)
        ,ylim = c(-40,10),ylab='Difference',xlab='AgeGroup')

#hypothesis testing new and old

library(ggplot2)

aggregate(oag$orders, list(AgeGroup=oag$AgeGroup), summary)
aggregate(oag$orders, list(Species=oag$gender), summary)

ggplot(oag, aes(x = AgeGroup, y = orders,fill=gender)) +
  geom_bar(stat='identity',position = "dodge") +ggtitle ("Old customers orders age and gender")

## What the consumer demographics Over the past 3 years? 
## age
## gender

purch3 <- sqldf("select cd.gender, [Age]/10*10 as [AgeGroup] ,count(*) as orders
            from Transactions t, CustomerDemographic cd
            where t.customer_id = cd.customer_id
            and cd.past_3_years_bike_related_purchases <> 0
            group by cd.gender, [Age]/10*10")


aggregate(purch3$orders, list(AgeGroup=purch3$AgeGroup), summary)
aggregate(purch3$orders, list(gender=purch3$gender), summary)

ggplot(purch3, aes(x = AgeGroup, y = orders,fill=gender)) +
  geom_bar(stat='identity',position = "dodge") +ggtitle ("Consumer purchases from the past 3 years age and gender")
  

## What are the location of consumers in AU?

aggregate(loc.gender$orders, list(state=loc.gender$state), summary)

loc.gender <- sqldf("select c.state,cd.gender, count(t.customer_id) orders
      from CustomerAddress c, Transactions t, CustomerDemographic cd
      where t.customer_id=c.customer_id
      and cd.customer_id=c.customer_id
      group by state,cd.gender")

ggplot(loc.gender, aes(x = state, y = orders,fill=gender)) +
  geom_bar(stat='identity',position = "dodge") +ggtitle ("Consumer purchases from different state")

gender <- table(CustomerDemographic$gender)
pct = round(gender/sum(gender)*100)
b = paste(c('Female','Male'), pct,'%',sep=" ")

install.packages('plotrix')
library(plotrix)

## Job industry of consumers 

industry <- sqldf("select cd.job_industry_category as industry, count(t.customer_id) orders
      from  Transactions t, CustomerDemographic cd
      where t.customer_id=cd.customer_id
      group by cd.job_industry_category
                  order by orders desc")

# new customer industry

industryN <- sqldf("select cd.job_industry_category as industry, count(t.customer_id) orders,'new' as Status
      from  recent1 t, CustomerDemographic cd
      where t.customer_id=cd.customer_id
      and cd.job_industry_category <> 'n/a'
      group by cd.job_industry_category
                  order by orders desc")

ggplot(industryN, aes(x = industry, y = orders,fill=orders)) +
  geom_bar(stat='identity',position = "dodge") +ggtitle ("New Consumers sales per Industry")


industryO <- sqldf("select cd.job_industry_category as industry, count(t.customer_id) orders,'old' as Status
      from  recent2 t, CustomerDemographic cd
      where t.customer_id=cd.customer_id
      and cd.job_industry_category <> 'n/a'
      group by cd.job_industry_category
                  order by orders desc")


ggplot(industryO, aes(x = industry, y = orders,fill=orders)) +
  geom_bar(stat='identity',position = "dodge")  +ggtitle (" Old Consumers sales per Industry")

#difference in each industry from old and new customers
diff.mean.Ind <- aggregate(industryN$orders, list(IndustryNew=industryN$industry), mean)$x-
  aggregate(industryO$orders, list(Industry0ld=industryO$industry), mean)$x

names(diff.mean.Ind) <- industryO$industry


barplot(diff.mean.Ind,col=topo.colors(8)
        ,ylim = c(-69,10),ylab='Difference',xlab='AgeGroup', main="Old vs New customer per industry")

indst <- rbind(industryN,industryO)
ggplot(indst, aes(x = industry, y = orders,fill=Status)) +
  geom_bar(stat='identity',position = "dodge") + scale_fill_brewer(palette="Set1")
  #+ggtitle (" Old Consumers sales per Industry")


aggregate(indst$orders , list(Industry=indst$industry ), summary)

# consumer wealth segmentation 

wsA <- sqldf("select cd.wealth_segment, [Age]/10*10 as [AgeGroup] ,count(*) as orders
            from Transactions t, CustomerDemographic cd
            where t.customer_id = cd.customer_id
            group by cd.wealth_segment, [Age]/10*10")

ggplot(wsA, aes(x =  AgeGroup, y =orders    ,fill=wealth_segment)) +
  geom_bar(stat='identity',position = "dodge") + scale_fill_brewer(palette="Set1")
  +ggtitle (" Consumer sales by Wealth segment and age group")

aggregate(wsA$orders , list(Industry=wsA$wealth_segment ), summary)

smi <- sqldf("select cd.job_industry_category as industry, count(t.customer_id) orders,cd.wealth_segment
      from  recent2 t, CustomerDemographic cd
      where t.customer_id=cd.customer_id
      and cd.job_industry_category <> 'n/a'
      group by cd.job_industry_category,cd.wealth_segment
                  order by orders desc")


aggregate(smi$orders  , list(industry=smi$industry ), summary)



ggplot(smi, aes(x = industry, y = orders,fill=wealth_segment)) +
  geom_bar(stat='identity',position = "dodge")  + scale_fill_brewer(palette="Set1")
   +ggtitle ("Wealth Segmentation by Industry")


# customers and car ownership

carOwners <- sqldf("select cd.wealth_segment, cd.owns_car ,count(t.customer_id) as orders
            from Transactions t, CustomerDemographic cd
            where t.customer_id = cd.customer_id
            group by cd.wealth_segment, owns_car")


ggplot(carOwners, aes(x =  wealth_segment, y =orders    ,fill=owns_car)) +
  geom_bar(stat='identity',position = "dodge") + scale_fill_brewer(palette="Set1")
+ggtitle ("Car owners by transactions and wealth segmentation")


state.car <- sqldf("select c.state,cd.owns_car ,count(t.customer_id) as orders
      from CustomerAddress c, Transactions t, CustomerDemographic cd
      where t.customer_id=c.customer_id
      and cd.customer_id=c.customer_id
      group by state,cd.owns_car")


ggplot(state.car, aes(x =  state , y =orders    ,fill=owns_car )) +
  geom_bar(stat='identity',position = "dodge") + scale_fill_brewer(palette="Set1")
+ggtitle ("Car owners by transactions and wealth segmentation")


oAge20s <- sqldf("select count(*) as Orders , t.product_line, t.product_class
      from CustomerDemographic c, Transactions t
      where t.customer_id = c.customer_id
        group by t.product_line,t.product_class
      order by Orders")

#and c.Age between 40 and 60
ggplot(oAge20s, aes(x = product_line, y = Orders,fill=product_class)) +
  geom_bar(stat='identity',position = "dodge")  +ggtitle ("Popular product lines")

brands <- sqldf("select count(*) as Orders , t.brand,t.product_class
      from CustomerDemographic c, Transactions t
      where t.customer_id = c.customer_id
        group by t.product_line,t.product_class
      order by Orders")

brands.class <- sqldf("select sum(t.profit) as sales, t.brand,t.product_class
      from CustomerDemographic c, Transactions t
      where t.customer_id = c.customer_id
        group by t.product_line,t.product_class
      order by sales")

ggplot(brands.class, aes(x = brand, y = sales,fill=product_class)) +
  geom_bar(stat='identity',position = "dodge")  +ggtitle ("Sales of bicycle brands")

ggplot(brands, aes(x = brand, y = Orders,fill=product_class)) +
  geom_bar(stat='identity',position = "dodge")  +ggtitle ("Popular bicycle brands")

#and c.Age between 40 and 60

barplot(as.vector(brands$Orders), names.arg = as.vector(oAge20s$brand), 
        col = topo.colors(4), main='Most popular Brands', ylab='Orders', xlab='Brands')

barplot(as.vector(oAge20s$Orders), names.arg = as.vector(oAge20s$product_line), 
        col = topo.colors(4), main='Most popular product line', ylab='Orders', xlab='Product line')

# wealth willing to pay

ws <- sqldf("select avg(t.profit) as sales, cd.wealth_segment
      from  recent2 t, CustomerDemographic cd
      where t.customer_id=cd.customer_id
      and cd.job_industry_category <> 'n/a'
      group by cd.wealth_segment
                  order by sales desc")


barplot(as.vector(ws$sales), names.arg = as.vector(ws$wealth_segment), 
        col = c('red','green','blue'), main='Wealth Segment spending', ylab='sales', xlab='Wealth segmentation')





ageg <- sqldf("select avg(t.profit) as sales, [Age]/10*10 as [AgeGroup]
      from  recent2 t, CustomerDemographic cd
      where t.customer_id=cd.customer_id
      and cd.job_industry_category <> 'n/a'
      group by AgeGroup
                  order by sales desc")

barplot(as.vector(ageg$sales), names.arg = as.vector(ageg$AgeGroup), 
        col = c('red','green','blue'), main='Age group spending', ylab='sales', xlab='Age groups')


# 2.  MODEL DEVELOPMENT
# hypothesis related to the business questions answered by the data. Perform statistical testing to determine the hypothesis or not
# Test the performance of the model using evaluation models like residual deviance,AIC, curves, and Rsquard. 

str(Transactions)

# Does wealth segmentation affect sales?

#H0: Wealth segmentation does not result in Sprocket's sales increase.
#HA: Wealth segmentation results in a statistically significant increase in Sprocket's sales

ws.testing <- sqldf("select t.profit as sales, cd.wealth_segment
      from  recent2 t, CustomerDemographic cd
      where t.customer_id=cd.customer_id
      and cd.job_industry_category <> 'n/a'
                  order by sales desc")

head(ws.testing)

aov.wealth <- aov(sales ~ wealth_segment , data = ws.testing)
summary(aov.wealth)
TukeyHSD(aov.wealth)


# Does the product lines affect increase in sales?

pl.testing <- sqldf("select t.profit as sales , t.product_line
      from  recent2 t, CustomerDemographic cd
      where t.customer_id=cd.customer_id
      and cd.job_industry_category <> 'n/a'
                  order by t.product_line desc")


head(pl.testing)
aov.pl <- aov(sales ~ product_line , data = pl.testing)
summary(aov.pl)
TukeyHSD(aov.pl)


# Does the product class affect bicycle sales?


pc.testing <- sqldf("select t.profit as sales , t.product_class
      from  recent2 t, CustomerDemographic cd
      where t.customer_id=cd.customer_id
      and cd.job_industry_category <> 'n/a'
                  order by t.product_class desc")


head(pc.testing)
aov.pl <- aov(sales ~ product_class , data = pc.testing)
summary(aov.pl)
TukeyHSD(aov.pl)

#Does having a car decreases Sprocket sales?

c.testing <- sqldf("select t.profit as sales , cd.owns_car
      from  recent2 t, CustomerDemographic cd
      where t.customer_id=cd.customer_id
      and cd.job_industry_category <> 'n/a'
                  order by cd.owns_car desc")

t.test(sales~owns_car, data= c.testing)


head(c.testing)
aov.pl <- aov(sales ~ owns_car , data = c.testing)
summary(aov.pl)
TukeyHSD(aov.pl)


###
install.packages("descriptr")
library(rfm)
#rfm_launch_app()

rfm_data_orders


#install.packages('lubridate')

library(lubridate)

str(Transactions)

Transactions$transaction_date <-  lubridate::as_datetime(Transactions$transaction_date)  #as.Date(Transactions$transaction_date)


#analysis_date <- ('2017/12/30')
analysis_date <- lubridate::as_datetime("2017-12-30") #, tz = "UTC")
rfm_result <- rfm_table_order(Transactions, customer_id, transaction_date, profit, analysis_date)
str(rfm_result)

view(segmented.users)

write.csv(segmented.users, "segmentedusers.csv")

cust[order(-cust.add$customer_id),]

#segmentation
segment_names <- c("Champions", "Loyal Customers", "Potential Loyalist",
                   "New Customers", "Promising", "Need Attention", "About To Sleep",
                   "At Risk", "Can't Lose Them", "Hibernating","Lost")



recency_lower <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
recency_upper <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1) # change loyalty 2-5,  
frequency_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
monetary_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
monetary_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2) # made upper monetary to 5 , covering 525, combination 

segmented.users <- rfm_segment(rfm_result, segment_names, recency_lower, recency_upper,
            frequency_lower, frequency_upper, monetary_lower, monetary_upper)

length(subset(segmented.users$customer_id, segmented.users$segment=="Others")) #401 >> 239 others, not classified users.

# checking how many top customers in the data
c1 <- length(subset(segmented.users$customer_id, segmented.users$segment=="Champions"))
c2 <- length(subset(segmented.users$customer_id, segmented.users$segment=="Potential Loyalist")) 
c3 <- length(subset(segmented.users$customer_id, segmented.users$segment=="Loyal Customers")) 


view(segmented.users)

(sum(c1,c2,c3)/3490)*100 # 1914 enough top 1000 users to focus on.

#others 215,314,115; bought long time, once but huge amount # can consider as Potential loyalist
# the rest of others can be decided by the business

others.segment<- segmented.users[ which(segmented.users$segment=='Others'), ]
view(others.segment)



write.csv(segmented.users, "segmented.users.csv")

rfm_heatmap(rfm_result)
rfm_bar_chart(rfm_result)
rfm_histograms(rfm_result)
rfm_rm_plot(rfm_result)
rfm_order_dist(rfm_result)

#recency

view(segmented.users)
rfm_plot_median_recency(segmented.users)
# frequency
rfm_plot_median_frequency(segmented.users)
#monetary
rfm_plot_median_monetary(segmented.users)

## what are the things that can affect us finding the top 1000 customers?
str(Transactions)
str(CustomerDemographic)
str(CustomerAddress)


# 1. We understand from the prior graphs that :
 # there's a slight decrease in sales
 # more customers comes from 40,50,60 age group
 # Customers are from the professional/working age, starting at 20 above.
 # There are more female customers.
 # NSW customers are the biggest state purchasers
 # Manufacturing and Finance and Health industries are the top bicycle equipment
 # The affluent and high networth comes from Manufacturing, Finance and health, ages 40-60 years old.
 # 

segments <- read.csv('segmented.users.csv')
str(segments)
str(summarySegment)

sg <- sqldf('select count(*) as NoofCustomers, s.segment
      from segments s
      group by segment')


activity <-c("Bought recently, buy often and spend the most!","Spend good money with us often. Responsive to promotions.","Recent customers, but spent a good amount and bought more than once.","Bought most recently, but not often.","Recent shoppers, but haven't spent much.","Above average recency, frequency and monetary values. May not have bought very recently though.","Below average recency, frequency and monetary values. Will lose them if not reactivated.","Spent big money and purchased often. But long time ago. Need to bring them back!","Made biggest purchases, and often. But haven't returned for a long time.","Last purchase was long back, low spenders and low number of orders.","Lowest recency, frequency and monetary scores.") 

#matrix(c(segment_names,activity),ncol=2)


write.csv(sg,file='segments.csv')


summarySegment <- data.frame(cbind("segment"=segment_names,"activity"=activity))

#brands
# what does age 20 below buy? what product id?
# Does product line affects the price of sales?
# what does female customers wanted?
# how much does high networth willing to pay?
# how much does affluents are willing to pay?
# how much does mass are willing to pay?
