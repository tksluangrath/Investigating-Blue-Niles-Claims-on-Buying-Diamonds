# Bring in Tidyverse package
library(tidyverse)
library(GGally)
# Read in data
Diamonds<-read.csv("diamonds4.csv", header=TRUE)

# Factor the items that need to be factored
Diamonds<-Diamonds%>%
  mutate(cut=cut%>%
           fct_relevel(c("Good","Very Good","Ideal","Astor Ideal")))%>%
  
# Using Humaid's carat bins.  Fairly different prices for chart, so break into several like
# groups that can be graphically depicted together with same x-y axis values.
  
carat_Bin_025_075 <- Diamonds %>%
  filter(carat<=0.75)%>%
  mutate(carat_bin = cut(carat, breaks = seq(0, 0.75, by = 0.25), right = FALSE))

carat_Bin_075_15 <- Diamonds %>%
  filter(carat>0.75)%>%
  mutate(carat_bin = cut(carat, breaks = seq(0.75, 1.5, by = 0.25), right = FALSE))

carat_Bin_15_2 <- Diamonds %>%
  filter(carat>1.5)%>%
  mutate(carat_bin = cut(carat, breaks = seq(1.5, 2, by = 0.25), right = FALSE))

carat_Bin_2_4 <- Diamonds %>%
  filter(carat>2)%>%
  mutate(carat_bin = cut(carat, breaks = seq(2, 4, by = 1), right = FALSE))

carat_Bin_max <- Diamonds %>%
  filter(carat>4)%>%
  mutate(carat_bin = cut(carat, breaks = seq(4, max(carat), by = 1), right = FALSE))

# Graph the bins.  
# The goal is to approximately determine the difference between the cuts at the q75 level inside of each bin increment.

carat_Bin_025_075%>%
  drop_na(cut,price,carat_bin)%>%
  ggplot(aes(x=cut, y=price))+
  geom_boxplot(fill="gold")+
  theme(plot.title = element_text(hjust=0.5))+
  coord_cartesian(ylim=c(0,4000))+
  facet_wrap(~carat_bin)+
  labs(x="Diamond Cut", y="Diamond Price",
       title="Diamond Price by Cut")

carat_Bin_075_15%>%
  drop_na(cut,price,carat_bin)%>%
  ggplot(aes(x=cut, y=price))+
  geom_boxplot(fill="gold")+
  theme(plot.title = element_text(hjust=0.5))+
  coord_cartesian(ylim=c(1000,15000))+
  facet_wrap(~carat_bin)+
  labs(x="Diamond Cut", y="Diamond Price",
       title="Diamond Price by Cut")

carat_Bin_15_2%>%
  drop_na(cut,price,carat_bin)%>%
  ggplot(aes(x=cut, y=price))+
  geom_boxplot(fill="gold")+
  theme(plot.title = element_text(hjust=0.5))+
  coord_cartesian(ylim=c(8000,20000))+
  facet_wrap(~carat_bin)+
  labs(x="Diamond Cut", y="Diamond Price",
       title="Diamond Price by Cut")

carat_Bin_2_4%>%
  drop_na(cut,price,carat_bin)%>%
  ggplot(aes(x=cut, y=price))+
  geom_boxplot(fill="gold")+
  theme(plot.title = element_text(hjust=0.5))+
  coord_cartesian(ylim=c(16000,60000))+
  facet_wrap(~carat_bin)+
  labs(x="Diamond Cut", y="Diamond Price",
       title="Diamond Price by Cut")

carat_Bin_max%>%
  drop_na(cut,price,carat_bin)%>%
  ggplot(aes(x=cut, y=price))+
  geom_boxplot(fill="gold")+
  theme(plot.title = element_text(hjust=0.5))+
  coord_cartesian(ylim=c(50000,300000))+
  facet_wrap(~carat_bin)+
  labs(x="Diamond Cut", y="Diamond Price",
       title="Diamond Price by Cut")

# At the 0-0.25 carat level, the change between the q75 price between the lowest value
# cut (very good), and the highest value cut (Astor Ideal), is (~562(Astor Ideal)-375(Very good))
# for a total of $187.

# At the 0.25-0.5 carat range, we're omitting Astor ideal for lack of product.  The change between
# the q75 price of the lowest value cut in this range (Ideal) and the highest value cut in this
# range (Good), is (~812(Good)-~750(Ideal)) for a total of $62.

# At the 0.5-0.75 carat range, the change between the q75 price of the lowest value cut in this range (Good)
# and the highest value cut in this range (Astor Ideal), is (~2200(Astor Ideal)~1750(Good)) for a total of
# $450.

# At the 0.75-1 carat range, the change for the q75 price between the lowest value cut (Very Good) and the
# highest value cut (Ideal) is (~5320(Ideal)-~4000(Very Good)) for a total of $1320.

# At the 1-1.25 carat range, the change for the q75 price between the lowest value cut (Good) and the
# highest value cut (Ideal) is (~8000(Ideal)-~5000(Good)) for a total of $3000.

# At the 1.25-1.5 carat range, the change for the q75 price between the lowest value cut and the
# highest value cut (between Ideal and Very good), is negligible.

# At the 1.5-1.75 carat range, the change for the q75 price between the lowest value cut (Very Good) and the
# highest value cut (Ideal) is (~16600(Ideal)-~11600(Very Good)) for a total of $5000.

# At the 1.75-2 carat range, the change for the q75 price between the lowest value cut (Very Good) and the
# highest value cut (Ideal) is (~18125(Ideal)-~17800(Very Good)) for a total of $325.


# Look at prices for different bin ranges.
# The goal is to determine the price increase for carat for every 0.25 carat range increase.
carat_bin_range <- Diamonds %>%
  filter(carat<=2)%>%
  mutate(carat_bin = cut(carat, breaks = seq(0, 2, by = 0.25), right = FALSE))%>%
  drop_na(cut,price,carat_bin)

ggplot(carat_bin_range, aes(x=carat_bin, y=price))+
  geom_boxplot(fill="lightblue")+
  theme(plot.title = element_text(hjust=0.5))+
  coord_cartesian(ylim=c(0,20000))+
  labs(x="Diamond Carat Bin", y="Diamond Price",
       title="Diamond Carat Bin")

# The difference between the q75 for 0-0.25 carat and q75 for 0.25-0.5 carat is
# approximately $150.  The difference between q75 for 0.25-0.5 carat and
# 0.5-0.75 carat ranges is ~$1,125.  The difference between q75 of 0.5-0.75 carat
# and q75 of 0.75-1 carat is ~$2,625.  The difference between the q75 of the
# 0.75-1 carat and 1-1.25 carat range is ~$2,500.  The difference between the
# q75 of the 1-1.25 carat range and 1.25-1.5 carat range is ~$4,750.
# The difference between the q75 of the 1.25-1.5 range and the 1.75-2 carat range is
# ~$6,250.  This carat range includes ~93% of the data.

# Prices were compared by analyzing the internal change in price based on cut compared to the
# external change in price based on carat.  This was done by zooming in graphs where required
# and analyzing the difference in price using the q75 mark, which constitutes the value under which
# 75% of the data falls and is depicted by the top horizontal line of each boxplot.  For the
# internal variation, the "most expensive" q75 mark for cut was read, typically "Ideal," which
# had subtracted from it the "least expensive" q75 mark for cut, typically "very good" or "good."
# This value represents the price increase between the q75 marks of the "most" and "least" expensive
# cut in each carat grouping.  For carat grouping, the q75 mark of the next grouping is subtracted
# from the q75 marking of the former grouping.  This value represents the increase in q75 value when raising
# the carat into the next 0.25 carat wide grouping.

# In a majority of cases, increasing the carat size into the next 0.25 carat wide grouping raises 
# the price more than increasing the quality within the 0.25 carat wide grouping.  An additional note
# is that on rare occasion, the most expensive cut q75 in a range will not be the highest quality cut.
# For example, at the 0.25-0.5 carat range, the good diamond q75 was the most expensive.  When increasing the quality
# from "very good" to ideal" in the 1.5-1.75 carat range, the q75 price increases by $5,000, which is the highest
# internal increase attributed to changing cut.  However, when increasing the carat range from 1.5-1.75 to
# the next 0.25 carat wide grouping of 1.75-2, the q75 value increases by approximately $6,250.  Based on
# the data in these graphs, carat may be more important than cut for price.  This is evidence against the claim
# that cut is the most important variable in price.