rm(list = ls())

# Bring in Tidyverse package
library(tidyverse)
# Read in data
Diamonds<-read.csv("diamonds4.csv", header=TRUE)

# Diamond cuts refer to the proportion, dimensions, and faceting of a diamond. 
# The cuts of a diamond are descriptive of the quality of the gem's faceting,
# proportion, and polish, as well as how symmetrical the piece is.  Higher grade cuts 
# are more symmetrical and have better light performance.  Diamonds in the "good" 
# category are considered to be in the top 25% of diamond cut quality, while 
# "very good" is in the top 15%, and "ideal" is in the top 3%.  
# Astor by Blue Nile is touted to "reflect the most light possible" and are 
# grade/certified by a number of third parties.

#Blue Nile. (n.d.). Diamond cut: Grading scale and buying tips. 
#https://www.bluenile.com/education/diamonds/cut?srsltid=AfmBOop9PWytZgjMIGYvLwuojS7LFcIV_5Pwh_pNHS44fwFoimTfBnbC 

# Order cut levels appropriately.
Diamonds_Ord<-Diamonds%>%
  mutate(cut=cut%>%
           fct_relevel(c("Good","Very Good","Ideal","Astor Ideal")))

# Create a frequency table to determine number of available diamonds per cut.
cut_freq_table<-table(Diamonds_Ord$cut)

# Plot available diamonds per cut.
ggplot(Diamonds_Ord, aes(x=cut))+
  geom_bar(fill="blue")+
  theme(plot.title = element_text(hjust=0.5))+
  scale_y_continuous(breaks=seq(0,800,50))+
  labs(x="Diamond Cut", y="Count Available",
       title="Diamond Cut Counts by Quality")

# Tried density plot. Looks strange.
ggplot(Diamonds_Ord, aes(x=cut))+
  geom_density()+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x="Diamond Cut", y="Density",
       title="Diamond Cut Counts by Quality")

# Create percentages for the diamond stock based on availability per cut type.
# Make it a dataframe.
cut_freq_perc<-data.frame(round((cut_freq_table/sum(cut_freq_table))*100,2))
# Rename defaults in table.
cut_freq_perc<-cut_freq_perc%>%
  rename(cut=Var1, percent=Freq)

# Create pie chart as a different method of representation.
ggplot(cut_freq_perc, aes(x="", y=percent, fill=cut))+ 
  geom_bar(color="black",stat="identity", width=1)+ 
  geom_text(aes(x=1.675,label=paste0(percent,"%")), position=position_stack(vjust=0.5))+
  coord_polar(theta="y", start=0)+ 
  labs(title="Percentage of Diamond Inventory by Cut")+ 
  theme_void()+
  theme(plot.title = element_text(hjust=0.5))

# The original zoomed out plot for response=price and predictor=cut.
ggplot(Diamonds_Ord, aes(x=cut, y=price))+
  geom_boxplot(fill="gold")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x="Diamond Cut", y="Diamont Price",
       title="Diamond Price by Cut")

# The modified zoomed in plot to show the boxes with less outliers.
# Needed to use coord_cartesian because ylim excludes data and skews the charts.
ggplot(Diamonds_Ord, aes(x=cut, y=price))+
  geom_boxplot(fill="gold")+
  coord_cartesian(ylim=c(0,10250))+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x="Diamond Cut", y="Diamont Price",
       title="Diamond Price by Cut")

# Calculating quantiles
cut_quant<-Diamonds_Ord%>%
  group_by(cut)%>%
  summarize(q0=quantile(price,probs=0),
            q25=quantile(price,probs=0.25),
            q50=quantile(price,probs=0.5),
            q75=quantile(price,probs=0.75),
            q100=quantile(price,probs=1),
            mean=mean(price))

# Call the data frame to view it.
cut_quant

# Try linear regression
# Use unordered dataset.

cut_lm<-lm(price~cut,data=Diamonds)
# Find original equation
summary(cut_lm)

# y-hat=5851.6 + 3615.7*Good* + 637.4*Ideal* + 1906.1*VeryGood*
# or
# y-hat=5851.6 + 3615.7*Good* + 1906.1*VeryGood* + 637.4*Ideal*

# Check residuals
par(mfrow=c(2,2))
plot(cut_lm)

par(mfrow=c(1,1))
acf(cut_lm$residuals, main="ACF plot of Residuals for cut linear model")

# Vertical variance is off.  Assumption 2 not very well met.

#Check boxcox
par(mfrow=c(1,1))

MASS::boxcox(cut_lm)

# Suggests 1/sqrt(y) (0.5), but also try log, since it is close.

ystar<-log(Diamonds$price)
Diamonds<-data.frame(Diamonds,ystar)
ystar_lm<-lm(ystar~cut,data=Diamonds)

ystar2<-1/sqrt(Diamonds$price)
Diamonds<-data.frame(Diamonds,ystar2)
ystar2_lm<-lm(ystar2~cut,data=Diamonds)

# Analyze

par(mfrow=c(2,2))
plot(ystar_lm)

# 1/sqrt(y) is better.
summary(ystar2_lm)

# Where y*=1/sqrt(y)
# y-hat*= 0.020890 + 0.001935*Good* + 0.006574*Ideal* + 0.003880*Verygood*

# The cut is highly skewed regarding available quantity of each type.  There are
# vastly more ideal cuts (739) and very good cuts (382) compared to the good
# cuts (73) and the Astor ideal cuts (20).  When viewing the original side-by-side
# boxplots, it is obvious that cut has a great deal of price variation and many outliers.
# This could suggest the other variables at play (carat, color, and clarity) causing deviations.
# The most outliers are witnessed in the very good and ideal ranges, however, they are
# the cuts with the largest quantity.  When viewing the plots in the boxes, which
# represents 50% of the data, the values are surprisingly close between good, very
# good, and ideal.  Astor ideal has a pretty wide range and trends more expensive
# in the 50% range, but the sample size is very low at only 20 diamonds, and it shares
# more than half of its range with very good and ideal cuts.  When fitting a linear regression,
# a 1/sqrt(y) was conducted to account for the vertical variance inconsistencies.
# Based on the summary, the only even slightly significant cut on its own is ideal compared to
# the reference of astor ideal.  The R2 of 0.01648 shows that this is an extremely poor fit for a model.
# Based on that value, and the rest of the analysis, very little of the price might be
# explained by the cut.