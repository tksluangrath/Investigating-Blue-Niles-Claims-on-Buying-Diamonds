# Bring in Tidyverse package
library(tidyverse)
# Read in data
Diamonds<-read.csv("diamonds4.csv", header=TRUE)

# I ordered the factors that weren't already in alphabetical order.
Diamonds<-Diamonds%>%
  mutate(cut=cut%>%
           fct_relevel(c("Good","Very Good","Ideal","Astor Ideal")))%>%
  mutate(clarity=clarity%>%
           fct_relevel(c("SI2","SI1","VS2","VS1","VVS2","VVS1","IF","FL")))

# Dataframe (If you use this method and encounter any errors, you can try ungroup() at the end. The mapping
# results seem to be the same.)
cut_color<-Diamonds%>%
  count(cut,color)%>%
  group_by(cut)%>%
  mutate(Percentage= round(n/sum(n)*100,2))

cut_clarity<-Diamonds%>%
  count(cut,clarity)%>%
  group_by(cut)%>%
  mutate(Percentage= round(n/sum(n)*100,2))

# Heat  maps.
ggplot(cut_color,aes(x=color,y=cut,fill=Percentage))+
  geom_tile()+
  scale_fill_gradient(low="white",high="purple")+
  geom_text(aes(label=Percentage))+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x="Diamond Color",y="Percentage",
       title="Diamond cut by color")

ggplot(cut_clarity,aes(x=clarity,y=cut,fill=Percentage))+
  geom_tile()+
  scale_fill_gradient(low="white",high="red")+
  geom_text(aes(label=Percentage))+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x="Diamond Color",y="Percentage",
       title="Diamond cut by clarity")