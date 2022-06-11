                           

                           #Mahmoud Hamed Ismael 
                                    #Taks3


library(ggplot2) #import ggplot2
library(gganimate)#animation
library(gifski)
library(png)  
library(dplyr)
library(plotly)    #interactive


##############################

d1 <- SampleSuperstore    #put dataset in varible to facilitate
sapply(d1, class)         #classfiy types of colums 
summary(d1, class)        #summary data 





#categeory according to region
p1 <- ggplot(d1,aes(x=Category))
p1.1 <- p1 + geom_bar(aes(fill=Region))
ggplotly(p1.1)      # with interactive 



#category according to ship 
p2 <- ggplot(d1,aes(x=Category)) 
p2+ geom_bar(aes(fill=Ship)) + transition_states(Ship) +    #annimation
  ease_aes('linear')
p2.1 <- p2 + geom_bar(aes(fill=Ship))
ggplotly(p2.1)     


#sub category according to segmant 
p3 <- ggplot(d1,aes(x=Sub))
p3.1 <- p3 + geom_bar(aes(fill=Segment))
ggplotly(p3.1)





#we explored data so, lets make some statistical for this data  




####



ggplot(d1, aes(x=Profit, y=Sales)) +
  geom_line(color=' dark green')
#from above when a sales increase profit increase 



ggplot(d1, aes(x=Profit, y=Discount)) +
  geom_line(color='red')
#when a discount decrease profit increase 



pairs(d1[c('Sales', 'Quantity', 'Discount','Profit')], col=' dark blue')
# From above visualiztion we observe that:
# Profit increases when Sales increases
# Profit decreases when discount increases


#sales for each Regoin Boxplot
b <- ggplot(d1, aes(Region, Sales))
b <- b + geom_boxplot()
b <- b + ggtitle("Sales for each Regoin Boxplot")
b <- b + xlab("Region") + ylab("Sales")
b 
ggplotly(b) #iteractive


#profit for each Regoin Boxplot
b2 <- ggplot(d1, aes(Region, Profit))
b2<- b2 + geom_boxplot()
b2 <- b2 + ggtitle("Profit for each Regoin Boxplot")
b2 <- b2 + xlab("Region") + ylab("Profit")
b2
ggplotly(b2) 


#Discount for each Regoin Boxplot
b3 <- ggplot(d1, aes(Region, Discount))
b3 <- b3 + geom_boxplot()
b3 <- b3 + ggtitle("Discount for each Regoin Boxplot")
b3 <- b3 + xlab("Region") + ylab("Discount")
b3 
ggplotly(b3) 





####

#lets make a gouped by fuction to calculate total(sales,profit,discount) for each state 
dfs = d1 %>% group_by(State)  %>%
  summarise(total_sales = sum(Sales),
            total_profits = sum(Profit),total_discount= sum(Discount),
            .groups = 'drop')
View(dfs)


#lets make a fuction gouped to calculate a mean profit for each state 
dfm = d1 %>% group_by(State)  %>%
  summarise(mean_sales = mean(Sales),
            mean_profits = mean(Profit),
            .groups = 'drop')
View(dfm)


#lets make a multiple gouped to calculate a total profit for both region and category
dff_multiple = d1 %>% group_by(Region, Category) %>%
  summarise(total_Sales = sum(Sales),
            total_Profit = sum(Profit),
            .groups = 'drop')

View(dff_multiple)



#lets make some statistical for data which grouped

#most and least area make profit 
p4<- ggplot(dfs,aes(x=State,y=total_profits))
p4.1 <- p4 + geom_bar(stat="identity")
ggplotly(p4.1)



#total profit for each region according to category 
p5 <- ggplot(dff_multiple,aes(x=Region,y=total_Profit))
p5.1 <- p5 + geom_bar(aes(fill=Category),stat="identity")
ggplotly(p5.1)




#most area makes discount 
p6 <- ggplot(dfs,aes(x=State,y=total_discount))
p6.1 <- p6 + geom_bar(stat="identity")
ggplotly(p6.1)



#mean of profits according to states so we can take a decision wisely
p7 <- ggplot(dfm,aes(x=State,y=mean_profits))
p7.1 <- p7 + geom_bar(stat="identity")
ggplotly(p7.1)
# as we see above the best state makes profit is vermont and least one is ohio 
