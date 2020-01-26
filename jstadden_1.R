#Jared Stadden
#Assignment 1

#1 Data Source:
#https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=y&type=8&season=2019&month=0&season1=2019&ind=0

#2 Importing data set:
#mydata <- read.csv(file='C:\\Users\\jared\\Desktop\\FanGraphs Leaderboard.csv')

#Fixing strangely named column
names(mydata)[names(mydata)=="ï..Name"]<-"Name"

#3 Descriptive Statistics for select variables
summary(mydata$Name)
summary(mydata$Team)
summary(mydata$HR)
summary(mydata$SB)
summary(mydata$AVG)
summary(mydata$WAR)

#4 Tranforming RBI Variable
mydata$AVG = 1000*mydata$AVG


#5a Plot of Quantitative
hist(mydata$WAR)

#5b Scatterplot
plot(mydata$HR,mydata$RBI,xlab = "Homeruns",ylab = "Runs Batted In")
