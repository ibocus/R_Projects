#import libraries
library(ggplot2)
library(dplyr)
library(ggthemes)
library(corrgram)
library(corrplot)

#import dataset
DS.2019 <- read.csv('DataSet2019.csv')

#DS.AA <- filter(DS.2019, Staff_Title == 'Administrator' | 'Assistant Administrator' & Process_Name =='Bank transfer instruction (per IB instruction)' & TimeTaken <= 45)
DS.Staff <- filter(DS.2019, Staff_Title == 'Administrator' | Staff_Title ==  'Assistant Administrator' | Staff_Title == 'Assistant Corporate Administrator' | Staff_Title == 'Assistant Trust Administrator' | Staff_Title == 'Corporate Administrator' | Staff_Title == 'Fund Administrator' 
                   | Staff_Title == 'Senior Administrator' | Staff_Title == 'Senior Corporate Administrator' | Staff_Title == 'Senior Fund Administrator' 
                   | Staff_Title == 'Senior Trust Administrator' | Staff_Title == 'Trust Administrator')
DS.AA <- filter(DS.Staff, Process_Name == 'Surveys' & TimeTaken <= 45)
#print(DS.AA)
summary(DS.AA)

# pl <- ggplot(data=DS.2019, aes(x=TimeTaken, y=Team_Name)) + geom_point()
#pl <- ggplot(DS.AA, aes(x=TimeTaken)) + geom_histogram
# pl <- ggplot(DS.AA, aes(x=TimeTaken)) + geom_histogram(bins=20,alpha=0.5, fill='blue') + ylab("Number of Process")

#print the histogram
pl <- ggplot(DS.AA, aes(x=TimeTaken)) + geom_histogram(aes(fill=Team_Name, binwidth=1))+ labs(title = "Histogram of Bank Transfer", x = "Time in hr", y = "Number of Process", fill = "Team Name") 
print(pl)


#standardize number
DS.AA$Number <-scale(DS.AA$Number)

#print the scatterplot
pl <-ggplot(DS.AA, aes(x=TimeTaken, y=Number)) + geom_point(aes(shape=Team_Name, color=TimeTaken), size=2) + scale_color_gradient(low='#31a354', high = '#c51b8a') + geom_smooth(se=TRUE) + theme_stata()
print(pl)



library(caTools)
pl <- lm(TimeTaken ~ Number, DS.AA)
print(pl)
print(summary(pl))
#confidence Interval in the slope
confint(pl, level = 0.95)
#plotting of different plots
#plot(pl)

##plotting the residual
#res <- residuals(pl)
#class(res)
#res <- as.data.frame(res)
#head(res)
#ggplot(res,aes(res)) + geom_histogram(fill='blue', alpha=0.5)


library(caTools)
#plot the Linear Regression
pl <-ggplot(DS.AA, aes(x=TimeTaken, y=Number)) + geom_point(aes(shape=Team_Name, color=Team_Name), size=1.5) +  stat_smooth(method = "lm", col="black", se=TRUE, size=1) + theme_stata()
print(pl)
print(summary(pl))


#predictions
pl.predict <- predict(pl, DS.AA)
results <-cbind(pl.predict,DS.AA$TimeTaken)
colnames(results) <-c('predicted', 'actual')
results <- as.data.frame(results)
print(head(results))




