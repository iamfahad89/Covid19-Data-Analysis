covid19_analysis <- read.csv(file.choose(), header = T)
covid19 = covid19_analysis


# 1. Are people with older age(50+ years) 
#    are more prone to death due to COVID-19 virus?
hist(covid19$age)
hist(covid19$death)
qplot(age,death, data=covid19, color=death)
qplot(age,death, data=covid19, facets = .~death, binwidth = 2)
qplot(age, death, data=covid19, geom = c("point", "smooth"))
install.packages("ggplot2")
install.packages("ggplot")
library(ggplot2)
library(ggplot)
ggplot(data=covid19[covid19$age>50,], aes(x=age, y=death, colour=death)) +
  geom_point(alpha=0.1) + geom_smooth()
#end of first question



# 2. Are people with "male" gender are more likely to die 
#    due to corona virus when compared to the female gender?
by(covid19$death, covid19$gender, summary)

summary(covid19)

covid19$gender
gender_table <- table(covid19$gender)
prop.table(gender_table)

install.packages("gmodels")
library(gmodels)

CrossTable(covid19$gender, covid19$death)
#end of second question




# 3.Is there any correlation between the geolocation
#   and the people recovering from COVID-19? 
#   In other worlds, what are the chances of a person's 
#   survival belonging to a particular geolocation say China or 
#   USA or maybe India.
by(covid19$country, covid19$recovered, summary)

location_table <- table(covid19$country)
prop.table(location_table)

CrossTable(covid19$country, covid19$recovered)
#end of third question
