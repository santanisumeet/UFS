
getwd()
family <- read.csv("Masterclient List_2018-08-28_1.csv")

View(family)
#a. # of unduplicated customers



a=unique(family$FOLDER_ID)
length(a)

# b. # of customers by Gender

install.packages("sqldf")
library(sqldf)
install.packages("dplyr")
library(dplyr)

 

family$SEX = dplyr::recode(family$SEX, F="Female", f="Female", m="Male", M="Male")


newFrame = sqldf("select FOLDER_ID, SEX, count(FOLDER_ID) as num from family group by SEX")
View(newFrame)





# c. Mean age at program entry


test <- sqldf("select FOLDER_ID, CAST(min(AgeatProgramEntry) as int) as age from family group by Folder_ID")

View(test)
mean(test$age)



#d Group Age at the time of program entry 
anyNA(test$age)


is.factor(test$age)
is.numeric(test$age)
typeof(test$age)


#Discretize age and give labeling 


test$age = cut(test$age,
               breaks = c(-1,6,11,14,18,25,Inf), 
               labels = c("toddler", "kid", "pre-teen","teen", "young", "last_youth" ))




length(test$age)

anyNA(test$age)
na.omit(test$age)


test = na.omit(test)


toddler = subset(test$age, test$age == 'toddler' )
percentage_toddler = (length(toddler) / length(test$age))*100
percentage_toddler


kid = subset(test$age, test$age == 'kid' )
percentage_kid = (length(kid) / length(test$age))*100
percentage_kid

pre_teen = subset(test$age, test$age == 'pre-teen' )
percentage_preteen = (length(pre_teen) / length(test$age))*100
percentage_preteen


teen = subset(test$age, test$age == 'teen' )
percentage_teen = (length(teen) / length(test$age))*100
percentage_teen


young = subset(test$age, test$age == 'young' )
percentage_young = (length(young) / length(test$age))*100
percentage_young

last_youth = subset(test$age, test$age == 'last_youth' )
percentage_last_youth = (length(last_youth) / length(test$age))*100
percentage_last_youth


slicesAge <- c(percentage_toddler,  percentage_kid , percentage_preteen , percentage_teen , percentage_young , percentage_last_youth)
labelsAge <- c("Toddler" , "Kid",  "Preteen", " Teen" , "Young",  "Old")

pie(slicesAge, labels = labelsAge, main="Age Group at Program Entry")


#e.Number of unduplicated, Hispanic customers, ages 14-17.



hispanic = subset(family, family$ETHNICITY == 'Hispanic/Latino')
View(hispanic)



hispanic <- sqldf("select FOLDER_ID, ETHNICITY, CAST(min(AgeatProgramEntry) as int) as age from hispanic group by Folder_ID")
typeof(hispanic$age)

View(hispanic)


newhispanic = data.frame(hispanic$age)
newhispanic
View(newhispanic)
anyNA(newhispanic)

newhispanic$hispanic.age = cut(newhispanic$hispanic.age,
               breaks = c(-1,6,11,14,18,25,Inf), 
               labels = c("toddler", "kid", "pre-teen","teen", "young", "last_youth" ))


summary(newhispanic)




# f.Mean length of service for customers who have discharged (in months)



admitdata <- read.csv("Masterclient List_2018-08-28 copy.csv")


admitdata <- sqldf("select FOLDER_ID, ADMIT_DATE, DISC_DATE, CAST(min(AgeatProgramEntry) as int) as age from admitdata group by Folder_ID")


View(admitdata)

admitdata = admitdata[- grep("#NULL!", admitdata$DISC_DATE),]


admitdata$date_diff <- as.Date(admitdata$DISC_DATE) - as.Date(admitdata$ADMIT_DATE)
admitdata$date_diff_months = admitdata$date_diff/30 #assuming there are 30 days in a month on an average

mean(admitdata$date_diff_months)

# g.Number of customers by county



county<- sqldf("select FOLDER_ID, COUNTY, CAST(min(AgeatProgramEntry) as int) as age from family group by Folder_ID")
View(county)


summary(county)
