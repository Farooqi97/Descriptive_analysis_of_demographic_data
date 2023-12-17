#Get the dimension of Data -> Has 454 Rows and 8 Columns
dim(censusData)
noOfcountires <- unique(censusData$Country.Name)
length(noOfcountires)

#check if the dataset has some NA values or 0
any(is.na(censusData))

#Create a new Data frame without NA values
censusData <- na.omit(censusData)

#Separate the data according to the year
censusData_2022 <- censusData[censusData$Year== 2022, ]
censusData_2002 <- censusData[censusData$Year == 2002, ]

#Task 1:  Describe the frequency distributions of the variables. Consider also the differences
#between the sexes and regions.


#Life expectancy variables for male and female are all continuous variables.
summary(censusData_2022$Life.Expectancy.at.Birth..Both.Sexes)
summary(censusData_2022$Life.Expectancy.at.Birth..Males)
summary(censusData_2022$Life.Expectancy.at.Birth..Females)
summary(censusData_2022$Under.Age.5.Mortality..Both.Sexes)

#Histogram for Life expectany for male and female
hist(censusData_2022$Life.Expectancy.at.Birth..Both.Sexes, main="Life expectancy for Both sexes",xlab="Life expectancy at Birth")
abline(v = mean(censusData_2022$Life.Expectancy.at.Birth..Both.Sexes), col = "red", lwd = 3)
abline(v = median(censusData_2022$Life.Expectancy.at.Birth..Both.Sexes), col = "blue", lwd = 3)
print(mean(censusData_2022$Life.Expectancy.at.Birth..Both.Sexes))
print(median(censusData_2022$Life.Expectancy.at.Birth..Both.Sexes))

#Histogram for Males
hist(censusData$Life.Expectancy.at.Birth..Males, main="Life expectancy for Males",xlab="Life expectancy at Birth")
abline(v = mean(censusData$Life.Expectancy.at.Birth..Males), col = "red", lwd = 3)
abline(v = median(censusData$Life.Expectancy.at.Birth..Males), col = "blue", lwd = 3)
print(mean(censusData$Life.Expectancy.at.Birth..Males))
print(median(censusData$Life.Expectancy.at.Birth..Males))

#Histogram for females
hist(censusData_2022$Life.Expectancy.at.Birth..Females, main="Life expectancy for Females",xlab="Life expectancy at Birth")
abline(v = mean(censusData_2022$Life.Expectancy.at.Birth..Females), col = "red", lwd = 3)
abline(v = median(censusData_2022$Life.Expectancy.at.Birth..Females), col = "blue", lwd = 3)
print(mean(censusData_2022$Life.Expectancy.at.Birth..Females))
print(median(censusData_2022$Life.Expectancy.at.Birth..Females))

#Histogram for both sexes
hist(censusData_2022$Life.Expectancy.at.Birth..Males, col = rgb(0,0,1,0.2))
hist(censusData_2022$Life.Expectancy.at.Birth..Females, col=rgb(1,0,0,0.2),add=TRUE)
legend('topright', c('Male', 'Female'),
       fill=c(rgb(0,0,1,0.2), rgb(1,0,0,0.2)))

#Histogram for Under Age 5 Motalitity Rate
hist(censusData_2022$Under.Age.5.Mortality..Both.Sexes, main="Under Age 5 Mortality Rate",xlab="Under Age 5 Mortality Rate")
abline(v = mean(censusData_2022$Under.Age.5.Mortality..Both.Sexes), col = "red", lwd = 3)
abline(v = median(censusData_2022$Under.Age.5.Mortality..Both.Sexes), col = "blue", lwd = 3)
print(mean(censusData_2022$Under.Age.5.Mortality..Both.Sexes))
print(median(censusData_2022$Under.Age.5.Mortality..Both.Sexes))

#Box plot comparing Life expectancy for Males and Females
boxplot(censusData_2022$Life.Expectancy.at.Birth..Females,
        censusData_2022$Life.Expectancy.at.Birth..Males,
        censusData_2022$Life.Expectancy.at.Birth..Both.Sexes,
        col="white",
        main="Comparison of Life expectancy by gender",
        names = c("Female","Male", "Life exp both sexes"),
        outline = TRUE)

#Task 3
#Are there bivariate correlations between the variables?

#Life expectancy and Mortaility rate for both sexes.
plot(x = censusData_2022$Life.Expectancy.at.Birth..Both.Sexes , y = censusData_2022$Under.Age.5.Mortality..Both.Sexes, main="Bivariate correlation: Life Exp for both sexes and Under Age 5 Mortality Rate",xlab="Life expectancy", ylab="Under Age 5 mortality rate")

#Life expectancy and Mortaility rate of Males.
plot(x = censusData_2022$Life.Expectancy.at.Birth..Males, y = censusData_2022$Under.Age.5.Mortality..Both.Sexes, xlab="Life expectancy", main="Bivariate correlation: Life Exp Male and Under Age 5 Mortality Rate",ylab="Under Age 5 mortality rate")

#Life expectancy and Mortaility rate of Females.
plot(x = censusData_2022$Life.Expectancy.at.Birth..Females, y = censusData_2022$Under.Age.5.Mortality..Both.Sexes, xlab="Life expectancy", main="Bivariate correlation: Life Exp Female and Under Age 5 Mortality Rate",ylab="Under Age 5 mortality rate")



#Select all the numeric variables, make a subset
colBiVarRelation <- c("Under.Age.5.Mortality..Both.Sexes", "Life.Expectancy.at.Birth..Both.Sexes", "Life.Expectancy.at.Birth..Males", "Life.Expectancy.at.Birth..Females")
subsetData <- censusData_2022[colBiVarRelation]

#round off to Pearson correlation below.
corrData<- round(cor(subsetData), 3)
corrData


# Scatterplot matrix
colLabels <- c("Mortality rate", "Life-Exp Both","Life.Exp..Males", "Life.Exp..Females")
pairs(subsetData,labels = colLabels,
      gap = 1,cex.labels = 1,
      font.labels = 1,upper.panel = NULL, 
      main = "Scatter plot matirx"
)

#Correlation Heat map
row.names(corrData) <- c("(A)Under age 5 Mortality both sexes","(B)Life-Exp Both Sexes","(C) Life-Exp Males","(D) Life-Expectancy Female")
colnames(corrData) <- c("A","B","C","D")
corrplot(corrData,method = "number")

#Separate the data according to the Region
censusData_2022_Region <- censusData[censusData$Region== "Asia", ]
censusData_2002_Region <- censusData[censusData$Region == "Asia", ]

#Task 2
#Are the values of the individual variables comparatively homogeneous within the
#individual subregions and heterogeneous between different subregions? To answer
#this question, first analyse the variability of the values within the individual
#subregions and then compare the measures of central tendency of the individual
#variables between different subregions.


#Under Age 5 Mortality Rate and subregion
ggplot(censusData_2022_Region,aes(x=Under.Age.5.Mortality..Both.Sexes,
                           y=reorder(Subregion,Under.Age.5.Mortality..Both.Sexes,
                                     FUN = ))) +
  geom_boxplot(width=0.5)+
  labs(x= "Under Age 5 Mortality Rate",y="Subregions", title)+
  theme(legend.position = "none",text = element_text(size=12))

censusData_2022_Region_Subregion <- censusData[censusData$Subregion== "Eastern Asia", ]

print(min(censusData_2022_Region_Subregion$Under.Age.5.Mortality..Both.Sexes))

censusData_2022_Region_Subregion_max <- censusData[censusData$Subregion== "South-Central Asia", ]

print(max(censusData_2022_Region_Subregion_max$Under.Age.5.Mortality..Both.Sexes))

#Life expectancy male
ggplot(censusData_2022_Region,aes(x=Life.Expectancy.at.Birth..Males,
                           y=reorder(Subregion,Life.Expectancy.at.Birth..Males,
                                     FUN = mean))) +
  geom_boxplot(width=0.5)+
  labs(x= "Life Expectancy Males",y="Subregions")+
  theme(legend.position = "none",text = element_text(size=12))

#Life expectancy female
ggplot(censusData_2022_Region,aes(x=Life.Expectancy.at.Birth..Females,
                           y=reorder(Subregion,Life.Expectancy.at.Birth..Females,
                                     FUN = mean))) +
  geom_boxplot(width=0.5)+
  labs(x= "Life Expectancy Females",y="Subregions")+
  theme(legend.position = "none",text = element_text(size=12))

#Life expectancy both sexes
ggplot(censusData_2022_Region,aes(x=Life.Expectancy.at.Birth..Both.Sexes,
                           y=reorder(Subregion,Life.Expectancy.at.Birth..Both.Sexes,
                                     FUN = mean))) +
  geom_boxplot(width=0.5)+
  labs(x= "Life Expectancy Both Sexes",y="Subregions")+
  theme(legend.position = "none",text = element_text(size=12))


#Variability for regions Region Asia
ggplot(censusData_2022_Region,aes(x=Under.Age.5.Mortality..Both.Sexes,
                           y=reorder(Region,Under.Age.5.Mortality..Both.Sexes,
                                     FUN = ))) +
  geom_boxplot(width=0.5)+
  labs(x= "Under Age 5 Mortality Rate",y="Subregions", title)+
  theme(legend.position = "none",text = element_text(size=12))

#Life expectancy male Region Asia
ggplot(censusData_2022_Region,aes(x=Life.Expectancy.at.Birth..Males,
                           y=reorder(Region,Life.Expectancy.at.Birth..Males,
                                     FUN = mean))) +
  geom_boxplot(width=0.5)+
  labs(x= "Life Expectancy Males",y="Subregions")+
  theme(legend.position = "none",text = element_text(size=12))

#Life expectancy female Region Asia
ggplot(censusData_2022_Region,aes(x=Life.Expectancy.at.Birth..Females,
                           y=reorder(Region,Life.Expectancy.at.Birth..Females,
                                     FUN = mean))) +
  geom_boxplot(width=0.5)+
  labs(x= "Life Expectancy Females",y="Subregions")+
  theme(legend.position = "none",text = element_text(size=12))

#Life expectancy both sexes Region Asia
ggplot(censusData_2022_Region,aes(x=Life.Expectancy.at.Birth..Both.Sexes,
                           y=reorder(Region,Life.Expectancy.at.Birth..Both.Sexes,
                                     FUN = mean))) +
  geom_boxplot(width=0.5)+
  labs(x= "Life Expectancy Both Sexes",y="Subregions")+
  theme(legend.position = "none",text = element_text(size=12))


#Task 4
#Total Mortality Rate Both sexes
boxplot(censusData$Under.Age.5.Mortality..Both.Sexes ~ censusData$Year,main = "Mortality Rate Comparision Between 2002 and 2022",xlab = "Year", ylab = "Mortality Rate both sexes")
print(median(censusData_2022$Under.Age.5.Mortality..Both.Sexes))
print(median(censusData_2002$Under.Age.5.Mortality..Both.Sexes))

#Life expectancy rate Male
boxplot(censusData$Life.Expectancy.at.Birth..Males ~ censusData$Year,main = "Life Expectancy Males Comparision Between 2002 and 2022",xlab = "Year", ylab = "Life expectancy Male")

#Life expectancy Females
boxplot(censusData$Life.Expectancy.at.Birth..Females~ censusData$Year,main = "Life Expectancy Females Comparision Btw 2002 and 2022",xlab = "Year", ylab = "Life expectancy Female")

#Life expetancy Both sexes 
boxplot(censusData$Life.Expectancy.at.Birth..Both.Sexes~ censusData$Year,main = "Life Expectancy Both Sexes Comparision Btw 2002 and 2022",xlab = "Year", ylab = "Life expectancy Both Sexes")
print(median(censusData_2022$Life.Expectancy.at.Birth..Both.Sexes))
print(median(censusData_2002$Life.Expectancy.at.Birth..Both.Sexes))