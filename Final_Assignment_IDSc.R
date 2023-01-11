#****************************************************************************
# Intro to Data Science:11516
# U3227622: Ambika H Kapanaiah  
# Description:Final Assignment
#****************************************************************************
#preparing the environment for loading data
#removing all env variables.
rm(list = ls())

#Loading the required libraries.

if(!("tidyverse" %in% rownames(installed.packages()))){
  install.packages("tidyverse")
}
library(tidyverse)
if(!("lubridate" %in% rownames(installed.packages()))){
  install.packages("lubridate")
}
library(lubridate)
if(!("GGally" %in% rownames(installed.packages()))){
  install.packages("GGally")
}
library(GGally)
if(!("modelr" %in% rownames(installed.packages()))){
  install.packages("modelr")
}
library(modelr)
if(!("DMwR2" %in% rownames(installed.packages()))){
  install.packages("DMwR2")
}
library(DMwR2)


#######-----Part B -Data Preparation, exploring and modelling-----#############

######---Task 1: Data Preparation and Wrangling: 

### 1. Load and read the data from the CSV files and store them into dataframes named
#appropriately. 



#1. Load and read the data from the CSV files and store them into dataframes named
# appropriately. 

#using relative paths for all the csv's to read into tibble.
Covid19_tibbl <- read_csv("./data/Covid19.csv")
Country_tibbl <- read_csv("./data/Countries.csv")
Recovered_tibbl <- read_csv("./data/Recovered.csv")
tests_tibbl <- read_csv("./data/Tests.csv")

Covid19_tibbl
Country_tibbl
Recovered_tibbl
tests_tibbl

#2. Tidy up the dataframe driven from the file "Recovered.csv" to be compatible 
#with the dataframe driven from the file "Covid19.csv", i.e. every observation 
#should have a record of recovered patients in one country in a single day.

#wider to longer data
#using gather function to achieve the above.
Recovered_tibbl <- Recovered_tibbl %>% 
  gather(key = "date", value = "recovrd_cases",-Country.Region) 
Recovered_tibbl

#3. Change the column names in the dataframes were loaded from the following files
# accordingly.

names(Covid19_tibbl) <- c("Code","Country","Continent","Date","NewCases","NewDeaths")
colnames(Covid19_tibbl)

names(Country_tibbl) <- c("Code","Country","Population","GDP","GDPCapita")
colnames(Country_tibbl)

names(Recovered_tibbl) <- c("Country","Date","Recovered")
colnames(Recovered_tibbl)

names(tests_tibbl) <- c("Code","Date","NewTests")
colnames(tests_tibbl)

#4. Ensure that all dates variables are of date data type and with the same
##Recovered_tibbl$Date <- gsub("\\.", "-", Recovered_tibbl$Date)

#checking the class of Date column and typecasting if the column is not of Date
#type
class(tests_tibbl$Date)
class(Covid19_tibbl$Date)
Recovered_tibbl$Date <- as.Date(Recovered_tibbl$Date, format="%Y.%m.%d")
class(Recovered_tibbl$Date)

#5. Considering the master dataframe is the one loaded from file "Covid19.csv",
#add new 5 variables to it from other files
#(Recovered.csv, Tests.csv, Countries.csv). The 5 new added variables should
# be named ("Recovered", "NewTests", "Population", "GDP","GDPCapita") accordingly.

#merging the coluns of dataframes using left_join
merged_df <- Covid19_tibbl %>%
  left_join(Country_tibbl, by=c("Code","Country"))
ncol(merged_df)

merged_df1 <- merged_df %>%
  left_join(Recovered_tibbl, by=c("Country","Date"))
ncol(merged_df1)

Covid19_mergd <- merged_df1 %>%
  left_join(tests_tibbl, by = c("Code","Date"))
ncol(Covid19_mergd)

#6. Check for Nas in all dataframes and change them to Zero. 
colSums(is.na(Covid19_mergd))
Covid19_mergd[is.na(Covid19_mergd)] = 0

#checking whether any NA's appear still
colSums(is.na(Covid19_mergd))


#7. Using existing "Date" variable; add month and week variables to the 
#master dataframe.[Hint: you may use functions from lubridate package]

Covid19_mergd$Month <- as.numeric(format(as.Date(Covid19_mergd$Date),
                                             "%m"))
Covid19_mergd$Week <- as.numeric(lubridate::week(ymd(Covid19_mergd$Date)))

head(Covid19_mergd)

##########################################################################

#Task 2: Exploratory Data Analysis: (40 marks)

#1. Add four new variables to the master dataframe ("CumCases", "CumDeaths",
#"CumRecovered", "CumTests") These variables should reflect the cumulative relevant
#data up to the date of the observation, i.e CumCases for country "X" at Date "Y" should
# reflect the total number of cases in country "X" since the beginning of recording data till
# the date "Y".


#grouping by Country and summarising using cumsum()
#to get newCases,Deaths,recovered and Tests cumulatives
Covid19_mergd <- Covid19_mergd %>% group_by(Country) %>%
  mutate(CumCases=cumsum(NewCases),
         CumDeaths=cumsum(NewDeaths),
         CumRecovered=cumsum(Recovered),
         CumTests=cumsum(NewTests))

Covid19_mergd

#2. Add two new variables to the master dataframe ("Active", "FatalityRate"). Active
#variable should reflect the infected cases that has not been closed yet (by either recovery
# or death), and it could be calculated from (CumCases - (CumDeaths + CumRecovered)).
#On the other hand, FatalityRate variable should reflect the percentages of death to the
#infected cases up to date and it could be calculated from (CumDeaths / CumCases). 

#Adding the mentioned 2 new columns after calculating column values
#using the formulas given in the question
Covid19_mergd$Active <- Covid19_mergd$CumCases-(Covid19_mergd$CumDeaths+Covid19_mergd$CumRecovered)
Covid19_mergd$FatalityRate <- round(Covid19_mergd$CumDeaths/Covid19_mergd$CumCases,
                                    4)

#3. Add four new variables to the master dataframe ("Cases_1M_Pop", "Deaths_1M_Pop",
# "Recovered_1M_Pop", "Tests_1M_Pop") These variables should reflect the cumulative
# relevant rate per one million of the corresponding country population, 
#(i.e Cases_1M_Pop for country "X" at Date "Y" should reflect the total number of 
#new cases up to date "Y" per million people of country "X" population)

#Adding the mentioned 4 new columns after calculating column values
#using the formulas given in the question
Covid19_mergd$Cases_1M_Pop <-  (Covid19_mergd$CumCases*(10^6)) / Covid19_mergd$Population


Covid19_mergd$Deaths_1M_Pop <- (Covid19_mergd$CumDeaths*(10^6)) / Covid19_mergd$Population

Covid19_mergd$Recovered_1M_Pop <- (Covid19_mergd$CumRecovered*(10^6)) / Covid19_mergd$Population

Covid19_mergd$Tests_1M_Pop <- (Covid19_mergd$CumTests*(10^6)) / Covid19_mergd$Population

Covid19_mergd

#4. Find the day with the highest reported death toll across the world. 
#Print the date and the death toll of that day.

#using which.max to find the row value of columns Date NewDeath and Country
#to get highest deaths reported Date and number of Deaths
Covid19_mergd[which.max(Covid19_mergd$NewDeaths),"Date"]
Covid19_mergd[which.max(Covid19_mergd$NewDeaths),"NewDeaths"]
Covid19_mergd[which.max(Covid19_mergd$NewDeaths),"Country"]

#5. Build a graph to show how the cumulative data of (Infected Cases, Deaths,
# Recovered,Tests) change over the time for the whole world collectively.
#[Hint: Use geom_line, use log for Y axis for better presentation, Use different 
# colour to distinguish between new cases, deaths, and recovered]

#Time Series plot of cumulative data of Infected Cases, Deaths,Recovered,Tests
#As it is mentioned for the whole world taking the sum of cumcases cumdeaths
#cumrecovered and cumdeaths.
Covid19_mergd %>% group_by(Date) %>%
  mutate(sum_cumsum=sum(CumCases),
            sum_cumdeath=sum(CumDeaths),
            sum_cumrecovered=sum(CumRecovered),
            sum_cumtest=sum(CumTests)) %>%
  gather(sum_cumsum,sum_cumdeath,sum_cumrecovered,sum_cumtest,key="var",value = "val") %>% 
  select(Country,Date, var, val) %>%
ggplot(aes(x=Date,y=log(val) ,color=var)) +
  geom_line(size=1)+
  geom_point()+
  ggtitle("Cumulative of Cases time series plot-2020")+
  xlab("2020-Jan to May")+
  ylab("Number of observations")


#6. Extract the last day (05/05/2020) data and save it in a separate 
#dataframe called "lastDay_data".
#[Hint: use filter function with Date = "2020-05-05"]

lastDay_data <- Covid19_mergd %>% filter(Date=="2020-05-05")
lastDay_data

#7. Based on the last day data, extract the whole records of the top 10 countries worldwide
#that have current active cases, total confirmed cases, and fatality rate in separate
#dataframes (i.e. top10activeW, top10casesW, top10fatalityW, top10testsMW).
#[Hint: you can use head(arranged_data, n=10) to get the top 10 records]

#Top 10 countries with Active Cases

#subsetting and arranging in descending order gives
#more number of active cases in the top
#selecting those first 10 rows gives the top 10 countries
top10_Actv_Case <- lastDay_data %>% subset(select = c(Country,Active)) %>%
  arrange(-Active) 
top10activeW <- as.data.frame(top10_Actv_Case[1:10, 1:2]) 
top10activeW

#Top 10 countries with Total number of cases
#subsetting and arranging in descending order gives
#more number of cumulative cases in the top
#selecting those first 10 rows gives the top 10 countries
top10_Tot_Case <- lastDay_data %>% subset(select = c(Country,CumCases)) %>%
  arrange(-CumCases) 
top10casesW <- as.data.frame(top10_Tot_Case[1:10, 1:2]) 
top10casesW

#Top 10 countries with Fatality Rate
#subsetting and arranging in descending order gives
#more number of fatalityrate in the top
#selecting those first 10 rows gives the top 10 countries
top10_fatrate <- lastDay_data %>% subset(select = c(Country,FatalityRate)) %>%
  arrange(-FatalityRate) 
top10fatalityW <- as.data.frame(top10_fatrate[1:10, 1:2]) 
top10fatalityW

#Top 10 countries with tests
#subsetting and arranging in descending order gives
#more number of tests in the top
#selecting those first 10 rows gives the top 10 countries
top10_tests <- lastDay_data %>% subset(select = c(Country,CumTests)) %>%
  arrange(-CumTests) 
top10testsMW <- as.data.frame(top10_tests[1:10, 1:2]) 
top10testsMW

#Top 10 countries with tests per 1-million population
#subsetting and arranging in descending order gives
#more number of Tests 1million per population in the top
#selecting those first 10 rows gives the top 10 countries
top10_tests1mP <- lastDay_data %>% subset(select = c(Country,Tests_1M_Pop)) %>%
  arrange(-Tests_1M_Pop) 
top10tests1mp <- as.data.frame(top10_tests1mP[1:10, 1:2]) 
top10tests1mp

#8. Based on the last day data, print the up to date confirmed, death, recovered cases as well
#as the tests for every continent.

#grouping the data by COntinent names and finding sum of the cumulative
#cases for each country under the Continent.
Data_continents <- lastDay_data %>% group_by(Continent) %>%
  summarise(confirmed_cases=sum(CumCases),
            Totl_deaths=sum(CumDeaths),
            Recovrd_cases=sum(CumRecovered),
            tests=sum(CumTests))

  
Data_continents  


#9. Build a graph to show the total number of cases over the time for the top 10 countries that
#have been obtained in question 7 (Use log for Y axis for better presentation).
#[Hint: first you need to get the data of the top-10 countries and then plot their lines]

#selecting the top10 country names from question7 solution
#selecting data from masterdataframe of those contries only.
countrynames= top10casesW$Country
Covid19top10_totcase = Covid19_mergd[(Covid19_mergd$Country) %in% countrynames,]
Covid19top10_totcase

Covid19top10_totcase %>% 
  ggplot(aes(x=Date,y=log(CumCases) ,color=Country, group=Country)) +
  geom_line(size=1)+
  geom_point()+
  ggtitle("Total number of Cases for top ten countriestime series plot")+
  xlab("2020-Jan to May")+
  ylab("Total Number of cases")


#10. Build a graph for the top 10 countries with current highest active cases which was
#obtained previously in question 7. The graph should have one subgraph (i.e. using facet
#function) for each of these countries, every subgraph should show how the new cases, new
#deaths, and new recovered cases were changing over time (Use log for Y axis for better
#presentation, Use different colour to distinguish between new cases, deaths, and
#recovered).
#[hint: geom_line function with date on x_axis and each of the values of the variables in y_axis]

#selecting the top10 country names from question7 solution
#selecting data from masterdataframe of those contries only.
countrynames= top10activeW$Country
Covid19top10_actvcase = Covid19_mergd[(Covid19_mergd$Country) %in% countrynames,]
Covid19top10_actvcase


Covid19top10_actvcase %>%
  gather(NewCases,NewDeaths,Recovered,key="var",value = "val") %>%
  ggplot(aes(x=Date,y=log(val),color=factor(var), group=factor(var))) +
  geom_line(size=1)+
  ggtitle("Number of newcaes,new deaths and recovered cases time series plot")+
  facet_wrap(~Country)+
  xlab("2020- Jan to May")+
  ylab("Number of cases")


#11. Build a graph for the top 10 countries with current highest total tests per one million of
#the population which was obtained previously in question 7. This graph should present
#total number of infected cases, total tests so far, and the total tests per million of the
#population for each country.
#[hint: you can use bar chart to achieve this task]

#selecting the top10 country names from question7 solution
#selecting data from last day data as asked for current highest of those contries only.
countrynames= top10tests1mp$Country
Covid19top10_tests1mp=lastDay_data[(lastDay_data$Country) %in% countrynames,]
Covid19top10_tests1mp


Covid19top10_tests1mp %>%
  gather(CumCases,CumTests,Tests_1M_Pop,key="var",value = "val") %>%
  ggplot(aes(x=Country,y=log(val),fill = factor(var))) +
  geom_bar(position="dodge",stat="identity") +
  ggtitle("Number of Cumcases,CumTestsa and tests per million population time series plot")+
  xlab("2020- Jan to May")+
  ylab("Number of Observatios")



#12. Build a graph to present the statistics of all continents which was obtained previously 
#in question 8 (Use log for Y axis for better presentation, Use Continent in the legend, make
#sure x-axis labels does not overlap). 

#Taking continent wise data obtained in question8 gathering all the mentioned 
#coulmns so that the data can be plotted using bar graph
Data_continents %>% 
  gather(variable, value,-Continent) %>%
  ggplot(aes(variable, log(value), fill = Continent,shape=factor(Continent))) +
  geom_bar(position="dodge",stat="identity") +
  ylab("Number of observations")+
  ggtitle("Bar plot for statistics of All Continents ")+
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(fill = NA),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "right",
        strip.placement = "outside")


##################################################################################

#Task 3: Data-Driven Modelling: 

#1. Based on the data of the last day, that you have extracted in the previous task, create a
#separate dataframe named "cor_data" with the data of these variables
#(CumCases, CumTests, Population, GDP, GDPCapita).
#[Hint: you can use select function on the lastday_data dataframe]

cor_data <-  lastDay_data %>% subset(select = c(Population,GDP,GDPCapita,
                                                CumCases,CumTests))
cor_data

#2. Compute the correlation matrix between the variables of the "cor_data" and visualise
#this correlation matrix.

Corr_Matrx <- round(cor(cor_data),2)
Corr_Matrx

ggcorr(cor_data,label=T,label_alpha = TRUE)

#3. visualize the distribution of the cumulative cases in the cor_data with and without
#changing the scale of the x axis to log transformation.
#[Hint: you can use the geom_histrogram function]

ggplot(cor_data, aes(x = CumCases, fill = ..count..)) + 
  geom_histogram(bins=30) +
  xlab("Cumulative Cases") +
  ylab("Number of observations") +
  ggtitle("Histogram of Cumulative Cases")


ggplot(cor_data, aes(x = log(CumCases), fill = ..count..)) + 
  geom_histogram(bins=30) +
  xlab("Cumulative Cases") +
  ylab("Number of observations") +
  ggtitle("Histogram of Cumulative Cases-log Transformation")


#4. Print the outlier values of the cumulative cases in "cor_data".
outlier_values <- boxplot.stats(cor_data$CumCases)$out 
outlier_values


#5. Divide the cor_data into training and testing, where training data represent 65%
#of the number of rows.

split <- resample_partition(cor_data, c(train=0.65, test=0.35))
train <- as.data.frame(split$train)
test <- as.data.frame(split$test)
train
test

#6. Train a linear regression model to predict cumulative cases from the GDP of the
#countries. Then, evaluate this model on the test data and print the root mean
#square error value.

#linear model for both train and test dataset using GDP alone as dependent variable
mlm_train <- lm(CumCases ~ GDP, train)
mlm_test <- lm(CumCases ~ GDP, test)
summary(mlm_train)
summary(mlm_test)

#Outliers shown for model with train dataset
library(car)
outlierTest(mlm_train)

#residual plots for both train and test dataset
par(mfrow=c(2, 2))
plot(mlm_train)
plot(mlm_test)

#RMSE for both train and test dataset
rmse(mlm_train, split$train)
rmse(mlm_test, split$test)


#7. Train another linear regression model to predict cumulative cases from all the
#other variables. Then, evaluate this model on the test data and print the root
#mean square error value.

#linear model of both train and test dataset using all the explanatory variables
#from cor_data
mlm1_train <- lm(CumCases ~., train)
mlm1_test <- lm(CumCases ~ ., test)
summary(mlm1_train)
summary(mlm1_test)

#residual plots for both train and test dataset models
par(mfrow=c(2, 2))
plot(mlm1_train)
plot(mlm1_test)

#RMSE values for both models
rmse(mlm1_train, split$train)
rmse(mlm1_test, split$test)



###################--------End of the code------################################



