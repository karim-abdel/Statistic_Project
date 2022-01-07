##We start by loading some useful libraries that we will need during this process

library(readr) #Used to read the csv
library(tidyverse) #Contains a lot of useful packages to clean the datas
library(gganimate) #To create animated plots
library(ggthemes) #To select some nice themes 
library(zoo) #To fill the N.A. values 
library(grid) #To display plots into a grid
library(gridExtra) #To display plots into a grid
library(ggpubr)#To display plots into a grid
library(olsrr)#For model analysis

##We now load the row csv files what we need to merge 

Cancer <- read_csv("Datasets/Cancer.csv")
Air_pollution <- read_csv("Datasets/Air pollution.csv")
Alcool <- read_csv("Datasets/Alcool.csv")
GDP_per_capta <- read_csv("Datasets/GDP per capta.csv")
Obesity <- read_csv("Datasets/Obesity.csv")
Old_age_dependency_ratio <- read_csv("Datasets/Old age dependency ratio.csv")
Smoking <- read_csv("Datasets/Smoking.csv")
Population <- read_csv("Datasets/population-since-1800.csv")
countryContinent <- read_csv("Datasets/countryContinent.csv")

#We proceed by taking the explanatory variable data set (Cancer) and adding the covariates as columns other columns, to do that we use the use the libraby "dplyr" as it makes it very easy to do and readable

Continent = countryContinent %>% select(country, continent)
Continentt = rename(Continent, Entity = country)
Cancer1 = left_join(Cancer, Air_pollution, by = c("Entity", "Code", "Year"))
Cancer2 = left_join(Cancer1, Alcool, by = c("Entity", "Code", "Year"))
Cancer3 = left_join(Cancer2, GDP_per_capta, by = c("Entity", "Code", "Year"))
Cancer4 = left_join(Cancer3, Obesity, by = c("Entity", "Code", "Year"))
Cancer5 = left_join(Cancer4, Old_age_dependency_ratio, by = c("Entity", "Code", "Year"))
Cancer6 = left_join(Cancer5, Smoking, by = c("Entity", "Code", "Year"))
Cancer7 = left_join(Cancer6, Continentt, by = "Entity")
Cancer_final = Cancer7

rm("Cancer1", "Cancer2", "Cancer3", "Cancer4", "Cancer5", "Cancer6", "Cancer7")
rm("Air_pollution", "Alcool", "Cancer", "GDP_per_capta", "Obesity", "Old_age_dependency_ratio", "Smoking", "Continent", "Continentt", "countryContinent")

#We now proceed by renaming the columns. This will make all the manipulations of the dataframe more readable:

colnames(Cancer_final)[4] <- "Cancer"
colnames(Cancer_final)[5] <- "AirPoll"
colnames(Cancer_final)[6] <- "Alcool"
colnames(Cancer_final)[7] <- "GDP"
colnames(Cancer_final)[8] <- "Obesity"
colnames(Cancer_final)[9] <- "Age"
colnames(Cancer_final)[10] <- "Smoking"

Cancer_final
write.csv(Cancer_final, "Cancer_final.csv", row.names=FALSE, quote=FALSE) #to save the file into our environment


#We read the file needed
share_of_population_with_cancer <- read_csv("Cancer.csv")  #read the file 
share_of_population_with_cancer = data.frame(lapply(share_of_population_with_cancer, function(x) {
  gsub("United States", "USA", x) #correction needed for the mapping 
}))
colnames(share_of_population_with_cancer)[1] <- "region" #Give the same name for the country in the two datasets
colnames(share_of_population_with_cancer)[4] <- "cancer" #Dropping the complicated name given by the data frame

# Create the actual map that we will use
#We will use the ggplo2 package for that

mapdata = map_data("world") #Used to take the latitude and longitude of the countries 
mapp <- mapdata %>%
  left_join(share_of_population_with_cancer, by = "region") #merge the two datasets
mapp2 = mapp %>% 
  filter(!is.na(mapp$cancer)) #dealing with NA
mapp2$cancer <- as.numeric(mapp2$cancer) #changing the value to numeric for plotting
mapp2$Year <- as.numeric(mapp2$Year) #changing the value to numeric for animation


## BUILDING THE MAP: Warning: This could take a very long time : (
#Here we create a static map
ggplot( data = mapp2, aes(long, lat, group = subregion)) +
  geom_map(
    aes(map_id = region),
    map = mapdata,
    color = "black", fill = "gray30", size = 0.3
  ) +
  geom_polygon(aes(group = group, fill = cancer), color = "black") +
  scale_fill_gradient2(low = "blue", high = "red" , midpoint = 1.0355583) +
  theme_minimal() +
  labs(
    title = "Share of population with cancer in 2017",
    x = "", y = "", fill = ""
  ) +
  theme_bw()
ggsave("Share of people with cancer in 2017.jpg")
##End of World map plotting

#Some data cleaning
Cancergraph = left_join(Cancer_final, Population, by = c("Entity", "Code", "Year"))
Cancergraph$Smoking = na.locf(Cancergraph$Smoking)
Cancergraph$AirPoll = na.locf(Cancergraph$AirPoll)
Cancergraph$Alcool = na.locf(Cancergraph$Alcool)
Cancergraph$GDP = na.locf(Cancergraph$GDP, fromLast = TRUE)
Cancergraph$Age = na.locf(Cancergraph$Age)
Cancergraph$Obesity = na.locf(Cancergraph$Obesity)
Cancergraph$`Population (historical estimates)` = na.locf(Cancergraph$`Population (historical estimates)`)


#Here we plots

Cancergraph = subset(Cancergraph, Year == "2017")
graphPoll = Cancergraph %>%
  ggplot(aes(x = AirPoll, y = Cancer, color = continent, size = `Population (historical estimates)`)) +
  geom_point(alpha = 0.9, stroke = 0) +
  scale_size(range = c(2,12), guide = "none") +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Cancer incidence vs Air Pollution", 
       x = "Air pollution exposure ", 
       y = "Share of people with Cancer",
       color = "Continent",
       caption = "Source: Our World in Data") +
  labs (subtitle = "Year: 2017}") 



graphAlco = Cancergraph %>%
  ggplot(aes(x = Alcool, y = Cancer, color = continent, size = `Population (historical estimates)`)) +
  geom_point(alpha = 0.9, stroke = 0) +
  scale_size(range = c(2,12), guide = "none") +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Cancer incidence vs Alcool consumption", 
       x = "Alcool consumed", 
       y = "Share of people with Cancer",
       color = "Continent",
       caption = "Source: Our World in Data") +
  labs (subtitle = "Year: 2017") 



graphGDP = Cancergraph %>%
  ggplot(aes(x = GDP, y = Cancer, color = continent, size = `Population (historical estimates)`)) +
  geom_point(alpha = 0.9, stroke = 0) +
  scale_size(range = c(2,12), guide = "none") +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Cancer incidence vs GDP", 
       x = "GDP per capta", 
       y = "Share of people with Cancer",
       color = "Continent",
       caption = "Source: Our World in Data") +
  labs (subtitle = "Year: 2017") 


graphObes = Cancergraph %>%
  ggplot(aes(x = Obesity, y = Cancer, color = continent, size = `Population (historical estimates)`)) +
  geom_point(alpha = 0.9, stroke = 0) +
  scale_size(range = c(2,12), guide = "none") +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Cancer incidence vs Obesity", 
       x = "Share of Obese people", 
       y = "Share of people with Cancer",
       color = "Continent",
       caption = "Source: Our World in Data") +
  labs (subtitle = "Year: 2017") 


graphAge = Cancergraph %>%
  ggplot(aes(x = Age, y = Cancer, color = continent, size = `Population (historical estimates)`)) +
  geom_point(alpha = 0.9, stroke = 0) +
  scale_size(range = c(2,12), guide = "none") +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Cancer incidence vs Age", 
       x = "Old people ratio", 
       y = "Share of people with Cancer",
       color = "Continent",
       caption = "Source: Our World in Data") +
  labs (subtitle = "Year: 2017") 


graphSmoke = Cancergraph %>%
  ggplot(aes(x = Smoking, y = Cancer, color = continent, size = `Population (historical estimates)`)) +
  geom_point(alpha = 0.9, stroke = 0) +
  scale_size(range = c(2,12), guide = "none") +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Cancer incidence vs Smoking", 
       x = "Number of Smokers", 
       y = "Share of people with Cancer",
       color = "Continent",
       caption = "Source: Our World in Data") +
  labs (subtitle = "Year: 2017") 


pplot = ggarrange(graphAge, graphAlco, graphGDP, graphObes, graphPoll, graphSmoke + rremove("x.text"), ncol = 2, nrow = 3)


# Delete the N.A.s
mydata = read.csv("Cancer_final.csv")
data5 = na.omit(mydata)

# Delete USA and Canada from the data set
data3 = subset(data5, Entity != "USA")

#Normality of the residuals
model = lm( Cancer~ Smoking + Obesity + GDP + Age + AirPoll + Alcool, data = data3)

#Analyzing the regression
summary(model)

#Perform Testing for normality of residuals
ols_test_normality(model)

#plot histogram of residuals
kop = residuals.lm(model)
hist(kop, breaks = 50, main = "Histogram of the residuals", xlab = "Residual")

#Check graphically using the function plot
plot(model)



#Model selection: Stepdown
frwfit <- ols_step_forward_p(model, pent = 0.05)
frwfit

#Model selection Stepdown
bkwfit <- ols_step_backward_p(model, prem = 0.05)
bkwfit

#Model Sekection Stepboth
bothfit <- ols_step_both_p(model, pent = 0.05, prem = 0.05)
bothfit

#Model Selection All subset models
allsub2 <- ols_step_best_subset(model)
allsub2

#Model Selection: Best models
allsub2 <- ols_step_best_subset(model)
allsub2





