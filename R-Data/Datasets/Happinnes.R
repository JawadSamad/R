library(dplyr)
library(ggplot2)
library(ggthemes)

#Hoofdvraag: is geluk uit te drukken in geld
df_2018 <- read.csv("2018.csv", sep = ",")
df_2019 <- read.csv("2019.csv", sep = ",")

#checks the header of my datasets
head(df_2018)
head(df_2019)

#structure of my datasets
str(df_2018)
str(df_2019)

#checks if there is any NA values
any(is.na(df_2018))
any(is.na(df_2019))

#shows the summary of the datasets
summary(df_2018)
summary(df_2019)

#--------------------------------------------------------------------------------------------#
#2018 - Basic tasks
#Arranging dataset on GDP and Score 2018
df_arrangedGdpAndLuck2018 <- arrange(df_2018, desc(GDP.per.capita, desc(Score)))
#Selecting specific columns just to show only those values
select(df_arrangedGdpAndLuck2018, Country.or.region, Score, GDP.per.capita, Healthy.life.expectancy)
#Creating a new dataframe which mutates all the columns into a new column named "Overall.score".
#This new column outputs the overall scores of all the columns - corruption.
df_overall.scores.2018 <- mutate(df_arrangedGdpAndLuck2018, Overall.scores = Score + GDP.per.capita + Social.support + Healthy.life.expectancy + Freedom.to.make.life.choices + Generosity - Perceptions.of.corruption)
#Getting the mean value of the overall.scores and removing (if applicable) all na values.
mean2018 <- mean(df_overall.scores.2018$Score, na.rm = TRUE)
#Calculating the standard deviation
sd2018 <- sd(df_overall.scores.2018$Score)
#Calculating the z scores of the scores in 2018
z.scores.2018 <- (df_overall.scores.2018$Score - mean2018) / sd2018
#output
z.scores.2018

#--------------------------------------------------------------------------------------------#
#2019 - Basic tasks
#Arranging dataset on GDP and Score 2019
df_arrangedGdpAndLuck2019 <- arrange(df_2019, desc(GDP.per.capita, desc(Score)))
#Selecting specific columns just to show only those values
select(df_arrangedGdpAndLuck2019, Country.or.region, Score, GDP.per.capita, Healthy.life.expectancy)
#Creating a new dataframe which mutates all the columns into a new column named "Overall.score".
#This new column outputs the overall scores of all the columns - corruption.
df_overall.scores.2019 <-mutate(df_arrangedGdpAndLuck2019, Overall.scores = Score + GDP.per.capita + Social.support + Healthy.life.expectancy + Freedom.to.make.life.choices + Generosity - Perceptions.of.corruption)
#Getting the mean value of the overall.scores and removing (if applicable) all na values.
mean2019 <- mean(df_overall.scores.2019$Score, na.rm = TRUE)
#Calculating the standard deviation
sd2019 <- sd(df_overall.scores.2019$Score)
#Calculating the z scores of the scores in 2019
z.scores.2019 <- (df_overall.scores.2019$Score - mean2019) / sd2019
#output
z.scores.2019

#--------------------------------------------------------------------------------------------#
#2018 - Visualisation
#Created a scatterplot which outputs the overall scores and the GDP. 
pl_2018 <- ggplot(data = df_overall.scores.2018, aes(x=GDP.per.capita, y=Score))
pl_2018 + geom_point(alpha=0.6, aes(size=Perceptions.of.corruption, color=Perceptions.of.corruption)) + scale_color_gradient(low="Blue", high = "Red")

#Created a histrogram which outputs the scores of the dataset 
pl_histrogram_2018 <- ggplot(data = df_overall.scores.2018, aes(x=Score))
print(pl_histrogram_2018 + geom_histogram(binwidth = 0.1, color="Red", fill="Pink", alpha=0.4))

#Created a barplot, functions as a heatmap to see if the life expectancy is better if a country has a higher GDP
pl_barplot_2018 <- ggplot(df_overall.scores.2018, aes(x=Healthy.life.expectancy, y=GDP.per.capita))
pl_barplot_2018 + geom_bin2d() + scale_fill_gradient(high = "red", low = "green")

#Created a points to label vector which I will use for corruption plot
pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Qatar", "Greece", "Argentina", "Brazil",
                   "Malta", "Italy", "China", "South Korea", "Spain",
                   "Botswana", "Indonesia", "Bhutan", "Colombia", "France",
                   "United States", "Germany", "Iran", "Luxembourg", "Norway", "Japan",
                   "New Zealand", "Singapore","Rwanda", "United Arab Emirates")

#Created a plot with circles representing the GDP of a country versus the perception of corruption
pl_corruption_2018 <- ggplot(df_overall.scores.2018, aes(x=GDP.per.capita, y=Perceptions.of.corruption, color=Country.or.region)) + geom_point(size=3, shape=1, show.legend = FALSE) + geom_smooth(aes(group=1), se=F, color="Red",show.legend = FALSE, formula = y~x)
#Used my pointsToLabel vector to indicate the specified countries which I wanted to show and not all of them
pl_corruptions_2018 <- pl_corruption_2018 + geom_text(aes(label=Country.or.region), show.legend = FALSE, data = subset(df_overall.scores.2018, Country.or.region %in% pointsToLabel),check_overlap = TRUE)
#Added in a theme for the presentation.
pl_corruptions_2018 + theme_economist_white()

#--------------------------------------------------------------------------------------------#
#2019 - Visualisation
#Created a scatterplot which outputs the overall scores and the GDP. 
pl_2019 <- ggplot(data = df_overall.scores.2019, aes(x=GDP.per.capita, y=Score))
pl_2019 + geom_point(alpha=0.6, aes(size=Perceptions.of.corruption, color=Perceptions.of.corruption)) + scale_color_gradient(low="Blue", high = "Red")

#Created a histrogram which outputs the scores of the dataset 
pl_histrogram_2019 <- ggplot(data = df_overall.scores.2019, aes(x=Score))
print(pl_histrogram_2019 + geom_histogram(binwidth = 0.1, color="Red", fill="Pink", alpha=0.4))

#Created a barplot, functions as a heatmap to see if the life expectancy is better if a country has a higher GDP
pl_barplot_2019 <- ggplot(df_overall.scores.2019, aes(x=Healthy.life.expectancy, y=GDP.per.capita))
pl_barplot_2019 + geom_bin2d() + scale_fill_gradient(high = "red", low = "green")

#Created a points to label vector which I will use for corruption plot
pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Qatar", "Greece", "Argentina", "Brazil",
                   "Malta", "Italy", "China", "South Korea", "Spain",
                   "Botswana", "Indonesia", "Bhutan", "Colombia", "France",
                   "United States", "Germany", "Iran", "Luxembourg", "Norway", "Japan",
                   "New Zealand", "Singapore","Rwanda", "United Arab Emirates")

#Created a plot with circles representing the GDP of a country versus the perception of corruption
pl_corruption_2019 <- ggplot(df_overall.scores.2019, aes(x=GDP.per.capita, y=Perceptions.of.corruption, color=Country.or.region)) + geom_point(size=3, shape=1, show.legend = FALSE) + geom_smooth(aes(group=1), se=F, color="Red",show.legend = FALSE, formula = y~x)
#Used my pointsToLabel vector to indicate the specified countries which I wanted to show and not all of them
pl_corruptions_2019 <- pl_corruption_2019 + geom_text(aes(label=Country.or.region), show.legend = FALSE, data = subset(df_overall.scores.2019, Country.or.region %in% pointsToLabel),check_overlap = TRUE)
#Added in a theme for the presentation.
pl_corruptions_2019 + theme_economist_white()


#--------------------------------------------------------------------------------------------#
#2018 - Machine learning
#Figuring out which columns has a correlation with each other
#Variable created which only subtracts the numeric values(columns) of the dataset
num.cols <- sapply(df_overall.scores.2018, is.numeric)
#Filtering the data
col.data <- cor(df_overall.scores.2018[,num.cols])
print(col.data)

#Using caTools for lineair regression
library(caTools)
#setting seed
set.seed(101)
# split up the sample, splitting the data into a 80-20 trainingdata and testingdata
sample_2018 <- sample.split(df_overall.scores.2018$Overall.scores, SplitRatio = 0.8)
# 80% of my data will be for training purposes
train_2018 <- subset(df_overall.scores.2018, sample_2018 == TRUE)
# 20% of my data will be for testing purposes
test_2018 <- subset(df_overall.scores.2018, sample_2018 == F)

#Train and building the model
model <- lm(Overall.scores ~ Score + GDP.per.capita + Social.support + Freedom.to.make.life.choices + Healthy.life.expectancy + Generosity - Perceptions.of.corruption, train_2018)
#plot(model) #for more info (plots) about the residuals

#Predictions
overall_scores_predictions <- predict(model, test_2018)

#Creating a dataframe
result <- cbind(overall_scores_predictions, test_2018$Overall.scores)
#Naming the columns 
colnames(result) <- c("Predicted", "Actual")
result <- as.data.frame(result)
#Printing the result and comparing the actual calc with the predicted calc
print(result)

#Grabbing the root mean squared errors, to see how off it is.
mse <- mean((result$Actual - result$Predicted)^2)
print(mse)