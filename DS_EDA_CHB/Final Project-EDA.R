main<-read.csv(file="C:\\Users\\bushc\\OneDrive\\Desktop\\DS-450\\2017_18 telemetry lock and dam 19 analysis master.csv",header=TRUE,sep=",")
boxplot(main$Species)
library(dplyr)
library(ggplot2)
library(tidyverse)


## CHECKING THE DATASET
head(main)
str(main)
dim(main)
summary(main)
colSums(is.na(main))
missing_percent <- colSums(is.na(main)) / nrow(main) * 100
missing_percent

## DATASET SUMMARY STATISTICS 
summary(main)


# select only numeric columns
num_data<-main %>% select(where(is.numeric))
# correlation matrix
cor_matrix<-cor(num_data,use= "complete.obs")
cor_df <- as.data.frame(as.table(cor_matrix))
ggplot(cor_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",      
    mid = "white",    
    high = "red",   
    midpoint = 0
  ) +
  theme_minimal() +
  labs(title = "Correlation Heatmap", fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

num_data %>%
  summarise(across(everything(),
                   list(mean = mean,
                        median = median,
                        sd = sd,
                        min = min,
                        max = max),
                   na.rm = TRUE))

main %>% count(Species)
main %>% count(Season)
main %>% count(AGENCY)


table(main$Species)
prop.table(table(main$Season))

sapply(main, sd, na.rm = TRUE)

species_count<-main %>%
  count(Species)
print(species_count)

## DATASET GRAPGICAL EXPLORATION 
ggplot(main, aes(Species)) + geom_bar()
ggplot(main, aes(Season)) + geom_bar()

ggplot(main, aes(Week, Temp)) + geom_line()

main$Date <- as.Date(main$Date)

head(main$Date)
main$Date <- as.Date(main$Date, format = "%m/%d/%Y")

ggplot(main, aes(x = Date, y = Stage.Ft)) +
  geom_line() +
  labs(title = "River Stage Height Over Time",
       x = "Date",
       y = "Stage.Ft") +
  theme_minimal()

boxplot(main$Temp,main="Boxplot of Temperature")
boxplot(main$Length,main="Boxplot of Fish Length")
boxplot(main$DURATION.min,main="Boxplot of Duration in Minutes")

