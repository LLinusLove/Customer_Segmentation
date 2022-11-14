# Import the required packages into R
install.packages("tidyverse","cluster")
install.packages("janitor")
library(tidyverse)
library(cluster)
library(janitor)

# Import and explore the dataset
mall_customers <- read.csv("Mall_Customers.csv")
View(mall_customers)

# Rename the column name
names(mall_customers)
mall_customers <- clean_names(mall_customers) # use clean_names function in janitor packagge 
mall_customers <- rename(mall_customers, annual_income = annual_income_k,
                         spending_score = spending_score_1_100)

# EDA on dataset 
# Is there any missing value in the dataset ? 
sum(is.na(mall_customers)) # The results showed that there is no missing value in the dataset 

# Explore numerical features
# statistical way 
summary(mall_customers)

# intuitive way (gender)
ggplot(mall_customers, aes(x = gender))+
  geom_bar(stat = "count", width = 0.5, fill = "grey")+
  theme_minimal()+
  labs(title = "Bar chart to display gender comparision",
       xlab = "Gender", ylab = "Count")

# intuitive way (age)
ggplot(mall_customers, aes(x = age))+
  geom_histogram()+
  labs(title = "Histogram to show age distribution")

ggplot(mall_customers, aes(x = age)) +
  geom_vline(aes(xintercept = mean(age)), color = "blue",
             linetype = "dashed", size = 1.5) +
  geom_histogram(binwidth = 5, aes(y = ..density..), 
                 color = "black", fill = "white") +
  geom_density(alpha = 0.4, fill = "red") +
  labs(title = "Histogram to Show Density of Age Class")

ggplot(mall_customers, aes(x = age, fill = gender, color = gender)) +
  geom_histogram(bins = 10, position = "identity", alpha = 0.5)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

# intuitive way (annual_income)
ggplot(mall_customers, aes(x = annual_income)) + 
  geom_density() +
  labs(title = "Density plot for the annual income") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))

# intuitive way (spending_score)
ggplot(mall_customers, aes(x = spending_score, y = gender))+
  geom_boxplot()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  labs(title = "Boxplot for spending score by gender")

# Determine the number of clusters (gap statistics method)
set.seed(125)
stat_gap <- clusGap(mall_customers[, 3:5], FUN = kmeans, nstart = 25, 
                    K.max = 10, B = 50)
plot(stat_gap)

# K-Means Clustering 
k6 <- kmeans(mall_customers[, 3:5], 6, iter.max = 100, nstart = 50,
             algorithm = "Lloyd")
k6
# Show the six KMeans clusters
clusplot(mall_customers, k6$cluster, color = TRUE, shade = TRUE,
         labels = 0, lines = 0)

# Perform PCA analysis 
pcclust <- prcomp(mall_customers[, 3:5], scale = FALSE)

# Summary of the PCA model 
summary(pcclust)

# Apply the PCA model on the data 
pcclust$rotation[, 1:2]

set.seed(1)

# Create a plot of the customers segments
ggplot(mall_customers, aes(x = annual_income , y = spending_score)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name = " ", 
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", 
                                "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", 
          subtitle = "Using K-means Clustering")



