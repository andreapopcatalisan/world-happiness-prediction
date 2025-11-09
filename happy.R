
#======================World Happiness Prediction 2020===========================#
#                           Andrea Pop-Catalisan                                 #

rm(list = ls())
library(tidyverse)
library(VIM)
library(lubridate)
library(plyr)
library(dplyr)
library(factoextra)
library(magrittr)
library(arules)
library(data.table)
library(testthat)
library(caret)
library(pROC)
library(xgboost)
library(rgdal)
library(stringr)
library(gridExtra)
library(tidyr)
library(ggthemes)
library(stringr)
library(readxl)
setwd("~/Desktop")
df <- read_excel("happy.xls")

################################Data cleaning#################################

# Cleaning the data set with missing values.
# In general, if less than 5% of the observations are missing, the missing data can simply be deleted,
# it this case, there are 0.7 missing values and removing these observations would be a loss of data, then I am going replace missing values with the mean of the column.
#CHECK MISSING VALUES:

str(df)
dim(df)
df = df[, colMeans(is.na(df)) <= 0.05]
dim(df)
#no columns removed

missing_values <- data.frame(column = names(sapply(df, class)),
                             class = sapply(df, class),
                             missing.count = colSums(is.na(df)),
                             missing.pct = round(colSums(is.na(df)) / nrow(df) * 100, digits = 1))
missing_values

#these are the variables with missing values:
df$explained_by_log_gdp_per_capita[is.na(df$explained_by_log_gdp_per_capita)]<-mean(df$explained_by_log_gdp_per_capita,na.rm=TRUE)
df$explained_by_social_support [is.na(df$explained_by_social_support )]<-mean(df$explained_by_social_support,na.rm=TRUE)
df$explained_by_healthy_life_expect[is.na(df$explained_by_healthy_life_expect)]<-mean(df$explained_by_healthy_life_expect,na.rm=TRUE)
df$explained_by_freedom_to_make_lif[is.na(df$explained_by_freedom_to_make_lif)]<-mean(df$explained_by_freedom_to_make_lif,na.rm=TRUE)
df$explained_by_generosity[is.na(df$explained_by_generosity)]<-mean(df$explained_by_generosity,na.rm=TRUE)
df$explained_by_perceptions_of_corr [is.na(df$explained_by_perceptions_of_corr)]<-mean(df$explained_by_perceptions_of_corr,na.rm=TRUE)
df

#now there are not missing values:
missing_values <- data.frame(column = names(sapply(df, class)),
                             class = sapply(df, class),
                             missing.count = colSums(is.na(df)),
                             missing.pct = round(colSums(is.na(df)) / nrow(df) * 100, digits = 1))
missing_values
#In the case of outliers, omitting these values would be mean loss of relevant data for the analysis.

################################Data visualization###################################

#install.packages("ggthemes")
library(ggthemes)
library(plotly)
library(ggplot2)
library(GGally)
library(corrplot)

# Density plot of ladder scores.
#png("latex/plot1.png")
ggplot(df, aes(x = ladder_score)) +
  geom_density(fill = "aliceblue") +
  theme_bw() +
  xlab("Ladder score")
#dev.off()

#Plot 1: Happiness per regions:
#png("latex/plot1.png")
ggplot(data = df, aes(x = df$regional_indicator, y = df$ladder_score)) +
  geom_boxplot(aes(color = regional_indicator, fill = regional_indicator), alpha = 0.5) +
  geom_point(aes(color = regional_indicator), position = position_jitter(width = .1)) +
  labs(title = "Happiness by Region", 
       x = "Region", 
       y = "Ladder Score") +
  theme_minimal() +
  theme(plot.title = element_text(size = rel(2.5)),
        axis.title = element_text(size = rel(1.5)),
        axis.text.x = element_blank())
#dev.off()

#Graph importance factors:
#png("latex/plot2.png")
gr<- subset(df, select = c("country_name", "regional_indicator", "ladder_score", "explained_by_log_gdp_per_capita", "explained_by_social_support", "explained_by_healthy_life_expect", "explained_by_freedom_to_make_lif" , "explained_by_generosity", "explained_by_perceptions_of_corr", "beer", "wine", "spirits"))
names(gr)[names(gr) == "country_name"] <- "Country"
names(gr)[names(gr) == "regional_indicator"] <- "Region"
names(gr)[names(gr) == "ladder_score"] <- "LadderScore"
names(gr)[names(gr) == "explained_by_log_gdp_per_capita"] <- "Economy"
names(gr)[names(gr) == "explained_by_social_support"] <- "Family"
names(gr)[names(gr) == "explained_by_healthy_life_expect"] <- "Health"
names(gr)[names(gr) == "explained_by_generosity"] <- "Generosity"
names(gr)[names(gr) == "explained_by_perceptions_of_corr"] <- "Trust"
names(gr)[names(gr) == "explained_by_freedom_to_make_lif"] <- "Freedom"
dfwide <- gr %>%
  head(10)
dflong <- gather(dfwide, Factor, `Importance of Factor`, Economy:Trust, factor_key=TRUE)
ggplot(data = dflong) +
  geom_bar(stat = "identity", 
           aes(x = Country, y = `Importance of Factor`, fill = Factor)) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "top") +
  labs(title = "Factors of Happiness per Country") +
  theme(plot.title = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.5)))
#dev.off()

#png("latex/plot3.png")
#install.packages("viridis")
library(dplyr)
library(formattable)
library(viridis)
open_data_happiness <- gr[c("Region", "LadderScore","Economy")]
ggplot(open_data_happiness, 
       aes(x = LadderScore, 
           y = Economy)) +
  geom_point(aes(colour = Region),
             size = 2) +
  geom_smooth(method="lm") +
  labs(x = "LadderScore",
       y = "Economy",
       title = "Relationship Between Happiness and Economics") +
  scale_color_viridis(discrete = T) +
  theme_minimal() +
  theme(text = element_text(size=16))
#dev.off()

################################Factor analysis#################################

# Factor analysis: MLE
# simply use "factanal" 
# MLE always scales the data.

library(readr)
#install.packages("chron")
library(chron)
library(tidyr)
library(forecast)

#Correlation matrix
#png("latex/plot4.png")
ggcorr(gr[,3:11], label = T)
#dev.off()

#WITH 3 FACTORS
# Let's fit a 3-factor model with no rotation
attach(df)
Q <- cbind(logged_gdp_per_capita, social_support, freedom_to_make_life_choices, healthy_life_expectancy, alcoloholperyear, alcoholperday, spirits)
Q.f <- factanal(Q, factors = 3, rotation="none", scores="regression")
Q.f
cbind(Q.f$loadings, Q.f$uniquenesses)

#Plot Bars
#png("latex/plot5.png")
par(mfrow=c(3,1))
barplot(Q.f$loadings[,1], names=F, las=2, col="cornflowerblue", ylim = c(-1, 1))
barplot(Q.f$loadings[,2], names=F, las=2, col="cornflowerblue", ylim = c(-1, 1))
barplot(Q.f$loadings[,3], las=2, col="cornflowerblue", ylim = c(-1, 1))
#dev.off()

#PLOT RESULTS:
#png(filename="latex/plot6.png")
Q.fa.none <- factanal(Q, factors = 2, rotation = "none")
Q.fa.varimax <- factanal(Q, factors = 2, rotation = "varimax")
Q.fa.promax <- factanal(Q, factors = 2, rotation = "promax")
par(mfrow = c(1,3))
plot(Q.fa.none$loadings[,1], 
     Q.fa.none$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "No rotation")
abline(h = 0, v = 0)
plot(Q.fa.varimax$loadings[,1], 
     Q.fa.varimax$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Varimax rotation")
text(Q.fa.varimax$loadings[,1]-0.08, 
     Q.fa.varimax$loadings[,2]+0.08,
     colnames(Q),
     col="cornflowerblue")
abline(h = 0, v = 0)
plot(Q.fa.promax$loadings[,1], 
     Q.fa.promax$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2",
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Promax rotation")
abline(h = 0, v = 0)
#<dev.off()

#NOW WITH 2 PACTORS (P-VALUE=0).
Q.f <- factanal(Q, factors = 2, rotation="varimax", scores="Bartlett", lower = 0.001)
Q.f
cbind(Q.f$loadings, Q.f$uniquenesses)
################################Cluster analysis#################################
library(NbClust)
library(cluster)
library(FactoMineR)
library(factoextra)
library(reshape2)

#Before implementing Kmeans, it is important to take a look to the PCA:
#PCA analysis:
h1<- subset(df, select = c("logged_gdp_per_capita", "healthy_life_expectancy", "social_support", "ladder_score"))
h1[, 1:4] <- scale(h1[, 1:4])
pca = prcomp(h1)
fviz_screeplot(pca, addlabels = TRUE)
#The elbow effect appears from the 1st component. This indicates that the projection of 
#the dataset on the first factorial plan (1,2) captures most of the information and allows to a better description of the data.

# Variability of each principal component
pr_var <- pca$sdev ^ 2
pr_var
# Variance explained by each principal component: pve
pve <- pr_var / sum(pr_var)

# Plot variance explained for each principal component
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

#Variables that are correlated with PC1 and PC2 are the most important in explaining the variability in the data.
# How many clusters?
#Pam Clustering Analysis to group countries by happiness, GDP, health and social support.

#png(filename="latex/plot7.png")
number <- NbClust(h1[, 1:4], distance="euclidean",
                  min.nc=2, max.nc=15, method='kmeans', index='all', alphaBeale = 0.1)
#dev.off()

# Other methods:
#png(filename="latex/nuevo1.png")
fviz_nbclust(h1, kmeans, method = 'wss')
#dev.off()
#png(filename="latex/nuevo2.png")
fviz_nbclust(h1, kmeans, method = 'silhouette')
#dev.off()
#According to the majority rule, the best number of clusters is 3.

# kmeans with 2 clusters
fit = kmeans(h1, centers=2, nstart=100)
groups = fit$cluster
groups
barplot(table(groups), col="blue")
centers=fit$centers
centers
# clusplot
#png(filename="latex/plot8.png")
fviz_cluster(fit, data = h1, geom = c("point"),ellipse.type = 'norm', pointsize=1)+
  theme_minimal()+geom_text(label=country_name,hjust=0, vjust=0,size=2,check_overlap = T)+scale_fill_brewer(palette="Paired")
#dev.off()

#cluster silhuette plot
#png(filename="latex/plot9.png")
pam <- pam(h1[, 1:4], diss=FALSE, 2, keep.data=TRUE)
fviz_silhouette(pam)
#dev.off()

#ONLY LOOKING AT LADDER SCORE AND GDP PER CAPITA:
#So that the procedure of the hierarchical clustering algorithm:
set.seed(1234)

df <- df %>% 
  sample_n(145)

df %>% 
  ggplot(aes(logged_gdp_per_capita, 
             ladder_score, 
             label = country_name )) +
  geom_point() +
  geom_text(size = 3,
            check_overlap = FALSE,
            vjust = 0, nudge_y = 0.5) +
  theme_classic() +
  ylab("Happiness Score") +
  xlab("Gross domestic product per person (log)")

#I create a new data record dfc, which only contains the variables that are going to be used 
#for the cluster analysis. In addition, I use the variable country_name in order to be able
#to label the data meaningfully in a later step.

library(dplyr)
dfc <- dplyr::select(df, c("country_name", 
                             "logged_gdp_per_capita", 
                             "ladder_score"))

#Check whether there are any missing values in the data:
sum(is.na(dfc))

#So that the values of the variables are available in a uniform value interval, 
#we use the z-transformation to standardize the data.

dfc$ladder_score <-  scale(dfc$ladder_score, 
                                           center = TRUE, 
                                           scale = TRUE)

dfc$logged_gdp_per_capita_sc <-  scale(dfc$logged_gdp_per_capita, 
                                         center = TRUE, 
                                         scale = TRUE)

#I use the Euclidean distance as a measure of proximity and save the result of the function dist (), 
#which calculates the distance between all countries, with the designation d. 
#Since I don't want to include the variable country_name in the calculation, I remove it in the select () command.

library(arules)
d <- 
  dfc %>% 
  dplyr::select(-country_name) %>% 
  dist(method = "euclidean")

#The next step is to use the hierarchical cluster analysis with the hclust () command. 
#Dendrogram:
hc <- hclust(d, method = "ward.D2") 
sort(unique(cophenetic(hc)))
plot(hc) 
hc$labels <- df$country_name
plot(hc)

#Dendogram with red borders:
#png(filename="latex/plot10.png")
hc$labels <- df$country_name
plot(hc)
rect.hclust(hc, k = 2, border = "red")
#dev.off()

#Tree dendogram:
#png(filename="latex/tree.png")
library("igraph")
set.seed(5665)
fviz_dend(x = hc,
          k = 2,
          k_colors = c("#2E9FDF", "#00AFBB"),
          color_labels_by_k = TRUE,
          cex = 0.8,
          type = "phylogenic",
          repel = TRUE)
#dev.off()

#Kmeans plot with 2 clusters
#png(filename="latex/plot11.png")
group2 <- cutree(hc, k = 2) 
dfc$cluster_2 <- group2

dfc %>% 
  ggplot(aes(logged_gdp_per_capita_sc, 
             ladder_score, 
             label = country_name, 
             color = factor(cluster_2))) +
  geom_point() +
  geom_text(size = 3,
            check_overlap = FALSE,
            vjust = 0, nudge_y = 0.5,
            show.legend = FALSE) +
  theme_classic() +
  ylab("GDP per capita (log)") +
  xlab("Ladder Score") +
  theme(legend.title=element_blank())
#dev.off()

#====================Discriminant Analysis and Classification===================#

#install.packages("rpart")
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
#install.packages("e1071")
library(e1071)
library(modelr)
library(broom)
library(ISLR)
library(ROCR)
library(MASS)
library(caret)
library(caTools)
library(FNN)

#################################################################################

#Clasification
df1<- subset(df, select = c("ladder_score", "social_support", "generosity" , "wine", "alcoloholperyear", "beer", "freedom_to_make_life_choices", "spirits", "healthy_life_expectancy", "explained_by_log_gdp_per_capita"))
subset.data <- df1 %>% mutate(ladder_score = ifelse(ladder_score > 5 , 1, 0))
            
#DISTRIBUTION OF THE DATA:
#png(filename="latex/plot12.png")
ggplot(subset.data, aes(x = as.factor(ladder_score))) +
  geom_bar(aes(y = (..count..) / sum(..count..)), fill = 'cornflowerblue') +
  scale_y_continuous(labels = scales::percent) +
  ylab('') +
  xlab('Ladder Score') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(labels = c('Ladder Score ≤5' , 'Ladder Score >5'))
#dev.off()

#################################################################################

subset.data$ladder_score <- as.factor(subset.data$ladder_score)
#LOGISTIC REGRESSION#:
#png(filename="latex/plot11.png")
n = dim(subset.data)[1]
train <- sample(1:n, n*0.75)  # 75% of the sample for training
# distribution of DELAY in training set
table(subset.data$ladder_score[train])/length(train)
# distribution of DELAY in testing set
table(subset.data$ladder_score[-train])/(n-length(train))
# Logistic regression:
#predict this variable as the function of the other variables ( ~ .) 
logit.model <- glm(ladder_score ~ ., family=binomial(link='logit'), data=subset.data[train,])
# make predictions:
probability <- predict(logit.model,newdata=subset.data[-train,], type='response')
head(probability)
# Prediction is made by Bayes' rule:
prediction1 <- ifelse(probability > 0.5,1,0)
head(prediction1)
# Performance: confusion matrix
ConfMat = table(prediction1, subset.data$ladder_score[-train])
ConfMat
confusionMatrix(ConfMat)
# Performance: classification error and accuracy
ClasificError <- mean(prediction1 != subset.data$ladder_score[-train])
print(paste('Error',ClasificError))
print(paste('Accuracy',1-ClasificError))

#################################################################################

#Linear Discriminant Analysis
attach(subset.data)
lda.model <- lda(ladder_score ~ ., data=subset.data, prior = c(.3, .7), CV=T)
lda.model
head(lda.model$class)
# Confusion matrix (for the maximum-probability rule)
ConfMat2 = table(subset.data$ladder_score, lda.model$class)
ConfMat2
confusionMatrix(ConfMat2)
# Computation of the proportion of errors
ClasificError <- (n - sum(diag(ConfMat2))) / n
ClasificError
print(paste('CV Accuracy',1-ClasificError))

#################################################################################

#Quadratic Discriminant Analysis (QDA)
# Now with CV, complete sample
qda.model <- qda(ladder_score ~ ., data=subset.data, prior = c(.3, .7), CV=T)
head(qda.model$class)
# Confusion matrix (for the maximum-probability rule)
ConfMat3 = table(subset.data$ladder_score, qda.model$class)
ConfMat3
confusionMatrix(ConfMat3)
# Computation of the proportion of errors
ClasificError3 <- (n - sum(diag(ConfMat3))) / n
ClasificError3
print(paste('CV Accuracy',1-ClasificError3))

#################################################################################

#NAIVE BAYES:
naive.model <- naiveBayes(ladder_score ~ ., data=subset.data[train,], laplace=1, prior = c(.3, .7))
naive.model
prediction2 = predict(naive.model, newdata=subset.data[-train,])
# Performance: confusion matrix
ConfMat4= table(prediction2, subset.data$ladder_score[-train])
ConfMat4
confusionMatrix(ConfMat4)
# Computation of the proportion of errors
ClasificError4 <- (n - sum(diag(ConfMat4))) / n
ClasificError4
print(paste('CV Accuracy',1-ClasificError4))

#################################################################################

