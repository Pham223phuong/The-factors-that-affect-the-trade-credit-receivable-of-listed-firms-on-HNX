#required packages
library(tidyverse)
library(readxl)
library(ggplot2)
library(lmtest)

#load data
data <- read_excel("D:/học tập/NĂM 3/hk 6/gói ứng dụng trong tài chính/040522 Data Mid-term test Final.xlsx")
head(data)
#rows and columns
dim(data)
#all variable names
colnames(data)

#select firms listed on HNX
df <- data %>% 
  filter(exchangename=='HANOI STOCK EXCHANGE')
dim(df)

#select features
df <- df %>% 
  select(firmcode, firmname, receivable, totalasset,
         currentliabilities, ppe, totalequity, industry)

#create a random sample
set.seed(927)
df <- 
  df[sample(1:nrow(df), 100), ]
dim(df)
head(df)

#identify number of NAs in data frame
colSums(is.na(df))

#replace missing values with the median value of the corresponding variable
df$receivable[is.na(df$receivable)]=median(df$receivable,na.rm=T)
df$totalasset[is.na(df$totalasset)]=median(df$totalasset,na.rm=T)
df$currentliabilities[is.na(df$currentliabilities)]=median(df$currentliabilities,na.rm=T)
df$ppe[is.na(df$ppe)]=median(df$ppe,na.rm=T)
df$totalequity[is.na(df$totalequity)]=median(df$totalequity,na.rm=T)

#check number of NAs again
colSums(is.na(df))

#create discrete variable from continuous variables
df$type_firm <- ifelse(df$totalequity <= 100000000000, 0, 1)

#create continuous variables
df <- df %>% 
  mutate(debt=currentliabilities/totalasset,
         FA=ppe/totalasset)

#create dependent variable
df <- df %>% 
  mutate(AR=receivable/totalasset)

head(df)

#5 firms with highest trade credit
top5_highest <- df %>% 
  select(firmcode, firmname, AR, industry) %>%
  arrange(desc(AR)) %>%
  head(5)
top5_highest

#5 firms with lowest trade credit
top5_lowest <- df %>% 
  select(firmcode, firmname, AR, industry) %>%
  arrange(AR) %>%
  head(5)
top5_lowest

#descriptive statistics of AR
#different categories of the discrete variable
discrete_variable <- df %>% 
  group_by(type_firm) %>% 
  summarize(mean_AR = mean(AR),
            median_AR = median(AR),
            min_AR = min(AR),
            max_AR = max(AR),
            sd_AR = sd(AR))
discrete_variable

#groups of above/below median of the continuous variable
continuous_variable <- df %>% 
  group_by(debt>median(debt)) %>% 
  summarize(mean_AR = mean(AR),
            median_AR = median(AR),
            min_AR = min(AR),
            max_AR = max(AR),
            sd_AR = sd(AR))
continuous_variable

#data visualization
#histogram of trade credit
ggplot(df, aes(x = AR)) +
  geom_histogram(fill = "lightblue", color = "black") +
  labs(title = "Histogram of AR", x = "AR", y = "Frequency")

#scatter plot of trade credit with the continuous variable
plot(AR ~ debt, data=df)
plot(AR ~ FA, data=df)

#boxplot of trade credit with the discrete variable
df %>% 
  ggplot(aes(x = type_firm,
             y = AR,
             fill = as.factor(type_firm))) +
  geom_boxplot()

#plot that allow the combination of continuous, discrete variables and trade credit
ggplot(df, aes(x = debt, y = AR, color = as.factor(type_firm))) + 
geom_jitter(width = .1)

#regression
trade_receivable.lm<-lm(AR ~ debt + FA + type_firm, data = df) #+ liquidity 0.388  + FA
summary(trade_receivable.lm)

#check important assumptions for linear regression
par(mfrow=c(2,2))   
plot(trade_receivable.lm)

#test of multicollinearity
cor(df$debt, df$FA)
cor(df$debt, df$type_firm)
cor(df$type_firm, df$FA)

#test of heteroskedasticity
bptest(trade_receivable.lm)

#count the number of firms in an industry
c = unique(df$industry)
industry_name <- "Industrials"
count <- 0
for (i in df$industry) {
  if (i == industry_name) {
    count <- count + 1}}
cat("Number of firms in", industry_name, "is", count, "firms")

#count the number of firms in an industry and with AR above median of AR
industry_name <- "Industrials"
median(df$AR)
count <- 0

for (i in 1:length(df$industry)) {
  if (df$industry[i] == industry_name && df$AR[i] > median(df$AR)) {
    count <- count + 1
  }}

cat("Number of firms in", industry_name, "with AR above median of AR is", count, "firms")

