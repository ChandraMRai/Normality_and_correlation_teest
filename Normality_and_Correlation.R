setwd("D:/")
library(readxl)
Data <- read_excel("D:/Correlation.xlsx")
View(Data)

library(dplyr)
library(ggpubr)

# Normality using density curve
ggdensity(Data$LST, Main = "Land Surface Temperature", xlab= "LST")
ggdensity(Data$RVI)
ggdensity(Data$IPVI)
ggdensity(Data$DVI)
ggdensity(Data$NDVI)
ggdensity(Data$NDWI)
ggdensity(Data$NDMI)

# Normality using ggqqplot
ggqqplot(Data$LST, Main = "Land Surface Temperature", xlab= "LST")
ggqqplot(Data$RVI)
ggqqplot(Data$IPVI)
ggqqplot(Data$DVI)
ggqqplot(Data$NDVI)
ggqqplot(Data$NDWI)
ggqqplot(Data$NDMI)

# Kolmogorov Smirnov test for normality (N>50)
ks.test(Data$LST, "pnorm") #since p value is less than 0.05, data is not normally distributed
ks.test(Data$RVI, "pnorm")
ks.test(Data$IPVI, "pnorm")
ks.test(Data$DVI, "pnorm")
ks.test(Data$NDVI, "pnorm")
ks.test(Data$NDWI, "pnorm")
ks.test(Data$NDMI, "pnorm")

# Since data is not normally distributed, we can conduct spearman correlation
cor.test(Data$LST, Data$RVI, method = "spearman") #There are many same data points, which shows the warning
cor.test(Data$LST, Data$RVI, method = "spearman", exact= FALSE) #one-sided
cor.test(Data$LST, Data$RVI, method = "spearman", exact= FALSE, alternative = "two.sided",  conf.level = 0.95) # two-sided; you can also change to "greater", "lesser"

cor.test(Data$LST, Data$IPVI, method = "spearman", exact= FALSE, alternative = "two.sided", 
         conf.level = 0.95)
cor.test(Data$LST, Data$DVI, method = "spearman", exact= FALSE, alternative = "two.sided",
         conf.level = 0.95)
cor.test(Data$LST, Data$NDVI, method = "spearman", exact= FALSE, alternative = "two.sided",
         conf.level = 0.95)
cor.test(Data$LST, Data$NDWI, method = "spearman", exact= FALSE, alternative = "two.sided",
         conf.level = 0.95)
cor.test(Data$LST, Data$NDMI, method = "spearman", exact= FALSE, alternative = "two.sided",
         conf.level = 0.95)

# spearmann correlationsuing Hmisc package
install.packages("Hmisc")
library(Hmisc)
SpearmanCor <- rcorr(as.matrix(Data), type = "spearman")
print (SpearmanCor)

# Correlation Graph
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(Data, smooth = TRUE, 
                  scale = FALSE,
                  type = "spearman")
# correlation graph
install.packages("GGally")
library(GGally)
ggpairs(Data,
        columns = 1:7,
        lower = list(continuous = "smooth"),
        upper = list(continuous = wrap ("cor",
       method = "spearman")))

# correlation graph
install.packages("psych")
library(psych)

pairs.panels(Data,
             smooth= TRUE,
             scale = FALSE,
             density = TRUE,
             ellipses = TRUE,
             method = "spearman",
             lm = FALSE,
             cor = TRUE,
             jiggle = FALSE,
             factors =2,
             hist.col = 4,
             stars= TRUE,
             ci = TRUE)
