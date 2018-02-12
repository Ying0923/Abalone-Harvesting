
# Load the abalone data set
abalone <- read.csv("mydata.csv", sep="")

str(abalone)
# Convert **RINGS** to factor because the value between does not have meaning
abalone$RINGS <- as.factor(abalone$RINGS)


options(digits=4)
numericSummaryStats<-function(x){
  c(
    Min=min(as.numeric(x)),
    Q = quantile (as.numeric(x), 0.25),
    Median= median(as.numeric(x)),
    Mean= mean(as.numeric(x)),
    Q = quantile (as.numeric(x), 0.75),
    Max=max(as.numeric(x))
  )}

NumericSummary<- sapply(abalone[,2:8], numericSummaryStats)

library("knitr","xtable")
kable(NumericSummary,format="pandoc",caption="Numeric Variable Summary")


plot(abalone[,2:8])

Sex_Class <- addmargins(table(abalone$SEX, abalone$CLASS))
kable(Sex_Class,format="pandoc",caption="Sex by Age Class")


VOLUME <- abalone$LENGTH * abalone$DIAM * abalone$HEIGHT
abalone <- data.frame(abalone, VOLUME)

library(ggplot2)
library(gridExtra)

ggplot(data=abalone, aes(x=WHOLE, y=VOLUME)) +
  geom_point(pch=16, color="black", size=2) +
  geom_smooth(method="lm",  se = FALSE, color="blue",  size=1.2, linetype=2) +
  labs(title="Plot WHOLE Versus VOLUME", x="Whole weight of abalone (grams)", y="VOLUME") +
  theme( 
    title = element_text(size = 13, color = "black", face = "bold"),
    axis.text = element_text(colour = "black", size = 12, face = "italic"),
    axis.text.y = element_text(colour = "black",size = 12),
    axis.title = element_text(size = 15, color = "black", face = "bold"),
    axis.title.y = element_text(size = 15, color = "black", face = "bold")
    
  )


DENSITY <- abalone$WHOLE/abalone$VOLUME
abalone <- data.frame(abalone, DENSITY) 
#     Using DENSITY, present a matrix showing histograms, boxplots and Q-Q plots differentiated by sex.
#     The first row would show the histograms, 
#     The second row would show the boxplots  
#     The third row would show the Q-Q plots.      
par(mfrow = c(3,3))

hist((abalone[abalone[,1] == "F", 12]),main="Female Histogram",xlab = NULL, col = "red") 
hist((abalone[abalone[,1] == "I", 12]),main="Infant Histogram",xlab = NULL,col = "blue")
hist((abalone[abalone[,1] == "M", 12]),main="Male Histogram",xlab = NULL, col = "green")

boxplot((abalone[abalone[,1] == "F", 12]), main="Female Boxplot", ylab="DENSITY",col = "red")
boxplot((abalone[abalone[,1] == "I", 12]), main="Infant Boxplot", ylab="DENSITY",col = "blue")
boxplot((abalone[abalone[,1] == "M", 12]), main="Male Boxplot", ylab="DENSITY",col = "green")

qqnorm(abalone[abalone[,1] == "F", 12], ylab = "Sample Quantiles for Female",
       main = "Q-Q Plot for Female",
       col =  "red",cex.axis = 1.5, cex = 1.5)
qqline(abalone[abalone[,1] == "F", 12])
qqnorm(abalone[abalone[,1] == "I", 12], ylab = "Sample Quantiles for Infant",
       main = "Q-Q Plot for Infant",
       col = "blue",cex.axis = 1.5, cex = 1.5)
qqline(abalone[abalone[,1] == "I", 12])
qqnorm(abalone[abalone[,1] == "M", 12], ylab = "Sample Quantiles for Male",
       main = "Q-Q Plot for Male",
       col = "green", cex.axis = 1.5, cex = 1.5)
qqline(abalone[abalone[,1] == "M", 12]) 


library(moments)
options(digits=4)
Skew.Stats<-function(x){
  c(
    Skewness = skewness(as.numeric(x)),
    kurtosis = kurtosis(as.numeric(x))
  )}

library(plyr)
SEX.Skewness<- tapply(abalone$DENSITY, abalone$SEX, FUN=Skew.Stats)

options(digits=4)
Dis.Stats<-function(x){
  c(
    Min=min(as.numeric(x)),
    Q = quantile (as.numeric(x), 0.25),
    Median= median(as.numeric(x)),
    Q = quantile (as.numeric(x), 0.75),
    Max=max(as.numeric(x)),
    IQR = (quantile (as.numeric(x), 0.75)- quantile (as.numeric(x), 0.25)),
    Range = (max(as.numeric(x))- min(as.numeric(x)))
  )}

# library(plyr)
SEX.Density<- tapply(abalone$DENSITY, abalone$SEX, FUN=Dis.Stats)
SEX.Density


# VOLUME boxplots differentiated by CLASS
ggplot(abalone, aes(x=CLASS, y=VOLUME)) +
  geom_boxplot(fill="cornflowerblue", color="black", notch= TRUE)+
  geom_point(position="jitter", color="blue", alpha=.5)+
  geom_rug(side="l", color="black") +
  labs(title= NULL, x="CLASS", y="VOLUME") +
  theme( 
    axis.text = element_text(colour = "black", size = 12, face = "italic"),
    axis.text.y = element_text(colour = "black",size = 12),
    axis.title = element_text(size = 15, color = "black", face = "bold"),
    axis.title.y = element_text(size = 15, color = "black", face = "bold"))


# WHOLE boxplots differentiated by CLASS
ggplot(abalone, aes(x=CLASS, y=WHOLE)) +
  geom_boxplot(fill="cornflowerblue", color="black", notch= TRUE)+
  geom_point(position="jitter", color="blue", alpha=.5)+
  geom_rug(side="l", color="black") +
  labs(title= NULL, x="CLASS", y="VOLUME") +
  theme( 
    axis.text = element_text(colour = "black", size = 12, face = "italic"),
    axis.text.y = element_text(colour = "black",size = 12),
    axis.title = element_text(size = 15, color = "black", face = "bold"),
    axis.title.y = element_text(size = 15, color = "black", face = "bold"))

# VOLUME boxplots differentiated by CLASS
ggplot(abalone, aes(x=SEX, y=VOLUME)) +
  geom_boxplot(fill="cornflowerblue", color="black", notch= TRUE)+
  geom_point(position="jitter", color="blue", alpha=.5)+
  geom_rug(side="l", color="black") +
  labs(title= NULL, x="CLASS", y="VOLUME") +
  theme( 
    axis.text = element_text(colour = "black", size = 12, face = "italic"),
    axis.text.y = element_text(colour = "black",size = 12),
    axis.title = element_text(size = 15, color = "black", face = "bold"),
    axis.title.y = element_text(size = 15, color = "black", face = "bold"))

# WHOLE boxplots differentiated by CLASS
ggplot(abalone, aes(x=SEX, y=WHOLE)) +
  geom_boxplot(fill="cornflowerblue", color="black", notch= TRUE)+
  geom_point(position="jitter", color="blue", alpha=.5)+
  geom_rug(side="l", color="black") +
  labs(title= NULL, x="CLASS", y="VOLUME") +
  theme( 
    axis.text = element_text(colour = "black", size = 12, face = "italic"),
    axis.text.y = element_text(colour = "black",size = 12),
    axis.title = element_text(size = 15, color = "black", face = "bold"),
    axis.title.y = element_text(size = 15, color = "black", face = "bold"))

# Use aggregate() to compute mean values of WHOLE for each combination of SEX and CLASS.
# Use the resulting object with ggplot to generate a plot of these mean values versus CLASS    

# Showing Mean WHOLE versus CLASS for Three Sexes
WHOLE.out <- aggregate(WHOLE~SEX+CLASS, data=abalone, mean) 

ggplot(data= WHOLE.out ,aes(x=CLASS, y=WHOLE, group=SEX, colour = SEX))+
  geom_line()+geom_point(size=4)+   
  ggtitle("Plot showing Mean WHOLE versus CLASS for Three Sexes")+
  theme( 
    title = element_text(size = 13, color = "black", face = "bold"),
    axis.text = element_text(colour = "black", size = 12),
    axis.text.y = element_text(colour = "black",size = 12),
    axis.title = element_text(size = 15, color = "black", face = "bold"),
    axis.title.y = element_text(size = 15, color = "black", face = "bold")
  )

# Showing Mean DENSITY versus CLASS for Three Sexes
DENSITY.out <- aggregate(DENSITY~SEX+CLASS, data=abalone, mean) 


ggplot(data= DENSITY.out ,aes(x=CLASS, y=DENSITY, group=SEX, colour = SEX))+
  geom_line()+geom_point(size=4)+   
  ggtitle("Plot showing Mean DENSITY versus CLASS for Three Sexes")+
  theme( 
    title = element_text(size = 13, color = "black", face = "bold"),
    axis.text = element_text(colour = "black", size = 12),
    axis.text.y = element_text(colour = "black",size = 12),
    axis.title = element_text(size = 15, color = "black", face = "bold"),
    axis.title.y = element_text(size = 15, color = "black", face = "bold")
  )


## Model Building--------------------------------------------------------------------

# Write a function in R to calculate the Pearson chi square statistic on 2x2 contingency tables   
# Refer to Black chapter 16.2

Defined_chi <- function(x) {
  # Expected values are calculated
  e11 <- x[3,1]*x[1,3]/x[3,3]
  e12 <- x[3,2]*x[1,3]/x[3,3]
  e21 <- x[3,1]*x[2,3]/x[3,3]
  e22 <- x[3,2]*x[2,3]/x[3,3]
  
  # Our next step is to use our observed (values in our actual table)
  # and expected values to calculate chi-square.
  
  chisq <- (x[1,1]-e11)^2/e11 + (x[1,2]-e12)^2/e12 + (x[2,1]-e21)^2/e21 + (x[2,2]-e22)^2/e22  
  return(chisq)
} # end of function



# To be used with 2x2 contingency tables that have margins added.  
shuck <- factor(abalone$SHUCK > median(abalone$SHUCK), labels = c("below", "above"))     # will return FLASE & TRUE, FALSE->"Below"
volume <- factor(abalone$VOLUME > median(abalone$VOLUME), labels = c("below", "above"))
shuck_volume <- addmargins(table(shuck,volume)) 

Defined_chi(shuck_volume)
pchisq(Defined_chi(shuck_volume),1,lower.tail = FALSE)

# critical chi-square with significant level 0.01:
qchisq(0.01, 1, lower.tail = FALSE)


plot(abalone$SHUCK,abalone$VOLUME, xlab=" SHUCK",  pch=16, ylab= "VOLUME", main= "SHUCK and VOLUME Scatterplot")
abline(lm(abalone$VOLUME~abalone$SHUCK), col="red", lty = 1, lwd=2) # regression line (y~x) 


cor(abalone$SHUCK,abalone$VOLUME)


The scatterplot for SHUCK and VOLUME. We can see that they have a positive relationship. From what we observed in Figure 1, having a SHUCK or VOLUME above (or below) its median means an individual is much more likely to have the other measure also above (or below). The correlation between SHUCK and VOLUME is 0.94, which is very close to 1. SHUCK and VOLUME have a very strong positive relationship.


RATIO <- abalone$SHUCK/abalone$VOLUME
abalone <- data.frame(abalone, RATIO)

library(ggplot2)
ggplot(abalone, aes(x=CLASS, y=RATIO)) +
  geom_boxplot(fill="cornflowerblue", color="black", notch= TRUE)+
  geom_point(position="jitter", color="blue", alpha=.5)+
  labs(title= " Boxplots of RATIO differentiated by CLASS", x="CLASS", y="RATIO") +
  theme( 
    title = element_text(size = 13, color = "black", face = "bold"),
    axis.text = element_text(colour = "black", size = 12, face = "italic"),
    axis.text.y = element_text(colour = "black",size = 12),
    axis.title = element_text(size = 15, color = "black", face = "bold"),
    axis.title.y = element_text(size = 15, color = "black", face = "bold")
  )


# Perform an analysis of variance using aov() on RATIO using CLASS and SEX

aov1 <- aov(RATIO~CLASS+SEX+CLASS*SEX, abalone)
summary(aov1)


ggplot(abalone, aes(x=SEX, y=RATIO)) +
  geom_boxplot(fill="cornflowerblue", color="black", notch= TRUE)+
  geom_point(position="jitter", color="blue", alpha=.5)+
  labs(title= " Boxplots of RATIO differentiated by SEX", x="SEX", y="RATIO") +
  theme( 
    title = element_text(size = 13, color = "black", face = "bold"),
    axis.text = element_text(colour = "black", size = 12, face = "italic"),
    axis.text.y = element_text(colour = "black",size = 12),
    axis.title = element_text(size = 15, color = "black", face = "bold"),
    axis.title.y = element_text(size = 15, color = "black", face = "bold")
  )


aov.sex<- aov(RATIO~SEX, abalone)
summary(aov.sex)  
TukeyHSD(aov.sex)

aov2 <- aov(RATIO~CLASS+SEX, abalone)
summary(aov2)

r <- residuals(aov2)
fitt <- fitted(aov2)

par(mfrow = c(1,2))
hist(r, col = "red", main = "Histogram of Residuals", xlab = "Residual")
boxplot(r, col = "red", main = "Boxplot Residuals", ylab = "Residual")
par(mfrow = c(1,1))

qqnorm(r, col = "red", pch = 16, main = "QQ Plot of Residuals")
qqline(r, col = "green", lty = 2, lwd = 2)

require(moments)
skewness(r)
kurtosis(r)

# 95% residule value
qnorm(0.025, mean = 0, sd = sd(r), lower.tail = TRUE)

plot(fitt,r, main = "Plot of residuals versus fitted values", xlab = "fitted values",
     ylab = "residuals", col = "red")
abline(h = 0, lty = 2, col = "green")
abline(h = 3.95, lty = 2, col = "blue")
abline(h = -3.95, lty = 2, col = "blue")

TukeyHSD(aov2)

# scatterplot of SHUCK versus VOLUME  

ggplot(data=abalone, aes(x=VOLUME, y=SHUCK, color=CLASS)) +
  geom_point(pch=16, size=2) +
  geom_smooth(method="lm",  se = FALSE, color="black",  size=1.2, linetype=1) +
  labs(title="Scatterplot of SHUCK versus VOLUME", x="VOLUME", y="SHUCK") +
  theme( 
    title = element_text(size = 13, color = "black", face = "bold"),
    axis.text = element_text(colour = "black", size = 12),
    axis.text.y = element_text(colour = "black",size = 12),
    axis.title = element_text(size = 15, color = "black", face = "bold"),
    axis.title.y = element_text(size = 15, color = "black", face = "bold")
  )

# Logarithms form
ggplot(data=abalone, aes(x=log(VOLUME), y=log(SHUCK), color=CLASS)) +
  geom_point(pch=16, size=2) +
  geom_smooth(method="lm",  se = FALSE, color="black",  size=1.2, linetype=1) +
  labs(title="Scatterplot of L_SHUCK versus L_VOLUME", x="L_VOLUME", y="L_SHUCK") +
  theme( 
    title = element_text(size = 13, color = "black", face = "bold"),
    axis.text = element_text(colour = "black", size = 12),
    axis.text.y = element_text(colour = "black",size = 12),
    axis.title = element_text(size = 15, color = "black", face = "bold"),
    axis.title.y = element_text(size = 15, color = "black", face = "bold")
    
  )


lm.model <- lm(log(SHUCK)~log(VOLUME)+CLASS+SEX, abalone)
summary(lm.model)


out <- lm(log(SHUCK)~log(VOLUME)+CLASS, abalone)
summary(out)

par(mfrow = c(1,2))
hist(out$residuals, col = "red",breaks = "fd",  main = "Histogram of Residuals", xlab = "Residual")
boxplot(out$residuals, col = "red", main = "Boxplot Residuals", ylab = "Residual")
par(mfrow = c(1,1))

qqnorm(out$residuals, col = "red", pch = 16, main = "QQ Plot of Residuals")
qqline(out$residuals, col = "black", lty = 2, lwd = 2)

require(moments)
skewness(out$residuals)
kurtosis(out$residuals)

resid.quant <- quantile(out$residuals, probs = c(0.025, 0.975))
resid.quant  

plot(fitted(out),out$residuals, main = "Plot of residuals versus fitted values", xlab = "fitted values",
     ylab = "residuals", col = "red")
abline(h = 0, lty = 2, col = "green")
abline(h = 0.399, lty = 2, col = "blue")
abline(h = -0.350, lty = 2, col = "blue")

# Using ggplot2 to plot above plots
# Residula histogram

ggplot(out, aes(x = out$residuals)) + geom_histogram()

# Residule QQ Plot
resid.quant <- quantile(out$residuals, probs = c(0.25, 0.75))
theor.quant <- qnorm(c(0.25, 0.75))

slope <- diff(resid.quant)/diff(theor.quant)
intercept <- resid.quant[1] - slope * theor.quant[1]
ggplot(out, aes(sample = out$residuals)) + stat_qq() +
  geom_abline(intercept = intercept, slope = slope)

#  Plot the residuals versus L_VOLUME coloring the data points by CLASS
ggplot(out, aes(x = log(abalone$VOLUME),y = out$residuals)) + geom_point(aes(color = CLASS)) +
  labs(x = "L_VOLUME", y = "Residual")   


# ggplot to present boxplots of the residuals differentiated by CLASS
ggplot(out, aes(x=abalone$CLASS, y=out$residuals)) +
  geom_boxplot(fill="cornflowerblue", color="black", notch= TRUE)+
  geom_point(position="jitter", color="blue", alpha=.5)+
  labs(title= " Boxplots of residuals differentiated by CLASS", x="CLASS", y="residuals") +
  theme( 
    title = element_text(size = 13, color = "black", face = "bold"),
    axis.text = element_text(colour = "black", size = 12, face = "italic"),
    axis.text.y = element_text(colour = "black",size = 12),
    axis.title = element_text(size = 15, color = "black", face = "bold"),
    axis.title.y = element_text(size = 15, color = "black", face = "bold")
  )

idxi <- abalone[,1]=="I"   # to evluate the "SEX" column, return "TRUE" /"FALSE" statement
idxf <- abalone[,1]=="F" 
idxm <- abalone[,1]=="M" 

max.v <- max(abalone$VOLUME)   # Maximum VOLUME
min.v <- min(abalone$VOLUME)   # Minimum VOLUME
delta <- (max.v - min.v)/100  # Divided VOLUME in to 100 

prop.infants <- numeric(0)    # Numeric(0) does not mean 0, it means a numeric vector of length zero (i.e., empty). 
volume.value <- numeric(0) 
total <- length(abalone[idxi,1])  # This value must be changed for adults. Total "Infant" observations
# nrow(subset(abalone, abalone$abalone=="I"))

for (k in 1:100) {    # We divided VOLUME into 100 parts in the begining 
  value <- min.v + k*delta   
  volume.value[k] <- value   
  prop.infants[k] <- sum(abalone$VOLUME[idxi] <= value)/total # we have defined "total" as total number of infants
} 

# prop.infants shows the impact of increasing the volume cutoff for harvesting.    
# The following code shows how to "split" the population at a 50% harvest of infants.

n.infants <- sum(prop.infants <= 0.5) 
split.infants <- min.v + (n.infants + 0.5)*delta  # This estimates the desired volume. 
# Same as quantile(mydata$VOLUME[idxi], probs=0.5)
# What's the VOLUMN for 50% blow

plot(volume.value, prop.infants, col = "green", main = "Proportion of Infants Not Harvested",
     type = "l", lwd = 2) 

abline(h=0.5) 
abline(v = split.infants) 

adults <- abalone[,1]!="I"   # to evluate the "SEX" column, return "TRUE" /"FALSE" statement


max.v <- max(abalone$VOLUME)   # Maximum VOLUME
min.v <- min(abalone$VOLUME)   # Minimum VOLUME
delta <- (max.v - min.v)/100  # Divided VOLUME in to 100 

prop.adults <- numeric(0)    # Numeric(0) does not mean 0, it means a numeric vector of length zero (i.e., empty). 
volume.value <- numeric(0) 
total.adults <- length(abalone[adults,1])  # Total "adults" observations


for (k in 1:100) {    # We divided VOLUME into 100 parts in the begining 
  value <- min.v + k*delta   
  volume.value[k] <- value   
  prop.adults[k] <- sum(abalone$VOLUME[adults] <= value)/total.adults
} 



n.adults <- sum(prop.adults <= 0.5) 
split.adults <- min.v + (n.adults + 0.5)*delta  # This estimates the desired volume. 


plot(volume.value, prop.adults, col = "green", main = "Proportion of Adults Not Harvested",
     type = "l", lwd = 2) 

abline(h=0.5) 
abline(v = split.adults) 

#  "difference" is the difference in adult and infant proportions harvested

difference <- (1-prop.adults)-(1-prop.infants)

plot(volume.value, difference, col = "green", main = " Harvested Difference Versus volume.value",
     type = "l", lwd = 2) 
abline(v = volume.value[which.max(difference)], lty = 2, col = "dodgerblue4")

text(volume.value[which.max(difference)] + 0.003, 0.4,
     paste("volume =",
           round(volume.value[which.max(difference)], 5)), srt = 90)

abline(h=max(difference),lty = 2, col = "dodgerblue4") 

# Determine the volume.value which corresponds to the observed "peak" difference
max(difference)
Peak.volume.value<- volume.value[which.max(difference)]
Peak.volume.value

# What harvest proportions for infants and adults 
# would result if this volume.value is used as a "cutoff" for decision making?  
# Should other cutoffs be considered?

harvest.infants <- 1- sum(abalone$VOLUME[idxi] <= Peak.volume.value)/total
Peak.volume.value

harvest.adults <- 1- sum(abalone$VOLUME[adults] <= Peak.volume.value)/total.adults
harvest.adults

# we simply need to plot the TPR, (1-prop.adults), as a function of the FPR
# we already have our true and false positive rates; TPR and FPR. (1-prop.adults) is our true positive rate
plot( 1-prop.infants, 1-prop.adults, col = "green", main = " ROC Curve",
      type = "l", lwd = 2) 
abline(a=0, b=1, col = "darkred", lty = 2, lwd = 2)


#  smallest cutoff (i.e. volume.value) for which no infant is harvested 

smallest.cutoff <- volume.value[prop.infants==1]  # which is the smallest VOLUMN of infants
smallest.cutoff


# Report this cutoff and the corresponding (1-prop.adults) value
1- sum(abalone$VOLUME[adults] <= smallest.cutoff)/total.adults

# find the largest A1 infant volume and rounds up to the desired precision.
#  the "+ 0.0003" is just to make sure round() rounds up to the thousandths place 
round(
  max(abalone[abalone$CLASS == "A1" &
                abalone$SEX == "I", "VOLUME"]) + 0.0003, 3)
