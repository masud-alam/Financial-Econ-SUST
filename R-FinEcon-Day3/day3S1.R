## Required library
library(summarytools)
library(tidyverse)
library(corrr)
library(finalfit)
library(psych)
library(officer)
library(flextable)
library(corrplot)
library(lessR)
library(GGally)
library(car)
library(mosaic)
library(lattice)
library(stargazer)

## Import house price data set

hprice <- read.csv("hprice.csv", header = T, stringsAsFactors = F)

glimpse(hprice1)
missing_glimpse(hprice1)
ff_glimpse(hprice1)
head(hprice1)
names(hprice)

## Descriptive analysis


hprice %>% 
  select(price,assess,bdrms,lotsize,sqrft,colonial) %>% 
  describe(fast = T)


## Correlation table

hprice %>% 
  select(price,assess,bdrms,lotsize,sqrft,colonial) %>% 
  correlate() %>% shave() %>% 
  fashion()

## Correlation plot

hprice %>% 
  select(price,assess,bdrms,lotsize,sqrft,colonial) %>% 
  correlate() %>% shave() %>% 
  rplot(colours = c("red","green"))

## Correlation table with p values

hprice %>% 
  select(price,assess,bdrms,lotsize,sqrft,colonial) %>% 
  corr.test()


## Correlation heat map

cor_mat <- hprice %>% 
  select(price,assess,bdrms,lotsize,sqrft,colonial) %>% 
  cor()

# Create a correlation heatmap
corrplot(cor_mat, 
         method = "color", # Use color to represent correlations
         type = "full",   # Display the lower triangle of the matrix
         tl.cex = 0.7,     # Adjust text size for variable labels
         tl.col = "black"  # Set variable label color to black
)


hprice %>% 
  select(price,assess,bdrms,lotsize,sqrft,colonial) %>% 
  ggpairs()

### Variable specific statistics
##preliminary tests to make sure that the test assumptions are met

##Violin plot Using lessR package

Plot(price, data = hprice)

Plot(price, data = hprice, by1 = colonial)


###density plot

ggplot(hprice, aes(x = price)) +
  geom_density(fill = "chartreuse2", alpha = 0.3) +  
  labs(x = "Price",y = "Density")+
  theme_minimal()


## Regression model (simple)

# Fit a simple linear regression model
lm_model <- lm(price ~ bdrms, data = hprice)

# Summarize the regression results
summary(lm_model)



## Diagnostic analysis: Method I----
# Method I

qqmath(~resid(lm_model))
qqmath(~resid(lm_model), type = c("p","r"))
plot(lm_model , which = c(1,2))

#Create density plot of residuals
plot(density(resid(lm_model)), col="blue", lwd=3)

shapiro.test(resid(lm_model))

# To test whether the error terms for each group have similar variance (homogeneity of variance),

plot(lm_model$residuals~lm_model$fitted.values, col="red")

lines(lowess(lm_model$fitted.values,lm_model$residuals), col="blue", lwd=2)


## Export the summary result: Simple linear regression

# Export as a txt file
stargazer(lm_model, type = "text", out = "C:/Users/USER/OneDrive/Desktop/day4/simlin.txt")

# Export for LaTeX
stargazer(lm_model, type = "latex", out = "E:/R_workshop_SUST/regression_summary.tex")

# Export as html
stargazer(lm_model, type = "html", out = "E:/R_workshop_SUST/regression_summary.html")



## Regression model (multiple)

# Fit a multiple linear regression model
lm_model2 <- lm(price ~ bdrms+bdrms+lotsize+sqrft, data = hprice)

# Summarize the regression results
summary(lm_model2)
## Multicollinearity test

vif(lm_model2)


# Fit a another linear regression model
lm_model3 <- lm(price ~ bdrms+bdrms+lotsize+sqrft+assess, data = hprice)

# Summarize the regression results
summary(lm_model3)
## Multicollinearity test

vif(lm_model3)


### Hypothesis test


# Fit a another linear regression model
un_mod <- lm(price ~ assess+bdrms+bdrms+lotsize+sqrft, data = hprice)
myH0 <- c("lotsize","sqrft")

linearHypothesis(un_mod, myH0)




## Export regression results 


library(stargazer)
stargazer(list(lm_model, lm_model2,lm_model3),type="text",keep.stat=c("n","rsq"), 
          out = "C:/Users/USER/OneDrive/Desktop/day4/reg_models.txt")





