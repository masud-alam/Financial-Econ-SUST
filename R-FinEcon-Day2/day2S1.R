### IQAC R Workshop: Day2- Session 1 


## Calling packages

# List of package names

library(haven)

## Import technology data
Technology_2018 <- read.csv("techdata.csv", header = T, 
                            stringsAsFactors = F)

View(Technology_2018)
str(Technology_2018)

# Labeling variables and values

str(Technology_2018$Q1)

# Step 1: Rename the Q1 column label
library(dplyr)

Technology_2018 <- Technology_2018 %>%
  rename(gender = Q1)

# Step 2: Convert numerical values to categorical variables

Technology_2018 <- Technology_2018 %>%
  mutate(fact_gender = recode_factor(gender, "1" = "Male", "2" = "Female"))

Technology_2018$fact1 <- ifelse(Technology_2018$gender == 1, "Male", "Female")

## Convert from categorical to numeric

Technology_2018$gender2 <- ifelse(Technology_2018$fact1 == "Male", 1, 2)


# Creating dummy variables from a factor variable
Technology_2018$dummy_gender <- model.matrix(~ Technology_2018$fact1 - 1)

# Printing the dummy variables
print(Technology_2018$dummy_gender)


## From continuous to categorical


df13 <- Technology_2018[,1:5]

str(df13$Q2)

df13 <- df13 %>% 
  mutate(age_range = Q2)

#df13$age_range <- df13$Q2

# Create the new column age_cat based on age_range values
# Simplify Your Code with %>% (ctrl+shift+M):Pipe operator

df13 <- df13 %>%
  mutate(age_cat = case_when(
    age_range == 1 ~ "Less than 18",
    age_range == 2 ~ "18 to 24",
    age_range == 3 ~ "25 to 29",
    age_range == 4 ~ "30 to 34",
    age_range == 5 ~ "35 to 39",
    age_range == 6 ~ "40 to 44",
    age_range == 7 ~ "45 to 49",
    age_range == 8 ~ "50 to 54",
    age_range == 9 ~ "55 to 64",
    age_range == 10 ~ "65 or more",
  ))



### Example : Categorical variables

## Step I
#download affairs data from the web

affairs <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/affairs.dta")

#alternatively you can download data from the GitHub page
# or you can use package Wooldridge
# require(wooldridge)
# data(package="wooldridge")
# data("affairs")
# View(affairs)

View(affairs)

head(affairs)
tail(affairs)

str(affairs)

View(affairs$kids)

class(affairs$kids)



## Step II: create factors for kids and for marriage and attach labels

haskids <- factor(affairs$kids,labels = c("no","yes"))

# haskids <- affairs %>%
#   mutate(haskids = factor(kids, labels = c("no", "yes"))) %>%
#   pull(haskids)


#for ratmarr collum, create five labels and convert the col values

mlab <- c("very unhappy","unhappy","average","happy","very happy")

marriage <- factor(affairs$ratemarr,labels = mlab)

marriage


# marriage <- affairs %>%
#   mutate(marriage = factor(ratemarr, labels = mlab)) %>%
#   pull(marriage)


table(haskids)#frequencies for kids

prop.table(table(marriage)) #marriage ratings and check the share/proportions

#Now make a contingency table and counts(display and store variables)

(countstab <- table(marriage,haskids))

#now we will see the share with in marriage,i.e with in a row (1)
prop.table(countstab,margin = 1)

#next check share within "haskids",i.e with in a column
prop.table(countstab,margin = 2)


## Step III: #let’s make some graph to depict above information

pie(table(marriage),col = c("blue","green","yellow","red","grey"),main = "Proportion of marriage couple")
table(marriage)

# 
# library(plotrix) # you need this package to draw 3D plot. it looks cool!!
# pie3D(table(marriage),labels=mlab,explode=0.1,
#        main="Distribution of marriage status ")

barplot(table(marriage),horiz = F,las=1,
        main = "Distribution of happiness",ylim = c(0,180), col = c("blue","green","yellow","red","purple"))

barplot(table(haskids, marriage),horiz = F,las=1,
        legend=T, args.legend = c(x="bottomright"),
        main = "Happiness by kids",col = c("green","purple"))



barplot(table(haskids, marriage), beside = T,
        legend = TRUE, args.legend = c(x = "topleft"),
        main = "Happiness by kids", col = c("green", "purple"),
        las = 1)




##### Data filtering-----


library(gapminder)
library(tidyverse)

data("gapminder")
View(gapminder)

# Filter and display data for Oman (first 10 rows)
gapminder %>%
  filter(country == "Oman") %>%
  head(10)

# Filter and display data for Oman, years 1981 to 2000 (first 10 rows)
gapminder %>%
  filter(country == "Oman" & year > 1980 & year <= 2000) %>%
  head()


gapminder %>% filter(country=="Oman") %>% head(10)

gapminder %>% filter(country=="Oman" & year>1980 & year<=2000
) %>% head()


##Example 5----

data("starwars")
View(starwars)



starwars %>% filter(height>150 & mass<200) %>% 
  mutate(height_in_meters=height/100) %>% 
  select(height_in_meters,mass) %>% 
  arrange(mass) %>% 
  View()


## Select variables

starwars %>% 
  select(name,height,mass)

starwars %>% select(1:3)

## Changing variable name
starwars %>% 
  rename("characters"="name") %>% 
  head()

## filter rows
starwars %>% 
  select(mass,sex) %>% 
  filter(mass<55 & sex=="male")


##Recode data

starwars %>%
  select(sex) %>%
  mutate(sex = case_when(
    sex == "male" ~ "man",
    sex == "female" ~ "woman",
    TRUE ~ sex  # Keep other values unchanged
  ))

df_star <- starwars %>% na.omit()


## Dealing with missing data

mean(starwars$height, na.rm = T)


# Remove NA values and calculate the mean of height
mean_height <- starwars %>%
  na.omit() %>%            # Remove rows with NA values
  summarise(mean_height = mean(height))

mean_height

## create or change a new variable

starwars %>% 
  mutate(height_m=height/100) %>% 
  select(name,height,height_m)


## conditional statement

starwars %>% 
  mutate(height_m=height/100) %>% 
  select(name,height,height_m) %>% 
  mutate(tallness=if_else(height_m<1,"short","tall"))


## Reshape the data: wide to long-----

# Create a wide messy dataset

df_reshape <- read.csv("reshape.csv")

# Reshape the data from wide to tidy long data
tidy_df <-df_reshape %>%
  gather(quarter, growth, q1_2017:q4_2017)

tidy_df


#The spread() function does the opposite of gather
# from long to wide data


# Reshape the data
df_wide <- tidy_df %>%
  spread(quarter, growth) 
df_wide



## Merging data----


## Let’s create two data sets

df_1 <- data_frame(id = c("a", "b", "c","d","f"), y = c(5,5,8,0,9))
df_2 <- data_frame(id = c("a", "b", "c","d","e"), y = c(15,7,3,10,19))


left_join(df_1, df_2, by ='id')
right_join(df_1, df_2, by = 'id')
inner_join(df_1, df_2, by = 'id')
full_join(df_1, df_2, by = 'id')













