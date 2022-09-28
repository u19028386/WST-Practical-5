# Practical 5

# Name: Sinenhlanhla Dlamini

# Student Number: u19028386

#Packages:

library(dplyr)
library(tidyr)
library(gapminder)
library(stringr)

# Data:

# The first dataset of interest is:

summary(gapminder)
?gapminder

# The second dataset of interest is:

sample_text <- 'I am a student from the University of Pretoria. I
                was born in Durban, South Africa and I have been a huge soccer
                fan for 9 years. I also have a passion for Philosophy 
                and love talking about it and discovering more things about the field. 
                Therefore, I am going to get a degree in Philosophy toimprove my chances 
                of becoming a Philosophy professor. I have been working towards this goal
                for 4 years. I am currently enrolled in a PhD program. It is very difficult, 
                but I am confident that it will be a good decision.'

# The third dataset of interest is:

string <- 'Hi my name are John and my email address are john.doe@somecompany.co.uk and my friend\'s 
email are jane_doe124@gmail.com, but the following are invalid emails: j@p, jj@p, j@pp and jp.com.'

# Question 1a: 
Q1a <- gapminder %>% 
       filter(year == 2002 , continent == "Africa")%>% 
       select(-c(continent,year,lifeExp,gdpPercap))

# Question 1b:
Q1b <- gapminder %>%
        filter(pop == max(pop)) %>%
       select(country)
       

# Question 1c:
Q1c <- gapminder$pop * gapminder$gdpPercap

# Question 1d:
Q1d <- gapminder %>%
  filter(pop * gdpPercap == max(pop * gdpPercap)) %>%
  select(country)

# Question 1e:
gapminder$mean_life_exp <- mean(gapminder$lifeExp)

# Question 1f:
Q1f <- gapminder %>%
  filter(lifeExp > mean(lifeExp)) %>%
  group_by(continent) %>%
  summarise(n = n()) 

# Question 1g:
gapminder$high_life_exp <- ifelse(gapminder$lifeExp > mean(gapminder$lifeExp), 1,0)
       
# Question 2.a) & Question 2.b)
pat2a <- 'Durban'
Q2a <- str_extract(sample_text, pat2a)
Q2b <- pat2a

# Question 2.c) & Question 2.d)
pat2b <-'\\d' 
Q2c <- str_extract_all(sample_text, pat2b, simplify = TRUE)
Q2d <- pat2b


# Question 2.e) & Question 2.f)
pat2e <- 'Philosophy'
Q2e <- str_locate(sample_text, pat2e)
Q2f <- pat2e



# Question 2.g) & Question 2.h)
pat2g <- 'PhD'
Q2g <- str_detect(sample_text, pat2g)
Q2h <- pat2g


# Question 3.a)
pat3a <- '[\\w.]+@[\\w.]+'
Q3a <- str_extract_all(string, pat3a, simplify = TRUE)
Q3a <- pat3a

# Question 3.b)
pat3b <- '[\\w.]{2,}@[\\w.]{2,}'
Q3b  <- str_extract_all(string, pat3b, simplify = TRUE )
Q3b <- pat3b

# Question 3.c)
pat3c <- 'are'
Q3c <- str_replace_all(string, pat3c, 'is')


