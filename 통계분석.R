library(stargazer)
library(dotwhisker)
library(broom)
library(dplyr)
library(gridExtra)
library(grid)
library(tidyverse)

df = read_csv("con_0526.csv")

df_nonzero = df  %>% filter(fta!=0)
df_nonprop = df  %>% filter(prop_rep!=1)

dim(df_nonzero)
dim(df_nonprop)

df$ruling3

df_18 = df %>% filter(when=="18대") 
df_19 = df %>% filter(when=="19대") 
df_t = df %>% filter(ruling3!="Others") 

stargazer_htest = function (data, ...) {
    summary = data.frame(`Test statistic` = data$statistic,
                         DF = data$parameter,
                         `p value` = data$p.value,
                         `Alternative hypothesis` = data$alternative,
                         check.names = FALSE)
    stargazer(summary, flip = TRUE, summary = FALSE,
              notes = paste(data$method, data$data.name, sep = ': '), ...)
}

stargazer_htest(t.test(fta ~ ruling3, data=df_t))

colnames(df)
dim(df_18)
dim(df_19)

# nor_sd = function(x){
#   result = (x - mean(x)) / sd(x)
#   return(result)
# }

### lm ###
summary(lm(fta ~ prop_rep + ruling3 + term_of_office, data=df))
summary(lm(fta ~ prop_rep + ruling3 + term_of_office + when, data=df))
summary(lm(fta ~ prop_rep + ruling3 + term_of_office, data=df_18))
summary(lm(fta ~ prop_rep + ruling3 + term_of_office, data=df_19))

summary(lm(fta ~ agrRatio, data=df))
summary(lm(fta ~ agrRatio, data=df_18))
summary(lm(fta ~ agrRatio, data=df_19))

summary(lm(fta ~ agrRatio + ruling3, data=df))
summary(lm(fta ~ agrRatio + ruling3, data=df_18))
summary(lm(fta ~ agrRatio + ruling3, data=df_19))

summary(lm(fta ~ agrRatio + ruling3 + term_of_office , data=df))
summary(lm(fta ~ agrRatio + ruling3 + term_of_office + when, data=df))
summary(lm(fta ~ agrRatio + ruling3 + term_of_office , data=df_18))
summary(lm(fta ~ agrRatio + ruling3 + term_of_office , data=df_19))

final_linear = lm(fta ~ agrRatio + manRatio + ruling3 + term_of_office , data=df)
summary(lm(fta ~ agrRatio + manRatio + ruling3 + term_of_office + when, data=df))
summary(lm(fta ~ agrRatio + manRatio + ruling3 + term_of_office , data=df_18))
summary(lm(fta ~ agrRatio + manRatio + ruling3 + term_of_office , data=df_19))

### glm ###

### 지역구 특징 vs 정당 특징 ###
prop = glm(fta ~ prop_rep + ruling3 + term_of_office, data=df, family = poisson)

# prop = glm(fta ~ prop_rep + ruling3 + term_of_office + when, data=df, family = poisson))

prop_18 = glm(fta ~ prop_rep + ruling3 + term_of_office, data=df_18, family = poisson)

prop_19 = glm(fta ~ prop_rep + ruling3 + term_of_office, data=df_19, family = poisson)

### 지역구 특징 중 농업
onlyAgr = glm(fta ~ agrRatio, data=df, family = poisson)
onlyAgr_18 = glm(fta ~ agrRatio, data=df_18, family = poisson)
onlyAgr_19 = glm(fta ~ agrRatio, data=df_19, family = poisson)

### 지역구 특징
onlyRegion = glm(fta ~ agrRatio + manRatio, data=df, family = poisson)
onlyRegion_18 = glm(fta ~ agrRatio + manRatio, data=df_18, family = poisson)
onlyRegion_19 = glm(fta ~ agrRatio + manRatio, data=df_19, family = poisson)

### 지역구 + 정당 특징
final = glm(fta ~ agrRatio + manRatio + ruling3 + term_of_office , data=df, family = poisson)
# summary(glm(fta ~ agrRatio + manRatio + ruling3 + term_of_office + when, data=df, family = poisson))
final_18 = glm(fta ~ agrRatio + manRatio + ruling3 + term_of_office , data=df_18, family = poisson)
final_19 = glm(fta ~ agrRatio + manRatio + ruling3 + term_of_office , data=df_19, family = poisson)

### 지역구 중 자동차 종사자 비율 반응성의 변화
#summary(glm(fta ~ agr2Ratio +carRatio + ruling3 + term_of_office , data=df, family = poisson))
# summary(glm(fta ~ agr2Ratio +carRatio + ruling3 + term_of_office + when, data=df, family = poisson))


car_18 = glm(fta ~ agr2Ratio +carRatio + ruling3 + term_of_office , data=df_18, family = poisson)
car_19 = glm(fta ~ agr2Ratio +carRatio + ruling3 + term_of_office , data=df_19, family = poisson)

tra_18 = glm(fta ~ agr2Ratio +traRatio + ruling3 + term_of_office , data=df_18, family = poisson)
tra_19 = glm(fta ~ agr2Ratio +traRatio + ruling3 + term_of_office , data=df_19, family = poisson)

stargazer(final_linear, onlyAgr, prop,  onlyRegion, final, type="html", out="basic.html")
stargazer(final, final_18, final_19, column.labels = c("All", "18대", "19대"), type="html", out="18vs19.html")
stargazer(car_18, car_19, type="html", out="car.html")
stargazer(tra_18, tra_19, type="html", out="transfortation.html")
stargazer(car_18, car_19, tra_18, tra_19, column.labels = c("Car", "Transfortation"), column.separate = c(2,2), type="html", out="secondLevel.html")