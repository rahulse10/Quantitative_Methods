#loading_libraries

library(tidyverse)
library(ggplot2)
library(ggcorrplot)


#Loading_dataset
ess <- read.csv('ESS9.csv')

#filter_for_norway
ess <- ess[ess$cntry == "NO",]
view(ess)

#creating_dataset
ess <- select(ess, imwbcnt, agea, eduyrs, lrscale, gndr, rlgdgr)
view(ess)


#exploring variables 
table(ess$imwbcnt)
table(ess$eduyrs) #few outliers
table(ess$lrscale)
table(ess$rlgdgr)
table(ess$gndr)

#cleaning dataset
ess$agea[ess$agea == 999] <- NA
sum(is.na(ess))
ess <- na.omit(ess)
summary(ess)

#cleaning  imwbcnt dataset
ess$imwbcnt[ess$imwbcnt == 88] <- NA
sum(is.na(ess))
ess <- na.omit(ess)
summary(ess)

#cleaning  lrscale dataset
ess$lrscale[ess$lrscale > 76] <- NA
sum(is.na(ess))
ess <- na.omit(ess)
summary(ess)

#cleaning  eduyrs  dataset
ess$eduyrs[ess$eduyrs  > 22] <- NA
sum(is.na(ess))
ess <- na.omit(ess)
summary(ess)


#cleaning  rlgdgr  dataset
ess$rlgdgr[ess$rlgdgr  == 88] <- NA
sum(is.na(ess))
ess <- na.omit(ess)
summary(ess)

#correlation test
ess %>%
  dplyr::select(eduyrs, lrscale, imwbcnt, rlgdgr, agea) %>%
  cor(.) %>% 
  ggcorrplot(., type = "lower", lab = TRUE)

#recoding gndr as dummy variable
#Recode gndr, 0 = Female, 1 = Male
ess[ess$gndr == 2, "gender"] <-0
ess[ess$gndr == 1, "gender"] <-1

#Regression models 
m1 <- lm(imwbcnt ~ eduyrs, data = ess)
summary(m1)

m2 <- lm(imwbcnt ~ eduyrs+agea+gender, data = ess)
summary(m2)

m3 <- lm(imwbcnt ~ eduyrs+agea+gender+rlgdgr+lrscale, data = ess)
summary(m3)
 
m4 <- lm(imwbcnt ~ eduyrs+agea+gender+rlgdgr+lrscale+agea*gender, data = ess)
summary(m4)



#scatter plot
ggplot(data = ess, aes(x=eduyrs, y=imwbcnt)) +
  geom_point (alpha = 0.1) + 
  geom_smooth(aes(x = eduyrs, y = imwbcnt), method = "lm", formula = y ~ x)+
  labs(x = 'Education years', y = 'Positive attitude towards immigrants')



#plot
ggplot(ess1_pred, aes(x = agea, y = happy, color = factor(religion,
                                                          labels = c("No",
                                                                     "Yes")))) +
  geom_point(alpha = 0.1) +
  geom_smooth(aes(x = agea, y = fit), method = "gam", formula = y ~ s(x, bs ="cs"),
              show.legend = TRUE, se = FALSE) +
  guides (color=guide_legend(override.aes = list(fill=NA)))+
  labs()
       x = "Age", y = "Happy", caption = "Source: European Social Survey", 
       color = "Belonging to a particular religion or denomination")+ 
  apatheme
