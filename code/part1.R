# problem set 4
# part 1

library(tidyverse)
library(GGally)
library("gridExtra")
library("here")
#
here()

#load data
murders_df<-read_csv(here::here("raw_data","murders.csv"))
murders_df <- filter(murders_df, year=='1995')

#question 1
summary(murders_df$murders)
murders_df %>% ggplot(aes(x = murders)) +
    geom_histogram(bins =25) + xlab("murders")

#question 2
q2<- select(murders_df,murders, popul,arrests,murdrate, arrestrate,percblack)
cor_coeff <- cor(q2, method='pearson')

#question 3 & 4
# first model:
# murders= %10~19 + %20~29 +  %male + %black + %income
names(murders_df)
lm_murd_q3<- lm(murders ~ perc1019+ perc2029 + percblack + percmale + rpcpersinc ,murders_df)
summary(lm_murd_q3)

#question 5
q5_df<- select(murders_df,murders, perc1019, perc2029 , percblack, percmale, rpcpersinc)
q5_df<- mutate(q5_df,"predicted"= predict(lm_murd_q3),
               "residuals"=residuals(lm_murd_q3),
               "stand_res" = rstandard(lm_murd_q3)
               ) # add column for fitted value, residuals, standardize residual


# residual vesus fitted value graph
q5_df %>% ggplot(aes(x = predicted, y = residuals)) + geom_point() +
  geom_smooth(method = "lm", se = F)
  ggsave(here("figures", "Part1_diag_pred_resd.png"))

# Standardised residuals histogram
q5_df %>% ggplot(aes(x = stand_res)) +
  geom_histogram(bins =25) + xlab("Standardized residuals") +
  coord_fixed(ratio=0.01)
ggsave(here("figures", "Part1_std_resd_hist.png"))
