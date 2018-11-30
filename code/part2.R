#Part 2:

#lm model for question 6:
# murders rate = %10~19 + %20~29 +  %male + %black + %income
lm_murdrate_q6<-lm(murdrate~perc1019+ perc2029 + percblack + percmale + rpcpersinc ,murders_df)
summary(lm_murdrate_q6)

q6_df<-select(murders_df,murdrate, perc1019, perc2029 , percblack, percmale, rpcpersinc)

q6_df<- q6_df%>%mutate("predicted"= predict(lm_murdrate_q6), "residuals"=residuals(lm_murdrate_q6),
                       "stand_res" = rstandard(lm_murdrate_q6))

# residual vesus fitted value plot
q6_df %>% ggplot(aes(x = predicted, y = residuals)) + geom_point() +
  geom_smooth(method = "lm", se = F)
ggsave(here("figures", "Part2_diag_pred_resd.png"))


# Standardised residuals histogram
q6_df %>% ggplot(aes(x = stand_res)) +
  geom_histogram(bins =25) + xlab("Standardized residuals") +
  coord_fixed(ratio=0.01)
ggsave(here("figures", "Part2_std_resd_hist.png"))
