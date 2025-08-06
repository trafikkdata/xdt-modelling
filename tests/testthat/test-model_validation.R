test_that("multiplication works", {
  source("R/model_validation.R")
  
  threshold_df <- data.frame(aadt_pred = 1:60000, threshold = eale_threshold(1:60000))
  
  #library(ggplot2)
  #ggplot(threshold_df, aes(x = aadt_pred, y = threshold)) +
  #  geom_line(linewidth = 1.2) +
  #  theme_bw()
  
  df_aadt <- data.frame(aadt = c(300, 5000, 12000, 12000), 
                        aadt_pred = c(300, 3700, 9500, 14500),
                        aadt_pred_lower = c(200, 3500, 9000, 14000),
                        aadt_pred_upper = c(400, 3900, 10000, 15000))
  
  test <- autoapprove(df_aadt$aadt, df_aadt$aadt_pred, 
                      df_aadt$aadt_pred_lower, 
                      df_aadt$aadt_pred_upper)
  
  approved_test <- df_aadt %>% mutate(approved = autoapprove(aadt, aadt_pred, 
                                            aadt_pred_lower, 
                                            aadt_pred_upper))
  
  expected_approval <- c(TRUE, FALSE, FALSE, FALSE)
  testthat::expect_equal(approved_test$approved, expected_approval)

})
