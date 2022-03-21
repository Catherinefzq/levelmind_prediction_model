library(tidyverse);library(ggplot2);library(rpart);library(corrplot);
library(tidymodels);library(labelled);library(rpart.plot);
library(party);library(ggpubr);library(sjPlot);library(vip);
library(factoextra);library(corrplot);library(gtsummary);

load("dat/20210927_2m_clean.rdata");

# Initial model using June 7 data
oneyear_2m <- clean_2m_demo %>% 
  filter(submit_time < as.Date('2021-06-01'))

# Visit counting
# 1607 have 2M submissions
# 474 have no less than 3 visits (1133 have less than 3)
# 55 have less than 2 no product at follow-up

set.seed(1234) 
# Data Pre-processing ----------------------------------------------------------
data_pre <- function(data){
  ## extract with more than 2 follow-up with no_product
  miss_no_prod <- data %>%
    group_by(subject_code) %>%
    mutate(bl = first(no_product)) %>% 
    slice(-1) %>% 
    filter(!is.na(no_product)) %>% 
    mutate(freq = n()) %>% 
    filter(freq >= 2) %>% 
    select(subject_code, no_of_2m, freq, no_product) %>% 
    ungroup()
  ## follow-up dataframe
  df_2m <- data %>% 
    group_by(subject_code) %>% 
    mutate(freq = n()) %>% 
    ungroup() %>% 
    filter(freq > 2 & subject_code %in% miss_no_prod$subject_code)
  rm(miss_no_prod)
  ## baseline dataframe
  df_bl <- df_2m %>% 
    group_by(subject_code) %>% 
    slice(1) %>% 
    ungroup() 
  ## Standardization, calculate summary statistics -----------------------------
  df_pre <-  
    with(df_bl %>% 
           dplyr::summarise(across(.cols = where(is.numeric),
                                   .fns = list(mean = ~ mean(.x, na.rm = T), 
                                               sd = ~ sd(.x, na.rm = T)), 
                                   .names = "{.col}_{.fn}")),
         ## Exclude baseline, calculate mean and SD
         df_2m %>% 
           group_by(subject_code) %>% 
           slice(2:n()) %>% 
           mutate(sf6_r = (sf6_z - sf6_z_mean)/sf6_z_sd,
                  k6_r = (k6_score - k6_score_mean)/k6_score_sd,
                  no_product_r = (no_product - no_product_mean)/no_product_sd) %>% 
           dplyr::summarise(across(.cols = ends_with("_r"),
                                   .fns = list(mean = ~ mean(.x, na.rm = T), 
                                               sd = ~ sd(.x, na.rm = T)), 
                                   .names = "{.col}_{.fn}")) %>% 
           left_join(., df_bl, by = "subject_code") %>% 
           ungroup() %>% 
           arrange(subject_code, no_of_2m)
    )
  
  ## Clean variable names
  names(df_pre) <- str_replace(names(df_pre), pattern = "_r","")
  return(df_pre)
}

df_pre <- data_pre(data = oneyear_2m)

df_model <- df_pre %>% 
  mutate(gender = if_else(gender == 1, "Male", "Female"),
         edu_yes_no = if_else(edu_yes_no == 1, "Yes", "No"),
         age = as.numeric(age),
         k6_score = as.integer(k6_score),
         self_harm = if_else(self_harm == 1, "Yes", "No")) %>% 
  arrange(submit_time)

## Generate train and test data
df_split <- initial_time_split(df_model, strata = k6_mean, 
                               breaks = 3, prop = .75)

df_train <- training(df_split) # 314
df_test <- testing(df_split) # 105

## Generate table 1 use gtsummary
theme_gtsummary_journal(journal = "jama", 
                        set_theme = T)
theme_gtsummary_compact()

df_train %>% 
 mutate(dataset = "Train") %>% 
  union_all(df_test %>% 
              mutate(dataset = "Test")) %>% 
  mutate(dataset = factor(dataset, levels = c("Train","Test"))) %>% 
  select(age, gender, edu_yes_no, k6_score, sf6_z, no_product, #dataset,
         hospitalization_days, self_harm) %>% 
  set_variable_labels(age = "Age", gender = "Gender", 
                      edu_yes_no = "Current Student",
                      k6_score = "K6 Score", sf6_z = "SF-6D Index",
                      no_product = "Days of No Productivity",
                      hospitalization_days = "Hospitalization (Days)",
                      self_harm = "Self Harm") %>% 
  tbl_summary(.,
              missing_text = "Missing",
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(hospitalization_days ~ c(2,2))) %>% 
  #add_n() %>% 
  # add_p() %>% 
  modify_header(label = "") %>% 
  bold_labels() %>%
  #bold_p(t = 0.05) %>% 
  as_flex_table() %>% 
  flextable::bold(part = "header") %>% 
  flextable::font(fontname = "Arial", part = "all")
reset_gtsummary_theme()


# PCA analysis -----------------------------------------------------------------
# keep no_product in PCA cuz it contributes more to the first PC, stronger relationship
pca.model <- 
  as.formula(~ sf6_mean + sf6_sd + k6_mean + k6_sd + no_product_mean + no_product_sd)

pca <- princomp(pca.model, df_train, scores = T, cor = T)

## Loadings
labels <- c("SF-6D (Mean)", "SF-6D (SD)",
            "K6 (Mean)", "K6 (SD)",
            "Days of No Productivity (Mean)", "Days of No Productivity (SD)")
tibble(labels, 
       pca[["loadings"]][,1]) %>% 
  knitr::kable(., digits = 3, col.names = c("","PC1 Loadings")) %>% 
  kableExtra::kable_classic_2(full_width = F, html_font = "Arial")

tibble(labels, loadings = pca[["loadings"]][,1]) %>% 
  ggbarplot(x = "labels", y = "loadings", fill = "grey",
            ggtheme = theme_bw()) +
  theme(legend.position = "none", 
        axis.text.y = element_text(color = "black"),
        text = element_text(size = 13)) +
  labs(x = "", y = "Loadings") +
  coord_flip()

## Scree plot
fviz_eig(pca, geom = "line") + 
  scale_y_continuous(n.breaks = 6, limits = c(0,60)) +
  theme_bw(base_size = 14) + 
  labs(title = "")

## Append PC1 score to data frame
df_train$pc.score <- pca$scores[,1]
df_split$data$pc.score <- predict(pca, df_split$data)[,1]

gghistogram(data = df_train, x = "pc.score", fill = "grey",
            ggtheme = theme_bw(base_size = 14)) +
  labs(x = "PC score", y = "Count", title = "")
gghistogram(testing(df_split), x = "pc.score")

## Correlation of PC1 and variables
cor_plot <- df_train[,c(2:7,44)] %>% 
  cor(.,use = "complete.obs") 
rownames(cor_plot) <- c(labels, "First PC score")
colnames(cor_plot) <- rownames(cor_plot)
corrplot(cor_plot, type = "lower", tl.col = "black", tl.cex = 0.7,
         tl.pos = "ld") + 
  theme_bw(base_size = 16)
rm(cor_plot)

# CART model -------------------------------------------------------------
load("dat/5-fold-20t-cv.Rdata")
cv_folds <- vfold_cv(df_train, v = 5, repeats = 20)

cart_grid <- 
  expand.grid(tree_depth = c(1:7))

cart_model <- decision_tree(
  cost_complexity = 0.01,
  tree_depth = tune()) %>% 
  set_engine('rpart') %>% 
  set_mode("regression")

cart_rec <- recipe(pc.score ~ k6_score + sf6_z, 
                   data = df_train)

cart_wf <- workflow() %>% 
  add_recipe(cart_rec) %>% 
  add_model(cart_model)

train_result <- cart_wf %>% 
  tune_grid(
    grid = cart_grid,
    resamples = cv_folds, 
    metrics = yardstick::metric_set(rmse, rsq, mae))


#save(train_result, file = "dat/10-fold-20t-cv.Rdata")
train_result %>% collect_metrics() %>% 
  mutate(.metric = case_when(.metric == "rmse" ~ "RMSE",
                             .metric == "mae" ~ "MAE",
                             .metric == "rsq" ~ "R-Squared") %>% 
           factor(., levels = c("RMSE", "R-Squared", "MAE"))) %>% 
  ggplot(aes(x = tree_depth, y = mean)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~ .metric, nrow = 3, scales = "free_y") +
  labs(x = "Tree Depth", y ="") +
  theme_bw(base_size = 14)

best_depth <- train_result %>% select_best("rmse","mae","rsq")

final_cart <- cart_wf %>% 
  finalize_workflow(best_depth) %>% 
  last_fit(df_split)

final_cart %>% 
  extract_fit_engine()

final_cart %>%
  extract_fit_parsnip() %>% 
  vi() %>%
  mutate(Variable = c("K6 score", "SF-6D"
                      #"Days of Hospitalization", "Self Harm"
                      )) %>% 
  arrange(Importance) %>% 
  ggscatter(x = "Variable", y = "Importance",
            ggtheme = theme_bw()) + 
  coord_flip() +
  theme(axis.text.y = element_text(color = "black"),
        text = element_text(size = 14))

last_fit_metrics <- metric_set(rmse, rsq, mae)

final_cart %>% 
  collect_predictions() %>% 
  last_fit_metrics(., truth = pc.score, estimate = .pred)

## metrics for depth 2, 3
depth_metric <- tibble()
for (i in 2:3) {
  depth_metric <- 
    union_all(depth_metric,
              cart_wf %>% 
                finalize_workflow(train_result %>% 
                                    collect_metrics() %>% 
                                    filter(tree_depth==i, 
                                           .metric == "mae")) %>% 
                last_fit(df_split) %>% 
                collect_predictions() %>% 
                last_fit_metrics(., truth = pc.score, estimate = .pred)
    )
}

depth_metric %>% 
  rename("Metric" = .metric, "Estimate" = .estimate) %>% 
  mutate(Metric = case_when(Metric == "rmse" ~ "RMSE",
                            Metric == "mae" ~ "MAE",
                            Metric == "rsq" ~ "R-Squared")) %>% 
  select(-.estimator) %>% 
  knitr::kable(., digits = 3) %>% 
  kableExtra::kable_classic_2(full_width = F, html_font = "Arial") %>% 
  kableExtra::pack_rows("Tree Depth = 2", 1, 3) %>% 
  kableExtra::pack_rows("Tree Depth = 3", 4, 6)
#rm(depth_metric)

## plot a rounded split version of CART tree
fit <- rpart(pc.score ~ K6 + `SF-6D`, 
             data = df_train %>% 
               rename(K6 = k6_score, `SF-6D` = sf6_z), 
             control = list(maxdepth = 2, cp = 0.01))

rpart.plot(fit,type = 4, box.palette = "-Blue", clip.right.labs = F, 
           xflip = T, roundint = T) 

test <- 
  df_train %>% 
  mutate(pred = factor(predict(fit, df_train %>% 
                          rename(K6 = k6_score, `SF-6D` = sf6_z)))
         ) %>% 
  select(subject_code, pred) %>% 
  right_join(df_2m)
ggplot(aes(x = submit_time, y = k6_score), 
       data = test) + 
  geom_line(aes(group = subject_code, color = subject_code)) + 
  geom_smooth(aes(x = submit_time, y = k6_score)) +
  facet_wrap(~ pred) +
  theme(legend.position = "none")

ggplot(aes(x = submit_time, y = sf6_z), 
       data = test) + 
  geom_line(aes(group = subject_code, color = subject_code)) + 
  geom_smooth(aes(x = submit_time, y = sf6_z)) +
  facet_wrap(~ pred) +
  theme(legend.position = "none") 
  
# Assess external validity using control data ----------------------------------
load("dat/20211017_qct_externalvalid.rdata")
qct_qol$sf6_z[qct_qol$sf6_z==0] = NA

df_ex_valid <- qct_qol %>% 
  rename(subject_code = parti_id, 
         no_of_2m = visit) %>% 
  group_by(subject_code, no_of_2m) %>% 
  filter(mean(c(sf6_z, k6_score), na.rm = T) >= 0) %>% 
  data_pre() %>% 
  mutate(pc.score = predict(pca, newdata = .)[,1]) %>% 
  ungroup() %>% 
  filter(pc.score >= min(df_train$pc.score) & pc.score <= max(df_train$pc.score)
         & subject_code %in% subject_code[grep("COM|ICY", subject_code)])

bind_rows(tibble(df_ex_valid[,9:11], 
                 data = "External Data"),
          tibble(df_train[,c(15,13,41)], 
                 data = "Included Hub Users")) %>% 
  select(sf6_z:data) %>% 
  rename(`SF-6D` = sf6_z, K6 = k6_score, 
         `Days of No Productivity` = no_product) %>% 
  gather("key", "value", -data) %>% 
  ggplot(aes(x = data, y = value)) +
  geom_violin(aes(fill = data), alpha = 0.7) +
  geom_boxplot(width=0.07) +
  stat_compare_means(vjust = -0.7) +
  facet_wrap(~ key, scales = "free")+
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom", axis.text.x = element_text(size = 0)) + 
  labs(x = "", y = "", fill = "") 
  
depth_metric2 <- tibble()
for (i in 2:3) {
  depth_metric2 <- 
    union_all(depth_metric2, 
              cart_wf %>% 
                finalize_workflow(train_result %>% 
                                    collect_metrics() %>% 
                                    filter(tree_depth == i, 
                                           .metric == "mae")) %>% 
                parsnip::fit(.,df_train) %>% 
                predict(., df_ex_valid) %>% 
                bind_cols(., df_ex_valid) %>% 
                last_fit_metrics(., truth = pc.score, estimate = .pred)
    )
}

depth_metric %>% 
  rename("Metric" = .metric, "Estimate" = .estimate) %>% 
  mutate(Metric = case_when(Metric == "rmse" ~ "RMSE",
                            Metric == "mae" ~ "MAE",
                            Metric == "rsq" ~ "R-Squared")) %>% 
  select(-.estimator) %>% 
  bind_cols(.,depth_metric2 %>% 
  rename("Metric" = .metric, "Estimate" = .estimate) %>% 
  mutate(Metric = case_when(Metric == "rmse" ~ "RMSE",
                            Metric == "mae" ~ "MAE",
                            Metric == "rsq" ~ "R-Squared")) %>% 
  select(-.estimator, - Metric)) %>% 
  knitr::kable(., digits = 3, 
               col.names = c("","Included Hub Users", "External Data")) %>% 
  kableExtra::kable_classic_2(full_width = F, html_font = "Arial") %>% 
  kableExtra::pack_rows("Tree Depth = 2", 1, 3, bold = F, italic = T) %>% 
  kableExtra::pack_rows("Tree Depth = 3", 4, 6, bold = F, italic = T) %>% 
  kableExtra::row_spec(0, bold = T)

rm(depth_metric2)



# Model comparison -------------------------------------------------------------
# Replicated logistic
df_split$data$.pred <- factor(predict(fit, df_split$data))
df_split$data$k6_x_sf6 <- df_split$data$k6_score*df_split$data$sf6_z
df_split$data$k6_x_k6 <- df_split$data$k6_score*df_split$data$k6_score

rep_log_mode <- multinom_reg(penalty = 0.1) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification") 

rep_log_rec <- recipe(.pred ~ k6_score + k6_x_sf6 + k6_x_k6,
                      data = training(df_split))

rep_log_wf <- workflow() %>% 
  add_recipe(rep_log_rec) %>% 
  add_model(rep_log_mode)

rep_log_fit <- rep_log_wf %>% 
  last_fit(df_split)

pred_df <- testing(df_split) %>% 
  select(pc.score) %>% 
  bind_cols(rep_log_fit %>% collect_predictions() %>% 
              select(.pred)) %>% 
  mutate(.pred = as.numeric(.pred))

last_fit_metrics(pred_df, truth = pc.score, estimate = .pred) %>% 
  select(-.estimator) %>% 
  knitr::kable(., digits = 3) %>% 
  kableExtra::kable_classic_2(full_width = F, html_font = "Arial")

## simple linear regression
rep_reg_mode <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

rep_reg_rec <- recipe(pc.score ~ k6_score + sf6_z,
                      data = training(df_split))

rep_reg_wf <- workflow() %>% 
  add_recipe(rep_reg_rec) %>% 
  add_model(rep_reg_mode)

rep_reg_fit <- rep_reg_wf %>% 
  last_fit(df_split)

quantile(df_split$data$pc.score, probs = c(0.14, 0.28, 0.51, 1))
pred_df <- testing(df_split) %>% 
  select(pc.score) %>% 
  bind_cols(rep_reg_fit %>% collect_predictions() %>% 
              select(.pred)) %>% 
  mutate(.pred = as.numeric(.pred),
         class = case_when(pc.score <= -2.0206087 ~ "poor",
                           pc.score > -2.0206087 
                           & pc.score <= -0.9247592 ~ "intermediate2",
                           pc.score > -0.9247592 
                           & pc.score <= 0.3544181 ~ "intermediate1",
                           pc.score > 0.3544181 
                           & pc.score <= 2.9045264 ~ "good") %>% 
           as.factor(),
         predc = case_when(.pred <= -2.0206087 ~ "poor",
                           .pred > -2.0206087 
                           & .pred <= -0.9247592 ~ "intermediate2",
                           .pred > -0.9247592 
                           & .pred <= 0.3544181 ~ "intermediate1",
                           .pred > 0.3544181 
                           & .pred <= 2.9045264 ~ "good") %>% 
           as.factor()) 

final_cart %>% 
  collect_predictions() %>% 
  mutate(class = case_when(pc.score <= -2.0206087 ~ "poor",
                           pc.score > -2.0206087 
                           & pc.score <= -0.9247592 ~ "intermediate2",
                           pc.score > -0.9247592 
                           & pc.score <= 0.3544181 ~ "intermediate1",
                           pc.score > 0.3544181 
                           & pc.score <= 2.9045264 ~ "good") %>% 
           as.factor(),
         predc = case_when(.pred <= -2.0206087 ~ "poor",
                           .pred > -2.0206087 
                           & .pred <= -0.9247592 ~ "intermediate2",
                           .pred > -0.9247592 
                           & .pred <= 0.3544181 ~ "intermediate1",
                           .pred > 0.3544181 
                           & .pred <= 2.9045264 ~ "good") %>% 
           as.factor()) %>% 
  accuracy(truth = class, estimate = predc)

accuracy(pred_df, truth = class, estimate = predc)

# baseline characterics excluded IDs -------------------------------------------
theme_gtsummary_journal(journal = "jama", 
                        set_theme = T)
theme_gtsummary_compact()

oneyear_2m %>% 
  group_by(subject_code) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(dataset = if_else(subject_code %in% df_model$subject_code, 
                           "Included", "Excluded"),
         age = as.numeric(age),
         gender = if_else(gender == 1, "Male", "Female"),
         edu_yes_no = if_else(edu_yes_no == 1, "Yes", "No")) %>% 
  select(age, gender, edu_yes_no, k6_score, sf6_z, no_product, dataset,
         hospitalization_days, self_harm) %>% 
  set_variable_labels(age = "Age", gender = "Gender", 
                      edu_yes_no = "Current Student",
                      k6_score = "K6 Score", sf6_z = "SF6 Index Score",
                      no_product = "Days of No Productivity",
                      hospitalization_days = "Hospitalization (Days)",
                      self_harm = "Self Harm") %>% 
  tbl_summary(., by = dataset,
              missing_text = "Missing",
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(hospitalization_days ~ c(2,2))) %>% 
  add_n() %>% 
  add_p() %>% 
  modify_header(label = "**Variable**") %>% 
  bold_labels() %>%
  bold_p(t = 0.05) %>% 
  as_flex_table() %>% 
  flextable::bold(part = "header") %>% 
  flextable::font(fontname = "Arial", part = "all")
reset_gtsummary_theme()
