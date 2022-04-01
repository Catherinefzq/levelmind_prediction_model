library(tidyverse);library(ggplot2);library(rpart);library(corrplot);
library(tidymodels);library(labelled);library(rpart.plot);
library(party);library(ggpubr);library(sjPlot);library(vip);
library(factoextra);library(corrplot);library(gtsummary);
library(readxl)

# YES data
df_yes_3m <- here("dat", "original", 
                  "(DONOTTOUCH) 3mFU Master Database 20220204.xlsx") %>% 
  read_excel(sheet = "Total", na = c("888","999","-999")) %>% 
  slice(-1) %>% 
  janitor::clean_names() %>% 
  rename(subject_code = yes_id) %>% 
  filter(!subject_code == 0) %>% 
  group_by(subject_code) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(k6_z = scale(m3_k6_tot),
         sds_z = scale(m3_sds_tot))

df_yes_demo <- here("dat", "original", 
                  "(DONOTTOUCH) HKYES Master Database_12 Jan 2022.xlsx") %>% 
  read_excel(sheet = "Raw",
             na = c("888","999","-999")) %>% 
  slice(-1) %>% 
  janitor::clean_names() %>% 
  dplyr::select(yes_id, age, sex, edu_yes_no) %>% 
  mutate_at(names(.)[-1], as.numeric) %>% 
  rename(subject_code = yes_id) %>% 
  filter(!subject_code == 0) %>% 
  group_by(subject_code) %>% 
  slice(1) %>% 
  ungroup()

df_yes_bl <- here("dat", "original", 
                  "(DONOTTOUCH) HKYES Master Database_12 Jan 2022.xlsx") %>% 
  read_excel(sheet = "Total",
             na = c("888","999","-999")) %>% 
  slice(-1) %>% 
  janitor::clean_names() %>% 
  select(yes_id, edeq_mean, names(.)[grep(
    "ses|bpaq|bhs|bis|foi|gcq|ppaaus|cdrisc|lon|yms|bps|maas|proc|rrs|erri
    |phq9|dass|gad7|ymrs|hcl32_tot|cape|pqb|ybocs|tsq|lsas_tot|
    |cidi|who|bfi|k6|sf12", 
    names(.))]) %>% 
  mutate_at(names(.)[-1], as.numeric) %>% 
  rename(subject_code = yes_id) %>% 
  group_by(subject_code) %>% 
  slice(1) %>% 
  ungroup()

# model prep
set.seed(1234) 
df_dimension <- df_yes_bl %>% 
  select(subject_code, edeq_mean, phq9_tot:lsas_tot) %>% 
  filter(cape_distress_tot >= 50)
  filter(!(phq9_tot > 9|(dass_anx + dass_stress + dass_depress) >= 60 
           |dass_depress >= 21|gad7_tot >= 10|ymrs_tot > 20|hcl32_tot >= 15
           |cape_distress_tot >= 50|pqb_distress_tot >= 6|ybocs_sym > 13
           |lsas_tot > 30|edeq_mean > 1.68))

gghistogram(df_yes_3m$m3_k6_tot) + labs(x = "K6")

df_model <- df_yes_bl %>% 
  left_join(., df_yes_3m %>% 
              select(subject_code, m3_k6_tot)) %>% 
  left_join(df_yes_demo) %>% 
  select(-sf12_pcs, -sf12_mcs, -cidi_mde_l:-cidi_audit_tot_12m, 
         -yms_tot, -k6_tot,
         -subject_code) 

df_split <- initial_split(df_model, prop = .75)
df_train <- training(df_split)
df_test <- testing(df_split)

theme_gtsummary_journal(journal = "jama", 
                        set_theme = T)
theme_gtsummary_compact()

df_train %>% 
  mutate(dataset = "Train") %>% 
  union_all(df_test %>% 
              mutate(dataset = "Test")) %>% 
  mutate(dataset = factor(dataset, levels = c("Train","Test"))) %>% 
  select(age, sex, edu_yes_no, dataset) %>% 
  set_variable_labels(age = "Age", sex = "Gender", 
                      edu_yes_no = "Current Student") %>% 
  tbl_summary(., by = dataset,
              missing_text = "Missing",
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  add_n() %>% 
   add_p() %>% 
  modify_header(label = "") %>% 
  bold_labels() %>%
  #bold_p(t = 0.05) %>% 
  as_flex_table() %>% 
  flextable::bold(part = "header") %>% 
  flextable::font(fontname = "Arial", part = "all")
reset_gtsummary_theme()


cv_folds <- vfold_cv(df_train, v = 5, repeats = 10)

cart_grid <- 
  expand.grid(tree_depth = c(1:7))

cart_model <- decision_tree(
  cost_complexity = 0.01,
  tree_depth = tune()) %>% 
  set_engine('rpart') %>% 
  set_mode("regression")

cart_rec <- recipe(m3_k6_tot ~ gad7_tot + dass_stress + dass_depress + phq9_tot 
                   + dass_anx + bfi_neuroticism, 
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
  arrange(Importance) %>% 
  ggscatter(x = "Variable", y = "Importance",
            ggtheme = theme_bw()) + 
  coord_flip() +
  theme(axis.text.y = element_text(color = "black"),
        text = element_text(size = 14))

last_fit_metrics <- metric_set(rmse, rsq, mae)

final_cart %>% 
  collect_predictions() %>% 
  last_fit_metrics(., truth = m3_k6_tot, estimate = .pred) %>% 
  knitr::kable() %>% 
  kableExtra::kable_classic_2(full = F)

fit <- rpart(m3_k6_tot ~ gad7_tot + dass_stress + dass_depress + phq9_tot 
             + dass_anx + bfi_neuroticism, 
             data = df_train, 
             control = list(maxdepth = 4, cp = 0.01))

rpart.plot(fit,type = 4, box.palette = "-Blue", clip.right.labs = F, 
           xflip = T, roundint = T) 
