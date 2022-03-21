library(tidyverse);library(ggplot2);library(rpart);library(here);
library(tidymodels);library(labelled);library(rpart.plot);library(rstatix)
library(party);library(ggpubr);library(sjPlot);library(vip);
library(factoextra);library(corrplot);library(gtsummary);

# load data
load("dat/20210927_2m_clean.rdata")
var_list <- names(clean_2m_demo)
rm(list = ls()[(ls() != 'var_list')])
load("dat/2m_cart_re-validation.rdata")
################# validation with 2M users visit 1 year later #################
raw <- 
  here("dat", "original", "25Jan2022_2M (cleaned).xlsx") %>% 
  readxl::read_xlsx() %>% 
  group_by(subject_code) %>%
  mutate(bl_date = min(submit_time))

raw_demo <- 
  here("dat", "original", "25Jan2022_Baseline (cleaned).xlsx") %>% 
  readxl::read_xlsx() %>% 
  select(subject_code:marriage, -submit_time) %>% 
  rename(age = ageby_sub)

oneyrlater_df <- raw %>% 
  filter(bl_date >= as.Date('2021-06-01')) %>% 
  rename(no_product = k10_k6_3,
         half_product = k10_k6_4) %>% 
  left_join(., raw_demo, by = "subject_code") %>% 
  select(var_list)

theme_gtsummary_journal(journal = "jama", 
                        set_theme = T)
theme_gtsummary_compact()

df_pre %>% 
  mutate_at(names(.)[-c(1,8,9,12,30)], as.numeric) %>%
  mutate(dataset = "Included Hub Users") %>% 
  select(-hub_name) %>% 
  union_all(revalid_2m %>% 
              mutate_at(names(.)[-c(1,8,9,12,30)], as.numeric) %>% 
              mutate(dataset = "After Jun01, 2021") %>% 
              select(-hub_name)) %>% 
  mutate(dataset = factor(dataset, levels = 
                            c("Included Hub Users", "After Jun01, 2021"))) %>% 
  select(age, gender, edu_yes_no, k6_score, sf6_z, no_product, dataset,
         hospitalization_days, self_harm) %>% 
  set_variable_labels(age = "Age", gender = "Gender", 
                      edu_yes_no = "Current Student",
                      k6_score = "K6 Score", sf6_z = "SF-6D Index",
                      no_product = "Days of No Productivity",
                      hospitalization_days = "Hospitalization (Days)",
                      self_harm = "Self Harm") %>% 
  tbl_summary(., by = "dataset", 
              missing_text = "Missing",
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(hospitalization_days ~ c(2,2))) %>% 
  add_n() %>% 
  add_p() %>% 
  modify_header(label = "") %>% 
  bold_labels() %>%
  bold_p(t = 0.05) %>% 
  as_flex_table() %>% 
  flextable::bold(part = "header") %>% 
  flextable::font(fontname = "Arial", part = "all")
reset_gtsummary_theme()

# prepare data
set.seed(1234) 
revalid_2m <- 
  data_pre(oneyrlater_df) %>% 
  mutate(pc.score = predict(pca, newdata = .)[,1]) %>% 
  ungroup() 

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
                predict(., revalid_2m) %>% 
                bind_cols(., revalid_2m) %>% 
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
               col.names = c("","Included Hub Users", "After Jun01, 2021")) %>% 
  kableExtra::kable_classic_2(full_width = F, html_font = "Arial") %>% 
  kableExtra::pack_rows("Tree Depth = 2", 1, 3, bold = F, italic = T) %>% 
  kableExtra::pack_rows("Tree Depth = 3", 4, 6, bold = F, italic = T) %>% 
  kableExtra::row_spec(0, bold = T)

######################### validation with yes data #############################
yes_bl <- here("dat", "original",
               "(DONOTTOUCH) HKYES Master Database_12 Jan 2022.xlsx") %>% 
  readxl::read_excel(sheet = "Total", 
             range = readxl::cell_cols("A:EHE"),
             na = c("888","999","-999")) %>% 
  slice(-1) %>% 
  janitor::clean_names() %>% 
  dplyr::select(yes_id, sf12_tot, k6_tot, sds_tot) %>% 
  left_join(., 
            here("dat", "original",
                 "(DONOTTOUCH) HKYES Master Database_12 Jan 2022.xlsx") %>% 
              readxl::read_excel(sheet = "Raw", na = c("888","999","-999")) %>% 
              slice(-1) %>% 
              janitor::clean_names() %>% 
              dplyr::select(yes_id, age, sex, edu_yes_no, sds_day),
            by = "yes_id") %>% 
  mutate_at(names(.)[2:length(names(.))], as.numeric) %>%
  rename(subject_code = yes_id,
         `SF-6D` = sf12_tot,
         K6 = k6_tot) %>% 
  #mutate(subject_code = toupper(subject_code)) %>% 
  filter(!subject_code == 0 & !is.na(K6) & !`SF-6D` > 1) %>% 
  group_by(subject_code) %>% 
  slice(1) %>% 
  ungroup() # 3294 one duplicate id, extract the first

yes_3m <- 
  here("dat", "original",
       "(DONOTTOUCH) 3mFU Master Database 20220204.xlsx") %>% 
  readxl::read_excel(sheet = "Raw_FINAL", na = c("888","999","-999")) %>% 
  slice(-1) %>% 
  janitor::clean_names() %>% 
  dplyr::select(yes_id, m3_sds_day) %>% 
  left_join(., 
            here("dat", "original",
                 "(DONOTTOUCH) 3mFU Master Database 20220204.xlsx") %>% 
              readxl::read_excel(sheet = "Total", na = c("888","999","-999")) %>% 
              slice(-1) %>% 
              janitor::clean_names(),
            by = "yes_id") %>% 
  # sds day 在過去30天中, 你有多少天因這些癥狀而無法上學/工作/處理你的日常生活事務?
  mutate_at(names(.)[2:length(names(.))], as.numeric) %>% 
  filter(!yes_id == 0 & !is.na(m3_k6_tot)) %>% 
  rename(subject_code = yes_id) %>% 
  #mutate(subject_code = toupper(subject_code)) %>% 
  group_by(subject_code) %>% 
  slice(1) %>% 
  ungroup()

set.seed(123)
yes_w_cart <- yes_bl %>% 
  mutate(pred = round(predict(fit, yes_bl),2),
         class = case_when(pred == -2.16 ~ "poor",
                           pred == -0.75 ~ "intermediate2",
                           pred == -0.21 ~ "intermediate1",
                           pred == 0.94 ~ "good") %>% 
           factor(., levels = c("good", "intermediate1", 
                                "intermediate2", "poor"))) %>% 
  left_join(., yes_3m, by = "subject_code") %>% 
  filter(!is.na(m3_k6_tot)) %>% #1535
  mutate(k6_diff = m3_k6_tot - K6,
         sds_day_dff = m3_sds_day - sds_day, 
         sds_diff = m3_sds_tot - sds_tot) 

ls_3m_var <- names(yes_3m)[-1] %>% as.list()
out.plots <- 
  lapply(ls_3m_var, 
         function(var){
           ggviolin(data = yes_w_cart, x = "class", y = var, fill = "class",
                    title = var, add = "mean_sd") +
             stat_compare_means(label = "p", method = "wilcox.test",
                                ref.group = "intermediate1") +
             stat_compare_means(label.y = max(yes_w_cart %>% 
                                                select(var), na.rm = T) + 2) + 
             stat_summary(fun.data = function(x) {
               data.frame(y = max(yes_w_cart %>% select(var), na.rm = T)/2, 
                          label = paste("Mean=", round(mean(x),2), "(",length(x),")",
                                        sep = ""))
             }, geom = "text") + 
             theme(legend.title = element_blank(), axis.text.x = element_blank(),
                   axis.title.x = element_blank()) %>% return()
         }
  )

out.plots[[18]]

merge.out <- 
  function(x){
  quartz(width = 18,height = 6,pointsize = 12,dpi = 100)
  ggarrange(plotlist = out.plots[(x):(x + 2)], ncol = 3)
  }
#merge.out 1:7
merge.out(26)

lapply(c("k6_diff", "sds_day_dff", "sds_diff"),
        function(x){
        gghistogram(data = yes_w_cart, x = x, binwidth = 1) + 
    scale_x_continuous(n.breaks = 10) 
  }
  ) %>% 
  ggarrange(plotlist = ., ncol = 1)

ls_test <- 
  mapply(
    function(x, y){
      long_yes_w_cart <- 
        yes_w_cart %>% 
        select(subject_code, class, x, y) %>% 
        filter(complete.cases(.)) %>% 
        rename("first" = x, "3M" = y) %>% 
        pivot_longer(names(.)[-1:-2],
                     names_to = "visit",
                     values_to = "score") 
      # mixed anova
      aov <- anova_test(data = long_yes_w_cart, dv = score,
                        wid = subject_code, between = class,
                        within = visit)
      
      #p <- ggpaired(long_yes_w_cart, x = "visit", y = "score",
       #             color = "class", line.color = "gray", line.size = 0.4,
        #            add = "mean_sd") +
      p <- ggviolin(long_yes_w_cart, x = "visit", y = "score", fill = "class",
                    add = "mean_sd") +
        facet_grid( ~ class) + 
        labs(x = x,
             subtitle = get_test_label(aov)) +
        stat_compare_means(label = "p", paired = T) +
        stat_summary(fun.data = function(x) {
          data.frame(y = -2, 
                     label = paste("Mean=", round(mean(x),2), "(",length(x)/2,")",
                                   sep = ""))
        }, geom = "text")
      return(list(aov, p))
    },
    x = c("K6", "sds_tot", "sds_day"),
    y = c("m3_k6_tot", "m3_sds_tot", "m3_sds_day"))

ls_test[2,]

check <- yes_w_cart %>% filter(class == "poor") %>% select(subject_code, K6, m3_k6_tot) %>% 
  filter(m3_k6_tot > 15)

# summary table
theme_gtsummary_journal(journal = "jama", set_theme = T)
theme_gtsummary_compact()

df_model %>% 
  select(subject_code, sf6_z, k6_score, age, gender, edu_yes_no) %>% 
  rename(`SF-6D` = sf6_z,
         K6 = k6_score, 
         sex = gender) %>% 
  mutate(dataset = "2M") %>% 
  union_all(yes_w_cart %>% mutate(dataset = "YES",
                              sex = if_else(sex == 1, "Male", "Female"),
                              edu_yes_no = if_else(edu_yes_no == 1, "Yes", "No") %>% 
                                as.factor()) %>% 
              select(names(yes_bl)[-c(4,8)], dataset)) %>% 
  mutate(dataset = factor(dataset, levels = c("2M","YES"))) %>% 
  select(-subject_code) %>% 
  set_variable_labels(age = "Age", sex = "Gender", 
                      edu_yes_no = "Current Student",
                      K6 = "K6 Score", `SF-6D` = "SF-6D Index") %>% 
  tbl_summary(., by = "dataset", 
              missing_text = "Missing",
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(`SF-6D` ~ c(4, 2))) %>% 
  add_p() %>% 
  modify_header(label = "") %>% 
  bold_labels() %>%
  bold_p(t = 0.05) %>% 
  as_flex_table() %>% 
  flextable::bold(part = "header") %>% 
  flextable::font(fontname = "Arial", part = "all")
reset_gtsummary_theme()

