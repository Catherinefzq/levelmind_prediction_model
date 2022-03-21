#######################################
# Plots for MEL Subcom & PMC Meetings #
#######################################

library(tidyverse);library(readxl);library(ggplot2)
library(geepack);library(sjPlot);library(MESS)
library(ggpmisc); library(rstatix); library(ggpubr);
library(stringr);library(cowplot)

# Import data
load("dat/20210831_2m_clean.rdat")

# Hub users
# Demographics
## Gender (hub)
demo = read_xlsx("dat/NoPHI.xlsx", sheet = "bl") %>% janitor::clean_names() %>% 
  mutate(gender = ls_replace(gender, c(1:2), c("Male","Female"))) %>% 
  mutate(edu_yes_no = ls_replace(edu_yes_no, c(0:1), c("No","Yes"))) 
demo = demo[!demo$subject_code=="FYTW0411",]

ggplot(aes(y = Freq, x = "", fill = Var1), data = table(demo$gender) %>% 
         as.data.frame()) + 
  geom_bar(stat="identity", width=1, color="white") + 
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(Freq*100/nrow(demo), digits = 2), "% (",Freq,")")), position = position_stack(vjust = 0.5)) + 
  labs(fill = "Gender") + 
  theme_void() 

ggplot(aes(y = Freq, x = "", fill = Var1), data = table(demo$edu_yes_no) %>% as.data.frame()) + geom_bar(stat="identity", width=1, color="white") + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(Freq*100/nrow(demo), digits = 2), "% (",Freq,")")), position = position_stack(vjust = 0.5)) + labs(fill = "Current Student") + theme_void() 
#demo$subject_code[!demo$subject_code %in% dt_2m$subject_code]

skimr::skim(dt_clean)
## Age (hub)
ggplot(aes(x=ageby_sub), data = demo) + geom_bar(fill="steelblue") + scale_x_continuous(breaks = c(1:35)) + geom_text(aes(label =..count..),stat='count', vjust = -2, size = 3) +  geom_text(aes(label = scales::percent(..prop..), group = 1), stat= "count", vjust = -.5, size = 3) +labs(x = "Age", y ="Count") 
skimr::skim(demo$ageby_sub)

## k6 over time (hub)
ggplot(dt_2m, aes(x=submit_time,y=k6_score)) + 
  geom_point(alpha = 0.5, aes(color = subject_code)) + 
  geom_smooth(method = 'loess') + 
  scale_x_date(date_labels = "%d %b %y", minor_breaks = "1 day", date_breaks = "1 month") + 
  labs(y = "K6", x = "Submit Time") + 
  theme(legend.position = "none", 
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 10)) 

## k6 by age (hub)
ggplot(subset(dt_2m_demo, !is.na(age)), aes(x=from_bl,y=k6_score)) + 
  geom_point(alpha = 0.7, aes(group = subject_code,color = age)) + 
  geom_line(alpha = 0.7, aes(group = subject_code,color = age)) + 
  labs(y = "K6", x = "Days from baseline") + 
  theme(axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 10)) 

## k6 density by gender (hub)
dt_2m_demo = dt_2m_demo %>% mutate(gender = ls_replace(gender, c(1:2), c("Male","Female")))
ggplot(subset(dt_2m_demo, !is.na(gender)&from_bl==0), aes(x=k6_score)) + 
  geom_density(alpha = 0.5, aes(group = factor(gender),color = factor(gender), fill = factor(gender))) +
  labs(x = "K6", y = "Density", fill = "Gender", color = "Gender") + 
  theme(axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 10)) 

subset(dt_2m_demo, !is.na(gender)&from_bl ==0) %>% group_by(gender) %>% skimr::skim()

## k6 by tiers (hub + non-hub)
qct_exclude_id = lapply(c("COM","HUB"),function(x){joint_dt$subject_code[grep(x,joint_dt$subject_code)]}) %>% unlist()
dt_all = dt_cb %>% mutate(project = ls_replace(project, c("2M", "Control"), c("Hub","Non-hub"))) %>% filter(!is.na(gender)) %>% filter(!subject_code %in% qct_exclude_id)
dt_all = dt_all[!dt_all$subject_code=="FYTW0411",]

ggplot(subset(dt_all, from_bl==0&project == "Hub"),aes(x = tier, 
                                                       y = ..count../sum(..count..))) + 
  geom_bar(aes(fill = tier, color = tier)) +
  labs(x = "Tier by K6", y = "Propotion", fill = "Tier", color = "Tier") + 
  geom_text(aes(label =..count..),stat='count', vjust = -2, size = 3) +  
  geom_text(aes(label = scales::percent(..prop..), group = 1), stat= "count", vjust = -.5, size = 3) +
  theme(axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 10)) +
  scale_y_continuous(labels = scales::percent, limits = c(0:1))
ggplot(subset(dt_all, from_bl==0&project == "Non-hub"),aes(x = tier,  y = ..count../sum(..count..))) + 
  geom_bar(aes(fill = tier, color = tier)) +
  labs(x = "Tier by K6", y = "Propotion", fill = "Tier", color = "Tier") + 
  geom_text(aes(label =..count..),stat='count', vjust = -2, size = 3) +  
  geom_text(aes(label = scales::percent(..prop..), group = 1), stat= "count", vjust = -.5, size = 3) +
  theme(axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 10)) +
  scale_y_continuous(labels = scales::percent,limits = c(0:1))
subset(dt_all, from_bl ==0) %>% filter(!is.na(gender))%>% group_by(project) %>% skimr::skim()

### tier 3 hub youths demo
dt_t3 = dt_all %>% filter(tier == "T3" & from_bl == 0 & project =="Hub") %>% mutate(gender = ls_replace(gender, c(1:2), c("Male","Female")))
## t3 gender
ggplot(aes(y = Freq, x = "", fill = Var1), data = table(dt_t3$gender) %>% as.data.frame()) + geom_bar(stat="identity", width=1, color="white") + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(Freq*100/nrow(demo), digits = 2), "% (",Freq,")")), position = position_stack(vjust = 0.5)) + labs(fill = "Gender") + theme_void() 

## t3 age
ggplot(aes(x=age), data = dt_t3) + geom_bar(fill="steelblue") + scale_x_continuous(breaks = c(1:35)) + geom_text(aes(label =..count..),stat='count', vjust = -2, size = 3) +  geom_text(aes(label = scales::percent(..prop..), group = 1), stat= "count", vjust = -.5, size = 3) +labs(x = "Age", y ="Count") 
skimr::skim(dt_t3)

## SF6D (hub)
ggplot(subset(dt_all, from_bl==0&project =="Hub"), aes(x = sf6_z,  y = ..count..)) + 
  geom_histogram(fill = "steelblue", color = "black",bins = 100) +
  scale_x_continuous(n.breaks = 10) +
  labs(x = "SF6D Index Score", y = "Count") + 
  theme(axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 10)) 
  #facet_grid(~gender)
subset(dt_hub, from_bl ==0&project=="Hub") %>% skimr::skim()

## SF6D (non-hub)
ggplot(subset(dt_all, from_bl==0&project =="Control"), aes(x = sf6_z,  y = ..count..)) + 
  geom_histogram(fill = "steelblue", color = "black",bins = 100) +
  scale_x_continuous(n.breaks = 10) +
  labs(x = "SF6D Index Score", y = "Count") + 
  theme(axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 10))
subset(dt_cb, from_bl ==0&project=="Control") %>% skimr::skim()

## QALY Gain (hub)
cbt_qol_plot = mbt_qol %>% janitor::clean_names() %>% select(subject_code, sf12_tot_before, sf12_tot_after, t2score_cut, int_vs_control) %>% left_join(.,dt_cb %>% group_by(subject_code) %>% slice(n()) %>% select(subject_code, from_bl) %>% ungroup())
### extract baseline sf6 score
bl = clean_2m %>% group_by(subject_code) %>% slice(1) %>% rename(sf6_bl = sf6_z, k6_bl = k6_score) %>% select(subject_code, sf6_bl, k6_bl) %>% mutate(tier = ls_replace(k6_bl, c(0:24), c(rep("T0", 9 ), rep("T1",2), rep("T2",4), rep("T3",10))))

### combine with baseline
dt_plot = cbt_qol_plot %>% mutate(qaly = as.numeric((sf12_tot_after - sf12_tot_before)*(from_bl/365))) %>% group_by(int_vs_control) %>% mutate(int_vs_control = factor(ls_replace(int_vs_control, c(0,1),c("Control","MBT")), levels = c("MBT","Control")), t2score_cut = ls_replace(t2score_cut, c(0,1),c("K6<11","K6>=11")))

dt_plot = clean_2m %>% group_by(subject_code) %>% arrange(subject_code, submit_time) %>% 
  slice(2) %>% 
  left_join(bl, by = "subject_code") %>% 
  filter(as.numeric(days_of_2m)>0) %>% 
  ungroup() %>% 
  mutate(qaly = as.numeric((as.numeric(sf6_z) - as.numeric(sf6_bl))*(as.numeric(days_of_2m)/365))) %>%
  ungroup()
dt_plot = dt_plot[!dt_plot$subject_code=="FYTW0411",]

#### include parti with more than one visit

stat.test = dt_plot %>% group_by(tier) %>% t_test(qaly~1, mu=0) %>% adjust_pvalue() %>% mutate(y.position = 0.021)
### hub plot label with frequency
aggregate(.~int_vs_control*t2score_cut, data = dt_plot %>% select(int_vs_control,t2score_cut, qaly), sd) 


aggregate(.~int_vs_control*t2score_cut, data = dt_plot %>% select(int_vs_control,t2score_cut, qaly), mean) %>% 
  left_join(.,data.frame(table(dt_plot$int_vs_control,dt_plot$t2score_cut)), by = c("int_vs_control"="Var1","t2score_cut" = "Var2")) %>% 
  ggplot(aes(x = int_vs_control, y = qaly, fill = int_vs_control), data=.) + 
  geom_text(aes(label=format(round(qaly, digits = 5)),y = qaly + 0.002 * sign(qaly)), colour = "black") +
  geom_text(aes(label=Freq, y = qaly + 0.005 * sign(qaly)), colour = "black")+
  geom_bar(aes(fill=int_vs_control), stat = "identity") +
  ylim(c(-0.02,0.07)) + 
  labs(x = "K6 Score", y = "Mean QALY gained", title = paste("MBT (N=",nrow(dt_plot),")", sep = ""), fill = "") +
  theme_bw() + facet_wrap(~t2score_cut)




aggregate(.~tier, data = dt_plot %>% select(tier, qaly), mean) %>% 
  left_join(.,data.frame(table(dt_plot$tier)), by = c("tier"="Var1")) %>% 
  ggplot(aes(x = tier, y = qaly, fill = tier), data=.) + 
  geom_text(aes(label=format(round(qaly, digits = 5))), vjust=-0.5, colour = "black") +
  geom_text(aes(label=Freq), vjust=-2, colour = "black")+
  geom_bar(aes(fill=tier), stat = "identity") +
  ylim(c(-0.01,0.05)) + 
  labs(x = "Tier Status by K6", y = "Mean QALY gained", title = paste("Hub Users (n=",nrow(dt_plot),")", sep = "")) + theme_bw()


dt_plot %>% group_by(tier) %>% skimr::skim()
dt_plot  %>% skimr::skim()
### nhub data
nhub_bl = joint_dt %>% 
  group_by(subject_code) %>% 
  slice(1) %>% 
  rename(sf6_bl = sf6_z, k6_bl = k6_score) %>% 
  select(subject_code, sf6_bl, k6_bl) %>% 
  mutate(tier = ls_replace(k6_bl, c(0:24), c(rep("T0", 9 ), rep("T1",2), rep("T2",4), rep("T3",10)))) %>% 
  ungroup()

nhub_plot = joint_dt %>% group_by(subject_code) %>% arrange(subject_code, from_bl) %>% slice(n()) %>% left_join(nhub_bl, by = "subject_code") %>% filter(from_bl>0) %>% mutate(qaly = (sf6_z - sf6_bl)*(from_bl/365)) %>% ungroup() %>% filter(!subject_code %in% qct_exclude_id) 

# add nhub plot
aggregate(.~tier, data = dt_plot %>% select(tier, qaly), mean) %>% 
  left_join(.,data.frame(table(dt_plot$tier)), by = c("tier"="Var1")) %>% 
  union(tibble(tier = "Non-hub", qaly = mean(nhub_plot$qaly, na.rm = T), Freq = nrow(nhub_plot))) %>%  
  ggplot(aes(x = tier, y = qaly, fill = tier), data=.) + 
  geom_text(aes(label=format(round(qaly, digits = 5)), y = qaly + 0.001 * sign(qaly)), colour = "black") +
  geom_text(aes(label=paste("(",tier," n = ",Freq, ")",sep = ""), y = qaly + .002 * sign(qaly)), colour = "black")+
  geom_bar(aes(fill=tier), stat = "identity", width = 0.8) + 
  ylim(c(0,0.03))+
  labs(x = "Tier Status by K6", y = "Mean QALY gained", title = paste("Hub Users (n=",nrow(dt_plot),")", sep = ""), fill = "") + theme_bw()

test = nhub_plot %>% select(subject_code, sf6_z, sf6_bl, from_bl, qaly)
nhub_plot %>% group_by(tier) %>% select(-eq5d_tot:-suicidal_thought) %>% skimr::skim()
nhub_plot %>% select(-eq5d_tot:-suicidal_thought) %>% skimr::skim()
dt_plot %>% group_by(tier) %>% t_test(qaly~1, mu=0) %>% adjust_pvalue() %>% mutate(y.position = 0.017)

t.test(subset(dt_plot, tier == "T0")$qaly, subset(nhub_plot, tier == "T0")$qaly)$p.value
t.test(subset(dt_plot, tier == "T1")$qaly, subset(nhub_plot, tier == "T1")$qaly)$p.value
t.test(subset(dt_plot, tier == "T2")$qaly, subset(nhub_plot, tier == "T2")$qaly)$p.value
t.test(subset(dt_plot, tier == "T3")$qaly, subset(nhub_plot, tier == "T3")$qaly)$p.value
t.test(dt_plot$qaly, nhub_plot$qaly)$p.value

## productivity (hub)
dt_product = dt_2m %>% group_by(subject_code) %>% slice(1) %>% rename(sf6_bl = sf6_z, k6_bl = k6_score)  %>% select(subject_code, sf6_bl, k6_bl) %>% mutate(tier = ls_replace(k6_bl, c(0:24), c(rep("T0", 9 ), rep("T1",2), rep("T2",4), rep("T3",10)))) %>%  right_join(dt_2m, ., by = "subject_code") %>% filter(!is.na(tier)) %>% filter(from_bl == 0)
skimr::skim(dt_product)

### no product
ggplot(aes(x = tier, y = no_product, fill = tier), data = dt_product) + 
  stat_summary(fun=mean, geom="bar") + 
  stat_summary(fun=mean, colour="black", geom="text", vjust = -0.5, aes(label=format(round(..y.., digits=3), scientific = F))) + 
  labs(x = "Tier by K6", y = str_wrap("Days of no productivity in past one month due to emotional distress",40), fill = "Tier", color = "Tier") +   
  coord_cartesian(ylim=c(0,13)) + 
  theme_bw() 

### half product
ggplot(aes(x = tier, y = half_product, fill = tier), data = dt_product) + 
  stat_summary(fun=mean, geom="bar") + 
  stat_summary(fun=mean, colour="black", geom="text", vjust = -0.5, aes(label=format(round(..y.., digits=3), scientific = F))) + 
  labs(x = "Tier by K6", y = str_wrap("Days of half or less productivity in past one month due to emotional distress",40), fill = "Tier", color = "Tier") + 
  coord_cartesian(ylim=c(0, 13)) + 
  theme_bw()

### Quarterly report K6 performance by hub
dt_quart = dt_2m_demo %>% filter(submit_time>as.Date('2020-10-31')& submit_time<as.Date('2021-02-01')) %>% arrange(subject_code, submit_time, visit) %>% group_by(subject_code) %>% slice(1) %>% ungroup() %>% mutate(tier = ls_replace(k6_score, c(0:24), c(rep("T0", 9), rep("T1" ,2), rep("T2",4), rep("T3",10)))) %>% group_by(hub_name)

#### Change hub name
ggplot(subset(dt_quart, hub_name == "HKPA"&district == "Kwun Tong"), aes(x = tier,  y = ..count../sum(..count..))) + 
  geom_bar(aes(fill = tier, color = tier)) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Tier by K6", y = "Propotion", fill = "Tier", color = "Tier") + 
  ggtitle("Tier Distribution (HKPA Kwun Tong)") + 
  geom_text(aes(label =..count..),stat='count', vjust = -2, size = 3) +  
  geom_text(aes(label = scales::percent(round(..prop.., digits = 2)), group = 1), stat= "count", vjust = -.5, size = 3) +
  theme(axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 8)) 


### Quarterly report K6 mean by hub boxplot
dt_quart = dt_2m_demo %>% filter(submit_time>as.Date('2020-10-31')& submit_time<as.Date('2021-02-01'))
means = aggregate(k6_score ~  hub_name, dt_quart, mean)

ggplot_box_legend <- function(family = "serif"){
  
  # Create data to use in the boxplot legend:
  set.seed(100)
  
  sample_df <- data.frame(parameter = "test",
                          values = sample(500))
  
  # Extend the top whisker a bit:
  sample_df$values[1:100] <- 701:800
  # Make sure there's only 1 lower outlier:
  sample_df$values[1] <- -350
  
  # Function to calculate important values:
  ggplot2_boxplot <- function(x){
    
    quartiles <- as.numeric(quantile(x, 
                                     probs = c(0.25, 0.5, 0.75)))
    
    names(quartiles) <- c("25th percentile", 
                          "50th percentile\n(median)",
                          "75th percentile")
    
    IQR <- diff(quartiles[c(1,3)])
    
    upper_whisker <- max(x[x < (quartiles[3] + 1.5 * IQR)])
    lower_whisker <- min(x[x > (quartiles[1] - 1.5 * IQR)])
    
    upper_dots <- x[x > (quartiles[3] + 1.5*IQR)]
    lower_dots <- x[x < (quartiles[1] - 1.5*IQR)]
    
    return(list("quartiles" = quartiles,
                "25th percentile" = as.numeric(quartiles[1]),
                "50th percentile\n(median)" = as.numeric(quartiles[2]),
                "75th percentile" = as.numeric(quartiles[3]),
                "IQR" = IQR,
                "upper_whisker" = upper_whisker,
                "lower_whisker" = lower_whisker,
                "upper_dots" = upper_dots,
                "lower_dots" = lower_dots))
  }
  
  # Get those values:
  ggplot_output <- ggplot2_boxplot(sample_df$values)
  # Lots of text in the legend, make it smaller and consistent font:
  update_geom_defaults("text", 
                       list(size = 3, 
                            hjust = 0,
                            family = family))
  # Labels don't inherit text:
  update_geom_defaults("label", 
                       list(size = 3, 
                            hjust = 0,
                            family = family))
  # Create the legend:
  # The main elements of the plot (the boxplot, error bars, and count)
  # are the easy part.
  # The text describing each of those takes a lot of fiddling to
  # get the location and style just right:
  explain_plot <- ggplot() + 
    stat_boxplot(data = sample_df, aes(x = parameter, y=values),geom ='errorbar', width = 0.3) + 
    geom_boxplot(data = sample_df,aes(x = parameter, y=values), width = 0.3, fill = "lightgrey") +  
    theme_minimal(base_size = 5, base_family = family) +
    geom_segment(aes(x = 2.3, xend = 2.3, y = ggplot_output[["25th percentile"]],  yend = ggplot_output[["75th percentile"]])) +  
    geom_segment(aes(x = 1.2, xend = 2.3, y = ggplot_output[["25th percentile"]], yend = ggplot_output[["25th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.3,y = ggplot_output[["75th percentile"]], yend = ggplot_output[["75th percentile"]])) + 
    geom_text(aes(x = 2.4, y = ggplot_output[["50th percentile\n(median)"]]),  label = "Interquartile\nrange", fontface = "bold", vjust = 0.4) +
    geom_text(aes(x = c(1.17,1.17), y = c(ggplot_output[["upper_whisker"]], ggplot_output[["lower_whisker"]]),label = c("Maximum", "Minimun")), fontface = "bold", vjust = 0.9) +    
    geom_text(aes(x = c(1.17),y =  ggplot_output[["lower_dots"]], label = "Mean"), vjust = 0.5, fontface = "bold") +     
    #geom_text(aes(x = c(1.9), y =  ggplot_output[["lower_dots"]],  label = "-Value is >1.5 times and"), vjust = 0.5) +
    #geom_text(aes(x = 1.17,  y = ggplot_output[["lower_dots"]], label = "<3 times the interquartile range\nbeyond either end of the box"), vjust = 1.5) +
    geom_label(aes(x = 1.17, y = ggplot_output[["quartiles"]], 
                   label = names(ggplot_output[["quartiles"]])),
               vjust = c(0.4,0.85,0.4), 
               fill = "white", label.size = 0) +
    ylab("") + xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = 4/3,
          plot.title = element_text(hjust = 0.5, size = 10)) +
    coord_cartesian(xlim = c(1.4,3.1), ylim = c(-600, 900)) +
    labs(title = "EXPLANATION")
  
  return(explain_plot) 
}

legend_plot = ggplot_box_legend()
# change hub name

p = ggplot(data = subset(dt_quart, hub_name == "BGCA"), aes(x = hub_name, y = k6_score)) + geom_boxplot() + stat_summary(fun=mean, geom="point", shape=20, size=4) + labs(x = "", y = "K6 Score") + ggtitle("Average K6 Score (BGCA)") + stat_summary(fun=mean, colour="black", geom="text",show.legend = FALSE, hjust = 0.5,vjust = -1,size= 4,aes(label=round(..y.., digits=3)))
plot_grid(p,
          legend_plot,
          nrow = 1)
### ID count by time
dt_count = dt_2m_demo %>% filter(!is.na(gender)) 
#%>% filter(submit_time<=as.Date("2021-02-22")&submit_time>=as.Date("2021-02-16"))
ggplot(data = dt_count,aes(x=submit_time)) + geom_line(aes(y = cumsum(..count..), color = "red"), stat = "bin") + geom_line(data = subset(dt_count, from_bl == 0),aes(y = cumsum(..count..), color = "blue"), stat = "bin")+ scale_x_date(breaks = "1 week",date_labels = "%b %d", minor_breaks = "1 week") + theme(plot.title = element_text(hjust = 0.5, size = 15),legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5), panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "gray", size = 0.5),panel.grid.major.x = element_blank(),panel.background = element_blank(),line = element_blank()) + scale_y_continuous(n.breaks = 16) + labs(x = "", y = "", title = "2M Hub Users Cumulative Headcount and Entries") + scale_color_manual(name = "", values = c("red" = "red", "blue"="blue"), labels = c("Headcount", "Entries"))


## Hub comparison ##
dt_compare = dt_cb %>% filter(project == "2M") %>% left_join(.,dt_2m_demo %>% select(subject_code, from_bl ,district, hub_name, no_product, half_product), by = c("subject_code","from_bl")) %>% mutate(hub_type = if_else(hub_name == "BGCA"|(hub_name == "Caritas" & district == "Kwai Tsing"), "Specialised hub", "Regular hub")) 
dt_cp_bl = dt_compare %>% filter(from_bl==0) %>% group_by(hub_type) %>% filter(!is.na(gender)) %>% mutate(tier_rcode = factor(ls_replace(tier, c("T0","T1","T2","T3"), c("0-8","9-10","11-14",">=15")), levels = c("0-8","9-10","11-14",">=15")))

### gender
dt_cp_gender = table(dt_cp_bl %>% select(gender)) %>% as.data.frame() %>%  left_join(.,as.data.frame(table(dt_cp_bl$hub_type)), by = c("hub_type"="Var1")) %>% mutate(gender = if_else(gender==1,"Male","Female")) %>% mutate(prop = Freq.x/Freq.y)
cp <- coord_polar(theta = "y")
cp$is_free <- function() TRUE
ggplot(aes(y = Freq.x, x = "", fill = gender), data = dt_cp_gender)  + geom_bar(stat="identity", width=1, color="white") + cp + facet_wrap(~hub_type, scales = "free") + geom_text(aes(label = paste0(round(Freq.x*100/Freq.y, digits = 2), "% (",Freq.x,")")), size = 4.5, position = position_stack(vjust = 0.5)) + labs(fill = "Gender") + theme_void(base_size = 16)
rm(cp)

prop.test(c(dt_cp_gender[1,3],dt_cp_gender[2,3]),c(dt_cp_gender[1,4],dt_cp_gender[2,4]))
prop.test(c(dt_cp_gender[3,3],dt_cp_gender[4,3]),c(dt_cp_gender[3,4],dt_cp_gender[4,4]))
### age
ggplot(aes(x=age), data = dt_cp_bl) + geom_bar(fill="steelblue") + scale_x_continuous(breaks = c(1:35)) +geom_text(aes(label =..count..),stat='count', vjust = -2, size = 3.5) +  geom_text(aes(label = scales::percent(..prop..), group = 1), stat= "count", vjust = -.5, size = 3.5) +labs(x = "Age", y ="Count") + facet_wrap(~hub_type, nrow = 2) + theme_grey(base_size = 14) + ylim(0, 65)
skimr::skim(dt_cp_bl %>% group_by(hub_type))
aggregate(dt_cp_bl$age, list(dt_cp_bl$hub_type),mean)
t.test(subset(dt_cp_bl, hub_type == "Mega")$age, subset(dt_cp_bl, hub_type == "Regular")$age)
### tier
dt_tier_pt = table(dt_cp_bl$tier, dt_cp_bl$hub_type) %>% as.data.frame() %>% left_join(.,as.data.frame(table(dt_cp_bl$hub_type)), by = c("Var2"="Var1")) 
dt_cp_k6 = dt_cp_bl %>% mutate(k6_bl = as.factor(k6_bl))
dt_tier_pt = table(dt_cp_k6$k6_bl, dt_cp_k6$hub_type) %>% as.data.frame() %>% left_join(.,as.data.frame(table(dt_cp_k6$hub_type)), by = c("Var2"="Var1"))


ggplot(aes(y = Freq.x*100/Freq.y, x = Var1, fill = Var1), data = dt_tier_pt)  + geom_bar(stat="identity", position = "dodge",width=1) + facet_wrap(~Var2, scales = "free") + geom_text(aes(label = paste(round(Freq.x*100/Freq.y,2), "%", sep = ""), group = Var1), position = position_dodge(width = 1),vjust = -0.8) + labs(fill = "K6 Score") + ylim(0,100) +labs(x = "K6 Score", y = "Propotion (%)") + theme_gray(base_size = 15) 

#ggplot(aes(y = Freq.x*100/Freq.y, x = Var1, fill = Var1), data = dt_tier_pt)  + geom_bar(stat="identity", position = "dodge",width=1) + facet_wrap(~Var2, scales = "free") + geom_text(aes(label = paste(round(Freq.x*100/Freq.y,2), "%", sep = ""), group = Var1), position = position_dodge(width = 1),vjust = -0.8) + labs(fill = "Tier") + ylim(0,100) +labs(x = "K6 Score", y = "Propotion (%)", fill = "Tier", color = "Tier") + theme_gray(base_size = 15)

#prop.test(c(123,239),c(205,422));prop.test(c(16,40),c(205,422));prop.test(c(36,86),c(205,422));prop.test(c(30,57),c(205,422))

dt_tier_pt = table(dt_cp_k6$k6_bl, dt_cp_k6$tier_rcode,dt_cp_k6$hub_type) %>% as.data.frame() %>% left_join(.,as.data.frame(table(dt_cp_k6$hub_type)), by = c("Var3"="Var1"))


ggplot(aes(x=k6_bl, fill = tier_rcode), data = dt_cp_bl) + geom_bar() + scale_x_continuous(breaks = c(1:35)) +labs(x = "K6 Score", fill = "K6 Range", y = "Count") + facet_grid(hub_type ~ tier_rcode, scales = "free_x", space = "free_x") + geom_text(aes(label =..count..),stat='count', vjust = -0.5, size = 3)+ theme_grey(base_size = 14)

## SF6D index
ggplot(dt_cp_bl, aes(x = sf6_z,  y = ..count..)) + 
  geom_histogram(fill = "steelblue", color = "black",bins = 100) +
  scale_x_continuous(n.breaks = 10) +
  labs(x = "SF6D Index Score", y = "Count") + 
  theme_grey(base_size = 14) + 
  facet_wrap(~hub_type, nrow = 2)
dt_cp_bl %>% group_by(hub_type) %>% skimr::skim()
t.test(sf6_z~hub_type, data = dt_cp_bl)

##QALY gain by tier
bl = dt_compare %>% group_by(subject_code) %>% slice(1) %>% rename(sf6_bl = sf6_z) %>% select(subject_code, sf6_bl, k6_bl)

### combine with baseline
dt_plot = dt_compare %>% select(-k6_bl)%>% group_by(subject_code) %>% arrange(subject_code, from_bl) %>% slice(n()) %>% left_join(bl, by = "subject_code") %>% filter(from_bl>0) %>% mutate(qaly = as.numeric((sf6_z - sf6_bl)*(from_bl/365))) %>% ungroup()

#### include parti with more than one visit

stat.test = dt_plot %>% group_by(tier) %>% t_test(qaly~1, mu=0) %>% adjust_pvalue() %>% mutate(y.position = 0.021)

ggplot(aes(x = tier, y = qaly, fill = tier), data = dt_plot) + stat_summary(fun=mean, geom="bar") + stat_summary(fun=mean, colour="black", geom="text", vjust = -0.5, aes(label=format(round(..y.., digits=5), scientific = F))) + labs(x = "Tier Status by K6", y = "Mean QALY gained", sep = "", fill = "Tier") + theme_bw(base_size = 14) +facet_wrap(~hub_type)

skimr::skim(dt_plot %>% group_by(tier, hub_type))
t.test(qaly~hub_type, data = subset(dt_plot, tier == "T0"));t.test(qaly~hub_type, data = subset(dt_plot, tier == "T1"));t.test(qaly~hub_type, data = subset(dt_plot, tier == "T2"));t.test(qaly~hub_type, data = subset(dt_plot, tier == "T3")) 

### compare with control ###
### nhub data
nhub_bl = joint_dt %>% group_by(subject_code) %>% slice(1) %>% rename(sf6_bl = sf6_z, k6_bl = k6_score) %>% mutate(tier = ls_replace(k6_bl, c(0:24), c(rep("T0", 9 ), rep("T1",2), rep("T2",4), rep("T3",10)))) %>% ungroup() %>% mutate(hub_type = "Control")

nhub_plot = joint_dt %>% group_by(subject_code) %>% arrange(subject_code, from_bl) %>% slice(n()) %>% select(subject_code, sf6_z, from_bl) %>% left_join(nhub_bl %>% select(-from_bl), by = "subject_code") %>% mutate(qaly = (sf6_z - sf6_bl)*(from_bl/365)) %>% ungroup() 

cb_plot = dt_cp_bl %>% select(tier, hub_type,gender,age,edu_yes_no,self_harm,suicidal_thought,firstlang,born_hk,job, suicidal_behaviour) %>% union_all(nhub_bl %>% select(tier, hub_type, gender,age,edu_yes_no,self_harm,suicidal_thought,firstlang,born_hk,job, suicidal_behaviour))

ggplot(aes(x = tier, y = qaly, fill = tier), data = cb_plot) + stat_summary(fun=mean, geom="bar") + stat_summary(fun=mean, colour="black", geom="text", vjust = -0.5, aes(label=format(round(..y.., digits=5), scientific = F))) + labs(x = "Tier Status by K6", y = "Mean QALY gained", sep = "", fill = "Tier") + theme_bw(base_size = 14) + facet_wrap(~hub_type)

test = nhub_plot %>% select(subject_code, sf6_z, sf6_bl, from_bl, qaly)
nhub_plot %>% select(-eq5d_tot:-suicidal_thought) %>% skimr::skim()

dt_plot %>% group_by(tier) %>% t_test(qaly~1, mu=0) %>% adjust_pvalue() %>% mutate(y.position = 0.017)

t.test(subset(cb_plot, tier == "T0" & hub_type == "Mega")$qaly, subset(cb_plot, tier == "T0"&hub_type =="Control")$qaly)
t.test(subset(cb_plot, tier == "T0" & hub_type == "Regular")$qaly, subset(cb_plot, tier == "T0"&hub_type =="Control")$qaly)
t.test(subset(cb_plot, tier == "T1" & hub_type == "Mega")$qaly, subset(cb_plot, tier == "T1"&hub_type =="Control")$qaly)
t.test(subset(cb_plot, tier == "T1" & hub_type == "Regular")$qaly, subset(cb_plot, tier == "T1"&hub_type =="Control")$qaly)
t.test(subset(cb_plot, tier == "T2" & hub_type == "Mega")$qaly, subset(cb_plot, tier == "T2"&hub_type =="Control")$qaly)
t.test(subset(cb_plot, tier == "T2" & hub_type == "Regular")$qaly, subset(cb_plot, tier == "T2"&hub_type =="Control")$qaly)
t.test(subset(cb_plot, tier == "T3" & hub_type == "Mega")$qaly, subset(cb_plot, tier == "T3"&hub_type =="Control")$qaly)
t.test(subset(cb_plot, tier == "T3" & hub_type == "Regular")$qaly, subset(cb_plot, tier == "T3"&hub_type =="Control")$qaly)
t.test(dt_plot$qaly, nhub_plot$qaly)$p.value

## productivity (hub)
dt_product = dt_compare %>% filter(from_bl == 0)
skimr::skim(dt_product)
### no product
ggplot(aes(x = tier, y = no_product, fill = tier), data = dt_product) + 
  stat_summary(fun=mean, geom="bar") + 
  stat_summary(fun=mean, colour="black", geom="text", vjust = -0.5, aes(label=format(round(..y.., digits=3), scientific = F))) + 
  labs(x = "Tier by K6", y = str_wrap("Days of no productivity in past one month due to emotional distress",40), fill = "Tier", color = "Tier") +   
  coord_cartesian(ylim=c(0,13)) + 
  theme_bw(base_size = 14) +
  facet_wrap(~hub_type)

t.test(no_product~hub_type, data = dt_product)

### half product
ggplot(aes(x = tier, y = half_product, fill = tier), data = dt_product) + 
  stat_summary(fun=mean, geom="bar") + 
  stat_summary(fun=mean, colour="black", geom="text", vjust = -0.5, aes(label=format(round(..y.., digits=3), scientific = F))) + 
  labs(x = "Tier by K6", y = str_wrap("Days of half or less productivity in past one month due to emotional distress",40), fill = "Tier", color = "Tier") + 
  coord_cartesian(ylim=c(0, 13)) + 
  theme_bw(base_size = 14) +
  facet_wrap(~hub_type)


### gender compare with control
#cb_plot = cb_plot %>% filter(!is.na(suicidal_thought))
dt_cb_gender = table(cb_plot %>% select(gender,hub_type)) %>% as.data.frame() %>%  left_join(.,as.data.frame(table(cb_plot$hub_type)), by = c("hub_type"="Var1")) %>% mutate(gender = if_else(gender==1,"Male","Female")) %>% mutate(prop = Freq.x/Freq.y)
cp <- coord_polar(theta = "y")
cp$is_free <- function() TRUE
ggplot(aes(y = Freq.x, x = "", fill = gender), data = dt_cb_gender)  + geom_bar(stat="identity", width=1, color="white") + cp + facet_wrap(~hub_type, scales = "free") + geom_text(aes(label = paste0(round(Freq.x*100/Freq.y, digits = 2), "% (",Freq.x,")")), size = 4.5, position = position_stack(vjust = 0.5)) + labs(fill = "Gender") + theme_void(base_size = 16)
rm(cp)

prop.test(c(dt_cb_gender[1,3],dt_cb_gender[3,3]),c(dt_cb_gender[1,4],dt_cb_gender[3,4]))
prop.test(c(dt_cb_gender[1,3],dt_cb_gender[5,3]),c(dt_cb_gender[1,4],dt_cb_gender[5,4]))
prop.test(c(dt_cb_gender[3,3],dt_cb_gender[5,3]),c(dt_cb_gender[3,4],dt_cb_gender[5,4]))
####age
ggplot(aes(x=age), data = cb_plot) + geom_bar(fill="steelblue") + scale_x_continuous(breaks = c(1:35)) +geom_text(aes(label =..count..),stat='count', vjust = -2, size = 3.5) +  geom_text(aes(label = scales::percent(..prop..), group = 1), stat= "count", vjust = -.5, size = 3.5) +labs(x = "Age", y ="Count") + facet_wrap(~hub_type, nrow = 3) + theme_grey(base_size = 14) + ylim(0, 65)
skimr::skim(cb_plot %>% group_by(hub_type))
aggregate(cb_plot$age, list(cb_plot$hub_type),mean)
t.test(subset(cb_plot, hub_type == "Regular hub")$age, subset(cb_plot, hub_type == "Specialised hub")$age)

#bimodal test of SF6
library(diptest)

modal.test = dip.test(subset(dt_2m_demo, gender == 1&from_bl==0)$sf6_z)

median(subset(dt_2m_demo, gender == 2)$sf6_z)
median(subset(dt_2m_demo, gender == 1)$sf6_z)

# Head counts for each tiers
count_plot = dt_cb %>% filter(project == "2M") %>% group_by(subject_code) %>% slice(1) %>% left_join(.,dt_2m %>% select(subject_code, submit_time))
ggplot(count_plot, aes(x=submit_time)) + 
  geom_area(stat = "bin", aes(fill = tier)) +
  scale_x_date(date_labels = "%b %Y",date_breaks = "1 month", limits = c(as.Date("2020-06-01"),as.Date("2021-08-01"))) + 
  labs(y = "New Youth", x = "Time") + 
  theme(legend.position = "none", 
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 10))  + 
  facet_wrap(~tier)


