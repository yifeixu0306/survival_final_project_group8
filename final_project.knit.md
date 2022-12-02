---
title: "P8108 Final Project"
output: pdf_document
date: "2022-11-28"
---





## 1 Import Dataset and Data Preprocess


```r
data(cancer, package="survival")

lung_df=lung

lung_df = lung_df %>%
  mutate(status = case_when(status == 1 ~ 0,
                            status == 2 ~ 1)) %>%
  mutate(sex = as.factor(sex),
         ph.ecog = as.factor(ph.ecog), 
         status = as.factor(status))
```


## 2 Exploratory Analysis

### 2.1 Summary table


```r
# Metadata Title
DATASET <- paste0("NCCTG Lung Cancer Dataset (from survival package ", 
                  packageVersion("survival"), ")")

# Save original options()
old <- options()  

# Global formatting options
options(digits = 3)

# Global ggplot settings
theme_set(theme_bw())

# Global table settings 
options(DT.options = list(pageLength = 10, 
                          language = list(search = 'Filter:'), 
                          scrollX = TRUE))

# Restore original options()
options(old)
```


```r
str(lung_df)
```

```
## 'data.frame':	228 obs. of  10 variables:
##  $ inst     : num  3 3 3 5 1 12 7 11 1 7 ...
##  $ time     : num  306 455 1010 210 883 ...
##  $ status   : Factor w/ 2 levels "0","1": 2 2 1 2 2 1 2 2 2 2 ...
##  $ age      : num  74 68 56 57 60 74 68 71 53 61 ...
##  $ sex      : Factor w/ 2 levels "1","2": 1 1 1 1 1 1 2 2 1 1 ...
##  $ ph.ecog  : Factor w/ 4 levels "0","1","2","3": 2 1 1 2 1 2 3 3 2 3 ...
##  $ ph.karno : num  90 90 90 90 100 50 70 60 70 70 ...
##  $ pat.karno: num  100 90 90 60 90 80 60 80 80 70 ...
##  $ meal.cal : num  1175 1225 NA 1150 NA ...
##  $ wt.loss  : num  NA 15 15 11 0 0 10 1 16 34 ...
```

```r
# Select variables of interest and change names to look nicer
lung_tab <- lung_df %>%  
  dplyr::select(age, sex, status, ph.ecog, ph.karno, pat.karno, meal.cal, wt.loss) %>%
  mutate(sex = case_when(sex == 1 ~ "Male",
                         sex == 2 ~ "Female")) %>%
  mutate(status = case_when(status == 0 ~ "Censored",
                            status == 1 ~ "Dead")) %>%
  mutate(sex = as.factor(sex),
         ph.ecog = as.factor(ph.ecog), 
         status = as.factor(status)) %>%
  rename("Age" = "age", 
         "Sex" = "sex", 
         "Censoring Status" = "status", 
         "ECOG Performance Score" = "ph.ecog", 
         "Karnofsky Performance Score Rated by Physician" = "ph.karno", 
         "Karnofsky Performance Score Rated by Patient" = "pat.karno", 
         "Calories Consumed at Meals" = "meal.cal", 
         "Weight Loss in Last Six Months"="wt.loss") 

# Create a table one
tab1 <- get_tableone(lung_tab)

# Render the tableone
render(tab1, title = "Overview over Lung Cancer patients", datasource = DATASET)
```

\setlength{\LTpost}{0mm}
\begin{longtable}{l|r}
\caption*{
{\large Overview over Lung Cancer patients}
} \\ 
\toprule
\multicolumn{1}{l}{} & Total (N=228) \\ 
\midrule
\multicolumn{2}{l}{Age} \\ 
\midrule
Mean (SD) & 62.4 (9.07) \\ 
Median (IQR) & 63 (56-69) \\ 
Min-max & 39-82 \\ 
Missing & 0 (0\%) \\ 
\midrule
\multicolumn{2}{l}{Sex} \\ 
\midrule
Female & 90 (39.5\%) \\ 
Male & 138 (60.5\%) \\ 
\midrule
\multicolumn{2}{l}{Censoring Status} \\ 
\midrule
Censored & 63 (27.6\%) \\ 
Dead & 165 (72.4\%) \\ 
\midrule
\multicolumn{2}{l}{ECOG Performance Score} \\ 
\midrule
0 & 63 (27.632\%) \\ 
1 & 113 (49.561\%) \\ 
2 & 50 (21.930\%) \\ 
3 & 1 (0.439\%) \\ 
Missing & 1 (0.439\%) \\ 
\midrule
\multicolumn{2}{l}{Karnofsky Performance Score Rated by Physician} \\ 
\midrule
Mean (SD) & 81.9 (12.3) \\ 
Median (IQR) & 80 (75-90) \\ 
Min-max & 50-100 \\ 
Missing & 1 (0.439\%) \\ 
\midrule
\multicolumn{2}{l}{Karnofsky Performance Score Rated by Patient} \\ 
\midrule
Mean (SD) & 80 (14.6) \\ 
Median (IQR) & 80 (70-90) \\ 
Min-max & 30-100 \\ 
Missing & 3 (1.32\%) \\ 
\midrule
\multicolumn{2}{l}{Calories Consumed at Meals} \\ 
\midrule
Mean (SD) & 929 (402) \\ 
Median (IQR) & 975 (635-1150) \\ 
Min-max & 96-2600 \\ 
Missing & 47 (20.6\%) \\ 
\midrule
\multicolumn{2}{l}{Weight Loss in Last Six Months} \\ 
\midrule
Mean (SD) & 9.83 (13.1) \\ 
Median (IQR) & 7 (0-15.8) \\ 
Min-max & -24-68 \\ 
Missing & 14 (6.14\%) \\ 
\bottomrule
\end{longtable}
\begin{minipage}{\linewidth}
Data Source: NCCTG Lung Cancer Dataset (from survival package 3.4.0)\\
\\
\end{minipage}

### 2.2 Distribution plots for each variable


```r
# Pie chart
## Gender-pie chart
lung_df_pie_1 = 
  lung_df %>% 
  group_by(sex) %>% 
  count() %>% 
  mutate(total_n = c(138 + 90)) %>% 
  mutate(perc = `n` / `total_n`) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc, 0.01)) %>%
  mutate(Sex = case_when(sex == 1 ~ "Male",
                         sex == 2 ~ "Female")) 
p_gender =
  ggplot(lung_df_pie_1, aes(x="", y=perc, fill=Sex)) + 
  geom_bar(width = 1, stat = "identity", color="white", alpha = 0.7) + 
  geom_label(aes(label = labels),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE,
             color = "white") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette="Set2") +
  theme_void() +
  guides(fill = guide_legend(title = "Sex")) 

## ECOG-pie chart
lung_df_pie_2 = lung_df %>% 
  group_by(ph.ecog) %>% 
  count() %>% 
  mutate(total_n = c(63 + 113 + 50 + 1 + 1)) %>% 
  mutate(perc = `n` / `total_n`) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc, 0.01)) %>%
  as.data.frame() %>%
  mutate(ph.ecog = as.character(ph.ecog)) 
         
lung_df_pie_2[is.na(lung_df_pie_2)] = "Missing"
p_ecog = 
  ggplot(lung_df_pie_2, aes(x="", y=perc, fill=ph.ecog)) + 
  geom_bar(width = 1, stat = "identity", color="white", alpha = 0.7) + 
  geom_label(aes(label = labels),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE,
             color = "white") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette="Set2") +
  theme_void() +
  guides(fill = guide_legend(title = "ECOG Score"))

## Status-pie chart
lung_df_pie_3 = lung_df %>%
  group_by(status) %>% 
  count() %>% 
  mutate(total_n = c(63 + 165)) %>% 
  mutate(perc = `n` / `total_n`) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc, 0.01)) %>%
  mutate(Status = case_when(status == 1 ~ "Dead",
                         status == 0 ~ "Censored")) %>%
  as.data.frame() %>%
  mutate(status = as.character(status)) 
  
p_status = 
  ggplot(lung_df_pie_3, aes(x="", y=perc, fill=Status)) + 
  geom_bar(width = 1, stat = "identity", color="white", alpha = 0.7) + 
  geom_label(aes(label = labels),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE,
             color = "white") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette="Set2") +
  theme_void() +
  guides(fill = guide_legend(title = "Status"))

# Histogram
## Time
p_time = 
  lung_df %>%
  ggplot(aes(x = time)) + 
  geom_histogram(aes(y=after_stat(density)), color="darkblue", fill="lightblue") +
  geom_density() +
  labs(x="Survival Time (Days)",
       y = "Density")

## Age
p_age = 
  lung_df %>%
  mutate(Sex = case_when(sex == 1 ~ "Male",
                         sex == 2 ~ "Female")) %>%
  ggplot(aes(x = age, fill=Sex, color = Sex)) + 
  geom_histogram(position = "identity", alpha = 0.5) +
  labs(x = "Age",
       y = "Count")

## ph.karno & pat.karno
df_karno = lung_df %>%
  rename("Patient" = pat.karno,
        "Physician" = ph.karno) %>%
  pivot_longer(
    Patient:Physician,
    names_to = "Rating",
    values_to = "Karnofsky"
  )

p_karno =
  df_karno %>%
  ggplot(aes(x=Karnofsky, fill = Rating, color = Rating)) + 
  geom_histogram(binwidth = 10, alpha=0.5, position="identity") + 
  labs(x = "Karnofsky Score",
       y = "Count")

## Weight loss
p_weight =
  lung_df %>% ggplot(aes(x = wt.loss)) + 
  geom_histogram(aes(y=after_stat(density)), color="darkblue", fill="lightblue") +
  geom_density() +
    labs(x = "Weight Loss (Lbs)",
       y = "Density")
## Meal
p_meal =
  lung_df %>%
  ggplot(aes(x = meal.cal)) + 
  geom_histogram(aes(y=after_stat(density)), color="darkblue", fill="lightblue") +
  geom_density() +
  labs(x = "Calories Consumed",
       y = "Density")
```


```r
# Combine
p_gender  + p_status + p_ecog
```

![](final_project_files/figure-latex/unnamed-chunk-6-1.pdf)<!-- --> 

```r
p_time + p_weight + p_meal
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](final_project_files/figure-latex/unnamed-chunk-6-2.pdf)<!-- --> 

```r
p_age + p_karno
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](final_project_files/figure-latex/unnamed-chunk-6-3.pdf)<!-- --> 

### 2.3 Correlation plot between variables


```r
# Drop missing values
lung_df_1 = lung_df %>%
  drop_na()

# Plot the correlation
corr = data.frame(lapply(lapply(lung_df_1, as.factor), as.numeric))
corrplot(cor(corr), type = "lower")
```

![](final_project_files/figure-latex/unnamed-chunk-7-1.pdf)<!-- --> 

### 2.4 Hazard rate summary table


```r
explanatory = c("age", "sex", "ph.ecog", "ph.karno", "pat.karno", "meal.cal", "wt.loss")
dependent = "Surv(time, status)"

lung %>%
  mutate(sex = case_when(sex == 2 ~ "Female",
                         sex == 1 ~ "Male")) %>%
  mutate(ph.ecog = ifelse(ph.ecog == 3, 2, ph.ecog)) %>%
  mutate(sex=as.factor(sex), 
         ph.ecog=as.factor(ph.ecog)) %>%
  hr_plot(dependent, 
          explanatory, 
          dependent_label = "Survival",
          remove_ref = FALSE,
          table_text_size=4, title_text_size=14,
          plot_opts=list(xlab("HR, 95% CI"), 
                         theme(axis.title = element_text(size=12)),
                         geom_point(aes(size = Total), shape=22, fill="black"),
                         geom_errorbarh(height=0.2)))
```

```
## Dependent variable is a survival object
```

![](final_project_files/figure-latex/unnamed-chunk-8-1.pdf)<!-- --> 


## 3 Non-parametric Estimation

### 3.1 Life-table


```r
life_table = biostat3::lifetab2(Surv(time, status==1) ~ 1, lung_df, breaks = seq(0, 1200, 100))

life_table %>%
  knitr::kable(caption = "Life-table Estimate",
               digits = 4)
```



Table: Life-table Estimate

|          | tstart| tstop| nsubs| nlost| nrisk| nevent|   surv|    pdf| hazard| se.surv| se.pdf| se.hazard|
|:---------|------:|-----:|-----:|-----:|-----:|------:|------:|------:|------:|-------:|------:|---------:|
|0-100     |      0|   100|   228|     1| 227.5|     31| 1.0000| 0.0014| 0.0015|  0.0000|  2e-04|    0.0003|
|100-200   |    100|   200|   196|    11| 190.5|     41| 0.8637| 0.0019| 0.0024|  0.0227|  3e-04|    0.0004|
|200-300   |    200|   300|   144|    23| 132.5|     29| 0.6778| 0.0015| 0.0025|  0.0313|  3e-04|    0.0005|
|300-400   |    300|   400|    92|    10|  87.0|     25| 0.5295| 0.0015| 0.0034|  0.0345|  3e-04|    0.0007|
|400-500   |    400|   500|    57|     4|  55.0|     12| 0.3773| 0.0008| 0.0024|  0.0356|  2e-04|    0.0007|
|500-600   |    500|   600|    41|     7|  37.5|     10| 0.2950| 0.0008| 0.0031|  0.0349|  2e-04|    0.0010|
|600-700   |    600|   700|    24|     0|  24.0|      8| 0.2163| 0.0007| 0.0040|  0.0333|  2e-04|    0.0014|
|700-800   |    700|   800|    16|     1|  15.5|      7| 0.1442| 0.0007| 0.0058|  0.0304|  2e-04|    0.0021|
|800-900   |    800|   900|     8|     3|   6.5|      2| 0.0791| 0.0002| 0.0036|  0.0247|  2e-04|    0.0025|
|900-1000  |    900|  1000|     3|     1|   2.5|      0| 0.0548| 0.0000| 0.0000|  0.0223|    NaN|       NaN|
|1000-1100 |   1000|  1100|     2|     2|   1.0|      0| 0.0548| 0.0000| 0.0000|  0.0223|    NaN|       NaN|
|1100-1200 |   1100|  1200|     0|     0|   0.0|      0| 0.0548|    NaN|    NaN|  0.0223|    NaN|       NaN|
|1200-Inf  |   1200|   Inf|     0|     0|   0.0|      0|    NaN|     NA|     NA|     NaN|     NA|        NA|

### 3.2 Kaplan-Meier method


```r
# Find the number of days a person was alive before they died.
km_dead <- with(lung_df, Surv(time, status))

km_dead <- survfit(Surv(time, status==1) ~ 1, data=lung_df)

# K-M plot
km_dead %>% autoplot() + labs(title="Kaplan-Meier Survival Cure with Confidence Interval", 
                              x="Time (Days)", 
                              y="Survival Probability S(t)")
```

![](final_project_files/figure-latex/unnamed-chunk-10-1.pdf)<!-- --> 

```r
# Median survival time
summary(km_dead)
```

```
## Call: survfit(formula = Surv(time, status == 1) ~ 1, data = lung_df)
## 
##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
##     5    228       1   0.9956 0.00438       0.9871        1.000
##    11    227       3   0.9825 0.00869       0.9656        1.000
##    12    224       1   0.9781 0.00970       0.9592        0.997
##    13    223       2   0.9693 0.01142       0.9472        0.992
##    15    221       1   0.9649 0.01219       0.9413        0.989
##    26    220       1   0.9605 0.01290       0.9356        0.986
##    30    219       1   0.9561 0.01356       0.9299        0.983
##    31    218       1   0.9518 0.01419       0.9243        0.980
##    53    217       2   0.9430 0.01536       0.9134        0.974
##    54    215       1   0.9386 0.01590       0.9079        0.970
##    59    214       1   0.9342 0.01642       0.9026        0.967
##    60    213       2   0.9254 0.01740       0.8920        0.960
##    61    211       1   0.9211 0.01786       0.8867        0.957
##    62    210       1   0.9167 0.01830       0.8815        0.953
##    65    209       2   0.9079 0.01915       0.8711        0.946
##    71    207       1   0.9035 0.01955       0.8660        0.943
##    79    206       1   0.8991 0.01995       0.8609        0.939
##    81    205       2   0.8904 0.02069       0.8507        0.932
##    88    203       2   0.8816 0.02140       0.8406        0.925
##    92    201       1   0.8772 0.02174       0.8356        0.921
##    93    199       1   0.8728 0.02207       0.8306        0.917
##    95    198       2   0.8640 0.02271       0.8206        0.910
##   105    196       1   0.8596 0.02302       0.8156        0.906
##   107    194       2   0.8507 0.02362       0.8056        0.898
##   110    192       1   0.8463 0.02391       0.8007        0.894
##   116    191       1   0.8418 0.02419       0.7957        0.891
##   118    190       1   0.8374 0.02446       0.7908        0.887
##   122    189       1   0.8330 0.02473       0.7859        0.883
##   131    188       1   0.8285 0.02500       0.7810        0.879
##   132    187       2   0.8197 0.02550       0.7712        0.871
##   135    185       1   0.8153 0.02575       0.7663        0.867
##   142    184       1   0.8108 0.02598       0.7615        0.863
##   144    183       1   0.8064 0.02622       0.7566        0.859
##   145    182       2   0.7975 0.02667       0.7469        0.852
##   147    180       1   0.7931 0.02688       0.7421        0.848
##   153    179       1   0.7887 0.02710       0.7373        0.844
##   156    178       2   0.7798 0.02751       0.7277        0.836
##   163    176       3   0.7665 0.02809       0.7134        0.824
##   166    173       2   0.7577 0.02845       0.7039        0.816
##   167    171       1   0.7532 0.02863       0.6991        0.811
##   170    170       1   0.7488 0.02880       0.6944        0.807
##   175    167       1   0.7443 0.02898       0.6896        0.803
##   176    165       1   0.7398 0.02915       0.6848        0.799
##   177    164       1   0.7353 0.02932       0.6800        0.795
##   179    162       2   0.7262 0.02965       0.6704        0.787
##   180    160       1   0.7217 0.02981       0.6655        0.783
##   181    159       2   0.7126 0.03012       0.6559        0.774
##   182    157       1   0.7081 0.03027       0.6511        0.770
##   183    156       1   0.7035 0.03041       0.6464        0.766
##   186    154       1   0.6989 0.03056       0.6416        0.761
##   189    152       1   0.6943 0.03070       0.6367        0.757
##   194    149       1   0.6897 0.03085       0.6318        0.753
##   197    147       1   0.6850 0.03099       0.6269        0.749
##   199    145       1   0.6803 0.03113       0.6219        0.744
##   201    144       2   0.6708 0.03141       0.6120        0.735
##   202    142       1   0.6661 0.03154       0.6071        0.731
##   207    139       1   0.6613 0.03168       0.6020        0.726
##   208    138       1   0.6565 0.03181       0.5970        0.722
##   210    137       1   0.6517 0.03194       0.5920        0.717
##   212    135       1   0.6469 0.03206       0.5870        0.713
##   218    134       1   0.6421 0.03218       0.5820        0.708
##   222    132       1   0.6372 0.03231       0.5769        0.704
##   223    130       1   0.6323 0.03243       0.5718        0.699
##   226    126       1   0.6273 0.03256       0.5666        0.694
##   229    125       1   0.6223 0.03268       0.5614        0.690
##   230    124       1   0.6172 0.03280       0.5562        0.685
##   239    121       2   0.6070 0.03304       0.5456        0.675
##   245    117       1   0.6019 0.03316       0.5402        0.670
##   246    116       1   0.5967 0.03328       0.5349        0.666
##   267    112       1   0.5913 0.03341       0.5294        0.661
##   268    111       1   0.5860 0.03353       0.5239        0.656
##   269    110       1   0.5807 0.03364       0.5184        0.651
##   270    108       1   0.5753 0.03376       0.5128        0.645
##   283    104       1   0.5698 0.03388       0.5071        0.640
##   284    103       1   0.5642 0.03400       0.5014        0.635
##   285    101       2   0.5531 0.03424       0.4899        0.624
##   286     99       1   0.5475 0.03434       0.4841        0.619
##   288     98       1   0.5419 0.03444       0.4784        0.614
##   291     97       1   0.5363 0.03454       0.4727        0.608
##   293     94       1   0.5306 0.03464       0.4669        0.603
##   301     91       1   0.5248 0.03475       0.4609        0.597
##   303     89       1   0.5189 0.03485       0.4549        0.592
##   305     87       1   0.5129 0.03496       0.4488        0.586
##   306     86       1   0.5070 0.03506       0.4427        0.581
##   310     85       2   0.4950 0.03523       0.4306        0.569
##   320     82       1   0.4890 0.03532       0.4244        0.563
##   329     81       1   0.4830 0.03539       0.4183        0.558
##   337     79       1   0.4768 0.03547       0.4121        0.552
##   340     78       1   0.4707 0.03554       0.4060        0.546
##   345     77       1   0.4646 0.03560       0.3998        0.540
##   348     76       1   0.4585 0.03565       0.3937        0.534
##   350     75       1   0.4524 0.03569       0.3876        0.528
##   351     74       1   0.4463 0.03573       0.3815        0.522
##   353     73       2   0.4340 0.03578       0.3693        0.510
##   361     70       1   0.4278 0.03581       0.3631        0.504
##   363     69       2   0.4154 0.03583       0.3508        0.492
##   364     67       1   0.4092 0.03582       0.3447        0.486
##   371     65       2   0.3966 0.03581       0.3323        0.473
##   387     60       1   0.3900 0.03582       0.3258        0.467
##   390     59       1   0.3834 0.03582       0.3193        0.460
##   394     58       1   0.3768 0.03580       0.3128        0.454
##   426     55       1   0.3700 0.03580       0.3060        0.447
##   428     54       1   0.3631 0.03579       0.2993        0.440
##   429     53       1   0.3563 0.03576       0.2926        0.434
##   433     52       1   0.3494 0.03573       0.2860        0.427
##   442     51       1   0.3426 0.03568       0.2793        0.420
##   444     50       1   0.3357 0.03561       0.2727        0.413
##   450     48       1   0.3287 0.03555       0.2659        0.406
##   455     47       1   0.3217 0.03548       0.2592        0.399
##   457     46       1   0.3147 0.03539       0.2525        0.392
##   460     44       1   0.3076 0.03530       0.2456        0.385
##   473     43       1   0.3004 0.03520       0.2388        0.378
##   477     42       1   0.2933 0.03508       0.2320        0.371
##   519     39       1   0.2857 0.03498       0.2248        0.363
##   520     38       1   0.2782 0.03485       0.2177        0.356
##   524     37       2   0.2632 0.03455       0.2035        0.340
##   533     34       1   0.2554 0.03439       0.1962        0.333
##   550     32       1   0.2475 0.03423       0.1887        0.325
##   558     30       1   0.2392 0.03407       0.1810        0.316
##   567     28       1   0.2307 0.03391       0.1729        0.308
##   574     27       1   0.2221 0.03371       0.1650        0.299
##   583     26       1   0.2136 0.03348       0.1571        0.290
##   613     24       1   0.2047 0.03325       0.1489        0.281
##   624     23       1   0.1958 0.03297       0.1407        0.272
##   641     22       1   0.1869 0.03265       0.1327        0.263
##   643     21       1   0.1780 0.03229       0.1247        0.254
##   654     20       1   0.1691 0.03188       0.1169        0.245
##   655     19       1   0.1602 0.03142       0.1091        0.235
##   687     18       1   0.1513 0.03090       0.1014        0.226
##   689     17       1   0.1424 0.03034       0.0938        0.216
##   705     16       1   0.1335 0.02972       0.0863        0.207
##   707     15       1   0.1246 0.02904       0.0789        0.197
##   728     14       1   0.1157 0.02830       0.0716        0.187
##   731     13       1   0.1068 0.02749       0.0645        0.177
##   735     12       1   0.0979 0.02660       0.0575        0.167
##   765     10       1   0.0881 0.02568       0.0498        0.156
##   791      9       1   0.0783 0.02462       0.0423        0.145
##   814      7       1   0.0671 0.02351       0.0338        0.133
##   883      4       1   0.0503 0.02285       0.0207        0.123
```

```r
summary(km_dead, times = c(300,305,310,315))
```

```
## Call: survfit(formula = Surv(time, status == 1) ~ 1, data = lung_df)
## 
##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
##   300     92     101    0.531  0.0346        0.467        0.603
##   305     87       3    0.513  0.0350        0.449        0.586
##   310     85       3    0.495  0.0352        0.431        0.569
##   315     83       0    0.495  0.0352        0.431        0.569
```
The median survival time is 310 days.


```r
# cumulative density with a confidence interval
cuminc(Surv(time, status) ~ 1, data = lung_df) %>% 
  ggcuminc() + 
  labs(
    x = "Time (Days)"
  ) + 
  add_confidence_interval() +
  add_risktable()
```

```
## Plotting outcome "1".
```

![](final_project_files/figure-latex/unnamed-chunk-11-1.pdf)<!-- --> 
As the number of survival days increase, the probability of a person dying decreases.

### 3.3 Nelson-Aalen method 

### 3.3.1 Overall Nelson-Aalen Estimator  


```r
nelsonaalen = function(data, timevar, statusvar) {
  if (!is.data.frame(data)) {
    stop("Data must be a data frame")
  }
  timevar <- as.character(substitute(timevar))
  statusvar <- as.character(substitute(statusvar))
  time <- data[, timevar, drop = TRUE]
  status <- data[, statusvar, drop = TRUE]

  hazard <- survival::basehaz(survival::coxph(survival::Surv(time, status==1) ~ 1))
  idx <- match(time, hazard[, "time"])
  hazard[idx, "hazard"]
}

hazard = nelsonaalen(lung_df, time, status)
# plot(x = lung_df$time, main="Cumulative Probability for Event of Interest (Death)", y = hazard, ylab = "Cumulative Probability of subject's death", xlab = "Number of Days") 


lung_df_2 = lung_df %>% 
  mutate(hazard = nelsonaalen(lung_df, time, status))

ggplot(data = lung_df_2, aes(x = time, y = hazard)) + 
  geom_point(alpha = .3) +
  geom_line(colour = "blue") + 
  theme(legend.position = "bottom") +
  labs(
    title = "Cumulative Hazard Rate for Event of Interest (Death)",
    x = "Time (Days)",
    y = "Cumulative Hazard Rate of Subject's Death"
  )
```

![](final_project_files/figure-latex/unnamed-chunk-12-1.pdf)<!-- --> 

### 3.3.2 Fleming-Harrington Estimator  

Once the H(t) of Nelson-Aalen hazard estimator is obtained, Fleming-Harrington estimator for survival function can be obtained.

```r
survfit(Surv(time, status==1) ~ 1, lung_df, stype = 1, ctype = 2)
```

```
## Call: survfit(formula = Surv(time, status == 1) ~ 1, data = lung_df, 
##     stype = 1, ctype = 2)
## 
##        n events median 0.95LCL 0.95UCL
## [1,] 228    165    310     285     363
```
Fleming-Harrington estimator for median survival is 310, which is equal to K-M estimator.


## 4 Hypothesis testing 

In this section, we will make comparison of the different groups of the attributes to identify the prognostic factors using the Kaplan-Meier Curve and conduct log-rank test.

### 4.1 Univariate

We plan to combine ecog=3 with ecog=2 since there was only one subject with ecog=3


```r
lung_df_e = lung_df %>%
  mutate(ph.ecog.n = case_when(ph.ecog == 0 ~ 0,
                               ph.ecog == 1 ~ 1,
                               ph.ecog == 2 ~ 2,
                               ph.ecog == 3 ~ 2)) %>%
  mutate(ph.ecog.n = as.factor(ph.ecog.n))
```


### 4.1.1 Sex


```r
splots <- list()

# fit
lung_sex = lung_df %>%
  drop_na(sex) %>%
  mutate(Sex = case_when(sex == 1 ~ "Male",
                         sex == 2 ~ "Female")) 

fit_sex = survfit(Surv(time, status==1) ~ Sex, data = lung_sex) 
summary(fit_sex)$table
```

```
##            records n.max n.start events    rmean se(rmean) median 0.95LCL
## Sex=Female      90    90      90     53 460.6473  34.68985    426     348
## Sex=Male       138   138     138    112 326.0841  22.91156    270     212
##            0.95UCL
## Sex=Female     550
## Sex=Male       310
```

```r
# visualization
splots[[1]] = ggsurvplot(fit_sex,
                         pval = TRUE, conf.int = TRUE,
                         surv.median.line = "hv", # Specify median survival
                         ggtheme = theme_bw(), # Change ggplot2 theme
                         legend = "bottom", 
                         legend.title = "Sex",
                         legend.lab = c("Female", "Male"))

# log-rank test
diff_sex = survdiff(Surv(time, status==1) ~ Sex, data = lung_sex) 
diff_sex
```

```
## Call:
## survdiff(formula = Surv(time, status == 1) ~ Sex, data = lung_sex)
## 
##              N Observed Expected (O-E)^2/E (O-E)^2/V
## Sex=Female  90       53     73.4      5.68      10.3
## Sex=Male   138      112     91.6      4.55      10.3
## 
##  Chisq= 10.3  on 1 degrees of freedom, p= 0.001
```

### 4.1.2 Age


```r
# fit
lung_age = lung_df %>%
  drop_na(age) %>%
  mutate(Age = case_when(age > 70 ~ "> 70",
                         age <= 70 ~ "<= 70")) 

fit_age = survfit(Surv(time, status==1) ~ Age, data = lung_age)
summary(fit_age)$table
```

```
##           records n.max n.start events    rmean se(rmean) median 0.95LCL
## Age=<= 70     182   182     182    127 397.2120  22.40235    345     291
## Age=> 70       46    46      46     38 301.4505  39.31644    283     201
##           0.95UCL
## Age=<= 70     429
## Age=> 70      353
```

```r
# visualization
splots[[2]] = ggsurvplot(fit_age,
                         pval = TRUE, conf.int = TRUE,
                         surv.median.line = "hv", 
                         ggtheme = theme_bw(), 
                         legend = "bottom", 
                         legend.title = "Age",
                         legend.lab = c("<= 70", "> 70"))

# log-rank test
diff_age = survdiff(Surv(time, status==1) ~ Age, data = lung_age) 
diff_age
```

```
## Call:
## survdiff(formula = Surv(time, status == 1) ~ Age, data = lung_age)
## 
##             N Observed Expected (O-E)^2/E (O-E)^2/V
## Age=<= 70 182      127    137.3     0.773      4.64
## Age=> 70   46       38     27.7     3.829      4.64
## 
##  Chisq= 4.6  on 1 degrees of freedom, p= 0.03
```

### 4.1.3 ECOG performance score


```r
# fit
lung_ecog = lung_df_e %>%
  drop_na(ph.ecog) %>%
  mutate(ph.ecog = as.factor(ph.ecog)) 

fit_ecog = survfit(Surv(time, status==1) ~ ph.ecog.n, data = lung_ecog)
summary(fit_ecog)$table
```

```
##             records n.max n.start events    rmean se(rmean) median 0.95LCL
## ph.ecog.n=0      63    63      63     37 465.3131  42.88835    394     348
## ph.ecog.n=1     113   113     113     82 384.8410  27.28943    306     268
## ph.ecog.n=2      51    51      51     45 255.7636  30.74032    183     153
##             0.95UCL
## ph.ecog.n=0     574
## ph.ecog.n=1     429
## ph.ecog.n=2     288
```

```r
# visualization
splots[[3]] = ggsurvplot(fit_ecog,
                         pval = TRUE, conf.int = TRUE,
                         surv.median.line = "hv", 
                         ggtheme = theme_bw(), 
                         legend = "bottom", 
                         legend.title = "ECOG Performance Score",
                         legend.lab = c("0", "1", "2"))

# log-rank test
diff_ecog = survdiff(Surv(time, status==1) ~ ph.ecog, data = lung_ecog) 
diff_ecog
```

```
## Call:
## survdiff(formula = Surv(time, status == 1) ~ ph.ecog, data = lung_ecog)
## 
##             N Observed Expected (O-E)^2/E (O-E)^2/V
## ph.ecog=0  63       37   54.153    5.4331    8.2119
## ph.ecog=1 113       82   83.528    0.0279    0.0573
## ph.ecog=2  50       44   26.147   12.1893   14.6491
## ph.ecog=3   1        1    0.172    3.9733    4.0040
## 
##  Chisq= 22  on 3 degrees of freedom, p= 7e-05
```

### 4.1.4 Karnofsky performance score rated by physician


```r
# fit
lung_phkarno = lung_df %>%
  drop_na(ph.karno) %>%
  mutate(ph.karno = case_when(ph.karno > 80 ~ "> 80",
                         ph.karno <= 80 ~ "<= 80")) 

fit_phkarno = survfit(Surv(time, status==1) ~ ph.karno, data=lung_phkarno)
summary(fit_phkarno)$table
```

```
##                records n.max n.start events    rmean se(rmean) median 0.95LCL
## ph.karno=<= 80     124   124     124     97 329.3139  24.55953    239     202
## ph.karno=> 80      103   103     103     67 431.8672  30.51428    394     340
##                0.95UCL
## ph.karno=<= 80     305
## ph.karno=> 80      473
```

```r
# visualization
splots[[4]] = ggsurvplot(fit_phkarno,
                         pval = TRUE, conf.int = TRUE,
                         surv.median.line = "hv", 
                         ggtheme = theme_bw(), 
                         legend = "bottom", 
                         legend.title = "Karnofsky Score by Physician",
                         legend.lab = c("<= 80", "> 80"))

# log-rank test
diff_phkarno = survdiff(Surv(time, status==1) ~ ph.karno, data = lung_phkarno) 
diff_phkarno
```

```
## Call:
## survdiff(formula = Surv(time, status == 1) ~ ph.karno, data = lung_phkarno)
## 
##                  N Observed Expected (O-E)^2/E (O-E)^2/V
## ph.karno=<= 80 124       97     79.2      4.01      7.84
## ph.karno=> 80  103       67     84.8      3.74      7.84
## 
##  Chisq= 7.8  on 1 degrees of freedom, p= 0.005
```

### 4.1.5 Karnofsky performance score as rated by patient


```r
# fit
lung_patkarno = lung_df %>%
  drop_na(pat.karno) %>%
  mutate(pat.karno = case_when(pat.karno > 80 ~ "> 80",
                         pat.karno <= 80 ~ "<= 80")) 

fit_patkarno = survfit(Surv(time, status==1) ~ pat.karno, data=lung_patkarno)
summary(fit_patkarno)$table
```

```
##                 records n.max n.start events    rmean se(rmean) median 0.95LCL
## pat.karno=<= 80     130   130     130    103 336.1857  24.34974    239     202
## pat.karno=> 80       95    95      95     59 440.3736  32.69968    371     320
##                 0.95UCL
## pat.karno=<= 80     348
## pat.karno=> 80      477
```

```r
# visualization
splots[[5]] = ggsurvplot(fit_patkarno,
                         pval = TRUE, conf.int = TRUE,
                         surv.median.line = "hv", 
                         ggtheme = theme_bw(), 
                         legend = "bottom", 
                         legend.title = "Karnofsky Score by Patient",
                         legend.lab = c("<= 80", "> 80"))

# log-rank test
diff_patkarno = survdiff(Surv(time, status==1) ~ pat.karno, data = lung_patkarno) 
diff_patkarno
```

```
## Call:
## survdiff(formula = Surv(time, status == 1) ~ pat.karno, data = lung_patkarno)
## 
##                   N Observed Expected (O-E)^2/E (O-E)^2/V
## pat.karno=<= 80 130      103     85.5      3.60      7.69
## pat.karno=> 80   95       59     76.5      4.02      7.69
## 
##  Chisq= 7.7  on 1 degrees of freedom, p= 0.006
```

### 4.1.6 Calories consumed at meals


```r
# fit
avg_meal_cal = mean(as.numeric(lung_df$meal.cal), na.rm = TRUE)

lung_meal_cal = lung_df %>%
  drop_na(meal.cal) %>%
  mutate(meal.cal = case_when(meal.cal > avg_meal_cal ~ "> average",
                         meal.cal <= avg_meal_cal ~ "<= average")) 

fit_meal_cal = survfit(Surv(time, status==1) ~ meal.cal, data = lung_meal_cal)
summary(fit_meal_cal)$table
```

```
##                     records n.max n.start events    rmean se(rmean) median
## meal.cal=<= average      82    82      82     60 353.7694  37.04280    285
## meal.cal=> average       99    99      99     74 383.4524  27.52114    348
##                     0.95LCL 0.95UCL
## meal.cal=<= average     212     351
## meal.cal=> average      285     450
```

```r
splots[[6]] = ggsurvplot(fit_meal_cal, 
                         pval = TRUE, conf.int = TRUE,
                         surv.median.line = "hv", 
                         ggtheme = theme_bw(), 
                         legend = "bottom",
                         legend.title = "Calories Consumed at Meals",
                         legend.lab = c("<= average", "> average"))

# log-rank test
diff_meal_cal = survdiff(Surv(time, status==1) ~ meal.cal, data = lung_meal_cal) 
diff_meal_cal
```

```
## Call:
## survdiff(formula = Surv(time, status == 1) ~ meal.cal, data = lung_meal_cal)
## 
##                      N Observed Expected (O-E)^2/E (O-E)^2/V
## meal.cal=<= average 82       60     55.2     0.419     0.723
## meal.cal=> average  99       74     78.8     0.294     0.723
## 
##  Chisq= 0.7  on 1 degrees of freedom, p= 0.4
```

### 4.1.7 Weight loss


```r
# fit
avg_wt_loss = mean(as.numeric(lung_df$wt.loss), na.rm = TRUE)

lung_wt_loss = lung_df %>%
  drop_na(wt.loss) %>%
  mutate(wt.loss = case_when(wt.loss > avg_wt_loss ~ "> average",
                         wt.loss <= avg_wt_loss ~ "<= average")) 

fit_wt_loss <- survfit(Surv(time, status==1) ~ wt.loss, data = lung_wt_loss)
summary(fit_wt_loss)$table
```

```
##                    records n.max n.start events    rmean se(rmean) median
## wt.loss=<= average     121   121     121     79 419.7671  28.90820    364
## wt.loss=> average       93    93      93     73 350.5749  28.34981    288
##                    0.95LCL 0.95UCL
## wt.loss=<= average     320     450
## wt.loss=> average      223     351
```

```r
# visualization
splots[[7]] = ggsurvplot(fit_wt_loss, 
                         pval = TRUE, conf.int = TRUE,
                         surv.median.line = "hv", 
                         ggtheme = theme_bw(), 
                         legend = "bottom",
                         legend.title = "Weight loss (Lbs)",
                         legend.lab = c("<= average", "> average"))

# log-rank test
diff_wt_loss = survdiff(Surv(time, status==1) ~ wt.loss, data = lung_wt_loss) 
diff_wt_loss
```

```
## Call:
## survdiff(formula = Surv(time, status == 1) ~ wt.loss, data = lung_wt_loss)
## 
##                      N Observed Expected (O-E)^2/E (O-E)^2/V
## wt.loss=<= average 121       79     89.7      1.27      3.12
## wt.loss=> average   93       73     62.3      1.82      3.12
## 
##  Chisq= 3.1  on 1 degrees of freedom, p= 0.08
```


```r
# combine the plots
arrange_ggsurvplots(splots, 
                    print = TRUE,
                    title = "Comparison of the Different Attribute Groups Using the Kaplan-Meier Curve",
                    ncol = 2, 
                    nrow = 4)
```

![](final_project_files/figure-latex/unnamed-chunk-22-1.pdf)<!-- --> 

Age, sex, ph.ecog, ph.karno, pat.karno can be identified as prognostic factors.

### 4.2 Multivariate

## 4.2.1 sex + ph.ecog


```r
# fit
lung_sex_ecog = lung_df_e %>%
  drop_na(sex, ph.ecog.n) %>%
  mutate(ph.ecog = as.factor(ph.ecog)) %>%
  mutate(Sex = case_when(sex == 1 ~ "Male",
                         sex == 2 ~ "Female")) 

fit_sex_ecog = survfit(Surv(time, status==1) ~ Sex + ph.ecog.n, data = lung_sex_ecog)
summary(fit_sex_ecog)$table
```

```
##                         records n.max n.start events    rmean se(rmean) median
## Sex=Female, ph.ecog.n=0      27    27      27      9 586.9179  95.23307  705.0
## Sex=Female, ph.ecog.n=1      42    42      42     28 475.3713  45.14073  450.0
## Sex=Female, ph.ecog.n=2      21    21      21     16 300.9587  44.61934  239.0
## Sex=Male, ph.ecog.n=0        36    36      36     28 415.5025  48.59025  353.0
## Sex=Male, ph.ecog.n=1        71    71      71     54 327.6528  31.77508  239.0
## Sex=Male, ph.ecog.n=2        30    30      30     29 222.4667  38.93727  164.5
##                         0.95LCL 0.95UCL
## Sex=Female, ph.ecog.n=0     350      NA
## Sex=Female, ph.ecog.n=1     345     687
## Sex=Female, ph.ecog.n=2     199     444
## Sex=Male, ph.ecog.n=0       303     558
## Sex=Male, ph.ecog.n=1       207     363
## Sex=Male, ph.ecog.n=2       105     288
```

```r
# subset
#fit_sex_ecog_1 = survfit(Surv(time, status==1) ~ Sex + ph.ecog, data = subset(lung_sex_ecog, ph.ecog %in% c(0,1)))
#summary(fit_sex_ecog_1)$table

# visualization
ggsurvplot(fit_sex_ecog,
           pval = TRUE, conf.int = FALSE,
           surv.median.line = "hv", 
           ggtheme = theme_bw(), 
           legend = "bottom")
```

![](final_project_files/figure-latex/unnamed-chunk-23-1.pdf)<!-- --> 

```r
# ggsurvplot(fit_sex_ecog_1,
#          pval = TRUE, conf.int = FALSE,
#          surv.median.line = "hv", 
#          ggtheme = theme_bw(), 
#          legend = "bottom")

# log-rank test
diff_sex_ecog = survdiff(Surv(time, status==1) ~ Sex + ph.ecog.n, data = lung_sex_ecog) 
diff_sex_ecog
```

```
## Call:
## survdiff(formula = Surv(time, status == 1) ~ Sex + ph.ecog.n, 
##     data = lung_sex_ecog)
## 
##                          N Observed Expected (O-E)^2/E (O-E)^2/V
## Sex=Female, ph.ecog.n=0 27        9     21.1     6.940     8.020
## Sex=Female, ph.ecog.n=1 42       28     40.2     3.707     4.999
## Sex=Female, ph.ecog.n=2 21       16     11.7     1.553     1.693
## Sex=Male, ph.ecog.n=0   36       28     33.1     0.772     0.986
## Sex=Male, ph.ecog.n=1   71       54     43.3     2.634     3.636
## Sex=Male, ph.ecog.n=2   30       29     14.6    14.236    15.740
## 
##  Chisq= 30.3  on 5 degrees of freedom, p= 1e-05
```
We find that female with ecog = 1 may have a higher survival probability than male with ecog = 0.

And then we explore more by SAS.


![Survival Function between sex and ecog](plot/sex+ecog.jpg)


## 4.2.2 age + ph.ecog


```r
# fit
lung_age_ecog = lung_df_e %>%
  drop_na(age, ph.ecog.n) %>%
  mutate(Age = case_when(age > 70 ~ "> 70",
                         age <= 70 ~ "<= 70")) 

fit_age_ecog = survfit(Surv(time, status==1) ~ Age + ph.ecog.n, data = lung_age_ecog)
summary(fit_age_ecog)$table
```

```
##                        records n.max n.start events    rmean se(rmean) median
## Age=<= 70, ph.ecog.n=0      53    53      53     28 506.6747  49.90976    433
## Age=<= 70, ph.ecog.n=1      97    97      97     70 384.1550  29.10006    345
## Age=<= 70, ph.ecog.n=2      31    31      31     28 279.1563  39.24119    208
## Age=> 70, ph.ecog.n=0       10    10      10      9 298.1000  61.39223    303
## Age=> 70, ph.ecog.n=1       16    16      16     12 390.8731  73.72300    306
## Age=> 70, ph.ecog.n=2       20    20      20     17 257.0818  68.02154    163
##                        0.95LCL 0.95UCL
## Age=<= 70, ph.ecog.n=0     348     705
## Age=<= 70, ph.ecog.n=1     230     460
## Age=<= 70, ph.ecog.n=2     156     329
## Age=> 70, ph.ecog.n=0      176      NA
## Age=> 70, ph.ecog.n=1      270      NA
## Age=> 70, ph.ecog.n=2       93     361
```

```r
# visualization
ggsurvplot(fit_age_ecog,
           pval = TRUE, conf.int = FALSE,
           surv.median.line = "hv", 
           ggtheme = theme_bw(), 
           legend = "bottom")
```

![](final_project_files/figure-latex/unnamed-chunk-24-1.pdf)<!-- --> 

```r
# log-rank test
diff_age_ecog = survdiff(Surv(time, status==1) ~ Age + ph.ecog.n, data = lung_age_ecog) 
diff_age_ecog
```

```
## Call:
## survdiff(formula = Surv(time, status == 1) ~ Age + ph.ecog.n, 
##     data = lung_age_ecog)
## 
##                         N Observed Expected (O-E)^2/E (O-E)^2/V
## Age=<= 70, ph.ecog.n=0 53       28    47.66   8.10776   11.6127
## Age=<= 70, ph.ecog.n=1 97       70    70.69   0.00683    0.0121
## Age=<= 70, ph.ecog.n=2 31       28    18.11   5.40377    6.1178
## Age=> 70, ph.ecog.n=0  10        9     6.50   0.96530    1.0129
## Age=> 70, ph.ecog.n=1  16       12    12.83   0.05403    0.0592
## Age=> 70, ph.ecog.n=2  20       17     8.21   9.40529   10.0304
## 
##  Chisq= 24.4  on 5 degrees of freedom, p= 2e-04
```


### 4.2.3 ph.karno + pat.karno


```r
# fit
lung_ph_pat = lung_df %>%
  drop_na(ph.karno, pat.karno) %>%
  mutate(ph.karno = case_when(ph.karno > 80 ~ "  > 80",
                         ph.karno <= 80 ~ "  <= 80")) %>%
   mutate(pat.karno = case_when(pat.karno > 80 ~ "  > 80",
                         ph.karno <= 80 ~ "  <= 80")) 

fit_ph_pat = survfit(Surv(time, status==1) ~ ph.karno + pat.karno, data=lung_ph_pat)
summary(fit_ph_pat)$table
```

```
##                                     records n.max n.start events    rmean
## ph.karno=  <= 80, pat.karno=  <= 80      87    87      87     72 323.1235
## ph.karno=  <= 80, pat.karno=  > 80       35    35      35     23 366.4192
## ph.karno=  > 80, pat.karno=  <= 80       42    42      42     30 368.5197
## ph.karno=  > 80, pat.karno=  > 80        60    60      60     36 484.2746
##                                     se(rmean) median 0.95LCL 0.95UCL
## ph.karno=  <= 80, pat.karno=  <= 80  27.36178    229     194     351
## ph.karno=  <= 80, pat.karno=  > 80   60.00332    283     230     524
## ph.karno=  > 80, pat.karno=  <= 80   49.36250    303     210     429
## ph.karno=  > 80, pat.karno=  > 80    38.18366    455     353     613
```

```r
# visualization
ggsurvplot(fit_ph_pat,
           pval = TRUE, conf.int = FALSE,
           surv.median.line = "hv", 
           ggtheme = theme_bw(), 
           legend = "bottom")
```

![](final_project_files/figure-latex/unnamed-chunk-25-1.pdf)<!-- --> 

```r
# log-rank test
diff_ph_pat = survdiff(Surv(time, status==1) ~ ph.karno + pat.karno, data = lung_ph_pat) 
diff_ph_pat
```

```
## Call:
## survdiff(formula = Surv(time, status == 1) ~ ph.karno + pat.karno, 
##     data = lung_ph_pat)
## 
##                                      N Observed Expected (O-E)^2/E (O-E)^2/V
## ph.karno=  <= 80, pat.karno=  <= 80 87       72     56.7     4.116     6.410
## ph.karno=  <= 80, pat.karno=  > 80  35       23     20.6     0.275     0.318
## ph.karno=  > 80, pat.karno=  <= 80  42       30     28.1     0.127     0.156
## ph.karno=  > 80, pat.karno=  > 80   60       36     55.6     6.883    10.636
## 
##  Chisq= 11.5  on 3 degrees of freedom, p= 0.009
```

We find that ph.karno $>$ 80 and pat.karno $\le$ 80 may have a higher survival probability than ph.karno $\le$ 80 and pat.karno $>$, which means that physicians may predict better.

And then we explore more by SAS.

![Survival Function between Karnofsky score rated by physician and patient](plot/karno.jpg)


## 5 Model Fitting (Cox Model)

### 5.1 Full model


```r
lung_mod = lung_df_e %>%
  drop_na()

# full model
cox_mod_1 = coxph(Surv(time, status==1) ~ sex + age + ph.ecog.n + ph.karno + pat.karno + meal.cal + wt.loss, data = lung_mod)
summary(cox_mod_1)
```

```
## Call:
## coxph(formula = Surv(time, status == 1) ~ sex + age + ph.ecog.n + 
##     ph.karno + pat.karno + meal.cal + wt.loss, data = lung_mod)
## 
##   n= 167, number of events= 120 
## 
##                  coef  exp(coef)   se(coef)      z Pr(>|z|)   
## sex2       -5.574e-01  5.727e-01  2.021e-01 -2.759  0.00580 **
## age         1.013e-02  1.010e+00  1.170e-02  0.866  0.38645   
## ph.ecog.n1  6.387e-01  1.894e+00  2.818e-01  2.266  0.02343 * 
## ph.ecog.n2  1.466e+00  4.332e+00  4.603e-01  3.184  0.00145 **
## ph.karno    2.140e-02  1.022e+00  1.125e-02  1.902  0.05717 . 
## pat.karno  -1.097e-02  9.891e-01  8.543e-03 -1.284  0.19901   
## meal.cal    3.704e-05  1.000e+00  2.586e-04  0.143  0.88608   
## wt.loss    -1.415e-02  9.859e-01  7.797e-03 -1.815  0.06947 . 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##            exp(coef) exp(-coef) lower .95 upper .95
## sex2          0.5727     1.7461    0.3854     0.851
## age           1.0102     0.9899    0.9873     1.034
## ph.ecog.n1    1.8940     0.5280    1.0902     3.291
## ph.ecog.n2    4.3316     0.2309    1.7572    10.678
## ph.karno      1.0216     0.9788    0.9993     1.044
## pat.karno     0.9891     1.0110    0.9727     1.006
## meal.cal      1.0000     1.0000    0.9995     1.001
## wt.loss       0.9859     1.0143    0.9710     1.001
## 
## Concordance= 0.653  (se = 0.029 )
## Likelihood ratio test= 27.51  on 8 df,   p=6e-04
## Wald test            = 27.12  on 8 df,   p=7e-04
## Score (logrank) test = 28.2  on 8 df,   p=4e-04
```

### 5.2 Select variables using different methods

### 5.2.1 Remove insignificant varibale


```r
cox_mod_2 = coxph(Surv(time, status==1) ~ sex + ph.ecog.n, data = lung_mod)
summary(cox_mod_2)
```

```
## Call:
## coxph(formula = Surv(time, status == 1) ~ sex + ph.ecog.n, data = lung_mod)
## 
##   n= 167, number of events= 120 
## 
##               coef exp(coef) se(coef)      z Pr(>|z|)    
## sex2       -0.5076    0.6019   0.1970 -2.576 0.009983 ** 
## ph.ecog.n1  0.3204    1.3776   0.2331  1.374 0.169289    
## ph.ecog.n2  0.9368    2.5518   0.2592  3.614 0.000301 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##            exp(coef) exp(-coef) lower .95 upper .95
## sex2          0.6019     1.6614    0.4091    0.8856
## ph.ecog.n1    1.3776     0.7259    0.8724    2.1753
## ph.ecog.n2    2.5518     0.3919    1.5354    4.2410
## 
## Concordance= 0.646  (se = 0.03 )
## Likelihood ratio test= 19.58  on 3 df,   p=2e-04
## Wald test            = 20.21  on 3 df,   p=2e-04
## Score (logrank) test = 20.97  on 3 df,   p=1e-04
```

### 5.2.2 Stepwise method 


```r
step(cox_mod_1, direction = "both")
```

```
## Start:  AIC=1004.72
## Surv(time, status == 1) ~ sex + age + ph.ecog.n + ph.karno + 
##     pat.karno + meal.cal + wt.loss
## 
##             Df    AIC
## - meal.cal   1 1002.7
## - age        1 1003.5
## - pat.karno  1 1004.4
## <none>         1004.7
## - wt.loss    1 1006.2
## - ph.karno   1 1006.5
## - sex        1 1010.7
## - ph.ecog.n  2 1011.1
## 
## Step:  AIC=1002.74
## Surv(time, status == 1) ~ sex + age + ph.ecog.n + ph.karno + 
##     pat.karno + wt.loss
## 
##             Df    AIC
## - age        1 1001.5
## - pat.karno  1 1002.4
## <none>         1002.7
## - wt.loss    1 1004.2
## - ph.karno   1 1004.5
## + meal.cal   1 1004.7
## - sex        1 1009.0
## - ph.ecog.n  2 1009.1
## 
## Step:  AIC=1001.48
## Surv(time, status == 1) ~ sex + ph.ecog.n + ph.karno + pat.karno + 
##     wt.loss
## 
##             Df    AIC
## - pat.karno  1 1001.1
## <none>         1001.5
## - ph.karno   1 1002.7
## + age        1 1002.7
## - wt.loss    1 1003.2
## + meal.cal   1 1003.5
## - ph.ecog.n  2 1007.8
## - sex        1 1007.9
## 
## Step:  AIC=1001.13
## Surv(time, status == 1) ~ sex + ph.ecog.n + ph.karno + wt.loss
## 
##             Df    AIC
## <none>         1001.1
## + pat.karno  1 1001.5
## - ph.karno   1 1001.9
## - wt.loss    1 1002.0
## + age        1 1002.4
## + meal.cal   1 1003.1
## - sex        1 1007.9
## - ph.ecog.n  2 1012.6
```

```
## Call:
## coxph(formula = Surv(time, status == 1) ~ sex + ph.ecog.n + ph.karno + 
##     wt.loss, data = lung_mod)
## 
##                 coef exp(coef)  se(coef)      z        p
## sex2       -0.573005  0.563829  0.199353 -2.874 0.004049
## ph.ecog.n1  0.641883  1.900055  0.280399  2.289 0.022069
## ph.ecog.n2  1.678042  5.355060  0.440036  3.813 0.000137
## ph.karno    0.017983  1.018146  0.011091  1.621 0.104927
## wt.loss    -0.012693  0.987387  0.007648 -1.660 0.096955
## 
## Likelihood ratio test=25.1  on 5 df, p=0.0001333
## n= 167, number of events= 120
```

```r
cox_mod_3 = coxph(Surv(time, status==1) ~ sex + ph.ecog.n + ph.karno + wt.loss, data = lung_mod)
summary(cox_mod_3)
```

```
## Call:
## coxph(formula = Surv(time, status == 1) ~ sex + ph.ecog.n + ph.karno + 
##     wt.loss, data = lung_mod)
## 
##   n= 167, number of events= 120 
## 
##                 coef exp(coef)  se(coef)      z Pr(>|z|)    
## sex2       -0.573005  0.563829  0.199353 -2.874 0.004049 ** 
## ph.ecog.n1  0.641883  1.900055  0.280399  2.289 0.022069 *  
## ph.ecog.n2  1.678042  5.355060  0.440036  3.813 0.000137 ***
## ph.karno    0.017983  1.018146  0.011091  1.621 0.104927    
## wt.loss    -0.012693  0.987387  0.007648 -1.660 0.096955 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##            exp(coef) exp(-coef) lower .95 upper .95
## sex2          0.5638     1.7736    0.3815    0.8334
## ph.ecog.n1    1.9001     0.5263    1.0967    3.2919
## ph.ecog.n2    5.3551     0.1867    2.2605   12.6860
## ph.karno      1.0181     0.9822    0.9963    1.0405
## wt.loss       0.9874     1.0128    0.9727    1.0023
## 
## Concordance= 0.643  (se = 0.031 )
## Likelihood ratio test= 25.1  on 5 df,   p=1e-04
## Wald test            = 25.05  on 5 df,   p=1e-04
## Score (logrank) test = 26.04  on 5 df,   p=9e-05
```

### 5.2.3 Add interaction term


```r
cox_mod_4 = coxph(Surv(time, status==1) ~ sex + ph.ecog.n + ph.karno + wt.loss + ph.karno*ph.ecog, 
                  data = lung_mod)
summary(cox_mod_4)
```

```
## Call:
## coxph(formula = Surv(time, status == 1) ~ sex + ph.ecog.n + ph.karno + 
##     wt.loss + ph.karno * ph.ecog, data = lung_mod)
## 
##   n= 167, number of events= 120 
## 
##                        coef exp(coef)  se(coef)      z Pr(>|z|)   
## sex2              -0.565567  0.568038  0.200532 -2.820   0.0048 **
## ph.ecog.n1        -0.576415  0.561909  3.861093 -0.149   0.8813   
## ph.ecog.n2         2.514900 12.365368  1.714401  1.467   0.1424   
## ph.karno           0.008061  1.008093  0.038598  0.209   0.8346   
## wt.loss           -0.013063  0.987022  0.007734 -1.689   0.0912 . 
## ph.ecog1                 NA        NA  0.000000     NA       NA   
## ph.ecog2          -1.617303  0.198433  2.912736 -0.555   0.5787   
## ph.ecog3                 NA        NA  0.000000     NA       NA   
## ph.karno:ph.ecog1  0.013208  1.013296  0.040979  0.322   0.7472   
## ph.karno:ph.ecog2  0.007151  1.007177  0.044380  0.161   0.8720   
## ph.karno:ph.ecog3        NA        NA  0.000000     NA       NA   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##                   exp(coef) exp(-coef) lower .95 upper .95
## sex2                 0.5680    1.76045 0.3834283    0.8415
## ph.ecog.n1           0.5619    1.77965 0.0002905 1087.0120
## ph.ecog.n2          12.3654    0.08087 0.4294434  356.0477
## ph.karno             1.0081    0.99197 0.9346438    1.0873
## wt.loss              0.9870    1.01315 0.9721725    1.0021
## ph.ecog1                 NA         NA        NA        NA
## ph.ecog2             0.1984    5.03948 0.0006581   59.8328
## ph.ecog3                 NA         NA        NA        NA
## ph.karno:ph.ecog1    1.0133    0.98688 0.9350926    1.0980
## ph.karno:ph.ecog2    1.0072    0.99287 0.9232716    1.0987
## ph.karno:ph.ecog3        NA         NA        NA        NA
## 
## Concordance= 0.646  (se = 0.031 )
## Likelihood ratio test= 26.22  on 8 df,   p=0.001
## Wald test            = 26.98  on 8 df,   p=7e-04
## Score (logrank) test = 29.06  on 8 df,   p=3e-04
```

### 5.3 Model Comparison


```r
# AIC
aic_1 = AIC(cox_mod_1)
aic_2 = AIC(cox_mod_2)
aic_3 = AIC(cox_mod_3)
aic_4 = AIC(cox_mod_4)

AIC = c(aic_1, aic_2, aic_3, aic_4)

# BIC
bic_1 = BIC(cox_mod_1)
bic_2 = BIC(cox_mod_2)
bic_3 = BIC(cox_mod_3)
bic_4 = BIC(cox_mod_4)

BIC = c(bic_1, bic_2, bic_3, bic_4)

mod_comp = data.frame("AIC" = AIC,
                      "BIC" = BIC)
rownames(mod_comp) = c("Model 1", "Model 2", "Model 3", "Model 4")

mod_comp
```

```
##              AIC      BIC
## Model 1 1004.719 1027.019
## Model 2 1002.654 1011.016
## Model 3 1001.133 1015.071
## Model 4 1006.017 1028.317
```

```r
anova(cox_mod_1, cox_mod_2, cox_mod_3, cox_mod_4)
```

```
## Analysis of Deviance Table
##  Cox model: response is  Surv(time, status == 1)
##  Model 1: ~ sex + age + ph.ecog.n + ph.karno + pat.karno + meal.cal + wt.loss
##  Model 2: ~ sex + ph.ecog.n
##  Model 3: ~ sex + ph.ecog.n + ph.karno + wt.loss
##  Model 4: ~ sex + ph.ecog.n + ph.karno + wt.loss + ph.karno * ph.ecog
##    loglik  Chisq Df Pr(>|Chi|)  
## 1 -494.36                       
## 2 -498.33 7.9352  5    0.15984  
## 3 -495.57 5.5207  2    0.06327 .
## 4 -495.01 1.1166  3    0.77307  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## 6 Model Diagnostics  

The Cox proportional hazards model makes two assumptions: \
(1) survival curves for different strata must have hazard functions that are proportional over the time t \
(2) the relationship between the log hazard and each covariate is linear, which can be verified with residual plots.

### 6.1 Schoenfeld Residuals


```r
test.ph = cox.zph(cox_mod_3)
test.ph
```

```
##            chisq df     p
## sex       1.6604  1 0.198
## ph.ecog.n 4.2677  2 0.118
## ph.karno  6.1752  1 0.013
## wt.loss   0.0958  1 0.757
## GLOBAL    8.4916  5 0.131
```

The proportional hazard assumption is supported by a non-significant relationship between residuals and time, and refuted by a significant relationship. Here, with a significance level of 0.05, the test is statistically significant for `ph.karno`. The global test is not statistically significant, so our proportional hazards assumption is reasonable. 



```r
ggcoxzph(test.ph)
```

![](final_project_files/figure-latex/unnamed-chunk-32-1.pdf)<!-- --> 

### 6.2 Graphical Approach - log-log curves

### 6.2.1 Sex


```r
gplots <- list()

test_sex <- survfit(Surv(time, status==1) ~ sex, data = lung_mod)

gplots[[1]] = ggsurvplot(test_sex, 
           fun = "cloglog",
           xlim = c(5, 1200),
           ggtheme = theme_bw(), 
           legend = "bottom", 
           legend.title = "Sex",
           legend.lab = c("Male", "Female"))
```

### 6.2.2 ECOG performance score


```r
data_ecog = lung_mod 

test_ecog <- survfit(Surv(time, status==1) ~ ph.ecog.n, data = data_ecog)

gplots[[2]]= ggsurvplot(test_ecog, 
           fun = "cloglog",
           xlim = c(5, 1200),
           ggtheme = theme_bw(), 
           legend = "bottom", 
           legend.title = "ECOG Performance Score",
           legend.lab = c("0", "1", "2"))
```


```r
# combine the plots
arrange_ggsurvplots(gplots, 
                    print = TRUE,
                    title = "Log of Negative Log of Estimated Survival Functions",
                    ncol = 2, 
                    nrow = 1)
```

![](final_project_files/figure-latex/unnamed-chunk-35-1.pdf)<!-- --> 

### 6.3 Test interaction for proportionality


```r
cox_mod_5 = coxph(Surv(time, status == 1) ~ sex + ph.ecog.n + ph.karno + wt.loss +
                  sex*log(time) + ph.ecog.n*log(time) + ph.karno*log(time)  + wt.loss*log(time) 
                  - log(time), 
                  lung_mod) 
```

### 6.4 Correction for violation of PH assumption

### 6.4.1 Stratified PH model

```r
cox_mod_5 = coxph(Surv(time, status==1) ~ (sex + wt.loss): strata(ph.ecog.n) + ph.karno, lung_mod) 
summary(cox_mod_5)
```

```
## Call:
## coxph(formula = Surv(time, status == 1) ~ (sex + wt.loss):strata(ph.ecog.n) + 
##     ph.karno, data = lung_mod)
## 
##   n= 167, number of events= 120 
## 
##                                  coef  exp(coef)   se(coef)      z Pr(>|z|)    
## ph.karno                    0.0207983  1.0210161  0.0119474  1.741 0.081715 .  
## sex1:strata(ph.ecog.n)0     1.0436802  2.8396484  0.4985491  2.093 0.036310 *  
## sex2:strata(ph.ecog.n)0            NA         NA  0.0000000     NA       NA    
## sex1:strata(ph.ecog.n)1     0.4530327  1.5730756  0.2773132  1.634 0.102332    
## sex2:strata(ph.ecog.n)1            NA         NA  0.0000000     NA       NA    
## sex1:strata(ph.ecog.n)2     1.4302644  4.1798043  0.4633990  3.086 0.002026 ** 
## sex2:strata(ph.ecog.n)2            NA         NA  0.0000000     NA       NA    
## wt.loss:strata(ph.ecog.n)0  0.0489525  1.0501705  0.0211402  2.316 0.020579 *  
## wt.loss:strata(ph.ecog.n)1 -0.0007336  0.9992667  0.0085458 -0.086 0.931589    
## wt.loss:strata(ph.ecog.n)2 -0.0667492  0.9354298  0.0172058 -3.879 0.000105 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##                            exp(coef) exp(-coef) lower .95 upper .95
## ph.karno                      1.0210     0.9794    0.9974    1.0452
## sex1:strata(ph.ecog.n)0       2.8396     0.3522    1.0688    7.5445
## sex2:strata(ph.ecog.n)0           NA         NA        NA        NA
## sex1:strata(ph.ecog.n)1       1.5731     0.6357    0.9135    2.7089
## sex2:strata(ph.ecog.n)1           NA         NA        NA        NA
## sex1:strata(ph.ecog.n)2       4.1798     0.2392    1.6854   10.3658
## sex2:strata(ph.ecog.n)2           NA         NA        NA        NA
## wt.loss:strata(ph.ecog.n)0    1.0502     0.9522    1.0075    1.0946
## wt.loss:strata(ph.ecog.n)1    0.9993     1.0007    0.9827    1.0161
## wt.loss:strata(ph.ecog.n)2    0.9354     1.0690    0.9044    0.9675
## 
## Concordance= 0.613  (se = 0.033 )
## Likelihood ratio test= 32.68  on 7 df,   p=3e-05
## Wald test            = 28.91  on 7 df,   p=2e-04
## Score (logrank) test = 30.78  on 7 df,   p=7e-05
```

```r
AIC(cox_mod_5)
```

```
## [1] 744.2391
```

```r
BIC(cox_mod_5)
```

```
## [1] 763.7516
```

```r
# check residuals
cox.zph(cox_mod_5)
```

```
##                           chisq df    p
## ph.karno                   1.49  1 0.22
## sex:strata(ph.ecog.n)      4.05  3 0.26
## wt.loss:strata(ph.ecog.n)  6.19  3 0.10
## GLOBAL                     9.36  7 0.23
```

### 6.4.2 Time-varying covariates


```r
# explore ph.karno
cox_karno = coxph(Surv(time, status==1) ~ ph.karno, data = lung_mod)
zph = cox.zph(cox_karno)

plot(zph[1],lwd=2) 
abline(0, 0, col=1,lty=3,lwd=2)
abline(h= cox_karno$coef[1], col=3, lwd=2, lty=2) 
legend("bottomright",
       legend=c("Reference line for null effect", "Average hazard over time", "Time-varying hazard"),
       lty=c(3,2,1), 
       col=c(1,3,1), lwd=2)
```

![](final_project_files/figure-latex/unnamed-chunk-38-1.pdf)<!-- --> 

```r
# split the time period based on the plot
lung_split = survSplit(Surv(time, status) ~ ., data= lung, cut=c(180, 350), episode= "tgroup", id="id")

cox_mod_6 = coxph(Surv(tstart, time, status) ~ sex + ph.ecog + ph.karno:strata(tgroup) + wt.loss, data=lung_split)
summary(cox_mod_6)
```

```
## Call:
## coxph(formula = Surv(tstart, time, status) ~ sex + ph.ecog + 
##     ph.karno:strata(tgroup) + wt.loss, data = lung_split)
## 
##   n= 440, number of events= 151 
##    (21 observations deleted due to missingness)
## 
##                                      coef exp(coef)  se(coef)      z Pr(>|z|)
## sex                             -0.632406  0.531312  0.176582 -3.581 0.000342
## ph.ecog                          0.708542  2.031029  0.196019  3.615 0.000301
## wt.loss                         -0.008293  0.991741  0.006494 -1.277 0.201599
## ph.karno:strata(tgroup)tgroup=1 -0.007260  0.992767  0.013641 -0.532 0.594599
## ph.karno:strata(tgroup)tgroup=2  0.014597  1.014705  0.014090  1.036 0.300192
## ph.karno:strata(tgroup)tgroup=3  0.026358  1.026708  0.012689  2.077 0.037776
##                                    
## sex                             ***
## ph.ecog                         ***
## wt.loss                            
## ph.karno:strata(tgroup)tgroup=1    
## ph.karno:strata(tgroup)tgroup=2    
## ph.karno:strata(tgroup)tgroup=3 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##                                 exp(coef) exp(-coef) lower .95 upper .95
## sex                                0.5313     1.8821    0.3759     0.751
## ph.ecog                            2.0310     0.4924    1.3831     2.982
## wt.loss                            0.9917     1.0083    0.9792     1.004
## ph.karno:strata(tgroup)tgroup=1    0.9928     1.0073    0.9666     1.020
## ph.karno:strata(tgroup)tgroup=2    1.0147     0.9855    0.9871     1.043
## ph.karno:strata(tgroup)tgroup=3    1.0267     0.9740    1.0015     1.053
## 
## Concordance= 0.647  (se = 0.027 )
## Likelihood ratio test= 36.18  on 6 df,   p=3e-06
## Wald test            = 35.42  on 6 df,   p=4e-06
## Score (logrank) test = 36.32  on 6 df,   p=2e-06
```

```r
AIC(cox_mod_6)
```

```
## [1] 1325.87
```

```r
BIC(cox_mod_6)
```

```
## [1] 1343.973
```

```r
# check residuals
cox.zph(cox_mod_6)
```

```
##                          chisq df    p
## sex                     2.0434  1 0.15
## ph.ecog                 0.2980  1 0.59
## wt.loss                 0.0386  1 0.84
## ph.karno:strata(tgroup) 3.2441  3 0.36
## GLOBAL                  5.6731  6 0.46
```

```r
# The result shows now that there is no correlation between transformed survival time and the scaled Schoenfeld residuals, indicating that the proportional hazards assumption is not violated with the stratified analysis, and judging by the global p-value, the model is fit.
```

Consider AIC and BIC, we prefer model 5:  
Surv(time, status) ~ (sex + wt.loss): strata(ph.ecog.n) + ph.karno.
Also, stratified model is more flexible and powerful.


## 7 Parametric Model

### 7.1 Model Fitting

```r
# exponential
fit_exp = survreg(Surv(time, status==1) ~ sex + age + ph.ecog.n + ph.karno + pat.karno + meal.cal + wt.loss,
                  data = lung_mod, dist = "exp")
summary(fit_exp)
```

```
## 
## Call:
## survreg(formula = Surv(time, status == 1) ~ sex + age + ph.ecog.n + 
##     ph.karno + pat.karno + meal.cal + wt.loss, data = lung_mod, 
##     dist = "exp")
##                 Value Std. Error     z      p
## (Intercept)  7.55e+00   1.55e+00  4.88  1e-06
## sex2         5.16e-01   2.03e-01  2.54 0.0109
## age         -7.95e-03   1.14e-02 -0.70 0.4843
## ph.ecog.n1  -5.00e-01   2.79e-01 -1.79 0.0736
## ph.ecog.n2  -1.23e+00   4.56e-01 -2.69 0.0071
## ph.karno    -1.65e-02   1.14e-02 -1.45 0.1481
## pat.karno    7.55e-03   8.14e-03  0.93 0.3536
## meal.cal     4.79e-06   2.48e-04  0.02 0.9846
## wt.loss      9.98e-03   7.42e-03  1.35 0.1785
## 
## Scale fixed at 1 
## 
## Exponential distribution
## Loglik(model)= -837.2   Loglik(intercept only)= -848
## 	Chisq= 21.63 on 8 degrees of freedom, p= 0.0057 
## Number of Newton-Raphson Iterations: 4 
## n= 167
```

```r
# weibull
fit_weibull = survreg(Surv(time, status==1) ~ sex + age + ph.ecog.n + ph.karno + pat.karno + meal.cal + wt.loss,
                      data = lung_mod, dist = "weibull")
summary(fit_weibull)
```

```
## 
## Call:
## survreg(formula = Surv(time, status == 1) ~ sex + age + ph.ecog.n + 
##     ph.karno + pat.karno + meal.cal + wt.loss, data = lung_mod, 
##     dist = "weibull")
##                 Value Std. Error     z       p
## (Intercept)  7.36e+00   1.07e+00  6.86 6.7e-12
## sex2         3.97e-01   1.43e-01  2.78 0.00548
## age         -5.89e-03   8.07e-03 -0.73 0.46529
## ph.ecog.n1  -4.37e-01   1.96e-01 -2.23 0.02552
## ph.ecog.n2  -1.06e+00   3.18e-01 -3.33 0.00086
## ph.karno    -1.58e-02   7.64e-03 -2.06 0.03919
## pat.karno    7.31e-03   5.93e-03  1.23 0.21752
## meal.cal    -1.71e-05   1.79e-04 -0.10 0.92378
## wt.loss      9.43e-03   5.39e-03  1.75 0.08021
## Log(scale)  -3.55e-01   7.24e-02 -4.90 9.6e-07
## 
## Scale= 0.702 
## 
## Weibull distribution
## Loglik(model)= -827.1   Loglik(intercept only)= -841.1
## 	Chisq= 27.9 on 8 degrees of freedom, p= 0.00049 
## Number of Newton-Raphson Iterations: 5 
## n= 167
```

```r
# log-logistics
fit_llogis = survreg(Surv(time, status==1) ~ sex + age + ph.ecog.n + ph.karno + pat.karno + meal.cal + wt.loss,
                     data = lung_mod, dist = "loglogistic")
summary(fit_llogis)
```

```
## 
## Call:
## survreg(formula = Surv(time, status == 1) ~ sex + age + ph.ecog.n + 
##     ph.karno + pat.karno + meal.cal + wt.loss, data = lung_mod, 
##     dist = "loglogistic")
##                 Value Std. Error     z       p
## (Intercept)  5.786266   1.379040  4.20 2.7e-05
## sex2         0.504379   0.162349  3.11  0.0019
## age         -0.002170   0.008917 -0.24  0.8077
## ph.ecog.n1  -0.340770   0.225044 -1.51  0.1300
## ph.ecog.n2  -0.943484   0.404198 -2.33  0.0196
## ph.karno    -0.005662   0.011388 -0.50  0.6191
## pat.karno    0.006411   0.006220  1.03  0.3027
## meal.cal     0.000188   0.000196  0.96  0.3366
## wt.loss      0.006379   0.005533  1.15  0.2490
## Log(scale)  -0.651987   0.077160 -8.45 < 2e-16
## 
## Scale= 0.521 
## 
## Log logistic distribution
## Loglik(model)= -831.7   Loglik(intercept only)= -847
## 	Chisq= 30.61 on 8 degrees of freedom, p= 0.00017 
## Number of Newton-Raphson Iterations: 4 
## n= 167
```

```r
# log-normal
fit_lnorm = survreg(Surv(time, status==1) ~ sex + age + ph.ecog.n + ph.karno + pat.karno + meal.cal + wt.loss,
                    data = lung_mod, dist = "lognormal")
summary(fit_lnorm)
```

```
## 
## Call:
## survreg(formula = Surv(time, status == 1) ~ sex + age + ph.ecog.n + 
##     ph.karno + pat.karno + meal.cal + wt.loss, data = lung_mod, 
##     dist = "lognormal")
##                 Value Std. Error     z       p
## (Intercept)  6.903492   1.469556  4.70 2.6e-06
## sex2         0.491426   0.181341  2.71  0.0067
## age         -0.011962   0.010129 -1.18  0.2376
## ph.ecog.n1  -0.300961   0.252437 -1.19  0.2332
## ph.ecog.n2  -1.047816   0.425026 -2.47  0.0137
## ph.karno    -0.012873   0.011694 -1.10  0.2710
## pat.karno    0.006390   0.006855  0.93  0.3513
## meal.cal     0.000242   0.000219  1.11  0.2680
## wt.loss      0.007127   0.006297  1.13  0.2577
## Log(scale)  -0.002009   0.065457 -0.03  0.9755
## 
## Scale= 0.998 
## 
## Log Normal distribution
## Loglik(model)= -838.4   Loglik(intercept only)= -853.4
## 	Chisq= 29.92 on 8 degrees of freedom, p= 0.00022 
## Number of Newton-Raphson Iterations: 4 
## n= 167
```

### 7.2 Model Comparison

```r
AIC(fit_exp, fit_weibull, fit_llogis, fit_lnorm)
```

```
##             df      AIC
## fit_exp      9 1692.421
## fit_weibull 10 1674.235
## fit_llogis  10 1683.443
## fit_lnorm   10 1696.821
```

```r
BIC(fit_exp, fit_weibull, fit_llogis, fit_lnorm)
```

```
##             df      BIC
## fit_exp      9 1720.483
## fit_weibull 10 1705.415
## fit_llogis  10 1714.623
## fit_lnorm   10 1728.001
```

```r
fit_sex = survfit(Surv(time, status == 1) ~ sex, data = lung_mod)

# -log(S(t))
ggsurvplot(fit_sex, 
           fun = "cumhaz",
           xlim = c(5, 1200),
           title = "Negative Log of Estimated Survival Functions",
           legend.lab = c("Male", "Female"),
           ggtheme = theme_bw())
```

![](final_project_files/figure-latex/unnamed-chunk-40-1.pdf)<!-- --> 

```r
# log(-log(S(t)))
ggsurvplot(fit_sex, 
           fun = "cloglog",
           xlim = c(5, 1200),
           title = "Log of Negative Log of Estimated Survival Functions",
           legend.lab = c("Male", "Female"),
           ggtheme = theme_bw())
```

![](final_project_files/figure-latex/unnamed-chunk-40-2.pdf)<!-- --> 

```r
# Weibull is better
```

### 7.2 Model Selection


```r
# full weibull model
web = psm(Surv(time, status==1) ~ sex + age + ph.ecog.n + ph.karno + pat.karno + meal.cal + wt.loss, data = lung_mod)

# backward selection
fastbw(web, rule="aic")
```

```
## 
##  Deleted   Chi-Sq d.f. P      Residual d.f. P      AIC  
##  meal.cal  0.01   1    0.9238 0.01     1    0.9238 -1.99
##  age       0.52   1    0.4690 0.53     2    0.7659 -3.47
##  pat.karno 1.54   1    0.2140 2.08     3    0.5565 -3.92
##  wt.loss   2.55   1    0.1103 4.63     4    0.3277 -3.37
##  ph.karno  3.36   1    0.0669 7.99     5    0.1570 -2.01
## 
## Approximate Estimates after Deleting Factors
## 
##                Coef   S.E. Wald Z         P
## (Intercept)  6.1568 0.1395 44.146 0.0000000
## sex=2        0.3645 0.1400  2.603 0.0092423
## ph.ecog.n=1 -0.2169 0.1634 -1.327 0.1844536
## ph.ecog.n=2 -0.6838 0.1842 -3.713 0.0002047
## 
## Factors in Final Model
## 
## [1] sex       ph.ecog.n
```

```r
# compare with cox model
web_1 = phreg(Surv(time, status==1) ~ sex + ph.ecog.n,
                      data = lung_mod, dist = "weibull")
cox_1 = coxreg(Surv(time, status==1) ~ sex + ph.ecog.n, data = lung_mod)

check.dist(web_1, cox_1)
```

![](final_project_files/figure-latex/unnamed-chunk-41-1.pdf)<!-- --> 

```r
# final model
web_final = survreg(Surv(time, status==1) ~ sex + ph.ecog.n,
                      data = lung_mod, dist = "weibull")


# Scale parameter
web_final$scale
```

```
## [1] 0.7259592
```

```r
# Shape parameter
(shapeParameter <- 1 / web_final$scale)
```

```
## [1] 1.377488
```

```r
## AFT interpretation
exp(coef(web_final))
```

```
## (Intercept)        sex2  ph.ecog.n1  ph.ecog.n2 
## 479.7465780   1.4499851   0.8098967   0.5040564
```

```r
## Geometric mean of survival time: 479.747
## female extends survival time by 1.45 times then male
## ecog = 1 shortens survival time by 0.81 times than ecog = 0
## ecog = 2 shortens survival time by 0.504 times than ecog = 0
```










