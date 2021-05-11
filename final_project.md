Final Project
================
David Nemirovsky & Jared Klug
5/13/21

## **EDA**

``` r
titanic_df = 
  read_csv("./data/train.csv") %>% 
  janitor::clean_names() %>% 
  mutate(survived = fct_recode(as.factor(survived), yes = "1", no = "0"), 
         pclass = as.factor(pclass), 
         sex = as.factor(sex), 
         embarked = as.factor(embarked))

train_df = 
  titanic_df %>% 
  select(-c(ticket, cabin, name, passenger_id)) %>% 
  drop_na()

tbl_summary(train_df)
```

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#sqaumxajvs .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#sqaumxajvs .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#sqaumxajvs .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#sqaumxajvs .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#sqaumxajvs .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#sqaumxajvs .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#sqaumxajvs .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#sqaumxajvs .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#sqaumxajvs .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#sqaumxajvs .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#sqaumxajvs .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#sqaumxajvs .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#sqaumxajvs .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#sqaumxajvs .gt_from_md > :first-child {
  margin-top: 0;
}

#sqaumxajvs .gt_from_md > :last-child {
  margin-bottom: 0;
}

#sqaumxajvs .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#sqaumxajvs .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#sqaumxajvs .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#sqaumxajvs .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#sqaumxajvs .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#sqaumxajvs .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#sqaumxajvs .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#sqaumxajvs .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#sqaumxajvs .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#sqaumxajvs .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#sqaumxajvs .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#sqaumxajvs .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#sqaumxajvs .gt_left {
  text-align: left;
}

#sqaumxajvs .gt_center {
  text-align: center;
}

#sqaumxajvs .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#sqaumxajvs .gt_font_normal {
  font-weight: normal;
}

#sqaumxajvs .gt_font_bold {
  font-weight: bold;
}

#sqaumxajvs .gt_font_italic {
  font-style: italic;
}

#sqaumxajvs .gt_super {
  font-size: 65%;
}

#sqaumxajvs .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="sqaumxajvs" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

<strong>Characteristic</strong>

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

<strong>N = 712</strong><sup class="gt_footnote_marks">1</sup>

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_left">

survived

</td>

<td class="gt_row gt_center">

288 (40%)

</td>

</tr>

<tr>

<td class="gt_row gt_left">

pclass

</td>

<td class="gt_row gt_center">

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">

1

</td>

<td class="gt_row gt_center">

184 (26%)

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">

2

</td>

<td class="gt_row gt_center">

173 (24%)

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">

3

</td>

<td class="gt_row gt_center">

355 (50%)

</td>

</tr>

<tr>

<td class="gt_row gt_left">

sex

</td>

<td class="gt_row gt_center">

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">

female

</td>

<td class="gt_row gt_center">

259 (36%)

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">

male

</td>

<td class="gt_row gt_center">

453 (64%)

</td>

</tr>

<tr>

<td class="gt_row gt_left">

age

</td>

<td class="gt_row gt_center">

28 (20, 38)

</td>

</tr>

<tr>

<td class="gt_row gt_left">

sib\_sp

</td>

<td class="gt_row gt_center">

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">

0

</td>

<td class="gt_row gt_center">

469 (66%)

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">

1

</td>

<td class="gt_row gt_center">

183 (26%)

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">

2

</td>

<td class="gt_row gt_center">

25 (3.5%)

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">

3

</td>

<td class="gt_row gt_center">

12 (1.7%)

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">

4

</td>

<td class="gt_row gt_center">

18 (2.5%)

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">

5

</td>

<td class="gt_row gt_center">

5 (0.7%)

</td>

</tr>

<tr>

<td class="gt_row gt_left">

parch

</td>

<td class="gt_row gt_center">

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">

0

</td>

<td class="gt_row gt_center">

519 (73%)

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">

1

</td>

<td class="gt_row gt_center">

110 (15%)

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">

2

</td>

<td class="gt_row gt_center">

68 (9.6%)

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">

3

</td>

<td class="gt_row gt_center">

5 (0.7%)

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">

4

</td>

<td class="gt_row gt_center">

4 (0.6%)

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">

5

</td>

<td class="gt_row gt_center">

5 (0.7%)

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">

6

</td>

<td class="gt_row gt_center">

1 (0.1%)

</td>

</tr>

<tr>

<td class="gt_row gt_left">

fare

</td>

<td class="gt_row gt_center">

16 (8, 33)

</td>

</tr>

<tr>

<td class="gt_row gt_left">

embarked

</td>

<td class="gt_row gt_center">

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">

C

</td>

<td class="gt_row gt_center">

130 (18%)

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">

Q

</td>

<td class="gt_row gt_center">

28 (3.9%)

</td>

</tr>

<tr>

<td class="gt_row gt_left" style="text-align: left; text-indent: 10px;">

S

</td>

<td class="gt_row gt_center">

554 (78%)

</td>

</tr>

</tbody>

<tfoot>

<tr class="gt_footnotes">

<td colspan="2">

<p class="gt_footnote">

<sup class="gt_footnote_marks"> <em>1</em> </sup>

Statistics presented: n (%); Median (IQR) <br />

</p>

</td>

</tr>

</tfoot>

</table>

</div>

<!--/html_preserve-->

``` r
featurePlot(x = select(mutate(train_df, 
                              pclass = as.numeric(pclass), 
                              sex = as.numeric(sex),
                              embarked = as.numeric(embarked)),
                       pclass:embarked), 
            y = train_df$survived,
            scales = list(x = list(relation = "free"), 
                          y = list(relation = "free")),
            plot = "density", 
            auto.key = list(columns = 2))
```

<img src="final_project_files/figure-gfm/eda-1.png" width="80%" style="display: block; margin: auto;" />

``` r
train_df %>% 
  ggplot(aes(x = age, fill = survived)) +
  geom_density(alpha = 0.75) + 
  facet_grid(sex ~ pclass)
```

<img src="final_project_files/figure-gfm/eda-2.png" width="80%" style="display: block; margin: auto;" />

``` r
train_df %>% 
  filter(fare < 100) %>% 
  ggplot(aes(x = fare, fill = survived)) +
  geom_density(alpha = 0.75) + 
  facet_grid(sex ~ pclass)
```

<img src="final_project_files/figure-gfm/eda-3.png" width="80%" style="display: block; margin: auto;" />

``` r
train_df %>% 
  #filter(fare < 100) %>% 
  ggplot(aes(x = age, y = fare, color = survived)) +
  geom_point(alpha = 0.75) + 
  facet_grid(sex ~ pclass)
```

<img src="final_project_files/figure-gfm/eda-4.png" width="80%" style="display: block; margin: auto;" />

``` r
train_df %>% 
  ggplot(aes(x = age, fill = survived)) + 
  geom_density(alpha = 0.75) + 
  facet_grid(sex ~ embarked)
```

<img src="final_project_files/figure-gfm/eda-5.png" width="80%" style="display: block; margin: auto;" />

``` r
train_df %>% 
  #filter(fare < 100) %>% 
  ggplot(aes(x = age, y = fare, color = survived)) +
  geom_point(alpha = 0.75) + 
  facet_grid(sex ~ embarked)
```

<img src="final_project_files/figure-gfm/eda-6.png" width="80%" style="display: block; margin: auto;" />

## **Model Training**

``` r
set.seed(37564)

ctrl = trainControl(method = "repeatedcv", summaryFunction = twoClassSummary, classProbs = T, number = 10, repeats = 5)

set.seed(37564)
mod_enet = train(survived ~ .,
                 na.action = na.exclude, 
                 data = train_df, 
                 method = "glmnet", 
                 family = "binomial", 
                 metric = "ROC", 
                 tuneGrid = expand.grid(alpha = seq(0, 0.5, length = 6), 
                                        lambda = exp(seq(-4, -8, length = 50))),
                 trControl = ctrl)
tuning_plot_enet = 
  ggplot(mod_enet, highlight = T) + 
  ggtitle("Elastic Net") +
  theme(plot.title = element_text(hjust = 0.5))
mod_enet$bestTune
```

    ##     alpha      lambda
    ## 231   0.4 0.003883492

``` r
set.seed(37564)
mod_mars = train(survived ~ ., 
                 na.action = na.exclude, 
                 data = train_df, 
                 method = "earth",
                 tuneGrid = expand.grid(degree = 1:3, nprune = 5:15), 
                 metric = "ROC", 
                 trControl = ctrl)
tuning_plot_mars = 
  ggplot(mod_mars, highlight = T) + 
  ggtitle("MARS") +
  theme(plot.title = element_text(hjust = 0.5))
mod_mars$bestTune
```

    ##    nprune degree
    ## 28     10      3

``` r
set.seed(37564)
mod_knn = train(survived ~ .,
                na.action = na.exclude, 
                data = train_df, 
                method = "knn",
                metric = "ROC", 
                preProcess = c("center","scale"),
                tuneGrid = data.frame(k = seq(1, 30, by = 1)), 
                trControl = ctrl)
tuning_plot_knn = 
  ggplot(mod_knn, highlight = T) + 
  ggtitle("KNN") +
  theme(plot.title = element_text(hjust = 0.5))
mod_knn$bestTune
```

    ##   k
    ## 8 8

``` r
set.seed(37564)
mod_boost = train(survived ~ .,
                  na.action = na.exclude,
                  data = train_df,
                  method = "gbm",
                  tuneGrid = expand.grid(n.trees = c(1000, 3000),
                                         interaction.depth = 1:6,
                                         shrinkage = c(0.001, 0.003, 0.005), 
                                         n.minobsinnode = 1),
                  metric = "ROC",
                  trControl = ctrl,
                  verbose = F)
tuning_plot_boost = 
  ggplot(mod_boost, highlight = T) + 
  ggtitle("Boosting") +
  theme(plot.title = element_text(hjust = 0.5))
mod_boost$bestTune
```

    ##    n.trees interaction.depth shrinkage n.minobsinnode
    ## 36    3000                 6     0.005              1

``` r
(tuning_plot_enet + tuning_plot_mars + tuning_plot_knn) / tuning_plot_boost
```

<img src="final_project_files/figure-gfm/models-1.png" width="80%" style="display: block; margin: auto;" />

``` r
res = resamples(list(ENET = mod_enet, MARS = mod_mars, KNN = mod_knn, BOOST = mod_boost))
summary(res)
```

    ## 
    ## Call:
    ## summary.resamples(object = res)
    ## 
    ## Models: ENET, MARS, KNN, BOOST 
    ## Number of resamples: 50 
    ## 
    ## ROC 
    ##            Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## ENET  0.7380952 0.8165637 0.8446176 0.8531036 0.8905434 0.9556650    0
    ## MARS  0.7479475 0.8370589 0.8653053 0.8657740 0.8949097 0.9474548    0
    ## KNN   0.7676519 0.8245074 0.8552736 0.8550413 0.8808498 0.9330870    0
    ## BOOST 0.7654370 0.8421784 0.8723317 0.8750384 0.9051438 0.9534884    0
    ## 
    ## Sens 
    ##            Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## ENET  0.7142857 0.8139535 0.8604651 0.8561351 0.8837209 0.9761905    0
    ## MARS  0.7619048 0.8333333 0.8809524 0.8702436 0.9047619 0.9761905    0
    ## KNN   0.7441860 0.8333333 0.8809524 0.8664563 0.9047619 0.9767442    0
    ## BOOST 0.7380952 0.8571429 0.8823367 0.8899889 0.9298173 1.0000000    0
    ## 
    ## Spec 
    ##            Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## ENET  0.5172414 0.6551724 0.7241379 0.7051970 0.7586207 0.8275862    0
    ## MARS  0.5172414 0.6551724 0.7142857 0.7122167 0.7586207 0.8965517    0
    ## KNN   0.4827586 0.6206897 0.6785714 0.6745567 0.7241379 0.8620690    0
    ## BOOST 0.5517241 0.6896552 0.7241379 0.7281773 0.7912562 0.8965517    0

``` r
bwplot(res, metric = "ROC", main = "ROC for Repeated 10-Fold CV Using Various Models")
```

<img src="final_project_files/figure-gfm/models-2.png" width="80%" style="display: block; margin: auto;" />

## **Variable Importance**

``` r
set.seed(37564)

#vip(mod_enet, 
#    method = "permute", 
#    train = train_df,
#    target = "survived",
#    metric = "auc",
#    reference_class = c("yes", "no"),
#    nsim = 1000,
#    pred_wrapper = predict,
#    geom = "boxplot", 
#    all_permutations = T,
#    mapping = aes_string(fill = "Variable", alpha = 0.75))

#vip(mod_mars, 
#    method = "permute", 
#    train = train_df,
#    target = "survived",
#    metric = "auc",
#    reference_class = c("yes", "no"),
#    nsim = 30,
#    pred_wrapper = predict,
#    geom = "boxplot", 
#    all_permutations = T,
#    mapping = aes_string(fill = "Variable", alpha = 0.75))

#vip(mod_knn, 
#    method = "permute", 
#    train = train_df,
#    target = "survived",
#    metric = "auc",
#    reference_class = c("yes", "no"),
#    nsim = 1000,
#    pred_wrapper = predict,
#    geom = "boxplot", 
#    all_permutations = T,
#    mapping = aes_string(fill = "Variable", alpha = 0.75))

vip(mod_boost, 
    method = "permute", 
    train = train_df,
    target = "survived",
    metric = "auc",
    reference_class = c("yes", "no"),
    nsim = 30,
    pred_wrapper = predict,
    geom = "boxplot", 
    all_permutations = T,
    mapping = aes_string(fill = "Variable", alpha = 0.75))
```

<img src="final_project_files/figure-gfm/vip-1.png" width="80%" style="display: block; margin: auto;" />

## Prediction

``` r
set.seed(37564)

test_df = 
  read_csv("./data/test.csv") %>% 
  janitor::clean_names() %>% 
  mutate(pclass = as.factor(pclass), 
         sex = as.factor(sex), 
         embarked = as.factor(embarked)) %>% 
  select(-c(ticket, cabin, name, passenger_id)) %>% 
  drop_na()

pred_mars = predict(mod_mars, newdata = test_df, type = "prob")[,1]
pred_boost = predict(mod_boost, newdata = test_df, type = "prob")[,1]
diff_df = 
  tibble(pred_boost, pred_mars) %>% 
  mutate(
    boost_surv = ifelse(pred_boost > 0.5, "Y", "N"), 
    mars_surv = ifelse(pred_mars > 0.5, "Y", "N"), 
    diff = ifelse(boost_surv == mars_surv, "Y", "N"))
diff_df %>% 
  filter(diff == "N")
```

    ## # A tibble: 34 x 5
    ##    pred_boost pred_mars boost_surv mars_surv diff 
    ##         <dbl>     <dbl> <chr>      <chr>     <chr>
    ##  1      0.889    0.349  Y          N         N    
    ##  2      0.667    0.375  Y          N         N    
    ##  3      0.814    0.358  Y          N         N    
    ##  4      0.592    0.0708 Y          N         N    
    ##  5      0.493    0.574  N          Y         N    
    ##  6      0.449    0.739  N          Y         N    
    ##  7      0.353    0.574  N          Y         N    
    ##  8      0.755    0.420  Y          N         N    
    ##  9      0.215    0.505  N          Y         N    
    ## 10      0.631    0.296  Y          N         N    
    ## # ... with 24 more rows
