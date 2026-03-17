##################### 0. 环境准备 ###############################
library(readxl)
library(dplyr)
library(stats)
library(broom)
library(purrr)
library(tidyr)

install.packages("pROC")
library(pROC)
library(broom)

##################### 1. 数据读取 ###############################

file_path <- "/Users/huanqiwu/Desktop/DATA117.xlsx"
df_raw <- read_excel(file_path)

##################### 2. 数据清洗与编码 #########################

df <- df_raw %>%
  mutate(
    # =========================
    # Outcome variables
    # =========================
    depressionscore = as.numeric(depressionscore),
    anxietyscore    = as.numeric(anxietyscore),
    depressionbin16 = as.integer(depressionbin16),
    depressionbin20 = as.integer(depressionbin20),
    anxietybin50    = as.integer(anxietybin50),
    
    # =========================
    # Body shape indices
    # =========================
    bmi  = as.numeric(bmi),
    wc   = as.numeric(wc),
    hc   = as.numeric(hc),
    nc   = as.numeric(nc),
    ht   = as.numeric(ht),
    whr  = as.numeric(whr),
    whtr = as.numeric(whtr),
    
    # Z-score standardization (per SD effect)
    bmi_z  = as.numeric(scale(bmi)),
    wc_z   = as.numeric(scale(wc)),
    hc_z   = as.numeric(scale(hc)),
    nc_z   = as.numeric(scale(nc)),
    whr_z  = as.numeric(scale(whr)),
    whtr_z = as.numeric(scale(whtr)),
    
    # =========================
    # Demographic covariates
    # =========================
    sex = factor(sex, levels = c(0, 1),
                 labels = c("Male", "Female")),
    
    ethnicity = factor(ethnicity, levels = c(0, 1),
                       labels = c("Han", "Minority")),
    
    marriage = factor(marriage, levels = c(0, 1),
                      labels = c("Unmarried_or_other",
                                 "Married_or_widowed")),
    
    education = ordered(
      education,
      levels = 1:6,
      labels = c("Illiterate", "Primary", "Middle",
                 "High", "College", "Bachelor_plus")
    ),
    
    workintensity = factor(
      workintensity,
      levels = c(1, 2, 3, 4),
      labels = c("Light", "Moderate", "Heavy", "Retired")
    ),
    
    age = as.numeric(age)
  )

##################### 3. 缺失值快速检查 #########################

colSums(is.na(df))

##################### 4. Step 1 主分析：体型指标逐一独立纳入 ####

# ---- 4.1 定义分析要素 ----

# 六个体型指标（per SD）
exposures <- c("bmi_z", "wc_z", "hc_z", "nc_z", "whr_z", "whtr_z")

# 两个结局
outcomes <- c("depressionbin16", "anxietybin50")

# DAG 最小调整集（完全一致）
covariates <- c(
  "age",
  "sex",
  "ethnicity",
  "education",
  "marriage",
  "workintensity"
)

# ---- 4.2 批量建模（6 × 2） ----

results <- expand.grid(
  exposure = exposures,
  outcome  = outcomes,
  stringsAsFactors = FALSE
) %>%
  mutate(
    model = map2(exposure, outcome, ~{
      fml <- as.formula(
        paste(.y, "~", .x, "+",
              paste(covariates, collapse = " + "))
      )
      glm(fml, data = df, family = binomial)
    }),
    tidy = map(model, ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE))
  )

##################### 5. 汇总结果（OR / CI / 方向） ################

result_table <- results %>%
  unnest(tidy) %>%
  filter(term %in% exposures) %>%
  transmute(
    Outcome   = outcome,
    Exposure  = term,
    OR        = estimate,
    CI_low    = conf.low,
    CI_high   = conf.high,
    p_value   = p.value,
    Direction = case_when(
      OR > 1 ~ "Positive",
      OR < 1 ~ "Negative",
      TRUE   ~ "Null"
    )
  )

# 查看主结果表
print(result_table)

############################################################
## Figure 1 – Forest Plot (6 exposures × 2 outcomes)
############################################################

library(ggplot2)
library(dplyr)

# 为绘图准备格式
forest_df <- result_table %>%
  mutate(
    Exposure = factor(Exposure,
                      levels = c("bmi_z","wc_z","hc_z",
                                 "nc_z","whr_z","whtr_z")),
    Outcome = factor(Outcome,
                     levels = c("depressionbin16","anxietybin50"),
                     labels = c("Depression","Anxiety"))
  )

# 绘图
p_forest <- ggplot(forest_df,
                   aes(x = Exposure, y = OR,
                       ymin = CI_low, ymax = CI_high,
                       color = Outcome)) +
  geom_pointrange(position = position_dodge(width = 0.6)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  coord_flip() +
  labs(
    y = "Odds Ratio (per SD)",
    x = "Body Shape Index",
    title = "Associations Between Body Shape Indices and Mental Health"
  ) +
  scale_color_manual(values = c("#0072B2","#D55E00")) +
  theme_classic(base_size = 14)

print(p_forest)


##################### 6. 跨结局一致性比较 #########################

cross_outcome <- result_table %>%
  select(Outcome, Exposure, OR, CI_low, CI_high, Direction) %>%
  pivot_wider(
    names_from  = Outcome,
    values_from = c(OR, CI_low, CI_high, Direction)
  )

print(cross_outcome)

############################################################
## 4. WHR_z 的 RCS + GAM 分析（主结果深化，统一版）
############################################################

## ---------- 4.1 通用设置（只需一次） ----------

library(rms)
library(mgcv)
library(ggplot2)
library(dplyr)

options(contrasts = c("contr.treatment", "contr.treatment"))

df <- df %>%
  mutate(
    education = factor(education),
    workintensity = factor(workintensity)
  )

dd <- datadist(df)
options(datadist = "dd")

############################################################
## 4.2 Depression：RCS + GAM
############################################################

## ---- RCS Logistic ----
rcs_dep <- lrm(
  depressionbin16 ~ rcs(whr_z, 4) +
    age + sex + ethnicity +
    education + marriage + workintensity,
  data = df
)

anova(rcs_dep)

# ===== 补充：RCS 模型估计量与拟合指标 =====
rcs_dep_metrics <- data.frame(
  Model = "RCS_Depression",
  AIC = AIC(rcs_dep),
  BIC = BIC(rcs_dep),
  LogLik = as.numeric(logLik(rcs_dep)),
  C_index = rcs_dep$stats["C"]
)

print(rcs_dep_metrics)


## ---- RCS 图（OR）----
pred_rcs_dep <- Predict(
  rcs_dep,
  whr_z,
  fun = exp,
  ref.zero = TRUE
)

p_rcs_dep <- ggplot(pred_rcs_dep, aes(x = whr_z, y = yhat)) +
  geom_line(size = 1.2, color = "#0072B2") +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.25, fill = "#0072B2") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(
    x = "WHR (per SD)",
    y = "Odds Ratio for Depression",
    title = "Restricted Cubic Spline Analysis of WHR (Depression)"
  ) +
  theme_classic(base_size = 14)

print(p_rcs_dep)


## ---- GAM Logistic ----
gam_dep <- gam(
  depressionbin16 ~ s(whr_z, k = 4) +
    age + sex + ethnicity +
    education + marriage + workintensity,
  data = df,
  family = binomial(link = "logit"),
  method = "REML"
)

summary(gam_dep)

# ===== 补充：GAM 模型拟合指标 =====
gam_dep_metrics <- data.frame(
  Model = "GAM_Depression",
  AIC = AIC(gam_dep),
  LogLik = as.numeric(logLik(gam_dep)),
  EDF = summary(gam_dep)$s.table[1, "edf"],
  Smooth_p = summary(gam_dep)$s.table[1, "p-value"],
  Deviance_explained = summary(gam_dep)$dev.expl * 100,
  UBRE = summary(gam_dep)$sp.criterion
)

print(gam_dep_metrics)

plot(
  gam_dep,
  select = 1,
  shade = TRUE,
  rug = TRUE,
  seWithMean = TRUE,
  main = "GAM Smooth for WHR (Depression, log-odds scale)"
)

## ---- GAM → OR 图 ----
newdata_dep <- data.frame(
  whr_z = seq(min(df$whr_z, na.rm = TRUE),
              max(df$whr_z, na.rm = TRUE),
              length.out = 200),
  age = mean(df$age, na.rm = TRUE),
  sex = "Female",
  ethnicity = "Han",
  education = levels(df$education)[1],
  marriage = "Married_or_widowed",
  workintensity = "Light"
)

pred_dep <- predict(gam_dep, newdata_dep, se.fit = TRUE)

newdata_dep <- newdata_dep %>%
  mutate(
    OR = exp(pred_dep$fit),
    lower = exp(pred_dep$fit - 1.96 * pred_dep$se.fit),
    upper = exp(pred_dep$fit + 1.96 * pred_dep$se.fit)
  )

p_gam_dep <- ggplot(newdata_dep, aes(x = whr_z, y = OR)) +
  geom_line(size = 1.2, color = "#D55E00") +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.25, fill = "#D55E00") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(
    x = "WHR (per SD)",
    y = "Odds Ratio for Depression",
    title = "GAM Analysis of WHR (Depression)"
  ) +
  theme_classic(base_size = 14)

print(p_gam_dep)

############################################################
## 4.3 Anxiety：RCS + GAM（完全对称）
############################################################

rcs_anx <- lrm(
  anxietybin50 ~ rcs(whr_z, 4) +
    age + sex + ethnicity +
    education + marriage + workintensity,
  data = df
)

anova(rcs_anx)

# ===== RCS Anxiety 模型指标 =====
rcs_anx_metrics <- data.frame(
  Model = "RCS_Anxiety",
  AIC = AIC(rcs_anx),
  BIC = BIC(rcs_anx),
  LogLik = as.numeric(logLik(rcs_anx)),
  C_index = rcs_anx$stats["C"]
)

print(rcs_anx_metrics)


gam_anx <- gam(
  anxietybin50 ~ s(whr_z, k = 4) +
    age + sex + ethnicity +
    education + marriage + workintensity,
  data = df,
  family = binomial(link = "logit"),
  method = "REML"
)

summary(gam_anx)

# ===== GAM Anxiety 模型指标 =====
gam_anx_metrics <- data.frame(
  Model = "GAM_Anxiety",
  AIC = AIC(gam_anx),
  LogLik = as.numeric(logLik(gam_anx)),
  EDF = summary(gam_anx)$s.table[1, "edf"],
  Smooth_p = summary(gam_anx)$s.table[1, "p-value"],
  Deviance_explained = summary(gam_anx)$dev.expl * 100,
  UBRE = summary(gam_anx)$sp.criterion
)

print(gam_anx_metrics)

## ---- RCS 图（OR）- Anxiety ----
pred_rcs_anx <- Predict(
  rcs_anx,
  whr_z,
  fun = exp,
  ref.zero = TRUE
)

p_rcs_anx <- ggplot(pred_rcs_anx, aes(x = whr_z, y = yhat)) +
  geom_line(size = 1.2, color = "#0072B2") +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.25, fill = "#0072B2") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(
    x = "WHR (per SD)",
    y = "Odds Ratio for Anxiety",
    title = "Restricted Cubic Spline Analysis of WHR (Anxiety)"
  ) +
  theme_classic(base_size = 14)

print(p_rcs_anx)


############################################################
## Robustness & Marginal Effects (GAM, separated plots)
############################################################

library(mgcv)
library(ggplot2)
library(dplyr)

theme_set(theme_classic(base_size = 14))

## ---------- 通用函数：GAM → 边际概率 ----------
gam_marginal <- function(outcome, exposure, xlab, data) {
  
  fml <- as.formula(
    paste0(outcome, " ~ s(", exposure, ", k = 4) + ",
           "age + sex + ethnicity + education + marriage + workintensity")
  )
  
  m <- gam(fml, data = data, family = binomial, me。/thod = "REML")
  
  nd <- data.frame(
    x = seq(min(data[[exposure]], na.rm = TRUE),
            max(data[[exposure]], na.rm = TRUE),
            length.out = 200),
    age = mean(data$age, na.rm = TRUE),
    sex = "Female",
    ethnicity = "Han",
    education = levels(data$education)[3],
    marriage = "Married_or_widowed",
    workintensity = "Retired"
  )
  names(nd)[1] <- exposure
  
  p <- predict(m, nd, type = "link", se.fit = TRUE)
  
  nd %>%
    mutate(
      prob = plogis(p$fit),
      lower = plogis(p$fit - 1.96 * p$se.fit),
      upper = plogis(p$fit + 1.96 * p$se.fit),
      Outcome = ifelse(grepl("depression", outcome), "Depression", "Anxiety"),
      Exposure = xlab
    )
}

## ---------- WHR ----------
whr_df <- bind_rows(
  gam_marginal("depressionbin16", "whr_z", "WHR (per SD)", df),
  gam_marginal("anxietybin50",    "whr_z", "WHR (per SD)", df)
)

p_whr <- ggplot(whr_df,
                aes(x = whr_z, y = prob, color = Outcome, fill = Outcome)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, color = NA) +
  labs(title = "Marginal Effects of WHR on Mental Health",
       x = "WHR (per SD)", y = "Predicted Probability") +
  scale_color_manual(values = c("#D55E00", "#0072B2")) +
  scale_fill_manual(values = c("#D55E00", "#0072B2")) +
  theme(legend.title = element_blank())

print(p_whr)

## ---------- BMI ----------
bmi_df <- bind_rows(
  gam_marginal("depressionbin16", "bmi_z", "BMI (per SD)", df),
  gam_marginal("anxietybin50",    "bmi_z", "BMI (per SD)", df)
)

p_bmi <- ggplot(bmi_df,
                aes(x = bmi_z, y = prob, color = Outcome, fill = Outcome)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, color = NA) +
  labs(title = "Marginal Effects of BMI on Mental Health",
       x = "BMI (per SD)", y = "Predicted Probability") +
  scale_color_manual(values = c("#D55E00", "#0072B2")) +
  scale_fill_manual(values = c("#D55E00", "#0072B2")) +
  theme(legend.title = element_blank())

print(p_bmi)








############################################################
## 6. GAM-based Marginal Effects（最终解释性分析）
############################################################

library(mgcv)
library(dplyr)
library(ggplot2)

## ---------- 6.1 拟合 GAM（一次性，供边际效应使用） ----------

gam_dep_whr <- gam(
  depressionbin16 ~ s(whr_z, k = 4) +
    age + sex + ethnicity + education + marriage + workintensity,
  data = df,
  family = binomial(link = "logit"),
  method = "REML"
)

gam_anx_whr <- gam(
  anxietybin50 ~ s(whr_z, k = 4) +
    age + sex + ethnicity + education + marriage + workintensity,
  data = df,
  family = binomial(link = "logit"),
  method = "REML"
)

gam_dep_bmi <- gam(
  depressionbin16 ~ s(bmi_z, k = 4) +
    age + sex + ethnicity + education + marriage + workintensity,
  data = df,
  family = binomial(link = "logit"),
  method = "REML"
)

gam_anx_bmi <- gam(
  anxietybin50 ~ s(bmi_z, k = 4) +
    age + sex + ethnicity + education + marriage + workintensity,
  data = df,
  family = binomial(link = "logit"),
  method = "REML"
)

## ---------- 6.2 构造“典型个体”预测数据 ----------

make_newdata <- function(xvar, df) {
  data.frame(
    x = seq(min(df[[xvar]], na.rm = TRUE),
            max(df[[xvar]], na.rm = TRUE),
            length.out = 200),
    age = mean(df$age, na.rm = TRUE),
    sex = "Female",
    ethnicity = "Han",
    education = levels(df$education)[3],
    marriage = "Married_or_widowed",
    workintensity = "Retired"
  ) %>% rename(!!xvar := x)
}

nd_whr <- make_newdata("whr_z", df)
nd_bmi <- make_newdata("bmi_z", df)

## ---------- 6.3 预测函数（logit → probability） ----------

predict_prob <- function(model, newdata) {
  p <- predict(model, newdata, type = "link", se.fit = TRUE)
  newdata %>%
    mutate(
      prob = plogis(p$fit),
      lower = plogis(p$fit - 1.96 * p$se.fit),
      upper = plogis(p$fit + 1.96 * p$se.fit)
    )
}

dep_whr <- predict_prob(gam_dep_whr, nd_whr) %>% mutate(outcome = "Depression")
anx_whr <- predict_prob(gam_anx_whr, nd_whr) %>% mutate(outcome = "Anxiety")

dep_bmi <- predict_prob(gam_dep_bmi, nd_bmi) %>% mutate(outcome = "Depression")
anx_bmi <- predict_prob(gam_anx_bmi, nd_bmi) %>% mutate(outcome = "Anxiety")

## ---------- 6.4 作图函数（统一风格） ----------

plot_marginal <- function(data, xvar, title) {
  ggplot(data, aes_string(x = xvar, y = "prob",
                          color = "outcome", fill = "outcome")) +
    geom_line(size = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                alpha = 0.25, color = NA) +
    labs(
      x = paste0(toupper(gsub("_z", "", xvar)), " (per SD)"),
      y = "Predicted Probability",
      title = title
    ) +
    scale_color_manual(values = c("Anxiety" = "#D55E00",
                                  "Depression" = "#0072B2")) +
    scale_fill_manual(values = c("Anxiety" = "#D55E00",
                                 "Depression" = "#0072B2")) +
    theme_classic(base_size = 14)
}

## ---------- 6.5 输出最终图 ----------

p_whr <- plot_marginal(
  bind_rows(dep_whr, anx_whr),
  "whr_z",
  "Marginal Effects of WHR on Mental Health"
)

p_bmi <- plot_marginal(
  bind_rows(dep_bmi, anx_bmi),
  "bmi_z",
  "Marginal Effects of BMI on Mental Health"
)

print(p_whr)
print(p_bmi)

## ---- 关键 SD 点的预测概率（用于 Results 描述） ----
extract_key_points <- function(data, xvar) {
  data %>%
    mutate(SD = round(.data[[xvar]], 1)) %>%
    filter(SD %in% c(-1, 0, 1)) %>%
    select(outcome, SD, prob, lower, upper) %>%
    arrange(outcome, SD)
}

key_probs_whr <- bind_rows(
  extract_key_points(dep_whr, "whr_z"),
  extract_key_points(anx_whr, "whr_z")
)

key_probs_bmi <- bind_rows(
  extract_key_points(dep_bmi, "bmi_z"),
  extract_key_points(anx_bmi, "bmi_z")
)

print(key_probs_whr)
print(key_probs_bmi)

## ---- WHR vs BMI 的概率变化幅度 ----
calc_delta_prob <- function(data) {
  data %>%
    group_by(outcome) %>%
    summarise(delta_prob = max(prob) - min(prob))
}

delta_compare <- bind_rows(
  calc_delta_prob(bind_rows(dep_whr, anx_whr)) %>% mutate(Exposure = "WHR"),
  calc_delta_prob(bind_rows(dep_bmi, anx_bmi)) %>% mutate(Exposure = "BMI")
)

print(delta_compare)
