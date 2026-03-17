#数据读取与整理
library(readxl)
library(dplyr)
library(tidyr)
#逻辑回归与整理输出
library(stats)
library(broom)
#可视化
library(ggplot2)
#限制立方样条
library(rms)
#GAM
library(mgcv)
#边际效应
library(margins)
library(pROC)
library(broom)


file_path <- "/Users/huanqiwu/Desktop/2026227B.xlsx"
df_raw <- read_excel(file_path)


df <- df_raw %>%
  mutate(
    
    bmi   = as.numeric(bmi),
    wc    = as.numeric(wc),
    hc    = as.numeric(hc),
    nc    = as.numeric(nc),
    whr   = as.numeric(whr),
    whtr  = as.numeric(whtr),
    age   = as.numeric(age),
    
    bmi_z  = as.numeric(scale(bmi)),
    wc_z   = as.numeric(scale(wc)),
    hc_z   = as.numeric(scale(hc)),
    nc_z   = as.numeric(scale(nc)),
    whr_z  = as.numeric(scale(whr)),
    whtr_z = as.numeric(scale(whtr)),
    age_z  = as.numeric(scale(age)),
    
    
    workload = factor(
      workload,
      levels = c(1,2,3,4),
      labels = c("Light","Moderate","Heavy","Retired")
    ),
    
    alcohol_use = factor(
      alcohol_use,
      levels = c(1,2,3),
      labels = c("No","Occasional","Frequent")
    ),
    
    tea_use = factor(
      tea_use,
      levels = c(1,2,3),
      labels = c("No","Occasional","Frequent")
    ),
    
    coffee_use = factor(
      coffee_use,
      levels = c(1,2,3),
      labels = c("No","Occasional","Frequent")
    ),
    
    weight_change = factor(
      weight_change,
      levels = c(1,2,3),
      labels = c("Loss","Stable","Gain")
    ),
    
    revenue = factor(
      revenue,
      levels = c(1,2,3),
      labels = c("Low","Middle","High")
    ),
    
    education = ordered(
      education,
      levels = 1:6,
      labels = c("Illiterate","Primary","Middle",
                 "High","College","Bachelor_plus")
    ),
    
    
    depression = factor(depression, levels=c(0,1)),
    anxiety    = factor(anxiety, levels=c(0,1)),
    
    sex  = factor(sex,  levels=c(0,1), labels=c("Female","Male")),
    race = factor(race, levels=c(0,1), labels=c("Han","Minority")),
    marriage = factor(marriage, levels=c(0,1),
                      labels=c("Married","Unmarried")),
    
    occupation = factor(occupation, levels=c(0,1),
                        labels=c("Mental","Manual")),
    
    healthinsurance = factor(healthinsurance,
                             levels=c(0,1),
                             labels=c("Rural","Urban")),
    
    hypertension = factor(hypertension, levels=c(0,1)),
    diabetes = factor(diabetes, levels=c(0,1)),
    dyslipidemia = factor(dyslipidemia, levels=c(0,1)),
    coronary_heart_disease = factor(coronary_heart_disease, levels=c(0,1)),
    heart_failure = factor(heart_failure, levels=c(0,1)),
    angina = factor(angina, levels=c(0,1)),
    stroke = factor(stroke, levels=c(0,1)),
    syncope = factor(syncope, levels=c(0,1)),
    atrial_fibrillation = factor(atrial_fibrillation, levels=c(0,1)),
    sleep_apnea = factor(sleep_apnea, levels=c(0,1)),
    premature_birth = factor(premature_birth, levels=c(0,1)),
    body_control = factor(body_control, levels=c(0,1)),
    
    divorce_separation = factor(divorce_separation, levels=c(0,1)),
    spouse_death = factor(spouse_death, levels=c(0,1)),
    family_death = factor(family_death, levels=c(0,1)),
    family_serious_illness = factor(family_serious_illness, levels=c(0,1)),
    natural_disaster = factor(natural_disaster, levels=c(0,1)),
    job_loss = factor(job_loss, levels=c(0,1)),
    violence_exposure = factor(violence_exposure, levels=c(0,1)),
    financial_loss = factor(financial_loss, levels=c(0,1)),
    unexpected_financial_gain = factor(unexpected_financial_gain, levels=c(0,1)),
    major_positive_event = factor(major_positive_event, levels=c(0,1))
    
  )

disease_vars <- c(
  "hypertension",
  "diabetes",
  "dyslipidemia",
  "coronary_heart_disease",
  "heart_failure",
  "angina",
  "stroke",
  "syncope",
  "atrial_fibrillation",
  "sleep_apnea"
)

df$disease <- rowSums(
  df[disease_vars] == "1",
  na.rm = TRUE
)

df$disease_cat4 <- factor(
  case_when(
    df$disease == 0 ~ "0",
    df$disease == 1 ~ "1",
    df$disease == 2 ~ "2",
    df$disease >= 3 ~ "3+"
  ),
  levels = c("0","1","2","3+")
)

############################################################
## PART 1: 运行 12 个 Logistic 模型 + 完整 summary
############################################################

covariates <- paste(
  c("sex","age_z","race","marriage","occupation","education",
    "healthinsurance","revenue",
    "disease_cat4",
    "premature_birth",
    "workload","alcohol_use","tea_use","coffee_use",
    "weight_change","body_control",
    "divorce_separation","spouse_death","family_death",
    "family_serious_illness","natural_disaster",
    "job_loss","violence_exposure","financial_loss",
    "unexpected_financial_gain","major_positive_event"),
  collapse = " + "
)

exposures <- c("bmi_z","wc_z","hc_z","nc_z","whr_z","whtr_z")
outcomes  <- c("depression","anxiety")

models <- list()
model_id <- 1

for (y in outcomes) {
  for (x in exposures) {
    
    cat("\n====================================================\n")
    cat("MODEL", model_id, "| Outcome:", y, "| Exposure:", x, "\n")
    cat("====================================================\n")
    
    formula_text <- paste(y, "~", x, "+", covariates)
    
    model <- glm(as.formula(formula_text),
                 data = df,
                 family = binomial)
    
    models[[model_id]] <- model
    
    print(summary(model))
    
    model_id <- model_id + 1
  }
}


############################################################
## PART 2: 生成 12 个模型综合结果表
############################################################

results_table <- data.frame()

model_id <- 1

for (y in outcomes) {
  for (x in exposures) {
    
    model <- models[[model_id]]
    
    ############################
    # 提取主暴露变量 OR + CI
    ############################
    
    coef_summary <- summary(model)$coefficients
    
    beta  <- coef_summary[x, "Estimate"]
    se    <- coef_summary[x, "Std. Error"]
    pval  <- coef_summary[x, "Pr(>|z|)"]
    
    OR  <- exp(beta)
    CI_low  <- exp(beta - 1.96 * se)
    CI_high <- exp(beta + 1.96 * se)
    
    ############################
    # 计算 McFadden R2
    ############################
    
    null_model <- glm(as.formula(paste(y, "~ 1")),
                      data = df,
                      family = binomial)
    
    loglik_full <- as.numeric(logLik(model))
    loglik_null <- as.numeric(logLik(null_model))
    
    McFadden_R2 <- 1 - (loglik_full / loglik_null)
    
    ############################
    # Likelihood Ratio test
    ############################
    
    LR_stat <- 2 * (loglik_full - loglik_null)
    df_diff <- attr(logLik(model), "df") - attr(logLik(null_model), "df")
    model_p <- pchisq(LR_stat, df=df_diff, lower.tail=FALSE)
    
    ############################
    # 合并结果
    ############################
    
    results_table <- rbind(
      results_table,
      data.frame(
        Model_ID = model_id,
        Outcome = y,
        Exposure = x,
        OR = OR,
        CI_2.5 = CI_low,
        CI_97.5 = CI_high,
        P_value = pval,
        McFadden_R2 = McFadden_R2,
        AIC = AIC(model),
        LR_Chi2 = LR_stat,
        Model_P = model_p
      )
    )
    
    model_id <- model_id + 1
  }
}

cat("\n================ 综合模型结果表 ================\n")
print(results_table)

############################################################
## 单图双颜色森林图（投稿优化版）
############################################################

library(ggplot2)

plot_data <- results_table

# 分别排序
dep_data <- subset(plot_data, Outcome == "depression")
dep_data <- dep_data[order(dep_data$OR), ]

anx_data <- subset(plot_data, Outcome == "anxiety")
anx_data <- anx_data[order(anx_data$OR), ]

# 合并（depression 在上，anxiety 在下）
plot_data <- rbind(dep_data, anx_data)

# 构造 y 顺序
plot_data$Y_order <- factor(
  1:nrow(plot_data),
  levels = 1:nrow(plot_data)
)

# 绘图
ggplot(plot_data,
       aes(x = OR,
           y = Y_order,
           color = Outcome)) +
  
  # 中心点
  geom_point(size = 2.5) +
  
  # 置信区间（无两端帽子）
  geom_errorbar(aes(xmin = CI_2.5, xmax = CI_97.5),
                orientation = "y",
                width = 0) +
  
  # OR=1 参考线
  geom_vline(xintercept = 1,
             linetype = "dashed",
             linewidth = 0.6) +
  
  scale_y_discrete(labels = plot_data$Exposure) +
  
  scale_color_manual(values = c(
    depression = "#0072B2",
    anxiety    = "#D55E00"
  )) +
  
  # 固定横轴范围
  scale_x_continuous(limits = c(0.65, 1.05)) +
  
  theme_classic(base_size = 16) +
  
  theme(
    text = element_text(family = "Times New Roman"),
    
    # 标题居中 + 放大
    plot.title = element_text(
      size = 20,
      hjust = 0.4,
      face = "bold"
    ),
    
    # 坐标轴字体放大
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    
    # 图例字体放大
    legend.position = "right",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16)
  )


## RCS 拟合：BMI → Depression

library(rms)

# 1️⃣ rms 必须初始化 datadist
dd <- datadist(df)
options(datadist = "dd")

# 2️⃣ 拟合 RCS 模型
fit_bmi_dep <- lrm(
  depression ~ rcs(bmi_z, 4) +
    sex + age_z + race + marriage + occupation + education +
    healthinsurance + revenue +
    hypertension + diabetes + dyslipidemia +
    coronary_heart_disease + heart_failure + angina + stroke +
    syncope + atrial_fibrillation + sleep_apnea +
    premature_birth +
    workload + alcohol_use + tea_use + coffee_use +
    weight_change + body_control +
    divorce_separation + spouse_death + family_death +
    family_serious_illness + natural_disaster +
    job_loss + violence_exposure + financial_loss +
    unexpected_financial_gain + major_positive_event,
  data = df
)

# 3️⃣ 查看模型
print(fit_bmi_dep)

pred_prob <- Predict(fit_bmi_dep, bmi_z, fun = plogis)

plot(pred_prob,
     xlab = "BMI (z-score)",
     ylab = "Predicted Probability of Depression",
     col = "#6BAED6",
     col.fill = "#9ECAE1",
     lwd = 3,
     cex.lab = 1.4,
     cex.axis = 1.3)

############################################################
# WHR → Depression (RCS)
############################################################

fit_whr_dep <- lrm(
  depression ~ rcs(whr_z, 4) +
    sex + age_z + race + marriage + occupation + education +
    healthinsurance + revenue +
    hypertension + diabetes + dyslipidemia +
    coronary_heart_disease + heart_failure + angina + stroke +
    syncope + atrial_fibrillation + sleep_apnea +
    premature_birth +
    workload + alcohol_use + tea_use + coffee_use +
    weight_change + body_control +
    divorce_separation + spouse_death + family_death +
    family_serious_illness + natural_disaster +
    job_loss + violence_exposure + financial_loss +
    unexpected_financial_gain + major_positive_event,
  data = df
)

print(fit_whr_dep)

pred_prob <- Predict(fit_whr_dep, whr_z, fun = plogis)

plot(pred_prob,
     xlab = "WHR (z-score)",
     ylab = "Predicted Probability of Depression",
     col = "#6BAED6",
     col.fill = "#9ECAE1",
     lwd = 3,
     cex.lab = 1.4,
     cex.axis = 1.3)

############################################################
# WC → Depression (RCS)
############################################################

fit_wc_dep <- lrm(
  depression ~ rcs(wc_z, 4) +
    sex + age_z + race + marriage + occupation + education +
    healthinsurance + revenue +
    hypertension + diabetes + dyslipidemia +
    coronary_heart_disease + heart_failure + angina + stroke +
    syncope + atrial_fibrillation + sleep_apnea +
    premature_birth +
    workload + alcohol_use + tea_use + coffee_use +
    weight_change + body_control +
    divorce_separation + spouse_death + family_death +
    family_serious_illness + natural_disaster +
    job_loss + violence_exposure + financial_loss +
    unexpected_financial_gain + major_positive_event,
  data = df
)

print(fit_wc_dep)

pred_prob <- Predict(fit_wc_dep, wc_z, fun = plogis)

plot(pred_prob,
     xlab = "WC (z-score)",
     ylab = "Predicted Probability of Depression",
     col = "#6BAED6",
     col.fill = "#9ECAE1",
     lwd = 3,
     cex.lab = 1.4,
     cex.axis = 1.3)

############################################################
# BMI → Anxiety (RCS)
############################################################

fit_bmi_anx <- lrm(
  anxiety ~ rcs(bmi_z, 4) +
    sex + age_z + race + marriage + occupation + education +
    healthinsurance + revenue +
    hypertension + diabetes + dyslipidemia +
    coronary_heart_disease + heart_failure + angina + stroke +
    syncope + atrial_fibrillation + sleep_apnea +
    premature_birth +
    workload + alcohol_use + tea_use + coffee_use +
    weight_change + body_control +
    divorce_separation + spouse_death + family_death +
    family_serious_illness + natural_disaster +
    job_loss + violence_exposure + financial_loss +
    unexpected_financial_gain + major_positive_event,
  data = df
)

print(fit_bmi_anx)

pred_prob <- Predict(fit_bmi_anx, bmi_z, fun = plogis)

plot(pred_prob,
     xlab = "BMI (z-score)",
     ylab = "Predicted Probability of Anxiety",
     col = "#6BAED6",
     col.fill = "#9ECAE1",
     lwd = 3,
     cex.lab = 1.4,
     cex.axis = 1.3)

############################################################
# WHR → Anxiety (RCS)
############################################################

fit_whr_anx <- lrm(
  anxiety ~ rcs(whr_z, 4) +
    sex + age_z + race + marriage + occupation + education +
    healthinsurance + revenue +
    hypertension + diabetes + dyslipidemia +
    coronary_heart_disease + heart_failure + angina + stroke +
    syncope + atrial_fibrillation + sleep_apnea +
    premature_birth +
    workload + alcohol_use + tea_use + coffee_use +
    weight_change + body_control +
    divorce_separation + spouse_death + family_death +
    family_serious_illness + natural_disaster +
    job_loss + violence_exposure + financial_loss +
    unexpected_financial_gain + major_positive_event,
  data = df
)

print(fit_whr_anx)

pred_prob <- Predict(fit_whr_anx, whr_z, fun = plogis)

plot(pred_prob,
     xlab = "WHR (z-score)",
     ylab = "Predicted Probability of Anxiety",
     col = "#6BAED6",
     col.fill = "#9ECAE1",
     lwd = 3,
     cex.lab = 1.4,
     cex.axis = 1.3)

############################################################
# WC → Anxiety (RCS)
############################################################

fit_wc_anx <- lrm(
  anxiety ~ rcs(wc_z, 4) +
    sex + age_z + race + marriage + occupation + education +
    healthinsurance + revenue +
    hypertension + diabetes + dyslipidemia +
    coronary_heart_disease + heart_failure + angina + stroke +
    syncope + atrial_fibrillation + sleep_apnea +
    premature_birth +
    workload + alcohol_use + tea_use + coffee_use +
    weight_change + body_control +
    divorce_separation + spouse_death + family_death +
    family_serious_illness + natural_disaster +
    job_loss + violence_exposure + financial_loss +
    unexpected_financial_gain + major_positive_event,
  data = df
)

print(fit_wc_anx)

pred_prob <- Predict(fit_wc_anx, wc_z, fun = plogis)

plot(pred_prob,
     xlab = "WC (z-score)",
     ylab = "Predicted Probability of Anxiety",
     col = "#6BAED6",
     col.fill = "#9ECAE1",
     lwd = 3,
     cex.lab = 1.4,
     cex.axis = 1.3)

extract_stats <- function(model, name){
  
  a <- anova(model)
  
  data.frame(
    Model = name,
    Overall_P = a[1,"P"],
    Nonlinear_P = a[2,"P"],
    C_index = model$stats["C"]
  )
}

results_rcs <- rbind(
  extract_stats(fit_bmi_dep, "BMI → Depression"),
  extract_stats(fit_whr_dep, "WHR → Depression"),
  extract_stats(fit_wc_dep,  "WC → Depression"),
  
  extract_stats(fit_bmi_anx, "BMI → Anxiety"),
  extract_stats(fit_whr_anx, "WHR → Anxiety"),
  extract_stats(fit_wc_anx,  "WC → Anxiety")
)

results_rcs

