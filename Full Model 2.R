############################################################
# 数据读取与整理
############################################################

library(readxl)
library(dplyr)
library(tidyr)

library(stats)
library(broom)

library(ggplot2)

library(rms)
library(mgcv)

library(margins)
library(pROC)
library(broom)

file_path <- "/Users/huanqiwu/Desktop/2026227B.xlsx"
df_raw <- read_excel(file_path)


############################################################
# 数据整理（Z-score 标准化）
############################################################

df <- df_raw %>%
  mutate(
    
    bmi   = as.numeric(bmi),
    wc    = as.numeric(wc),
    hc    = as.numeric(hc),
    nc    = as.numeric(nc),
    whr   = as.numeric(whr),
    whtr  = as.numeric(whtr),
    age   = as.numeric(age),
    
    # Z-score 标准化（per 1 SD increase）
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
                             labels=c("Rural","Urban"))
  )


############################################################
# 慢病计数
############################################################

disease_vars <- c(
  "hypertension","diabetes","dyslipidemia",
  "coronary_heart_disease","heart_failure",
  "angina","stroke","syncope",
  "atrial_fibrillation","sleep_apnea"
)

df$disease <- rowSums(df[disease_vars] == "1", na.rm = TRUE)

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
# 暴露变量
############################################################

exposures <- c("bmi_z","wc_z","hc_z","nc_z","whr_z","whtr_z")

outcomes <- c("depression","anxiety")

sex_groups <- c("Female","Male")

df %>%
  summarise(across(
    c(bmi, wc, hc, nc, whr, whtr, age),
    ~ sd(., na.rm = TRUE)
  ))

library(dplyr)

vars_numeric <- c("bmi","wc","hc","nc","whr","whtr","age")

df %>%
  group_by(sex) %>%
  summarise(across(
    all_of(vars_numeric),
    list(
      mean = ~ mean(. , na.rm = TRUE),
      sd   = ~ sd(. , na.rm = TRUE)
    )
  ))
############################################################
# 协变量（删除 sex 与 age_z）
############################################################

covariates <- paste(
  c("age_z","race","marriage","occupation","education",
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

############################################################
# PART 1：按 sex 分层运行 Logistic 模型
############################################################

models <- list()
model_id <- 1

for (s in sex_groups) {
  
  df_sex <- df %>% filter(sex == s)
  
  for (y in outcomes) {
    for (x in exposures) {
      
      cat("\n====================================================\n")
      cat("MODEL", model_id, "| Sex:", s,
          "| Outcome:", y,
          "| Exposure:", x, "\n")
      cat("====================================================\n")
      
      formula_text <- paste(y, "~", x, "+", covariates)
      
      model <- glm(as.formula(formula_text),
                   data = df_sex,
                   family = binomial)
      
      models[[model_id]] <- model
      
      print(summary(model))
      
      model_id <- model_id + 1
    }
  }
}

############################################################
# PART 2：生成结果表
############################################################

results_table <- data.frame()

model_id <- 1

for (s in sex_groups) {
  
  df_sex <- df %>% filter(sex == s)
  
  for (y in outcomes) {
    for (x in exposures) {
      
      model <- models[[model_id]]
      
      coef_summary <- summary(model)$coefficients
      
      beta  <- coef_summary[x, "Estimate"]
      se    <- coef_summary[x, "Std. Error"]
      pval  <- coef_summary[x, "Pr(>|z|)"]
      
      OR  <- exp(beta)
      CI_low  <- exp(beta - 1.96 * se)
      CI_high <- exp(beta + 1.96 * se)
      
      null_model <- glm(as.formula(paste(y, "~ 1")),
                        data = df_sex,
                        family = binomial)
      
      loglik_full <- as.numeric(logLik(model))
      loglik_null <- as.numeric(logLik(null_model))
      
      McFadden_R2 <- 1 - (loglik_full / loglik_null)
      
      LR_stat <- 2 * (loglik_full - loglik_null)
      
      df_diff <- attr(logLik(model), "df") -
        attr(logLik(null_model), "df")
      
      model_p <- pchisq(LR_stat,
                        df=df_diff,
                        lower.tail=FALSE)
      
      results_table <- rbind(
        results_table,
        data.frame(
          Sex = s,
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
}

cat("\n================ 综合模型结果表 ================\n")
print(results_table)


############################################################
# 排序变量（关键修改）
############################################################

results_table$Sex <- factor(results_table$Sex,
                            levels=c("Female","Male"))

results_table <- results_table %>%
  arrange(Exposure, Outcome, Sex)

############################################################
# 森林图（合并左侧标签）
############################################################

plot_data <- results_table

plot_data$Exposure_clean <- toupper(gsub("_z","",plot_data$Exposure))

plot_data <- plot_data %>%
  arrange(Exposure, Outcome, Sex)

plot_data$RowID <- factor(
  1:nrow(plot_data),
  levels = rev(1:nrow(plot_data))
)

############################################################
# 创建合并标签
############################################################

labels <- plot_data$Exposure_clean

for(i in seq_along(labels)){
  
  if(i %% 4 != 1){
    labels[i] <- ""
  }
}

labels <- rev(labels)

############################################################
# 绘图
############################################################

ggplot(plot_data,
       aes(x = OR,
           y = RowID,
           color = Outcome,
           shape = Sex)) +
  
  geom_point(size = 2.2) +
  
  geom_errorbar(aes(xmin = CI_2.5,
                    xmax = CI_97.5),
                width = 0,
                linewidth = 0.5) +
  
  geom_vline(xintercept = 1,
             linetype = "dashed",
             linewidth = 0.5) +
  
  scale_y_discrete(labels = labels) +
  
  scale_color_manual(values = c(
    depression = "#0072B2",
    anxiety    = "#D55E00"
  )) +
  
  scale_shape_manual(values = c(
    Female = 16,
    Male   = 17
  )) +
  
  labs(
    x = "Odds Ratio (95% CI, per 1 SD increase)",
    y = "",
    color = "Outcome",
    shape = "Sex"
  ) +
  
  theme_classic(base_size = 16) +
  
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "right",
    axis.text.y = element_text(size = 13)
  )

############################################################
# SECOND FOREST PLOT
# WHR / WHtR per 0.1 increase
############################################################

############################################################
# 创建新的变量（仅WHR和WHtR缩放）
############################################################

df$whr_01  <- df$whr  * 10
df$whtr_01 <- df$whtr * 10

exposures_01 <- c("bmi","wc","hc","nc","whr_01","whtr_01")

############################################################
# 重新运行模型
############################################################

models_01 <- list()
model_id <- 1

for (s in sex_groups) {
  
  df_sex <- df %>% filter(sex == s)
  
  for (y in outcomes) {
    for (x in exposures_01) {
      
      formula_text <- paste(y, "~", x, "+", covariates)
      
      model <- glm(as.formula(formula_text),
                   data = df_sex,
                   family = binomial)
      
      models_01[[model_id]] <- model
      
      model_id <- model_id + 1
      
    }
  }
}

############################################################
# 生成结果表
############################################################

results_table_01 <- data.frame()

model_id <- 1

for (s in sex_groups) {
  
  df_sex <- df %>% filter(sex == s)
  
  for (y in outcomes) {
    for (x in exposures_01) {
      
      model <- models_01[[model_id]]
      
      coef_summary <- summary(model)$coefficients
      
      beta  <- coef_summary[x, "Estimate"]
      se    <- coef_summary[x, "Std. Error"]
      pval  <- coef_summary[x, "Pr(>|z|)"]
      
      OR  <- exp(beta)
      CI_low  <- exp(beta - 1.96 * se)
      CI_high <- exp(beta + 1.96 * se)
      
      results_table_01 <- rbind(
        results_table_01,
        data.frame(
          Sex = s,
          Outcome = y,
          Exposure = x,
          OR = OR,
          CI_2.5 = CI_low,
          CI_97.5 = CI_high,
          P_value = pval
        )
      )
      
      model_id <- model_id + 1
      
    }
  }
}

############################################################
# 整理标签
############################################################

results_table_01$Exposure <- recode(
  results_table_01$Exposure,
  whr_01  = "WHR (per 0.1)",
  whtr_01 = "WHtR (per 0.1)",
  bmi  = "BMI",
  wc   = "WC",
  hc   = "HC",
  nc   = "NC"
)

results_table_01$Sex <- factor(results_table_01$Sex,
                               levels=c("Female","Male"))

results_table_01 <- results_table_01 %>%
  arrange(Exposure, Outcome, Sex)

############################################################
# 绘制森林图
############################################################

plot_data <- results_table_01

plot_data$RowID <- factor(
  1:nrow(plot_data),
  levels = rev(1:nrow(plot_data))
)

ggplot(plot_data,
       aes(x = OR,
           y = RowID,
           color = Outcome,
           shape = Sex)) +
  
  geom_point(size = 2.2) +
  
  geom_errorbar(aes(xmin = CI_2.5,
                    xmax = CI_97.5),
                width = 0,
                linewidth = 0.5) +
  
  geom_vline(xintercept = 1,
             linetype = "dashed",
             linewidth = 0.5) +
  
  scale_y_discrete(labels = labels) +
  
  scale_color_manual(values = c(
    depression = "#0072B2",
    anxiety    = "#D55E00"
  )) +
  
  scale_shape_manual(values = c(
    Female = 16,
    Male   = 17
  )) +
  
  labs(
    x = "Odds Ratio (95% CI, WHR / WHtR per 0.1 increase)",
    y = "",
    color = "Outcome",
    shape = "Sex"
  ) +
  
  theme_classic(base_size = 16)














############################################################
# RCS 分析（按 sex 分层 + 原始变量）
############################################################

library(rms)

sex_groups <- c("Female","Male")

exposures <- c("bmi","whr","wc","nc")

outcomes <- c("depression","anxiety")

adjust_vars <- paste(
  c("age","race","marriage","occupation","education",
    "healthinsurance","revenue",
    "disease_cat4",
    "premature_birth",
    "workload","alcohol_use","tea_use","coffee_use",
    "weight_change","body_control",
    "divorce_separation","spouse_death","family_death",
    "family_serious_illness","natural_disaster",
    "job_loss","violence_exposure","financial_loss",
    "unexpected_financial_gain","major_positive_event"),
  collapse=" + "
)

############################################################
# datadist 只运行一次（关键修改）
############################################################

dd <- datadist(df)
options(datadist="dd")

models_rcs  <- list()
results_rcs <- data.frame()

model_id <- 1

for (s in sex_groups) {
  
  df_sex <- df %>% filter(sex == s)
  
  for (y in outcomes) {
    for (x in exposures) {
      
      cat("\n==============================\n")
      cat("Sex:",s,"| Outcome:", y, "| Exposure:", x, "\n")
      cat("==============================\n")
      
      formula_text <- paste0(
        y," ~ rcs(",x,",4) + ",adjust_vars
      )
      
      model <- lrm(as.formula(formula_text), data=df_sex)
      
      print(model)
      
      models_rcs[[model_id]] <- model
      
      ####################################################
      # 预测曲线
      ####################################################
      
      pred <- do.call(
        Predict,
        list(model, as.name(x), fun = plogis)
      )
      
      plot(
        pred,
        xlab = toupper(x),
        ylab = paste("Predicted Probability of",y),
        col = "#6BAED6",
        col.fill = "#9ECAE1",
        lwd = 3
      )
      
      ####################################################
      # 提取统计量
      ####################################################
      
      a <- anova(model)
      
      results_rcs <- rbind(
        results_rcs,
        data.frame(
          Sex         = s,
          Outcome     = y,
          Exposure    = x,
          Overall_P   = a[1,"P"],
          Nonlinear_P = ifelse(nrow(a)>1,a[2,"P"],NA),
          C_index     = model$stats["C"]
        )
      )
      
      model_id <- model_id + 1
      
    }
  }
}


############################################################
# 寻找 turning point
############################################################

find_turning_point <- function(x,y){
  
  dy <- diff(y)
  sign_change <- diff(sign(dy))
  
  idx <- which(sign_change != 0)
  
  if(length(idx)==0){
    return(NA)
  }
  
  return(x[idx[1]+1])
}

############################################################
# 输出结果
############################################################

results_rcs


library(mgcv)
library(ggplot2)
library(dplyr)

exposures <- c("bmi","wc","nc","whr")

outcomes <- c("depression","anxiety")

sex_groups <- c("Female","Male")

adjust_vars <- c(
  "age","race","marriage","occupation","education",
  "healthinsurance","revenue",
  "disease_cat4",
  "premature_birth",
  "workload","alcohol_use","tea_use","coffee_use",
  "weight_change","body_control",
  "divorce_separation","spouse_death","family_death",
  "family_serious_illness","natural_disaster",
  "job_loss","violence_exposure","financial_loss",
  "unexpected_financial_gain","major_positive_event"
)

for(x in exposures){
  
  plot_df <- data.frame()
  
  for(s in sex_groups){
    
    df_sex <- df %>% filter(sex==s)
    
    formula_dep <- as.formula(
      paste("depression ~ s(",x,",k=5) +",
            paste(adjust_vars,collapse=" + "))
    )
    
    formula_anx <- as.formula(
      paste("anxiety ~ s(",x,",k=5) +",
            paste(adjust_vars,collapse=" + "))
    )
    
    gam_dep <- gam(formula_dep,data=df_sex,family=binomial,method="REML")
    gam_anx <- gam(formula_anx,data=df_sex,family=binomial,method="REML")
    
    x_grid <- seq(min(df_sex[[x]],na.rm=TRUE),
                  max(df_sex[[x]],na.rm=TRUE),
                  length.out=200)
    
    newdata <- df_sex[rep(1,length(x_grid)),]
    
    newdata[[x]] <- x_grid
    newdata$age <- mean(df_sex$age,na.rm=TRUE)
    
    p_dep <- predict(gam_dep,newdata=newdata,type="response")
    p_anx <- predict(gam_anx,newdata=newdata,type="response")
    
    plot_df <- rbind(
      plot_df,
      data.frame(x=x_grid,prob=p_dep,sex=s,outcome="Depression"),
      data.frame(x=x_grid,prob=p_anx,sex=s,outcome="Anxiety")
    )
  }
  
  print(
    ggplot(plot_df,
           aes(x=x,
               y=prob,
               color=outcome,
               linetype=sex))+
      geom_line(size=1.3)+
      scale_color_manual(values=c(
        Depression="#0072B2",
        Anxiety="#D55E00"
      ))+
      labs(
        x=toupper(x),
        y="Predicted Probability",
        color="Outcome",
        linetype="Sex"
      )+
      theme_classic(base_size=16)
  )
}








find_turning_point <- function(x,y){
  
  idx <- which.min(y)
  
  return(x[idx])
}

############################################################
# WHR 四线图 + turning point
############################################################

library(mgcv)
library(dplyr)
library(ggplot2)

x <- "whr"

plot_df <- data.frame()
turn_df <- data.frame()

for(s in sex_groups){
  
  df_sex <- df %>% filter(sex==s)
  
  ################################################
  # 模型
  ################################################
  
  formula_dep <- as.formula(
    paste("depression ~ s(",x,",k=5) +",
          paste(adjust_vars,collapse=" + "))
  )
  
  formula_anx <- as.formula(
    paste("anxiety ~ s(",x,",k=5) +",
          paste(adjust_vars,collapse=" + "))
  )
  
  gam_dep <- gam(formula_dep,data=df_sex,family=binomial)
  gam_anx <- gam(formula_anx,data=df_sex,family=binomial)
  
  ################################################
  # 生成预测
  ################################################
  
  x_grid <- seq(min(df_sex[[x]],na.rm=TRUE),
                max(df_sex[[x]],na.rm=TRUE),
                length.out=200)
  
  newdata <- df_sex[rep(1,length(x_grid)),]
  newdata[[x]] <- x_grid
  
  p_dep <- predict(gam_dep,newdata=newdata,type="response")
  p_anx <- predict(gam_anx,newdata=newdata,type="response")
  
  ################################################
  # turning point
  ################################################
  
  tp_dep <- find_turning_point(x_grid,p_dep)
  tp_anx <- find_turning_point(x_grid,p_anx)
  
  turn_df <- rbind(
    turn_df,
    data.frame(x=tp_dep,outcome="Depression",sex=s),
    data.frame(x=tp_anx,outcome="Anxiety",sex=s)
  )
  
  ################################################
  # 保存曲线
  ################################################
  
  plot_df <- rbind(
    plot_df,
    data.frame(x=x_grid,prob=p_dep,sex=s,outcome="Depression"),
    data.frame(x=x_grid,prob=p_anx,sex=s,outcome="Anxiety")
  )
}

############################################################
# 绘图
############################################################

p <- ggplot(plot_df,
            aes(x=x,
                y=prob,
                color=outcome,
                linetype=sex))+
  
  geom_line(size=1.4)+
  
  scale_color_manual(values=c(
    Depression="#0072B2",
    Anxiety="#D55E00"
  ))+
  
  labs(
    x="WHR",
    y="Predicted Probability",
    color="Outcome",
    linetype="Sex"
  )+
  
  theme_classic(base_size=17)

############################################################
# 添加 turning point
############################################################

turn_df <- turn_df %>% filter(!is.na(x))

p <- p +
  geom_vline(data=turn_df,
             aes(xintercept=x,color=outcome),
             linetype="dashed",
             linewidth=0.9)+
  
  geom_text(data=turn_df,
            aes(x=x,
                y=max(plot_df$prob)*0.9,
                label=round(x,2),
                color=outcome),
            angle=90,
            vjust=-0.3,
            size=5)

print(p)











############################################################
# BMI vs WHR comparison (Z-score, limited to ±3 SD)
############################################################

library(mgcv)
library(ggplot2)
library(dplyr)

exposures <- c("bmi_z","whr_z")
outcomes  <- c("depression","anxiety")
sex_groups <- c("Female","Male")

for(s in sex_groups){
  
  plot_df <- data.frame()
  
  df_sex <- df %>% filter(sex == s)
  
  ############################################################
  # 限制横轴范围 ±3 SD
  ############################################################
  
  x_range <- c(-3,3)
  
  for(x in exposures){
    for(y in outcomes){
      
      formula_text <- as.formula(
        paste(y,"~ s(",x,",k=5) +",
              paste(adjust_vars,collapse=" + "))
      )
      
      model <- gam(formula_text,
                   data=df_sex,
                   family=binomial,
                   method="REML")
      
      ############################################################
      # 统一 x 轴
      ############################################################
      
      x_grid <- seq(x_range[1],
                    x_range[2],
                    length.out=200)
      
      newdata <- df_sex[rep(1,length(x_grid)),]
      newdata[[x]] <- x_grid
      
      pred <- predict(model,newdata=newdata,type="response")
      
      plot_df <- rbind(
        plot_df,
        data.frame(
          x = x_grid,
          prob = pred,
          outcome = y,
          exposure = x
        )
      )
      
    }
  }
  
  ############################################################
  # 绘图
  ############################################################
  
  p <- ggplot(plot_df,
              aes(x=x,
                  y=prob,
                  color=outcome,
                  linetype=exposure))+
    
    geom_line(linewidth=0.9)+
    
    scale_color_manual(values=c(
      depression="#0072B2",
      anxiety="#D55E00"
    ))+
    
    scale_linetype_manual(values=c(
      bmi_z="solid",
      whr_z="dashed"
    ))+
    
    coord_cartesian(xlim=x_range)+
    
    labs(
      title=paste("BMI vs WHR association with mental health (",s,")"),
      x="Standardized adiposity (Z-score)",
      y="Predicted probability",
      color="Outcome",
      linetype="Exposure"
    )+
    
    theme_classic(base_size=17)
  
  print(p)
}







############################################################
# Female BMI vs WHR scatter plot
############################################################

library(ggplot2)
library(dplyr)

df_female <- df %>% filter(sex=="Female")

############################################################
# 计算相关系数
############################################################

cor_val <- cor(df_female$bmi,
               df_female$whr,
               use="complete.obs")

############################################################
# 绘图
############################################################

p <- ggplot(df_female,
            aes(x=bmi,
                y=whr))+
  
  geom_point(
    color="#6BAED6",
    alpha=0.5,
    size=2
  )+
  
  geom_smooth(
    method="lm",
    color="darkblue",
    linewidth=0.8
  )+
  
  labs(
    title="Relationship between BMI and WHR in females",
    x="BMI",
    y="WHR"
  )+
  
  annotate(
    "text",
    x=min(df_female$bmi,na.rm=TRUE),
    y=max(df_female$whr,na.rm=TRUE),
    label=paste0("r = ",round(cor_val,2)),
    hjust=0,
    size=6
  )+
  
  theme_classic(base_size=17)

print(p)







############################################################
# Marginal effects: BMI vs WHR
############################################################

library(margins)
library(ggplot2)
library(dplyr)

exposures <- c("bmi_z","whr_z")
outcomes  <- c("depression","anxiety")
sex_groups <- c("Female","Male")

me_table <- data.frame()

for(s in sex_groups){
  
  df_sex <- df %>% filter(sex == s)
  
  for(y in outcomes){
    for(x in exposures){
      
      formula_text <- paste(y, "~", x, "+", covariates)
      
      model <- glm(as.formula(formula_text),
                   data=df_sex,
                   family=binomial)
      
      ############################################################
      # 计算 Average Marginal Effect
      ############################################################
      
      me <- margins(model, variables=x)
      
      summary_me <- summary(me)
      
      me_table <- rbind(
        me_table,
        data.frame(
          Sex = s,
          Outcome = y,
          Exposure = x,
          AME = summary_me$AME,
          SE = summary_me$SE,
          lower = summary_me$AME - 1.96*summary_me$SE,
          upper = summary_me$AME + 1.96*summary_me$SE
        )
      )
      
    }
  }
}

############################################################
# 整理变量名
############################################################

me_table$Exposure <- ifelse(me_table$Exposure=="bmi_z","BMI","WHR")

me_table$Outcome <- factor(me_table$Outcome,
                           levels=c("depression","anxiety"))

############################################################
# 绘图
############################################################

p <- ggplot(me_table,
            aes(x=Exposure,
                y=AME,
                color=Outcome,
                shape=Sex))+
  
  geom_point(size=4,
             position=position_dodge(width=0.4))+
  
  geom_errorbar(aes(ymin=lower,ymax=upper),
                width=0.1,
                position=position_dodge(width=0.4))+
  
  scale_color_manual(values=c(
    depression="#0072B2",
    anxiety="#D55E00"
  ))+
  
  labs(
    x="Adiposity indicator",
    y="Average marginal effect\n(change in probability per 1 SD increase)",
    color="Outcome",
    shape="Sex"
  )+
  
  theme_classic(base_size=17)

print(p)






############################################################
# MER (Marginal effect at representative values)
# BMI vs WHR comparison
############################################################

library(mgcv)
library(ggplot2)
library(dplyr)

exposures <- c("bmi_z","whr_z")
outcomes  <- c("depression","anxiety")
sex_groups <- c("Female","Male")

############################################################
# 构造 representative individual
############################################################

rep_values <- df %>%
  summarise(across(
    everything(),
    ~ if(is.numeric(.)) mean(.,na.rm=TRUE) else names(sort(table(.),decreasing=TRUE))[1]
  ))

############################################################
# MER plotting
############################################################

for(s in sex_groups){
  
  plot_df <- data.frame()
  
  df_sex <- df %>% filter(sex==s)
  
  for(x in exposures){
    for(y in outcomes){
      
      ####################################################
      # GAM model
      ####################################################
      
      formula_text <- as.formula(
        paste(y,"~ s(",x,",k=5) +",
              paste(adjust_vars,collapse=" + "))
      )
      
      model <- gam(
        formula_text,
        data=df_sex,
        family=binomial,
        method="REML"
      )
      
      ####################################################
      # ±3 SD range
      ####################################################
      
      x_sd  <- sd(df_sex[[x]],na.rm=TRUE)
      
      x_grid <- seq(-3*x_sd,3*x_sd,length.out=200)
      
      ####################################################
      # 构造 representative dataset
      ####################################################
      
      newdata <- rep_values[rep(1,length(x_grid)),]
      
      newdata$sex <- s
      newdata[[x]] <- x_grid
      
      ####################################################
      # 预测概率
      ####################################################
      
      pred <- predict(model,newdata=newdata,type="response")
      
      plot_df <- rbind(
        plot_df,
        data.frame(
          x = x_grid,
          prob = pred,
          outcome = y,
          exposure = x
        )
      )
    }
  }
  
  ############################################################
  # 绘图
  ############################################################
  
  p <- ggplot(
    plot_df,
    aes(x=x,
        y=prob,
        color=outcome,
        linetype=exposure)
  ) +
    
    geom_line(size=1.1) +
    
    scale_color_manual(values=c(
      depression="#1F78B4",
      anxiety="#E66101"
    )) +
    
    scale_linetype_manual(values=c(
      bmi_z="solid",
      whr_z="dashed"
    )) +
    
    labs(
      title=paste("MER of adiposity indicators on mental health (",s,")"),
      x="Adiposity indicator (Z-score, ±3 SD)",
      y="Predicted probability",
      color="Outcome",
      linetype="Indicator"
    ) +
    
    theme_classic(base_size=17)
  
  print(p)
  
}