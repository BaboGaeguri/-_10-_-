install.packages("corrplot")
install.packages("MASS")
install.packages("leaps")

df <- longley

# =============================
# 0. 데이터 설정
# =============================
# longley 데이터를 df라는 이름으로 사용한다고 가정
df <- longley

# =============================
# 1. 기초 데이터 분석
# =============================
summary(df)           # 평균, 중앙값, 4분위 등 요약 통계 확인
str(df)               # 자료형 확인
names(df)             # 열 이름 확인

colSums(is.na(df))    # 결측치 확인

# 반응변수(Employed) 박스플롯
par(mfrow = c(1, 1))
boxplot(df$Employed,
        main = "Employed 분포 (반응변수)",
        xlab = "Employed",
        col = "lightblue")

# 설명변수 박스플롯
par(mfrow = c(2, 3))
boxplot(df$GNP.deflator,
        main = "GNP.deflator 분포",
        xlab = "GNP.deflator",
        col = "lightpink")

boxplot(df$GNP,
        main = "GNP 분포",
        xlab = "GNP",
        col = "lightyellow")

boxplot(df$Unemployed,
        main = "Unemployed 분포",
        xlab = "Unemployed",
        col = "lightcoral")

boxplot(df$Armed.Forces,
        main = "Armed.Forces 분포",
        xlab = "Armed.Forces",
        col = "lavender")

boxplot(df$Population,
        main = "Population 분포",
        xlab = "Population",
        col = "lightcyan")

boxplot(df$Year,
        main = "Year 분포",
        xlab = "Year",
        col = "lightgreen")

par(mfrow = c(1, 1))

# =============================
# 2. 선형성 파악
# =============================
par(mfrow = c(2, 3))

# 1. GNP.deflator
plot(df$GNP.deflator, df$Employed,
     main = "GNP.deflator vs Employed",
     xlab = "GNP.deflator",
     ylab = "Employed",
     pch = 19, col = "blue")
abline(lm(Employed ~ GNP.deflator, data = df),
       col = "red", lwd = 2)

# 2. GNP
plot(df$GNP, df$Employed,
     main = "GNP vs Employed",
     xlab = "GNP",
     ylab = "Employed",
     pch = 19, col = "red")
abline(lm(Employed ~ GNP, data = df),
       col = "red", lwd = 2)

# 3. Unemployed
plot(df$Unemployed, df$Employed,
     main = "Unemployed vs Employed",
     xlab = "Unemployed",
     ylab = "Employed",
     pch = 19, col = "green")
abline(lm(Employed ~ Unemployed, data = df),
       col = "red", lwd = 2)

# 4. Armed.Forces
plot(df$Armed.Forces, df$Employed,
     main = "Armed.Forces vs Employed",
     xlab = "Armed.Forces",
     ylab = "Employed",
     pch = 19, col = "purple")
abline(lm(Employed ~ Armed.Forces, data = df),
       col = "red", lwd = 2)

# 5. Population
plot(df$Population, df$Employed,
     main = "Population vs Employed",
     xlab = "Population",
     ylab = "Employed",
     pch = 19, col = "orange")
abline(lm(Employed ~ Population, data = df),
       col = "red", lwd = 2)

# 6. Year
plot(df$Year, df$Employed,
     main = "Year vs Employed",
     xlab = "Year",
     ylab = "Employed",
     pch = 19, col = "brown")
abline(lm(Employed ~ Year, data = df),
       col = "red", lwd = 2)

par(mfrow = c(1, 1))

# =============================
# 3. 상관계수 검정
# =============================

cor.test(df$Employed, df$GNP.deflator)  # Employed vs GNP.deflator
cor.test(df$Employed, df$GNP)          # Employed vs GNP
cor.test(df$Employed, df$Unemployed)   # Employed vs Unemployed
cor.test(df$Employed, df$Armed.Forces) # Employed vs Armed.Forces
cor.test(df$Employed, df$Population)   # Employed vs Population
cor.test(df$Employed, df$Year)         # Employed vs Year

# (a) Employed를 반응변수로 하고, 나머지는 설명변수로 하여 다중회귀모형을 구하시오.
model1 <- lm(Employed ~ GNP + Unemployed + Armed.Forces + Year, data = longley)
summary(model1)

# (b) 설명변수 간의 상관계수를 구하시오.
library(corrplot)
X <- df[, c("GNP.deflator", "GNP", "Unemployed", "Armed.Forces", "Population", "Year")]
cor_mat <- cor(X)
corrplot(cor_mat, method = "color", addCoef.col = "black")

# (c) 각 설명변수에 대해 VIF를 구하고 해석하시오.
library(car)
vif(model1)

# (d) 변수선택법을 적용하여 최종 모형을 제안하시오.
full_model <- lm(Employed ~ GNP.deflator + GNP + Unemployed + Armed.Forces + Population + Year, data = longley)
null_model <- lm(Employed ~ 1, data = longley)
# 1) 전진선택법
forward_model <- step(null_model,
                      scope = list(lower = null_model, upper = full_model),
                      direction = "forward",
                      trace = TRUE)
summary(forward_model)
# 2) 후진소거법
backward_model <- step(full_model,
                       direction = "backward",
                       trace = TRUE)
summary(backward_model)
# 3) 단계적 방법
library(MASS)
step <- stepAIC(full_model, direction = "both")
step$anova
# 4) 모든 가능한 회귀
library(leaps)
all_subsets <- regsubsets(Employed ~ GNP.deflator + GNP + Unemployed + Armed.Forces + Population + Year, 
                        data = longley,
                          nbest = 1,   # 각 개수별로 가장 좋은 모형 1개
                          nvmax = 6)   # 최대 변수 개수(설명변수 6개)

subsum <- summary(all_subsets)
subsum
subsum$adjr2 # adj R²함께 비교
# 5) Cp
x <- as.matrix(df[, c("GNP.deflator", "GNP", "Unemployed", "Armed.Forces", "Population", "Year")])
y <- longley$Employed
lp <- leaps(x,y, method="Cp")
lp

cp_vals  <- lp$Cp
sizes    <- lp$size
valid_idx <- which(sizes != max(sizes)) # 풀모델 제외한 인덱스 찾기
dist_vals <- abs(cp_vals[valid_idx] - sizes[valid_idx]) # Cp=p 거리 계산 (풀모델 제외)
remove_idx <- order(dist_vals, decreasing = TRUE)[1:2] # dist_vals가 큰 순서대로 10개 제외
remaining_idx <- valid_idx[-remove_idx] # 제거된 나머지 모델 index
best5_idx <- remaining_idx[order(abs(cp_vals[remaining_idx] - sizes[remaining_idx]))[1:5]] # 남은 모델들 중 Cp=p 선에 가장 가까운 5개 선택
# 시각화
plot(sizes[remaining_idx], cp_vals[remaining_idx],
     xlab = "Number of predictors (p)",
     ylab = "Mallows Cp",
     main = "Models & Cp = p line (y = x)",
     pch = 19, col = "gray60", xlim = c(0, max(sizes)), ylim = c(0, 10))
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)
# best5 강조
points(sizes[best5_idx], cp_vals[best5_idx],
       pch = 19, col = "blue", cex = 1.8)
text(sizes[best5_idx], cp_vals[best5_idx],
     labels = paste0("p = ", sizes[best5_idx]),
     pos = 3, cex = 0.9, col = "blue")
# 모델과 설명력 함께 출력
var_names <- colnames(x)
for (i in best5_idx) {
  included <- lp$which[i, ]
  selected_vars <- var_names[included]
  formula_str <- paste("Employed ~", paste(selected_vars, collapse = " + "))
  model_formula <- as.formula(formula_str)
  fit <- lm(model_formula, data = df)
  adjR2 <- summary(fit)$adj.r.squared
  cat("\n● Model index:", i,
      "\n   p =", sizes[i],
      "\n   Variables:", paste(selected_vars, collapse=", "),
      "\n   Adjusted R²:", round(adjR2, 5), "\n")
}

# (e) GNP에 대한 회귀계수가 유의한지 유의수준 5%에서 검정하시오.
# t-test로
fit_full <- lm(Employed ~ GNP.deflator + GNP + Unemployed +
                 Armed.Forces + Population + Year,
               data = df)

summary(fit_full)$coefficients["GNP", ]
# F-test로
fit_reduced <- lm(Employed ~ GNP.deflator + Unemployed +
                    Armed.Forces + Population + Year,
                  data = df)

anova(fit_reduced, fit_full)

# (f) Unemployed와 Armed Forces에 대한 회귀계수가 모두 0인 귀무가설에 대해 유의수준 5%에서 검정하시오.
fit_full <- lm(Employed ~ GNP.deflator + GNP + Unemployed +
                 Armed.Forces + Population + Year,
               data = df)
fit_reduced <- lm(Employed ~ GNP.deflator + GNP + Population + Year,
                  data = df)

anova(fit_reduced, fit_full)
