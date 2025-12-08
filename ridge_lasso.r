library(readxl) # readxl 라이브러리 설치

df <- read_excel("C:/Users/김상희/OneDrive/바탕 화면/수업자료/빅융/2. 응용회귀분석/교수님 제공 코드/회귀분석_데이터/table4.6_fighter.xlsx")
print(df , n=25)

# 반응변수
# FFD(calculated First Fight Day) = 1940년 1월 이후의 첫 비행일(in month)

## 설명변수(Technological Parameters)
# 1) COMBAT: 속도 성능과 기동 성능을 나타내는 변수
# SRP(Specific Power) = 비출력, 단위 무게당 전력에 비례(= Thrust / Weight, 전투기의 가속 성능, 전반적 동력 성능을 나타내는 대표 지표.)
# SLF(Sustained Load Factor) = 지속된 하중의 계수(전투기가 선회·기동 중 유지할 수 있는 지속 G 값)

# 2) CRUISE: 항속거리와 탑재능력을 나타내는 변수
# RGF(Breguet Range Factor) = 비행 범위 인자 (비행 범위 계수)(“순수 기술 기반 항속 효율”을 평가하기 위해 만든 무차원 계수)
# PLF(Payload Fraction) = 비행기의 총 중량 중 유효 탑재량(전투기의 총중량 대비 임무 탑재량 비율)

# 3) OTHER: 특정 설계 또는 임무 특성을 나타내는 기타 변수
# CAR(Carrier Capability) = 비행기가 캐리어(carrier)에 착륙할 수 있으면 1, 아니면 0


# =============================
# 1. 기초 데이터 분석
# =============================
summary(df) #평균, 중앙값, 4분위 확인
str(df) # 자료형 확인
names(df) # 열이름 확인

colSums(is.na(df)) #결측치 확인

# 반응변수(Species) 박스플롯
par(mfrow = c(1, 1))
boxplot(df$FFD, main="FFD 분포 (반응변수)", xlab="FFD", col="lightblue")

# 설명변수 박스플롯
par(mfrow = c(2, 3))
boxplot(df$SPR, main="SPR 분포", xlab="SPR", col="lightpink")
boxplot(df$RGF, main="RGF 분포", xlab="RGF", col="lightyellow")
boxplot(df$PLF, main="PLF 분포", xlab="PLF", col="lightcoral")
boxplot(df$SLF, main="SLF 분포", xlab="SLF", col="lavender")
boxplot(df$CAR, main="CAR 분포", xlab="CAR", col="lightcyan")
par(mfrow = c(1, 1))

# =============================
# 2. 선형성 파악
# =============================
# 2-1. FFD의 선형성 파악
# FFD를 반응변수로 하는 산점도 플롯
par(mfrow = c(2, 3))

# 1) SPR
plot(df$SPR, df$FFD,
     main = "SPR vs FFD",
     xlab = "SPR",
     ylab = "FFD",
     pch = 19, col = "blue")
abline(lm(FFD ~ SPR, data = df), col = "red", lwd = 2)

# 2) RGF
plot(df$RGF, df$FFD,
     main = "RGF vs FFD",
     xlab = "RGF",
     ylab = "FFD",
     pch = 19, col = "red")
abline(lm(FFD ~ RGF, data = df), col = "red", lwd = 2)

# 3) PLF
plot(df$PLF, df$FFD,
     main = "PLF vs FFD",
     xlab = "PLF",
     ylab = "FFD",
     pch = 19, col = "green")
abline(lm(FFD ~ PLF, data = df), col = "red", lwd = 2)

# 4) SLF
plot(df$SLF, df$FFD,
     main = "SLF vs FFD",
     xlab = "SLF",
     ylab = "FFD",
     pch = 19, col = "purple")
abline(lm(FFD ~ SLF, data = df), col = "red", lwd = 2)

# 5) CAR
plot(df$CAR, df$FFD,
     main = "CAR vs FFD",
     xlab = "CAR",
     ylab = "FFD",
     pch = 19, col = "orange")
abline(lm(FFD ~ CAR, data = df), col = "red", lwd = 2)

par(mfrow = c(1, 1))

# 상관계수 검정 (FFD와 각 설명변수)
cor.test(df$FFD, df$SPR) # FFD vs SPR
cor.test(df$FFD, df$RGF) # FFD vs RGF
cor.test(df$FFD, df$PLF) # FFD vs PLF
cor.test(df$FFD, df$SLF) # FFD vs SLF
cor.test(df$FFD, df$CAR) # FFD vs CAR

# 2-2. 로그변환된 FFD의 선형성 파악
# 로그변환
df$log_FFD <- log(df$FFD)

# log(FFD)를 반응변수로 하는 산점도 플롯
par(mfrow = c(2, 3))

# 1) SPR
plot(df$SPR, df$log_FFD,
     main = "SPR vs log(FFD)",
     xlab = "SPR",
     ylab = "log(FFD)",
     pch = 19, col = "blue")
abline(lm(log_FFD ~ SPR, data = df), col = "red", lwd = 2)

# 2) RGF
plot(df$RGF, df$log_FFD,
     main = "RGF vs log(FFD)",
     xlab = "RGF",
     ylab = "log(FFD)",
     pch = 19, col = "red")
abline(lm(log_FFD ~ RGF, data = df), col = "red", lwd = 2)

# 3) PLF
plot(df$PLF, df$log_FFD,
     main = "PLF vs log(FFD)",
     xlab = "PLF",
     ylab = "log(FFD)",
     pch = 19, col = "green")
abline(lm(log_FFD ~ PLF, data = df), col = "red", lwd = 2)

# 4) SLF
plot(df$SLF, df$log_FFD,
     main = "SLF vs log(FFD)",
     xlab = "SLF",
     ylab = "log(FFD)",
     pch = 19, col = "purple")
abline(lm(log_FFD ~ SLF, data = df), col = "red", lwd = 2)

# 5) CAR
plot(df$CAR, df$log_FFD,
     main = "CAR vs log(FFD)",
     xlab = "CAR",
     ylab = "log(FFD)",
     pch = 19, col = "orange")
abline(lm(log_FFD ~ CAR, data = df), col = "red", lwd = 2)

par(mfrow = c(1, 1))

# 상관계수 검정 (log(FFD)와 각 설명변수)
cor.test(df$log_FFD, df$SPR) # log(FFD) vs SPR
cor.test(df$log_FFD, df$RGF) # log(FFD) vs RGF
cor.test(df$log_FFD, df$PLF) # log(FFD) vs PLF
cor.test(df$log_FFD, df$SLF) # log(FFD) vs SLF
cor.test(df$log_FFD, df$CAR) # log(FFD) vs CAR

#####################################
# 3. 다중회귀식 적합
#####################################
# 다중회귀식 적합(t-test, F-test, 설명력)
# 다중회귀모델 (FFD를 모든 설명변수로 예측)
model1 <- lm(log_FFD ~ SPR + RGF + PLF + SLF + CAR, data = df)
summary(model1)

#####################################
# 4. 다중공선성 판단
#####################################
# 1) 다중공선성 진단
# a. 설명변수들 간의 산점도 행렬
X <- df[, c("SPR", "RGF", "PLF", "SLF", "CAR")]
pairs(X, main = "설명변수 간 산점도 행렬", pch = 19, col = "blue")
# 설명변수들 간의 상관행렬에서 절댓값이 큰 상관계수를 갖는 경우
library(corrplot)
cor_mat <- cor(X)
corrplot(cor_mat, method = "color", addCoef.col = "black")
# b. j 번째 설명변수를 반응변수 위치에 놓고 나머지 설명변수들로 회귀모형을 적합했을 때 결정계수가 1에 가까운 경우
for (var in colnames(X)) {
  others <- colnames(X)[colnames(X) != var]
  formula_str <- paste(var, "~", paste(others, collapse = "+"))
  model_temp <- lm(as.formula(formula_str), data = X)
  cat("\n======================\n")
  cat("반응변수 =", var, "\n")
  print(summary(model_temp)$r.squared)
}
# c. 가장 작은 고유값이 30보다 큰 경우
# 공분산행렬의 고유값
X_matrix <- as.matrix(X)  # 데이터프레임을 숫자형 행렬로 변환
XtX <- crossprod(X_matrix)  # t(X) %*% X와 동일
eigen_vals_XtX <- eigen(XtX)$values
# 조건수 = sqrt(λ_max / λ_min)
condition_number <- sqrt(max(eigen_vals_XtX)) / sqrt(min(eigen_vals_XtX))
condition_number
# d. VIF
library(car)
vif(model1)

# 2) 다중공선성의 왜곡 방지를 위한 영향점 진단
# a. 지렛대 그림
h <- hat(model.matrix(model1))
plot(h, type = "h", xlab="case index", main="leverage plot")
# b. 쿡의 거리
cutoff <- 4/((nrow(df))-length(model1$coefficients)-2)
plot(model1, which=4, cook.levels=cutoff)
# c. DFFITS
dffits(model1)
# d. 영향점 시각화
influencePlot(model1, main="Influence Plot",
     sub="Circle size is proportial to Cook's Distance")
# e. 영향점 종합
# 진단하는 여러 통계량들 쫙 출력(직접 영향점인지 판단해야함)
ls.diag(model1)
# influence.measures = 모델 영향점과 관련된 지표를 계산해서 리스트 형태로 저장하는 함수
inflm.fit<-influence.measures(model1)
# 영향력이 있으면 별 표시
summary(inflm.fit)
inflm.fit
# 행(row) 단위로 그 관측치에서 하나라도 TRUE가 있으면 → TRUE, 전부 FALSE면 → FALSE
# which = True인 값들의 인덱스를 반환
which(apply(inflm.fit$is.inf, 1, any))

# 3) 다중공선성의 왜곡 방지를 위한 이상점 진단
# a. 이상점 확인
# a-1. 관측치(index) vs 스튜던트화 잔차 플롯
student_resid <- rstudent(model1)
plot(student_resid, pch="*", main="Studentized Residuals")
abline(h=c(-2, 0, 2), col=c("red", "black", "red"))
# a-2 스튜던트 잔차의 Q-Q plot
qqPlot(rstudent(model1))
# b. 이상점 검정
# b-1. 이상점 검정을 위한 오차항 정규분포 확인
# 스튜던트 잔차에 대한 shapiro-Wilks Test(그냥 잔차에 대한 정규성 검정보다 더 엄밀함)
student_resid <- rstudent(model1)
shapiro.test(student_resid)
# 스튜던트 잔차에 대한 히스토그램(정규성 시각화)
library(MASS)
hist(student_resid, freq=FALSE, main="Disturbution fo Studentized Residuals")
xfit<-seq(min(student_resid), max(student_resid), length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)
# b-3. 일정 기준을 넘는 애를 잡는 법
crit <- qt(1 - 0.05/2, df = model1$df.residual)
out_idx <- which(abs(student_resid) > crit)
out_idx
# b-4 가장 튀는 한 점 t-test로 검정
outlierTest(model1)

# 4) 영향점 및 이상점 제거 후 다중공선성 재판단
df_clean <- df[-c(3, 22), ]

# 제거 후 모델 재적합
model_clean <- lm(log_FFD ~ SPR + RGF + PLF + SLF + CAR, data = df_clean)
summary(model_clean)

# a. 설명변수들 간의 산점도 행렬
X_clean <- df_clean[, c("SPR", "RGF", "PLF", "SLF", "CAR")]
pairs(X_clean, main = "설명변수 간 산점도 행렬", pch = 19, col = "blue")

# b. 설명변수들 간의 상관행렬에서 절댓값이 큰 상관계수를 갖는 경우
library(corrplot)
cor_mat_clean <- cor(X_clean)
corrplot(cor_mat_clean, method = "color", addCoef.col = "black")

# c. j 번째 설명변수를 반응변수 위치에 놓고 나머지 설명변수들로 회귀모형을 적합했을 때 결정계수가 1에 가까운 경우
for (var in colnames(X_clean)) {
  others <- colnames(X_clean)[colnames(X_clean) != var]
  formula_str <- paste(var, "~", paste(others, collapse = "+"))
  model_temp <- lm(as.formula(formula_str), data = X_clean)
  cat("\n======================\n")
  cat("반응변수 =", var, "\n")
  print(summary(model_temp)$r.squared)
}

# d. 가장 작은 고유값이 30보다 큰 경우
# 공분산행렬의 고유값
X_clean_matrix <- as.matrix(X_clean)  # 데이터프레임을 숫자형 행렬로 변환
XtX_clean <- crossprod(X_clean_matrix)  # t(X) %*% X와 동일
eigen_vals_XtX_clean <- eigen(XtX_clean)$values
# 조건수 = sqrt(λ_max / λ_min)
condition_number_clean <- sqrt(max(eigen_vals_XtX_clean)) / sqrt(min(eigen_vals_XtX_clean))
condition_number_clean

# e. VIF (제거 후 모델 사용)
library(car)
vif(model_clean)

#####################################
# 5. 능형회귀 (Ridge Regression)
#####################################
library(MASS)
library(car)

model_ridge <- lm.ridge(log_FFD ~ SPR + RGF + PLF + SLF + CAR, 
data = df_clean, lambda = seq(0, 1, 0.001))

model_ridge$coef
plot(model_ridge)
select(model_ridge)

model_ridge_2 <- lm.ridge(log_FFD ~ SPR + RGF + PLF + SLF + CAR, 
data = df_clean, lambda = seq(0, 2, 0.001))

model_ridge_2$coef
plot(model_ridge_2)
select(model_ridge_2)

model_ridge_3 <- lm.ridge(log_FFD ~ SPR + RGF + PLF + SLF + CAR,
data = df_clean, lambda = 1)

#####################################
# 6. LASSO
#####################################
install.packages("lars")
library(lars)

# 1. 데이터 준비
X <- as.matrix(df_clean[, c("SPR", "RGF", "PLF", "SLF", "CAR")])
y <- df_clean$log_FFD

# 2. LASSO 적합
model_lasso <- lars(X, y, type="lasso", trace=TRUE)
plot(model_lasso)

# 3. Cp 기반 최적 step 선택
cp_vals <- model_lasso$Cp
best_step <- which.min(cp_vals)
best_step

# 4. 표준화 해제 후 최적 계수 추출
coef_matrix <- model_lasso$beta
coef_rescaled <- scale(coef_matrix, center=FALSE, 1/model_lasso$normx)
coef_lasso_best <- coef_rescaled[best_step, ]
coef_lasso_best

# 5. LASSO tuning parameter 계산
s1 <- apply(abs(coef1), 1, sum)
s1/max(s1)

# 6. 모델 점검
coef(model_lasso)
model_lasso$R2
model_lasso$RSS

#####################################
# 7. OLS vs Ridge vs LASSO 비교
#####################################

# 1. 계수 비교
# OLS 계수
ols_coef <- coef(model_clean)
cat("\nOLS 계수:\n")
print(ols_coef)

# Ridge 계수 (λ=1) - 절편 제외
ridge_coef_full <- coef(model_ridge_3)
ridge_coef <- ridge_coef_full  # 나중에 예측에 사용
cat("\nRidge 계수 (λ=1):\n")
print(ridge_coef_full)

# LASSO 계수 (최적 step)
cat("\nLASSO 계수 (best step):\n")
print(coef_lasso_best)

# 계수 비교표 (설명변수만)
# 차원 확인
cat("\n계수 차원 확인:\n")
cat("OLS 계수 (절편 제외):", length(ols_coef[-1]), "\n")
cat("Ridge 계수:", length(ridge_coef_full), "\n")
cat("LASSO 계수:", length(coef_lasso_best), "\n")

# Ridge 계수에서 절편 제거 (만약 6개면 첫 번째가 절편)
if (length(ridge_coef_full) == 6) {
  ridge_coef_only <- ridge_coef_full[-1]  # 절편 제외
} else {
  ridge_coef_only <- ridge_coef_full
}

coef_comparison <- data.frame(
  Variable = c("SPR", "RGF", "PLF", "SLF", "CAR"),
  OLS = as.numeric(ols_coef[-1]),
  Ridge = as.numeric(ridge_coef_only),
  LASSO = as.numeric(coef_lasso_best)
)
row.names(coef_comparison) <- NULL

cat("\n======================\n")
cat("계수 비교표 (절편 제외)\n")
print(coef_comparison)

# 2. 계수 시각화
par(mfrow = c(1, 1))
barplot(t(as.matrix(coef_comparison[, -1])),
        beside = TRUE,
        names.arg = coef_comparison$Variable,
        col = c("blue", "darkgreen", "coral"),
        legend.text = c("OLS", "Ridge (λ=1)", "LASSO"),
        main = "OLS vs Ridge vs LASSO 계수 비교",
        ylab = "계수 값",
        las = 2,
        cex.names = 0.8,
        args.legend = list(x = "topright"))
abline(h = 0, col = "black", lwd = 1)

# 3. 예측 성능 비교 (df_clean 데이터)
# OLS 예측
ols_pred_clean <- predict(model_clean)

# Ridge 예측
# lm.ridge는 중심화된 데이터로 작동하므로, 예측 시에도 중심화 필요
ridge_coef_numeric <- as.numeric(ridge_coef_only)
# X를 중심화 (모델 학습 시 사용된 평균으로)
X_means <- model_ridge_3$xm  # lm.ridge가 사용한 X의 평균
X_centered <- scale(X, center = X_means, scale = FALSE)
# 예측: y_mean + X_centered %*% coef
ridge_pred_clean <- as.vector(model_ridge_3$ym + X_centered %*% ridge_coef_numeric)

# LASSO 예측
lasso_pred_clean <- predict(model_lasso, newx = X, s = best_step, mode = "step")$fit

# MSE 계산
mse_ols_clean <- mean((df_clean$log_FFD - ols_pred_clean)^2)
mse_ridge_clean <- mean((df_clean$log_FFD - ridge_pred_clean)^2)
mse_lasso_clean <- mean((df_clean$log_FFD - lasso_pred_clean)^2)

cat("\n======================\n")
cat("예측 성능 비교 (df_clean)\n")
cat("======================\n")
cat("OLS MSE:", mse_ols_clean, "\n")
cat("Ridge (λ=1) MSE:", mse_ridge_clean, "\n")
cat("LASSO MSE:", mse_lasso_clean, "\n")

# R² 계산
r2_ols <- summary(model_clean)$r.squared
r2_ridge <- 1 - mse_ridge_clean / var(df_clean$log_FFD)
r2_lasso <- 1 - mse_lasso_clean / var(df_clean$log_FFD)

cat("\n======================\n")
cat("R² 비교\n")
cat("======================\n")
cat("OLS R²:", r2_ols, "\n")
cat("Ridge R²:", r2_ridge, "\n")
cat("LASSO R²:", r2_lasso, "\n")

# 4. 성능 비교 시각화
performance <- data.frame(
  Model = c("OLS", "Ridge", "LASSO"),
  MSE = c(mse_ols_clean, mse_ridge_clean, mse_lasso_clean),
  R2 = c(r2_ols, r2_ridge, r2_lasso)
)

par(mfrow = c(1, 2))

# MSE 비교
barplot(performance$MSE,
        names.arg = performance$Model,
        col = c("blue", "darkgreen", "coral"),
        main = "MSE 비교",
        ylab = "MSE")

# R² 비교
barplot(performance$R2,
        names.arg = performance$Model,
        col = c("blue", "darkgreen", "coral"),
        main = "R² 비교",
        ylab = "R²",
        ylim = c(0, 1))

par(mfrow = c(1, 1))

# 5. 예측값 vs 실제값 산점도
par(mfrow = c(1, 3))

plot(df_clean$log_FFD, ols_pred_clean,
     pch = 19, col = "blue",
     xlab = "실제값", ylab = "예측값",
     main = "OLS")
abline(0, 1, col = "red", lwd = 2)

plot(df_clean$log_FFD, ridge_pred_clean,
     pch = 19, col = "darkgreen",
     xlab = "실제값", ylab = "예측값",
     main = "Ridge (λ=1)")
abline(0, 1, col = "red", lwd = 2)

plot(df_clean$log_FFD, lasso_pred_clean,
     pch = 19, col = "coral",
     xlab = "실제값", ylab = "예측값",
     main = "LASSO")
abline(0, 1, col = "red", lwd = 2)

par(mfrow = c(1, 1))

#####################################
# 8. log-linear 쌍 비교
#####################################

# 4가지 변환 조합 비교 (영향점 제거 전 데이터 사용)
# 1) linear-linear: FFD ~ SPR + RGF + PLF + SLF + CAR
# 2) log-linear: log(FFD) ~ SPR + RGF + PLF + SLF + CAR
# 3) linear-log: FFD ~ log(SPR) + log(RGF) + log(PLF) + log(SLF) + CAR
# 4) log-log: log(FFD) ~ log(SPR) + log(RGF) + log(PLF) + log(SLF) + CAR

# 로그 변환 데이터 준비
# PLF에 0 값이 있으므로 log(x+0.001)을 사용 (작은 상수 추가)
df$log_SPR <- log(df$SPR)
df$log_RGF <- log(df$RGF)
df$log_PLF <- log(df$PLF + 0.001)  # 0 값 처리
df$log_SLF <- log(df$SLF)

# 1) linear-linear
model_linear_linear <- lm(FFD ~ SPR + RGF + PLF + SLF + CAR, data = df)

# 2) log-linear
model_log_linear <- lm(log_FFD ~ SPR + RGF + PLF + SLF + CAR, data = df)

# 3) linear-log
model_linear_log <- lm(FFD ~ log_SPR + log_RGF + log_PLF + log_SLF + CAR, data = df)

# 4) log-log
model_log_log <- lm(log_FFD ~ log_SPR + log_RGF + log_PLF + log_SLF + CAR, data = df)

# 모델 요약 비교
cat("\n======================\n")
cat("1) Linear-Linear 모델\n")
cat("======================\n")
print(summary(model_linear_linear))

cat("\n======================\n")
cat("2) Log-Linear 모델\n")
cat("======================\n")
print(summary(model_log_linear))

cat("\n======================\n")
cat("3) Linear-Log 모델\n")
cat("======================\n")
print(summary(model_linear_log))

cat("\n======================\n")
cat("4) Log-Log 모델\n")
cat("======================\n")
print(summary(model_log_log))

# 성능 비교표
comparison_table <- data.frame(
  Model = c("Linear-Linear", "Log-Linear", "Linear-Log", "Log-Log"),
  R_squared = c(
    summary(model_linear_linear)$r.squared,
    summary(model_log_linear)$r.squared,
    summary(model_linear_log)$r.squared,
    summary(model_log_log)$r.squared
  ),
  Adj_R_squared = c(
    summary(model_linear_linear)$adj.r.squared,
    summary(model_log_linear)$adj.r.squared,
    summary(model_linear_log)$adj.r.squared,
    summary(model_log_log)$adj.r.squared
  ),
  AIC = c(
    AIC(model_linear_linear),
    AIC(model_log_linear),
    AIC(model_linear_log),
    AIC(model_log_log)
  ),
  BIC = c(
    BIC(model_linear_linear),
    BIC(model_log_linear),
    BIC(model_linear_log),
    BIC(model_log_log)
  )
)

cat("\n======================\n")
cat("모델 성능 비교표\n")
cat("======================\n")
print(comparison_table)

#####################################
# 8-2. Ridge 회귀에서 4가지 변환 조합 비교
#####################################

library(MASS)

# 1) Ridge: linear-linear (FFD ~ SPR + RGF + PLF + SLF + CAR)
X_linear <- as.matrix(df[, c("SPR", "RGF", "PLF", "SLF", "CAR")])
y_linear <- df$FFD

ridge_linear_linear <- lm.ridge(FFD ~ SPR + RGF + PLF + SLF + CAR,
                                data = df, lambda = seq(0, 10, 0.1))
lambda_linear_linear <- which.min(ridge_linear_linear$GCV)
ridge_model_linear_linear <- lm.ridge(FFD ~ SPR + RGF + PLF + SLF + CAR,
                                      data = df, lambda = lambda_linear_linear * 0.1)

# 2) Ridge: log-linear (log(FFD) ~ SPR + RGF + PLF + SLF + CAR)
y_log <- df$log_FFD

ridge_log_linear <- lm.ridge(log_FFD ~ SPR + RGF + PLF + SLF + CAR,
                             data = df, lambda = seq(0, 10, 0.1))
lambda_log_linear <- which.min(ridge_log_linear$GCV)
ridge_model_log_linear <- lm.ridge(log_FFD ~ SPR + RGF + PLF + SLF + CAR,
                                   data = df, lambda = lambda_log_linear * 0.1)

# 3) Ridge: linear-log (FFD ~ log(SPR) + log(RGF) + log(PLF) + log(SLF) + CAR)
ridge_linear_log <- lm.ridge(FFD ~ log_SPR + log_RGF + log_PLF + log_SLF + CAR,
                             data = df, lambda = seq(0, 10, 0.1))
lambda_linear_log <- which.min(ridge_linear_log$GCV)
ridge_model_linear_log <- lm.ridge(FFD ~ log_SPR + log_RGF + log_PLF + log_SLF + CAR,
                                   data = df, lambda = lambda_linear_log * 0.1)

# 4) Ridge: log-log (log(FFD) ~ log(SPR) + log(RGF) + log(PLF) + log(SLF) + CAR)
ridge_log_log <- lm.ridge(log_FFD ~ log_SPR + log_RGF + log_PLF + log_SLF + CAR,
                          data = df, lambda = seq(0, 10, 0.1))
lambda_log_log <- which.min(ridge_log_log$GCV)
ridge_model_log_log <- lm.ridge(log_FFD ~ log_SPR + log_RGF + log_PLF + log_SLF + CAR,
                                data = df, lambda = lambda_log_log * 0.1)

cat("\n======================\n")
cat("Ridge 회귀: 4가지 변환 조합\n")
cat("======================\n")
cat("\n1) Ridge Linear-Linear (λ =", lambda_linear_linear * 0.1, ")\n")
print(coef(ridge_model_linear_linear))
cat("GCV:", ridge_model_linear_linear$GCV, "\n")

cat("\n2) Ridge Log-Linear (λ =", lambda_log_linear * 0.1, ")\n")
print(coef(ridge_model_log_linear))
cat("GCV:", ridge_model_log_linear$GCV, "\n")

cat("\n3) Ridge Linear-Log (λ =", lambda_linear_log * 0.1, ")\n")
print(coef(ridge_model_linear_log))
cat("GCV:", ridge_model_linear_log$GCV, "\n")

cat("\n4) Ridge Log-Log (λ =", lambda_log_log * 0.1, ")\n")
print(coef(ridge_model_log_log))
cat("GCV:", ridge_model_log_log$GCV, "\n")

# Ridge 예측값 계산 (각 모델별로)
# lm.ridge의 coef()는 절편을 포함하지 않으므로 바로 사용

# 1) Ridge linear-linear 예측
ridge_coef_ll <- coef(ridge_model_linear_linear)
cat("\nRidge coef_ll 길이:", length(ridge_coef_ll), "\n")
cat("Ridge coef_ll:\n")
print(ridge_coef_ll)

# coef가 named vector일 수 있으므로 matrix로 변환
ridge_coef_ll_matrix <- matrix(as.numeric(ridge_coef_ll), ncol = 1)
X_ll_centered <- scale(X_linear, center = ridge_model_linear_linear$xm, scale = FALSE)
ridge_pred_ll <- as.vector(ridge_model_linear_linear$ym + X_ll_centered %*% ridge_coef_ll_matrix)

# 2) Ridge log-linear 예측
ridge_coef_logl <- coef(ridge_model_log_linear)
ridge_coef_logl_matrix <- matrix(as.numeric(ridge_coef_logl), ncol = 1)
X_logl_centered <- scale(X_linear, center = ridge_model_log_linear$xm, scale = FALSE)
ridge_pred_logl <- as.vector(ridge_model_log_linear$ym + X_logl_centered %*% ridge_coef_logl_matrix)

# 3) Ridge linear-log 예측
X_log <- as.matrix(df[, c("log_SPR", "log_RGF", "log_PLF", "log_SLF", "CAR")])
ridge_coef_llog <- coef(ridge_model_linear_log)
ridge_coef_llog_matrix <- matrix(as.numeric(ridge_coef_llog), ncol = 1)
X_llog_centered <- scale(X_log, center = ridge_model_linear_log$xm, scale = FALSE)
ridge_pred_llog <- as.vector(ridge_model_linear_log$ym + X_llog_centered %*% ridge_coef_llog_matrix)

# 4) Ridge log-log 예측
ridge_coef_loglog <- coef(ridge_model_log_log)
ridge_coef_loglog_matrix <- matrix(as.numeric(ridge_coef_loglog), ncol = 1)
X_loglog_centered <- scale(X_log, center = ridge_model_log_log$xm, scale = FALSE)
ridge_pred_loglog <- as.vector(ridge_model_log_log$ym + X_loglog_centered %*% ridge_coef_loglog_matrix)

# Ridge MSE 및 R² 계산
ridge_mse_ll <- mean((y_linear - ridge_pred_ll)^2)
ridge_r2_ll <- 1 - sum((y_linear - ridge_pred_ll)^2) / sum((y_linear - mean(y_linear))^2)

ridge_mse_logl <- mean((y_log - ridge_pred_logl)^2)
ridge_r2_logl <- 1 - sum((y_log - ridge_pred_logl)^2) / sum((y_log - mean(y_log))^2)

ridge_mse_llog <- mean((y_linear - ridge_pred_llog)^2)
ridge_r2_llog <- 1 - sum((y_linear - ridge_pred_llog)^2) / sum((y_linear - mean(y_linear))^2)

ridge_mse_loglog <- mean((y_log - ridge_pred_loglog)^2)
ridge_r2_loglog <- 1 - sum((y_log - ridge_pred_loglog)^2) / sum((y_log - mean(y_log))^2)

ridge_comparison <- data.frame(
  Model = c("Ridge Linear-Linear", "Ridge Log-Linear", "Ridge Linear-Log", "Ridge Log-Log"),
  Lambda = c(lambda_linear_linear * 0.1, lambda_log_linear * 0.1,
             lambda_linear_log * 0.1, lambda_log_log * 0.1),
  MSE = c(ridge_mse_ll, ridge_mse_logl, ridge_mse_llog, ridge_mse_loglog),
  R_squared = c(ridge_r2_ll, ridge_r2_logl, ridge_r2_llog, ridge_r2_loglog),
  GCV = c(ridge_model_linear_linear$GCV, ridge_model_log_linear$GCV,
          ridge_model_linear_log$GCV, ridge_model_log_log$GCV)
)

cat("\n======================\n")
cat("Ridge 모델 성능 비교표\n")
cat("======================\n")
print(ridge_comparison)

#####################################
# 8-3. LASSO 회귀에서 4가지 변환 조합 비교
#####################################

library(lars)

# 1) LASSO: linear-linear
lasso_linear_linear <- lars(X_linear, y_linear, type = "lasso")
cp_ll <- summary(lasso_linear_linear)$Cp
best_step_ll <- which.min(cp_ll)
lasso_coef_ll <- coef(lasso_linear_linear, s = best_step_ll, mode = "step")
lasso_pred_ll <- predict(lasso_linear_linear, X_linear, s = best_step_ll, mode = "step")$fit

# 2) LASSO: log-linear
lasso_log_linear <- lars(X_linear, y_log, type = "lasso")
cp_logl <- summary(lasso_log_linear)$Cp
best_step_logl <- which.min(cp_logl)
lasso_coef_logl <- coef(lasso_log_linear, s = best_step_logl, mode = "step")
lasso_pred_logl <- predict(lasso_log_linear, X_linear, s = best_step_logl, mode = "step")$fit

# 3) LASSO: linear-log
lasso_linear_log <- lars(X_log, y_linear, type = "lasso")
cp_llog <- summary(lasso_linear_log)$Cp
best_step_llog <- which.min(cp_llog)
lasso_coef_llog <- coef(lasso_linear_log, s = best_step_llog, mode = "step")
lasso_pred_llog <- predict(lasso_linear_log, X_log, s = best_step_llog, mode = "step")$fit

# 4) LASSO: log-log
lasso_log_log <- lars(X_log, y_log, type = "lasso")
cp_loglog <- summary(lasso_log_log)$Cp
best_step_loglog <- which.min(cp_loglog)
lasso_coef_loglog <- coef(lasso_log_log, s = best_step_loglog, mode = "step")
lasso_pred_loglog <- predict(lasso_log_log, X_log, s = best_step_loglog, mode = "step")$fit

cat("\n======================\n")
cat("LASSO 회귀: 4가지 변환 조합\n")
cat("======================\n")
cat("\n1) LASSO Linear-Linear (Step =", best_step_ll, ")\n")
print(lasso_coef_ll)
cat("Cp:", cp_ll[best_step_ll], "\n")

cat("\n2) LASSO Log-Linear (Step =", best_step_logl, ")\n")
print(lasso_coef_logl)
cat("Cp:", cp_logl[best_step_logl], "\n")

cat("\n3) LASSO Linear-Log (Step =", best_step_llog, ")\n")
print(lasso_coef_llog)
cat("Cp:", cp_llog[best_step_llog], "\n")

cat("\n4) LASSO Log-Log (Step =", best_step_loglog, ")\n")
print(lasso_coef_loglog)
cat("Cp:", cp_loglog[best_step_loglog], "\n")

# LASSO MSE 및 R² 계산
lasso_mse_ll <- mean((y_linear - lasso_pred_ll)^2)
lasso_r2_ll <- 1 - sum((y_linear - lasso_pred_ll)^2) / sum((y_linear - mean(y_linear))^2)

lasso_mse_logl <- mean((y_log - lasso_pred_logl)^2)
lasso_r2_logl <- 1 - sum((y_log - lasso_pred_logl)^2) / sum((y_log - mean(y_log))^2)

lasso_mse_llog <- mean((y_linear - lasso_pred_llog)^2)
lasso_r2_llog <- 1 - sum((y_linear - lasso_pred_llog)^2) / sum((y_linear - mean(y_linear))^2)

lasso_mse_loglog <- mean((y_log - lasso_pred_loglog)^2)
lasso_r2_loglog <- 1 - sum((y_log - lasso_pred_loglog)^2) / sum((y_log - mean(y_log))^2)

lasso_comparison <- data.frame(
  Model = c("LASSO Linear-Linear", "LASSO Log-Linear", "LASSO Linear-Log", "LASSO Log-Log"),
  Step = c(best_step_ll, best_step_logl, best_step_llog, best_step_loglog),
  MSE = c(lasso_mse_ll, lasso_mse_logl, lasso_mse_llog, lasso_mse_loglog),
  R_squared = c(lasso_r2_ll, lasso_r2_logl, lasso_r2_llog, lasso_r2_loglog),
  Cp = c(cp_ll[best_step_ll], cp_logl[best_step_logl],
         cp_llog[best_step_llog], cp_loglog[best_step_loglog])
)

cat("\n======================\n")
cat("LASSO 모델 성능 비교표\n")
cat("======================\n")
print(lasso_comparison)