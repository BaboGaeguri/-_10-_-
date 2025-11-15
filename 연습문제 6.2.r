install.packages("readxl")
install.packages("corrplot")
install.packages("leaps")
install.packages("MASS")
install.packages("car")

# 파일 불러오기
library(readxl)
df <- read_excel("C:/Users/김상희/OneDrive/바탕 화면/수업자료/빅융/2. 응용회귀분석/교수님 제공 코드/회귀분석_데이터/table6.1_galapagos.xlsx")
df$Endemics_ratio <- df$Endemics / df$Species # 고유종 비율 구하기

# =============================
# 1. 기초 데이터 분석
# =============================
summary(df) #평균, 중앙값, 4분위 확인
str(df) # 자료형 확인
names(df) # 열이름 확인

colSums(is.na(df)) #결측치 확인

# 반응변수(Endemics_ratio) 박스플롯
par(mfrow = c(1, 1))
boxplot(df$Endemics_ratio, main="Endemics_ratio 분포 (반응변수)", xlab="Species", col="lightblue")

# 설명변수 박스플롯
par(mfrow = c(2, 3))
boxplot(df$Area, main="Area 분포", xlab="Area", col="lightpink")
boxplot(df$Elevation, main="Elevation 분포", xlab="Elevation", col="lightyellow")
boxplot(df$Nearest, main="Nearest 분포", xlab="Nearest", col="lightcoral")
boxplot(df$Scruz, main="Scruz 분포", xlab="Scruz", col="lavender")
boxplot(df$Adjacent, main="Adjacent 분포", xlab="Adjacent", col="lightcyan")
par(mfrow = c(1, 1))

# =============================
# 2. 선형성 파악
# =============================
# Species를 반응변수로 하는 산점도 플롯
par(mfrow = c(2, 3))

# 1. Area
plot(df$Area, df$Endemics_ratio,
     main = "Area vs Endemics_ratio",
     xlab = "Area",
     ylab = "Endemics_ratio",
     pch = 19, col = "blue")
abline(lm(Endemics_ratio ~ Area, data = df), col = "red", lwd = 2)

# 2. Elevation
plot(df$Elevation, df$Endemics_ratio,
     main = "Elevation vs Endemics_ratio",
     xlab = "Elevation",
     ylab = "Endemics_ratio",
     pch = 19, col = "red")
abline(lm(Endemics_ratio ~ Elevation, data = df), col = "red", lwd = 2)

# 3. Nearest
plot(df$Nearest, df$Endemics_ratio,
     main = "Nearest vs Endemics_ratio",
     xlab = "Nearest",
     ylab = "Endemics_ratio",
     pch = 19, col = "green")
abline(lm(Endemics_ratio ~ Nearest, data = df), col = "red", lwd = 2)

# 4. Scruz
plot(df$Scruz, df$Endemics_ratio,
     main = "Scruz vs Endemics_ratio",
     xlab = "Scruz",
     ylab = "Endemics_ratio",
     pch = 19, col = "purple")
abline(lm(Endemics_ratio ~ Scruz, data = df), col = "red", lwd = 2)

# 5. Adjacent
plot(df$Adjacent, df$Endemics_ratio,
     main = "Adjacent vs Endemics_ratio",
     xlab = "Adjacent",
     ylab = "Endemics_ratio",
     pch = 19, col = "orange")
abline(lm(Endemics_ratio ~ Adjacent, data = df), col = "red", lwd = 2)

par(mfrow = c(1, 1))

# 상관계수 검정 (Endemics_ratio와 각 설명변수)
cor.test(df$Endemics_ratio, df$Area) # Endemics_ratio vs Area
cor.test(df$Endemics_ratio, df$Elevation) # Endemics_ratio vs Elevation
cor.test(df$Endemics_ratio, df$Nearest) # Endemics_ratio vs Nearest
cor.test(df$Endemics_ratio, df$Scruz) # Endemics_ratio vs Scruz
cor.test(df$Endemics_ratio, df$Adjacent) # Endemics_ratio vs Adjacent

# (a) 고유종(Endemics)에 대한 종의 수(Species)에 대한 비율을 반응변수로 하여 다중회귀분석을 하시오.
df$Endemics_ratio <- df$Endemics / df$Species # 고유종 비율 구하기
model1 <- lm(Endemics_ratio ~ Area + Elevation + Nearest + Scruz + Adjacent, data = df)
summary(model1)

# (b) 결측값이 존재하는 Elevation 변수를 제외하고 다중회귀분석을 하시오.
colSums(is.na(df))
model2 <- lm(Endemics_ratio ~ Area + Nearest + Scruz + Adjacent, data = df)
summary(model2)

############## model1에 대하여 ##############
# (c) 다중공선성을 고려하여 변수선택을 하시오.
# 1) 설명변수들 간의 상관행렬에서 절댓값이 큰 상관계수를 갖는 경우
library(corrplot)
X <- df[, c("Area", "Elevation", "Nearest", "Scruz", "Adjacent")]
cor_mat <- cor(X)
corrplot(cor_mat, method = "color", addCoef.col = "black")
# 2) j 번째 설명변수를 반응변수 위치에 놓고 나머지 설명변수들로 회귀모형을 적합했을 때 결정계수가 1에 가까운 경우
for (var in colnames(X)) {
  others <- colnames(X)[colnames(X) != var]
  formula_str <- paste(var, "~", paste(others, collapse = "+"))
  model_temp <- lm(as.formula(formula_str), data = X)
  cat("\n======================\n")
  cat("반응변수 =", var, "\n")
  print(summary(model_temp)$r.squared)
}
# 3) 고유값을 이용해 계산한 조건수가 30보다 큰 경우
X <- apply(X, 2, as.numeric)
XtX <- crossprod(X)
eigen_vals_XtX <- eigen(XtX)$values
# 조건수 = sqrt(λ_max / λ_min)
condition_number <- sqrt(max(eigen_vals_XtX)) / sqrt(min(eigen_vals_XtX))
condition_number
# 4) VIF
library(car)
vif(model1)

# (d) 단계별 변수선택법으로 변수선택을 하시오.
full_model <- lm(Endemics_ratio ~ Area + Elevation + Nearest + Scruz + Adjacent, data = df)
null_model <- lm(Endemics_ratio ~ 1, data = df)
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
all_subsets <- regsubsets(Endemics_ratio ~ Area + Elevation + Nearest + Scruz + Adjacent,
                          data = df,
                          nbest = 1,   # 각 개수별로 가장 좋은 모형 1개
                          nvmax = 6)   # 최대 변수 개수(설명변수 6개)

subsum <- summary(all_subsets)
subsum
subsum$adjr2 # adj R²함께 비교
# 5) Cp
x <- as.matrix(df[, c("Area", "Elevation", "Nearest", "Scruz", "Adjacent")])
y <- df$Endemics_ratio
lp <- leaps(x,y, method="Cp")
lp

cp_vals  <- lp$Cp
sizes    <- lp$size
valid_idx <- which(sizes != max(sizes)) # 풀모델(size = 7) 제외한 인덱스 찾기
dist_vals <- abs(cp_vals[valid_idx] - sizes[valid_idx]) # Cp=p 거리 계산 (풀모델 제외)
remove_idx <- order(dist_vals, decreasing = TRUE)[1:10] # dist_vals가 큰 순서대로 10개 제외
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
# best5 출력
var_names <- colnames(x)
for (i in best5_idx) {
  cat("\n● Model index:", i, "(p =", sizes[i], ")\n")
  included <- lp$which[i, ]     # 이 모델에서 선택된 변수 (TRUE/FALSE)
  selected_vars <- var_names[included]
  cat("   Variables:\n")
  print(selected_vars)
}

############## model2에 대하여 ##############
# (c) 다중공선성을 고려하여 변수선택을 하시오.
# 1) 설명변수들 간의 상관행렬에서 절댓값이 큰 상관계수를 갖는 경우
library(corrplot)
X <- df[, c("Area", "Nearest", "Scruz", "Adjacent")]
cor_mat <- cor(X)
corrplot(cor_mat, method = "color", addCoef.col = "black")
# 2) j 번째 설명변수를 반응변수 위치에 놓고 나머지 설명변수들로 회귀모형을 적합했을 때 결정계수가 1에 가까운 경우
for (var in colnames(X)) {
  others <- colnames(X)[colnames(X) != var]
  formula_str <- paste(var, "~", paste(others, collapse = "+"))
  model_temp <- lm(as.formula(formula_str), data = X)
  cat("\n======================\n")
  cat("반응변수 =", var, "\n")
  print(summary(model_temp)$r.squared)
}
# 3) 고유값을 이용해 계산한 조건수가 30보다 큰 경우
X <- apply(X, 2, as.numeric)
XtX <- crossprod(X)
eigen_vals_XtX <- eigen(XtX)$values
# 조건수 = sqrt(λ_max / λ_min)
condition_number <- sqrt(max(eigen_vals_XtX)) / sqrt(min(eigen_vals_XtX))
condition_number
# 4) VIF
vif(model2)

# (d) 단계별 변수선택법으로 변수선택을 하시오.
full_model <- lm(Endemics_ratio ~ Area + Nearest + Scruz + Adjacent, data = df)
null_model <- lm(Endemics_ratio ~ 1, data = df)
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
all_subsets <- regsubsets(Endemics_ratio ~ Area + Nearest + Scruz + Adjacent,
                          data = df,
                          nbest = 1,   # 각 개수별로 가장 좋은 모형 1개
                          nvmax = 6)   # 최대 변수 개수(설명변수 6개)

subsum <- summary(all_subsets)
subsum
subsum$adjr2 # adj R²함께 비교
# 5) Cp
x <- as.matrix(df[, c("Area", "Nearest", "Scruz", "Adjacent")])
y <- df$Endemics_ratio
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
for (i in best5_idx) {
  included <- lp$which[i, ]
  selected_vars <- var_names[included]
  formula_str <- paste("Endemics_ratio ~", paste(selected_vars, collapse = " + "))
  model_formula <- as.formula(formula_str)
  fit <- lm(model_formula, data = df)
  adjR2 <- summary(fit)$adj.r.squared
  cat("\n● Model index:", i,
      "\n   p =", sizes[i],
      "\n   Variables:", paste(selected_vars, collapse=", "),
      "\n   Adjusted R²:", round(adjR2, 5), "\n")
}

# (e) Elevation의 결측값을 추정하는 방법을 생각해 보시오.
# ...?