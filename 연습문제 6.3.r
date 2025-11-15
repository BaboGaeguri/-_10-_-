install.packages("corrplot")
install.packages("MASS")
install.packages("leaps")

df <- longley

# (a) Employed를 반응변수로 하고, 나머지는 설명변수로 하여 다중회귀모형을 구하시오.
model1 <- lm(Employed ~ GNP.deflator + GNP + Unemployed + Armed.Forces + Population + Year, data = longley)
summary(model1)

# (b) 설명변수 간의 상관계수를 구하시오.
library(corrplot)
X <- df[, c("GNP.deflator", "GNP", "Unemployed", "Armed.Forces", "Population", "Year")]
cor_mat <- cor(X)
corrplot(cor_mat, method = "color", addCoef.col = "black")

# (c) 변수선택법을 적용하여 최종 모형을 제안하시오.
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
     pch = 19, col = "gray60", xlim = c(0, max(sizes)), ylim = c(0, 20))
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

# (d) GNP에 대한 회귀계수가 유의한지 유의수준 5%에서 검정하시오.
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
