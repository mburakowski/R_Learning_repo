# ==============================================================
# ZADANIE 3  – Regresja logistyczna: przewidywanie choroby serca
# autor: Maciej Burakowski
# --------------------------------------------------------------

# 1) Wczytanie danych i wstępna konwersja -----------------------
df <- read.csv("heart_disease_dataset.csv", stringsAsFactors = FALSE)

#   • Zmienna docelowa: "Disease" (True / False) → factor No/Yes
df$Disease <- factor(df$Disease == "True",
                     levels = c(FALSE, TRUE),
                     labels = c("No", "Yes"))

#   • Płeć jako factor (male / female)
df$Sex <- factor(df$Sex)

#   • Typ bólu w klatce piersiowej (0–3) traktujemy kategorycznie
df$Chest.pain.type <- factor(df$Chest.pain.type)

#   • Ból dławicowy wywołany wysiłkiem (True / False) → factor
df$Exercise.induced.angina <- factor(df$Exercise.induced.angina == "True",
                                     levels = c(FALSE, TRUE),
                                     labels = c("No", "Yes"))

# 2) Podział na zbiór treningowy (70 %) i testowy (30 %) --------
set.seed(42)                                # powtarzalność
idx   <- sample(nrow(df), 0.7 * nrow(df))   # losowe indeksy
train <- df[idx, ]                          # dane treningowe
test  <- df[-idx, ]                         # dane testowe

# 3) Budowa modelu regresji logistycznej -------------------------
model <- glm(
  Disease ~ Age + Sex + Chest.pain.type +
            Resting.blood.pressure + Serum.cholesterol.in.mg.dl +
            Maximum.heart.rate.achieved + Exercise.induced.angina +
            Number.of.major.vessels,
  data   = train,
  family = binomial              # link = "logit" domyślnie
)

# 4) Współczynniki i ilorazy szans -------------------------------
summary(model)                   # tabela β, SE, p-values
OR <- exp(coef(model))           # ilorazy szans = e^β
print(OR)

# 5) Predykcja na zbiorze testowym + metryki ---------------------
p_test <- predict(model, newdata = test, type = "response")      # P(choroba)
pred   <- factor(ifelse(p_test >= 0.5, "Yes", "No"),
                 levels = c("No", "Yes"))
true   <- test$Disease

cm <- table(Predicted = pred, Actual = true)   # confusion matrix
print(cm)

#   Obliczamy podstawowe metryki:
TP <- cm["Yes","Yes"]; TN <- cm["No","No"]
FP <- cm["Yes","No"];  FN <- cm["No","Yes"]

accuracy    <- (TP + TN) / sum(cm)
sensitivity <- TP / (TP + FN)      # recall
specificity <- TN / (TN + FP)
precision   <- TP / (TP + FP)

cat("Accuracy:   ", round(accuracy,    4), "\n")
cat("Sensitivity:", round(sensitivity, 4), "\n")
cat("Specificity:", round(specificity, 4), "\n")
cat("Precision:  ", round(precision,   4), "\n")

# 6) Krzywa ROC i AUC (pakiet pROC) ------------------------------
library(pROC)

roc_obj <- roc(response  = true,
               predictor = p_test,
               levels    = c("No", "Yes"),
               direction = "<")       # większe prawdop. = klasa pozytywna

auc_val <- auc(roc_obj)
cat("AUC:", round(auc_val, 4), "\n")

plot(roc_obj,
     main = paste("ROC curve (AUC =", round(auc_val, 3), ")"),
     col  = "blue", lwd = 2)
abline(0, 1, lty = 2, col = "gray")   # linia losowego klasyfikatora
