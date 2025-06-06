# ============================================
# ZADANIE 1  – Analiza symulowanego wzrostu
# autor: Maciej Burakowski
# ============================================

set.seed(123)                 # 1. zapewnia powtarzalność losowania
wzrost <- rnorm(1000, 175, 12)  # 1. symuluje 1000 wartości ~ N(175, 12)

# --- Statystyki opisowe ---------------------------------------------
srednia  <- mean(wzrost)        # 2. średnia
mediana  <- median(wzrost)      # 2. mediana
odch_sd  <- sd(wzrost)          # 2. odchylenie standardowe
kwantyle <- quantile(wzrost,                   # 4. kwartyle
                     probs = c(0.25, 0.50, 0.75))

# --- Histogram -------------------------------------------------------
hist(wzrost,
     breaks = 30,               # 3. liczba słupków
     main   = "Histogram wzrostu",
     xlab   = "Wzrost (cm)",
     col    = "lightblue",
     border = "black")

# --- Wartości odstające (1.5 × IQR) ---------------------------------
Q1  <- kwantyle[1];  Q3 <- kwantyle[3]
IQR <- Q3 - Q1
lim_dol <- Q1 - 1.5 * IQR
lim_gor <- Q3 + 1.5 * IQR
odstajace <- wzrost[wzrost < lim_dol | wzrost > lim_gor]  # 5.

# --- Test hipotezy H0: μ = 170 cm -----------------------------------
test_t <- t.test(wzrost, mu = 170)  # 6.

# --- P-stwo wzrostu > 190 cm ----------------------------------------
prob_190 <- mean(wzrost > 190)      # 7.

# --- Wyświetlenie podstawowych wyników ------------------------------
cat("Średnia:",  round(srednia,2),
    "  Mediana:", round(mediana,2),
    "  SD:",      round(odch_sd,2), "\n")
cat("25%, 50%, 75%:", round(kwantyle,2), "\n")
cat("Liczba odstających:", length(odstajace), "\n")
cat("p-value (μ=170):",  signif(test_t$p.value, 3), "\n")
cat("P(wzrost > 190 cm):", round(prob_190, 4), "\n")
