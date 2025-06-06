# ================================================================
# ZADANIE 2 – Analiza zbioru Heart Disease Dataset (14 zmiennych)
# autor: Maciej Burakowski
# ================================================================

# 0) Wczytanie danych -------------------------------------------------------
dane <- read.csv("heart_disease_dataset.csv", stringsAsFactors = FALSE)

# 1) Częstość choroby serca wg płci ----------------------------------------
#    • filtrujemy wiersze z chorobą (Disease == "True")
#    • table() → liczy kobiety / mężczyzn
#    • pct    → udział procentowy
#    • diff   → różnica punktów procentowych
tbl  <- table(dane$Sex[dane$Disease == "True"])
pct  <- round(100 * tbl / sum(tbl), 2)
diff <- abs(pct["male"] - pct["female"])

print(tbl)   # liczba chorych: female / male
print(pct)   # procentowy udział
print(diff)  # różnica w p.p.

# 2) Średni cholesterol całkowity  -----------------------------------------
#    • grupujemy po płci (Sex) i stanie choroby (Disease)
#    • aggregate(..., FUN = mean) oblicza średnią w każdej z 4 podgrup
cholesterol_avg <- aggregate(
  Serum.cholesterol.in.mg.dl ~ Sex + Disease,
  data = dane,
  FUN  = function(x) round(mean(x, na.rm = TRUE), 2)
)
print(cholesterol_avg)

# 3) Histogram wieku osób z chorobą ----------------------------------------
wiek_chorych <- dane$Age[dane$Disease == "True"]
h <- hist(wiek_chorych, breaks = 10,        # 10 słupków po ~5-6 lat
          main = "Histogram wieku chorych",
          xlab = "Wiek (lata)",
          col  = "lightblue",
          border = "black")

# (opcjonalnie) który przedział ma najwięcej obserwacji?
dominujacy_przedzial_pocz <- h$breaks[which.max(h$counts)]
cat("Najliczniejszy przedział wieku zaczyna się od:", dominujacy_przedzial_pocz, "lat\n")

# 4) Box-plot maksymalnego tętna a choroba ----------------------------------
boxplot(Maximum.heart.rate.achieved ~ Disease,
        data  = dane,
        main  = "Maksymalne tętno vs choroba",
        xlab  = "Choroba serca (False / True)",
        ylab  = "Maksymalne tętno (bpm)",
        col   = c("green", "red"))

# 5) Wykres słupkowy: ból dławicowy a choroba -------------------------------
barplot(table(dane$Exercise.induced.angina, dane$Disease),
        beside      = TRUE,
        col         = c("red", "green"),
        legend.text = TRUE,
        args.legend = list(title = "Choroba", x = "topright"),
        main        = "Choroba a ból dławicowy (angina)",
        xlab        = "Ból dławicowy (False / True)",
        ylab        = "Liczba pacjentów")
