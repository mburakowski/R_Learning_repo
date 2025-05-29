#
#@author Maciej Burakowski
#

# 1) Pole trójkąta metodą Herona
heron <- function(a, b, c) {
  # 1) wszystkie boki > 0
  if (a <= 0 || b <= 0 || c <= 0) {
    cat("Błąd: wszystkie boki muszą być większe od 0.\n")
    return(NULL)
  }
  # 2) trójkąt istnieje?
  if (a + b <= c || a + c <= b || b + c <= a) {
    cat("Błąd: z podanych boków nie da się zbudować trójkąta.\n")
    return(NULL)
  }
  # 3) oblicz pole
  p <- (a + b + c) / 2
  S <- sqrt(p * (p - a) * (p - b) * (p - c))
  cat(sprintf("Pole trójkąta o bokach %s, %s, %s wynosi %s\n", a, b, c, S))
  return(S)
}

cat("Przykłady dla zadania 1:\n")
heron(3, 4, 5)   # Pole = 6
heron(1, 2, 3)   # błąd  


# 2) Część wspólna dwóch "kolekcji"
wspolne <- function(x, y) {
  # spróbuj utworzyć tabele częstości
  tbls <- tryCatch({
    list(cx = table(x), cy = table(y))
  }, error = function(e) {
    cat("Błąd: oba argumenty muszą być kolekcjami.\n")
    return(NULL)
  })
  if (is.null(tbls)) return(NULL)
  
  cx <- tbls$cx; cy <- tbls$cy
  wsp <- intersect(names(cx), names(cy))
  
  wynik <- unlist(lapply(wsp, function(el) {
    razy <- min(cx[[el]], cy[[el]])
    # odtwórz oryginalny typ (np. liczba vs. znak)
    val <- type.convert(el, as.is = TRUE)
    rep(val, razy)
  }), use.names = FALSE)
  
  cat("Część wspólna:", if (length(wynik) > 0) paste(wynik, collapse = " ") else "", "\n")
  return(wynik)
}

cat("Przykłady dla zadania 2:\n")
wspolne(c(1,2,2,3), c(2,2,4))
wspolne(strsplit("abca", "")[[1]], strsplit("aac", "")[[1]])


# 3) Wszystkie podzbiory (power set)
podzbiory <- function(x) {
  lista <- tryCatch({
    as.vector(x)
  }, error = function(e) {
    cat("Błąd: argument musi być iterowalny.\n")
    return(NULL)
  })
  if (is.null(lista)) return(NULL)
  
  n <- length(lista)
  wynik <- vector("list", 2^n)
  idx <- 1
  for (mask in 0:(2^n - 1)) {
    pod <- vector()
    for (i in seq_len(n)) {
      if (bitwAnd(bitwShiftR(mask, i-1), 1) == 1) {
        pod <- c(pod, lista[i])
      }
    }
    wynik[[idx]] <- pod
    idx <- idx + 1
  }
  
  cat("Podzbiory (łącznie", length(wynik), "):\n")
  print(wynik)
  return(wynik)
}

cat("Przykłady dla zadania 3:\n")
podzbiory(c("a","b","c"))


# 4a) Fibonacci iteracyjnie
fib_iter <- function(n) {
  if (!is.numeric(n) || n < 0) {
    cat("Błąd: n musi być liczbą nieujemną.\n")
    return(NULL)
  }
  n <- as.integer(n)
  if (n == 0) {
    print(integer(0))
    return(integer(0))
  }
  if (n == 1) {
    print(0)
    return(0)
  }
  
  fib <- c(0, 1)
  for (i in 3:n) {
    fib <- c(fib, fib[i-1] + fib[i-2])
  }
  cat("Pierwsze", n, "wyrazów ciągu Fib.:", paste(fib, collapse = " "), "\n")
  return(fib)
}

cat("Przykłady dla zadania 4a:\n")
fib_iter(10)


# 4b) Fibonacci rekurencyjnie
fib_rec <- function(n) {
  if (!is.numeric(n) || n < 0) {
    cat("Błąd: n musi być liczbą nieujemną.\n")
    return(NULL)
  }
  n <- as.integer(n)
  if (n == 0) return(integer(0))
  if (n == 1) return(0)
  if (n == 2) return(c(0, 1))
  
  prev <- fib_rec(n - 1)
  new  <- prev[length(prev)] + prev[length(prev) - 1]
  return(c(prev, new))
}

cat("Przykłady dla zadania 4b:\n")
print(fib_rec(10))


# 5) Ciąg Collatza
collatz <- function(c0) {
  if (!is.numeric(c0) || c0 < 0) {
    cat("Błąd: c0 musi być liczbą nieujemną.\n")
    return(NULL)
  }
  c <- as.integer(c0)
  seq <- integer(0)
  seen <- integer(0)
  while (!(c %in% seen)) {
    seen <- c(seen, c)
    seq  <- c(seq, c)
    if (c %% 2 == 0) {
      c <- c / 2
    } else {
      c <- 3 * c + 1
    }
  }
  cat("Ciąg Collatza od", c0, "do pierwszego powtórzenia:", paste(seq, collapse = " "), "\n")
  return(seq)
}

testuj_collatz <- function(max_c0) {
  best_val <- list(idx = NA, val = 0)
  best_len <- list(idx = NA, len = 0)
  for (i in 0:max_c0) {
    seq <- collatz(i)
    if (is.null(seq)) next
    if (max(seq) > best_val$val) {
      best_val$idx <- i; best_val$val <- max(seq)
    }
    if (length(seq) > best_len$len) {
      best_len$idx <- i; best_len$len <- length(seq)
    }
  }
  cat("Maksymalna wartość w ciągu: (", best_val$idx, ",", best_val$val, ")\n")
  cat("Najdłuższy ciąg: (", best_len$idx, ",", best_len$len, ")\n")
  return(list(best_val = best_val, best_len = best_len))
}

cat("Przykłady dla zadania 5:\n")
collatz(6)
# testuj_collatz(100)  # odkomentować, żeby użyć


# 6) Komplement i transkrypcja DNA
komplement <- function(dna_kodujaca) {
  mapping <- c(A="T", T="A", C="G", G="C")
  chars  <- strsplit(toupper(dna_kodujaca), "")[[1]]
  wynik  <- character(0)
  for (n in chars) {
    if (!n %in% names(mapping)) {
      cat(sprintf("Błąd: nieznany nukleotyd '%s'.\n", n))
      return(NULL)
    }
    wynik <- c(wynik, mapping[n])
  }
  res <- paste(wynik, collapse = "")
  cat("Nić matrycowa:", res, "\n")
  return(res)
}

transkrybuj <- function(dna_matryc) {
  chars <- strsplit(toupper(dna_matryc), "")[[1]]
  wynik <- character(0)
  for (n in chars) {
    if (n == "T") {
      wynik <- c(wynik, "U")
    } else if (n %in% c("A","C","G")) {
      wynik <- c(wynik, n)
    } else {
      cat(sprintf("Błąd: nieznany nukleotyd '%s'.\n", n))
      return(NULL)
    }
  }
  rna <- paste(wynik, collapse = "")
  cat("mRNA:", rna, "\n")
  return(rna)
}

cat("Przykłady dla zadania 6:\n")
komp <- komplement("ATGCCG")    
rna  <- transkrybuj(komp)       
