"""
@author Maciej Burakowski
"""

import math

def heron(a, b, c):
    # 1) wszystkie boki > 0
    if a <= 0 or b <= 0 or c <= 0:
        print("Błąd: wszystkie boki muszą być większe od 0.")
        return None
    # 2) trójkąt istnieje?
    if a + b <= c or a + c <= b or b + c <= a:
        print("Błąd: z podanych boków nie da się zbudować trójkąta.")
        return None
    # 3) oblicz pole
    p = (a + b + c) / 2
    S = math.sqrt(p * (p - a) * (p - b) * (p - c))
    print(f"Pole trójkąta o bokach {a}, {b}, {c} wynosi {S}")
    return S

# przykłady
print("Przykłady dla zadania 1:")
heron(3, 4, 5)   # Pole = 6.0
heron(1, 2, 3)   # błąd 


def wspolne(x, y):
    # próbujemy zliczyć elementy; jeśli argument nie jest iterowalny, dostaniemy wyjątek
    try:
        cx = {}
        for el in x:
            cx[el] = cx.get(el, 0) + 1
        cy = {}
        for el in y:
            cy[el] = cy.get(el, 0) + 1
    except Exception:
        print("Błąd: oba argumenty muszą być kolekcjami.")
        return None

    wynik = []
    for el in cx:
        if el in cy:
            razy = min(cx[el], cy[el])
            wynik += [el] * razy

    print("Część wspólna:", wynik)
    return wynik

print("Przykłady dla zadania 2:")
wspolne([1,2,2,3], [2,2,4])     
wspolne("abca", "aac")        


def podzbiory(x):
    # próbujemy rzucić x na listę
    try:
        lista = list(x)
    except Exception:
        print("Błąd: argument musi być iterowalny.")
        return None

    n = len(lista)
    wynik = []
    for mask in range(1 << n):
        pod = []
        for i in range(n):
            if (mask >> i) & 1:
                pod.append(lista[i])
        wynik.append(pod)

    print("Podzbiory (łącznie", len(wynik), "):", wynik)
    return wynik

print("Przykłady dla zadania 3:")
podzbiory({'a','b','c'})


def fib_iter(n):
    if n < 0:
        print("Błąd: n musi być liczbą nieujemną.")
        return None
    if n == 0:
        print([]); return []
    if n == 1:
        print([0]); return [0]

    fib = [0, 1]
    while len(fib) < n:
        fib.append(fib[-1] + fib[-2])

    print("Pierwsze", n, "wyrazów ciągu Fib.:", fib)
    return fib

print("Przykłady dla zadania 4a:")
fib_iter(10)

def fib_rec(n):
    if n < 0:
        print("Błąd: n musi być liczbą nieujemną.")
        return None
    if n == 0:
        return []
    if n == 1:
        return [0]
    if n == 2:
        return [0, 1]

    prev = fib_rec(n - 1)
    return prev + [prev[-1] + prev[-2]]

print("Przykłady dla zadania 4b:") 
print(fib_rec(10))

def collatz(c0):
    if c0 < 0:
        print("Błąd: c0 musi być liczbą nieujemną.")
        return None

    seq = []
    seen = set()
    c = c0
    while c not in seen:
        seen.add(c)
        seq.append(c)
        if c % 2 == 0:
            c = c // 2
        else:
            c = 3 * c + 1

    print("Ciąg Collatza od", c0, "do pierwszego powtórzenia:", seq)
    return seq

def testuj_collatz(max_c0):
    best_val = (None, 0)
    best_len = (None, 0)
    for i in range(max_c0 + 1):
        seq = collatz(i)
        if not seq:
            continue
        if max(seq) > best_val[1]:
            best_val = (i, max(seq))
        if len(seq) > best_len[1]:
            best_len = (i, len(seq))
    print("Maksymalna wartość w ciągu:", best_val)
    print("Najdłuższy ciąg:", best_len)
    return best_val, best_len

print("Przykłady dla zadania 5:")
collatz(6)
#testuj_collatz(100) #odkomentować w celu użycia


def komplement(dna_kodujaca):
    """
    Dla sekwencji nici kodującej DNA (A,C,G,T) zwraca nić matrycową (komplementarną).
    W razie napotkania innego znaku drukuje błąd i zwraca None.
    """
    mapping = {'A':'T', 'T':'A', 'C':'G', 'G':'C'}
    wynik = []
    for n in dna_kodujaca.upper():
        if n not in mapping:
            print(f"Błąd: nieznany nukleotyd '{n}'.")
            return None
        wynik.append(mapping[n])
    res = ''.join(wynik)
    print("Nić matrycowa:", res)
    return res

def transkrybuj(dna_matryc):
    """
    Dla sekwencji nici matrycowej DNA (A,C,G,T) zwraca sekwencję mRNA (A,C,G,U).
    W razie napotkania innego znaku drukuje błąd i zwraca None.
    """
    wynik = []
    for n in dna_matryc.upper():
        if n == 'T':
            wynik.append('U')
        elif n in ('A','C','G'):
            wynik.append(n)
        else:
            print(f"Błąd: nieznany nukleotyd '{n}'.")
            return None
    rna = ''.join(wynik)
    print("mRNA:", rna)
    return rna

print("Przykłady dla zadania 6:")
komp = komplement("ATGCCG")    # -> "TACGGC"
rna  = transkrybuj(komp)       # -> "UACGGC"
 