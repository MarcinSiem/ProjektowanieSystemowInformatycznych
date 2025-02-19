# 1. Stwórz funkcję o nazwie kostka, która będzie symulować n rzutów kostką.
# Wskazówka: Użyj funkcji sample() do losowania liczby oczek od 1 do 6.

kostka <- function(n) {
    sample(1:6,n,replace = TRUE, prob = NULL) # nolint
}

print(kostka(10))

# 2. Stwórz funkcję, która będzie tworzyć wektor o zadanej długości.
# Funkcja ma zwracać wektor liczb całkowitych od 1 do n:
#  długość wektora wynosi n, a wartości w wektorze to sekwencja liczb od 1 do n.

wektor <- function(n) {
    return(1:n) # nolint
}
y <- wektor(5)
print(y)
