f <- function(x) {
  print(x)
  print(y)
  print(z)
  g <- function(y) {
    y + z
    print(x)
    print(y)
    print(z)
  }
  z <- 4
  x + g(x)
  print(x)
  print(y)
  print(z)
}