
# i)
states <- c("H","S","D")

mu.hs <- 0.1
mu.hd <- 0.05
mu.hh <- 0 - mu.hs - mu.hd
mu.sh <- 0.6
mu.sd <- 0.2
mu.ss <- 0 - mu.sh - mu.sd
mu.dh <- 0
mu.ds <- 0
mu.dd <- 0 - mu.dh - mu.ds

(A <- matrix(c(mu.hh, mu.hs, mu.hd,
               mu.sh, mu.ss, mu.sd,
               mu.dh, mu.ds, mu.dd),
             nrow = 3, ncol = 3,byrow = TRUE,
             dimnames = list(states, states)))


# iii)
Ph <- function(h, A){
  diag(nrow(A)) + h * A
}

Pn <- function(P, n) {
  M <- diag(nrow(P))
  for (j in 1:n) {
    M <- M %*% P 
  }
  dimnames(M) <- dimnames(P)
  M
}

Pt <- function(t, h, A){
  n = t / h
  Pn(Ph(h, A), n)
}

Pt(8, 1, A)
Pt(8, 0.5, A)
Pt(8, 0.25, A)
Pt(8, 0.125, A)
Pt(8, 0.0625, A)


# v)
for (h in 2 ^ -(0:15)){
  print(Pt(16, h, A))
}


# vi)
Pt(16, 0.5^(15), A)["H","S"]
Pt(16, 0.5^(15), A)[1, 2]


Pt(20, 0.5^(15), A)["S","H"]
Pt(20, 0.5^(15), A)[2,1]

Pt(13 + 1/3, 0.5^(15), A)["S","D"]
Pt(13 + 1/3, 0.5^(15), A)[2,3]