
# First level function
llr = function(x, y, z, omega) {
    fits = sapply(z, compute_f_hat, x, y, omega)
    return(fits)
}

# Second level function
compute_f_hat = function(z, x, y, omega) {
    Wz = make_weight_matrix(z, x, omega)
    X = make_predictor_matrix(x)
    f_hat = c(1, z) %*% solve(t(X) %*% Wz %*% X) %*% t(X) %*% Wz %*% y
    return(f_hat)
}

# Third level function
make_weight_matrix = function(z, x, omega) {
    r = (abs(x - z))/(omega)
    w = ifelse((abs(r) < 1), (1-abs(r)^3)^3, 0)
    W = return(diag(w))
    }


make_predictor_matrix = function(x) {
    X <- cbind(1,x)
    return(X)
}
