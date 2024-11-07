library(compiler)


upper_tri_base_1 <- function(x) {
    d <- dim(x)
    return(.row(d) < .col(d))
}
lower_tri_base_1 <- function(x) {
    d <- dim(x)
    return(.row(d) > .col(d))
}
upper_tri_base_2 <- function(x) {
    d <- dim(x)
    return(.row(d) <= .col(d))
}

upper_tri <- cmpfun(upper_tri_base_1)
lower_tri <- cmpfun(lower_tri_base_1)
upper_tri_diag <- cmpfun(upper_tri_base_2)
kronecker_cmp <- cmpfun(base::kronecker)

lambda_1_base <- function(n_mm, n_aa, susceptibilities) {
    lambda_small <- matrix(0, nrow = n_aa, ncol = n_aa, byrow = TRUE)
    lambda_small[upper_tri(lambda_small)] <- susceptibilities
    lower_tri <- t(lambda_small)
    lower_tri[upper_tri_diag(lower_tri)] <- 0
    lambda_small <- lower_tri + lambda_small
    diag(lambda_small) <- 1
    return(kronecker_cmp(lambda_small, diag(n_mm)))
}

lambda_2_base <- function(n_mm, n_aa, susceptibilities) {
    lambda_small <- matrix(1, nrow = n_aa, ncol = n_aa, byrow = TRUE)
    half_elements <- n_aa * (n_aa - 1) / 2
    lambda_small[upper_tri(lambda_small)] <- susceptibilities[1:half_elements]
    lambda_small[lower_tri(lambda_small)] <- susceptibilities[(half_elements + 1):(2 * half_elements)]
    return(kronecker_cmp(lambda_small, diag(n_mm)))
}

lambda_3_base <- function(n_mm, n_aa, susceptibilities) {
    lambda_small <- matrix(0, nrow = n_aa, ncol = n_aa, byrow = TRUE)
    lambda_small[upper_tri(lambda_small)] <- susceptibilities[2:length(susceptibilities)]
    lower_tri <- t(lambda_small)
    lower_tri[upper_tri_diag(lower_tri)] <- 0
    lambda_small <- lower_tri + lambda_small
    diag(lambda_small) <- susceptibilities[1]
    return(kronecker_cmp(lambda_small, diag(n_mm)))
}

lambda_4_base <- function(n_mm, n_aa, susceptibilities) {
    lambda_small <- matrix(0, nrow = n_aa, ncol = n_aa, byrow = TRUE)
    half_elements <- n_aa * (n_aa - 1) / 2
    lambda_small[upper_tri(lambda_small)] <- susceptibilities[2:(half_elements + 1)]
    lambda_small[lower_tri(lambda_small)] <- susceptibilities[(half_elements + 2):(2 * half_elements + 1)]
    diag(lambda_small) <- susceptibilities[1]
    return(kronecker_cmp(lambda_small, diag(n_mm)))
}

lambda_5_base <- function(n_mm, n_aa, susceptibilities) {
    lambda_small <- matrix(susceptibilities, nrow = n_aa, ncol = n_aa, byrow = TRUE)
    return(kronecker_cmp(lambda_small, diag(n_mm)))
}

lambda_1 <- cmpfun(lambda_1_base)
lambda_2 <- cmpfun(lambda_2_base)
lambda_3 <- cmpfun(lambda_3_base)
lambda_4 <- cmpfun(lambda_4_base)
lambda_5 <- cmpfun(lambda_5_base)

lambda_gen <- function(p, params) {
    modify_mobility <- function(p_going, mobility) {
        mobility <- mobility * p_going
        diag(mobility) <- 1 - p_going
        return(mobility)
    }
    mobility <- modify_mobility(p_going = p, params$mobility)
    mobility_normalized <- matrix(0, nrow = nrow(mobility), ncol = ncol(mobility))
    for (i in seq_len(ncol(mobility))) {
        tmp <- unlist(mobility[, i] * params$pop_reg)
        tmp <- tmp / sum(tmp)
        mobility_normalized[i, ] <- tmp
    }

    beta_1 <- 1 / params$epsilon_1 * as.matrix(params$c_matrix) * as.matrix(params$q_1) # q_matrix_fn(x[8:22])
    beta_2 <- 1 / params$epsilon_2 * as.matrix(params$c_matrix) * as.matrix(params$q_2) # q_matrix_fn(x[23:37])

    lambda_1 <- beta_1 %x% mobility_normalized
    lambda_2 <- beta_2 %x% mobility_normalized

    return(list(
        lambda_1 = lambda_1,
        lambda_2 = lambda_2
    ))
}
lambda_gen <- cmpfun(lambda_gen)

lambda_gen_2 <- function(p, x, params) {
    modify_mobility <- function(p_going, mobility) {
        mobility <- mobility * p_going
        diag(mobility) <- 1 - p_going
        return(mobility)
    }
    mobility <- modify_mobility(p_going = p, params$mobility)
    mobility_normalized <- matrix(0, nrow = nrow(mobility), ncol = ncol(mobility))
    for (i in seq_len(ncol(mobility))) {
        tmp <- unlist(mobility[, i] * params$pop_reg)
        tmp <- tmp / sum(tmp)
        mobility_normalized[i, ] <- tmp
    }

    beta_1 <- matrix(x[1:16], ncol = 4, byrow = TRUE)
    beta_2 <- matrix(x[17:32], ncol = 4, byrow = TRUE)

    lambda_1 <- beta_1 %x% mobility_normalized
    lambda_2 <- beta_2 %x% mobility_normalized

    return(list(
        lambda_1 = lambda_1,
        lambda_2 = lambda_2
    ))
}
lambda_gen_2 <- cmpfun(lambda_gen_2)

# Benchmarking
# library(microbenchmark)
# library(ggplot2)
# mbm <- microbenchmark(
#     big_lambda_1 = lambda_1(2, 3, c(1:3) + runif(1)),
#     big_lambda_2 = lambda_2(2, 3, c(1:6) + runif(1)),
#     big_lambda_1_base = lambda_1_base(2, 3, c(1:3) + runif(1)),
#     big_lambda_2_base = lambda_2_base(2, 3, c(1:6) + runif(1)),
#     times = 1000
# )
# autoplot(mbm)