# Functions for energy-cycling repository

# Distance-dependent mode switch probs
iac <- function(x, a = 0.3, b = 0.2){
  a * exp(1)^(-b * x)
}
iac(1:10, 0.5, 0.1)

# most common factor in variable
commonest <- function(f){
  tt <- table(f)
  names(tt[which.max(tt)])
}

# Make age-prob decay function
age_prob_fun <- function(age){
  age_prob <- rep(1, length(age))
  age_prob[age > 60] <- 1 - (age[age > 60] - 60) / 40
  age_prob[age > 100] <- 0
  age_prob
}

# Testing the function:
age <- c(0, 5, 60, 62, 88, 110)
plot(age, age_prob_fun(age))
age_prob_fun(age)

age_recat <- function(a){
    a2 <- factor(rep(NA, length(a)), levels = c("0-14", "15-29", "30-44", "45-59", "60-69", "70-79", "80+"))
    a2[a == "0 - 4 years" | a == "5 - 10 years" | a == "11 - 15 years"] <- "0-14"
    a2[a == "16 - 19 years" | a == "20 - 29 years"] <- "15-29"
    a2[a == "30 - 39 years" ] <- "30-44"
    f40_t49 <- which(a == "40 - 49 years") # we need to split this variable in two
    o30_44 <- sample(f40_t49, size = length(f40_t49) / 2)
    o45_59 <- f40_t49[!f40_t49 %in% o30_44]
    a2[o30_44] <- "30-44"
    a2[o45_59] <- "45-59"
    a2[a == "50 - 59 years" ] <- "45-59"
    a2[a == "60 - 69 years" ] <- "60-69"
    a2[a == "70 + years" ] <- "70-79" # all people over 70...
    over70 <- which(a == "70 + years")
    over80 <- sample(over70, size = (length(over70) * 0.397))
    a2[over80] <- "80+" # all people over 70...
  a2
}