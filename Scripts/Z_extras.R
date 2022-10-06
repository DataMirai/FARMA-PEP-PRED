

df1 <- data.frame(A = c(1, 2, 3), B = c(0,0,3), C = c(3,2,1)) 
df2 <- data.frame(A = c(0, 2, 4), B = c(1,0,3), C = c(0,1,4))


map2_dfr(
  .x = df1,
  .y = df2,
  ~ case_when(
    .x == 0 & .y > 0 ~ "colonised",
    .x > 0 & .y == 0 ~ "extinct",
    .x < .y  ~ "increased",
    .x == .y ~ "stable",
    .x > .y  ~ "decreased"))
