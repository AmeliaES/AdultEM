#bar chart

#M4 inputs = 3290
#M6 inputs = 1280
#M6L inputs = 2019

M4_other_inputs <- c(21, 21, 18, 17, 13, 7, 6, 6, 6, 3, 3, 3, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
M6_other_inputs <- c(1253, 705, 52, 30, 8, 1, 1, 1, 0, 0, 0)
M6L_other_inputs <- c(76, 20, 17, 6, 3, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)

norm <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

norm_M4 <- norm(M4_other_inputs)
norm_M6 <- norm(M6_other_inputs)
norm_M6L <- norm(M6L_other_inputs)

boxplot(norm_M6, boxwex = 0.3,  at = 1, names = "M6", show.names = TRUE, ylab = "Non-MVP2 inputs distal to MVP2 inputs (normalised)")
boxplot(norm_M6L, boxwex = 0.3, add = TRUE, at = 1.3, names = "M6L", show.names = TRUE)
boxplot(norm_M4, boxwex = 0.3, add = TRUE, at = 0.7,  names = "M4", show.names = TRUE)


