# NLCD 2001 Alaska error matrix, from Selkowitz and Stehman (2011), Remote Sensing of Environment 115:1401-1407,
# doi:10.1016/j.rse.2011.01.020. Writes inst/extdata/nlcd_ak2001_error_matrix.csv, read by
# ak_landcover(error_matrix = "selkowitz2011").
#
# Table 4a gives estimated percent of Alaska's area per cell (rows map class, columns reference class; agreement
# with the primary or the alternate reference label, so the diagonal sums to the 76.2% overall accuracy), with each
# map class's area (row total) and its number of sample pixels. adjust_class_shares() needs counts, so each row is
# turned into effective counts: its conditional proportions (cell / row sum) times an effective sample size. The
# sample was a three-stage cluster sample (30 blocks of 27 x 27 km, flight lines, pixels), not a random sample by
# map class, so the pixel counts overstate the information. The effective size is chosen so the stratified variance
# formula reproduces the design-based SE of each class's user's accuracy in Table 3a: n = UA (1 - UA) / SE^2 + 1.

library(dplyr)

classes <- c("11", "12", "31", "41", "42", "43", "51", "52", "71", "72", "90", "95")

# Table 4a, as printed. Two cells disagree with both their row and column totals and with Table 4b; corrected below.
table_4a <- matrix(c(
  5.64, 0.10, 0.00, 0.08,  0.05, 0.00,  0.00,  0.10, 0.00, 0.00, 0.05, 0.13,
  0.02, 7.17, 0.00, 0.00,  0.00, 0.00,  0.07,  0.00, 0.00, 0.00, 0.00, 0.00,
  0.03, 0.29, 6.59, 0.00,  0.00, 0.00,  0.56,  0.21, 0.06, 0.00, 0.00, 0.00,
  0.00, 0.00, 0.00, 2.50,  0.85, 0.29,  0.00,  0.48, 0.00, 0.00, 0.00, 0.05,
  0.08, 0.00, 0.00, 0.64, 11.90, 0.56,  0.08,  0.37, 0.00, 0.00, 0.17, 0.34,
  0.05, 0.00, 0.00, 0.64,  0.86, 0.79,  0.02,  0.28, 0.00, 0.00, 0.10, 0.00,
  0.00, 0.05, 0.33, 0.03,  0.03, 0.03, 15.81,  2.12, 0.05, 0.37, 0.15, 0.92,
  0.06, 0.00, 0.17, 1.11,  3.10, 0.30,  0.81, 14.61, 0.04, 0.22, 1.23, 0.51,
  0.00, 0.00, 0.00, 0.00,  0.00, 0.00,  0.00,  0.27, 0.17, 0.00, 0.08, 0.40,
  0.05, 0.00, 0.05, 0.01,  0.00, 0.00,  0.95,  0.45, 0.00, 6.30, 0.09, 0.40,
  0.09, 0.00, 0.00, 0.33,  0.96, 0.05,  0.00,  0.54, 0.00, 0.00, 3.58, 0.18,
  0.17, 0.00, 0.00, 0.00,  0.01, 0.00,  0.00,  0.07, 0.00, 0.08, 0.36, 1.11
), nrow = 12, byrow = TRUE, dimnames = list(map = classes, reference = classes))
row_total <- c(6.15, 7.26, 7.74, 4.17, 14.15, 2.16, 19.89, 22.15, 0.51, 8.30, 5.74, 1.80)
column_total <- c(6.19, 7.61, 7.14, 4.74, 17.75, 2.02, 18.30, 19.49, 0.32, 6.98, 5.80, 3.66)

# Map 43 (mixed forest) x reference 41 is printed 0.64: row 43 then sums to 2.74 (total 2.16), column 41 to 5.34
# (total 4.74), and the forest block exceeds Table 4b's Level I forest agreement (18.84). 0.04 fits all three, and
# row 43's user's accuracy (0.79 / 2.16 = 36.6%) matches Table 3a (36.4%).
# Map 71 (grassland) x reference 95 is printed 0.40 (the same as the cell below it): row 71 then sums to 0.92
# (total 0.51), column 95 to 4.04 (total 3.66), and Table 4b's map-herbaceous x reference-wetland cell (0.56) is
# smaller than the Level II cells it contains. 0.00 fits all three.
table_4a["43", "41"] <- 0.04
table_4a["71", "95"] <- 0.00
stopifnot(abs(rowSums(table_4a) - row_total) < 0.03, abs(colSums(table_4a) - column_total) < 0.03)   # rounding

# Table 3a: user's accuracy and its SE (%), agreement with the primary or alternate label
table_3a <- tibble(
  class = classes,
  users_accuracy = c(91.8, 98.9, 85.1, 59.9, 84.1, 36.4, 79.5, 65.9, 32.7, 75.9, 62.3, 61.8),
  se = c(3.1, 1.2, 3.5, 8.3, 3.2, 13.1, 5.1, 6.5, 13.4, 7.6, 6.9, 10.7),
  pixels = c(185, 129, 146, 101, 360, 49, 429, 546, 16, 190, 130, 40)
) |>
  mutate(effective_n = (users_accuracy / 100) * (1 - users_accuracy / 100) / (se / 100)^2 + 1)

counts <- table_4a / rowSums(table_4a) * table_3a$effective_n
write.csv(round(counts, 3), "inst/extdata/nlcd_ak2001_error_matrix.csv")
