test_that ("SeqGroup works on sequential grouping", {
  # Unrepeated Sequential Grouping -----------------
  x1 <- c (0, 1, 2, 5, 6)

  ## Basic grouping
  res <- SeqGroup (x = x1, Step = 1, Min = 0, Max = 6,
                   Wrap = FALSE, Simple = TRUE)

  ## Full output
  res1 <- SeqGroup (x = x1, Step = 1, Min = 0, Max = 6,
                    Wrap = FALSE, Simple = FALSE)


  # ---- Structure checks ----
  # Check that the output is a list with two components  (lengths, values).
  expect_type (res, "list")
  expect_type (res$lengths, "integer")
  expect_type (res$values, "character")

  expect_type (res1, "list")

  # ---- Relationship checks ----
  # Check that groups correspond to contiguous runs of Step
  expect_equal (as.vector (res$lengths), c (3L, 2L))

  # mins and maxs should reflect the group minima and maxima
  expect_equal (as.vector (res1$mins), c (0, 5))
  expect_equal (as.vector (res1$maxs), c (2, 6))

  # ---- Content checks ----
  # Values are formatted as Ini-End
  expect_equal (res$values, c ("0-2", "5-6"))
  expect_equal (res1$values, res$values)

  # groups should be a list of integer indices for each group
  expect_true (is.list (res1$groups))
  expect_equal (res1$groups [[1]], seq_len (3))
  expect_equal (res1$groups [[2]], c (4,5))

  # ---- Error checks ----
  # Empty input vector should trigger an informative error.
  expect_error (SeqGroup (numeric (0), Step = 1))
  # Non-numeric Step should raise an error  (type validation).
  expect_error (SeqGroup (x = x1, Step = "one", Min = 0, Max = 6))
  # Non-numeric x should error
  expect_error (SeqGroup (x = c ("a", "b"), Step = 0.5))




  # Complex Repeated Sequence --------------
  ## Floating Step formatting and tolerance -------------------------
  x2 <- FlyEast_adj$Hour

  res2 <- SeqGroup (x = x2, Step = 1/60, Wrap = FALSE, Simple = TRUE)
  res3 <- SeqGroup (x = x2, Step = 1/60, Wrap = FALSE, Simple = FALSE)
  res4 <- SeqGroup (x = x2, Step = 1/60, Wrap = TRUE,  Simple = TRUE)


  # ---- Structure checks ----
  # Check that the output is a list with two components  (lengths, values).
  expect_type (res2, "list")
  expect_type (res2$lengths, "integer")
  expect_type (res2$values, "character")

  expect_type (res3, "list")

  # ---- Relationship checks ----
  # Check that groups correspond to contiguous runs of Step
  ResLn <- as.vector (res2$lengths)
  expect_equal (ResLn,
                c (600L,1440L,1440L,1440L,1440L,1440L,1440L,1440L,1440L,1440L,
                   1440L,1440L,1440L,1440L,1440L,1440L,1440L,1440L,1440L,1440L,
                   1440L,1440L,1440L,1440L,1440L,1440L,1440L,1440L,1440L,1440L,
                   1440L,1440L,1440L,1440L,727L))

  # mins and maxs should reflect the group minima and maxima
  expect_equal (min (res3$mins), 0)
  expect_equal (max (res3$mins), 14L)
  expect_equal (min (res3$maxs), 12.1)
  expect_equal (max (res3$maxs), 23.9833333)

  expect_true (length  (res4$values) == 2 * length (ResLn) - 1)
  expect_false (all (res4$values %in% res2$values))
  expect_true (any (res4$values %in% res2$values))

  # ---- Content checks ----
  # Values are formatted as Ini-End
  expect_equal (res2$values [c (1,2,35)], c ("14.00-23.98",
                                             "0.00-23.98",
                                             "0.00-12.10"))
  expect_equal (res3$values, res2$values)

  # groups should be a list of integer indices for each group
  expect_true (is.list (res3$groups))
  expect_equal (res3$groups [[1]], seq_len (ResLn [1]))
  expect_equal (res3$groups [[2]], ResLn [1] + seq_len (ResLn [2]))


  expect_true ("23.98-23.98" %in% res4$values)


  # Tolerance behavior -------------------------
  res5 <- SeqGroup (x = x2, Step = 1/60, Wrap = TRUE,  Simple = TRUE,
                    tol = 1e-20)

  # ---- Structure checks ----
  # Check that the output is a list with two components  (lengths, values).
  expect_type (res5, "list")

  # ---- Relationship checks ----
  # Floating value exceed > tol leads to elementwise separation
  expect_equal (length (res5$values), length (x2))

  # ---- Content checks ----
  # Values are formatted as Ini-End
  expect_true (all (res5$length == 1L))
})










# SeqFill -------------------------
test_that ("SeqFill works on sequential filling", {

  ##  Matrix form -------------------------
  x <- c (0, 1, 4, 5, 9)
  res_matrix <- SeqFill (x, Step = 1, Min = 0, Max = 10,
                        Filler = NA, Format = "Matrix")

  # ---- Structure checks ----
  # Check output dimension and structure
  expect_true (is.matrix (res_matrix))
  expect_type (res_matrix, "double")
  expect_equal (length (res_matrix), 1 * 11)
  expect_equal (dim (res_matrix), c (1, 11))

  # ---- Relationship checks ----
  # Positional checks  (1-based indexing): verify mapping from input -> grid.
  expect_equal (res_matrix [1, 1], 0)
  expect_equal (res_matrix [1, 2], 1)
  expect_equal (res_matrix [1, 5], 4)
  expect_equal (res_matrix [1, 6], 5)
  expect_equal (res_matrix [1, 10], 9)

  # ---- Content checks ----
  # Check the full matrix content equals the expected pattern with NA fillers.
  expected_matrix <- matrix (NA_real_, nrow = 1, ncol = 11)
  expected_matrix [1, c (1,2,5,6,10)] <- c (0,1,4,5,9)
  expect_equal (res_matrix, expected_matrix)

  # ---- Error checks ----
  # Wrong type of object should trigger an error  (non-numeric x).
  expect_error (SeqFill (c ("a","b"), Step = 1, Min = 0, Max = 10,
                       Filler = NA, Format = "Matrix"))


  ## Vector form -------------------------
  res_vector <- SeqFill (x, Step = 1, Min = 0, Max = 10,
                        Filler = NA, Format = "Vector")

  # ---- Structure checks ----
  # Check that the output is a numeric vector of expected length.
  # - expect_type (..., "double") ensures numeric storage mode.
  # - expect_equal (length (...), 11) ensures vector covers grid 0..10.
  expect_type (res_vector, "double")
  expect_equal (length (res_vector), 11)
  expect_equal (length (res_vector), length (0:10))

  # ---- Relationship checks ----
  # Positional checks in the linearized vector representation.
  expect_equal (res_vector [1], 0)
  expect_equal (res_vector [2], 1)
  expect_equal (res_vector [5], 4)
  expect_equal (res_vector [6], 5)
  expect_equal (res_vector [10], 9)

  # ---- Content checks ----
  # Full vector should equal expected vector with NA fillers.
  expected_vec <- rep (NA_real_, 11)
  expected_vec [c (1,2,5,6,10)] <- c (0,1,4,5,9)
  expect_equal (res_vector, expected_vec)

  # ---- Error checks ----
  # Invalid Step type should trigger an error.
  expect_error (SeqFill (x, Step = "one", Min = 0, Max = 10,
                       Filler = NA, Format = "Vector"))



  # SeqFill: All form with Gap -------------------------
  x2 <- c (0, 1, 2, 8, 9)
  out_all <- SeqFill (x2, Step = 1, Min = 0, Max = 10,
                     Filler = NA, Gap = 3, Format = "All")

  # ---- Structure checks ----
  # Check that the output is a list and contains expected components.
  expect_type (out_all, "list")
  expect_true (all (c ("meta", "matrix", "vector", "id") %in% names (out_all)))
  expect_true (is.matrix (out_all$matrix))
  expect_type (out_all$vector, "double")
  expect_type (out_all$id, "integer")

  # ---- Relationship checks ----
  # Matrix dimensions should reflect grouping by Gap  (two groups -> two rows).
  # id should map to 1-based grid positions for the input values.
  expect_equal (dim (out_all$matrix), c (2, 11))
  expect_equal (as.integer (out_all$id), as.integer (c (1,2,3,20,21)))

  # ---- Content checks ----
  # Matrix: first row contains 0,1,2; second row contains 8,9.
  expected_matrix2 <- matrix (NA_real_, nrow = 2, ncol = 11)
  expected_matrix2 [1, 1:3] <- c (0,1,2)
  expected_matrix2 [2, 9:10] <- c (8,9)
  expect_equal (out_all$matrix, expected_matrix2)

  # Vector: linearized representation of the full grid with groups placed.
  expected_vector2 <- c (0,1,2, rep (NA_real_, 16), 8,9, NA_real_)
  expect_equal (out_all$vector, expected_vector2)

  # Meta: meta should be tabular and indicate two groups with expected columns.
  expect_true (is.data.frame (out_all$meta) || is.matrix (out_all$meta))
  expect_equal (nrow (out_all$meta), 2)
  expect_true (all (c ("group_id", "na_in_group") %in%
                      colnames (out_all$meta)))

  # ---- Error checks ----
  # Non-numeric Gap should trigger an error.
  expect_error (SeqFill (x2, Step = 1, Min = 0, Max = 10,
                       Filler = NA, Gap = "three", Format = "All"))



  ## Floating point Step  (tolerance-safe) -------------------------
  x3 <- c (0.0, 0.5, 1.0, 2.0)
  res_float <- SeqFill (x3, Step = 0.5, Min = 0, Max = 2,
                       Filler = NA, Format = "Matrix")

  # ---- Structure checks ----
  # Ensure numeric matrix and expected number of columns for the floating grid.
  expect_true (is.matrix (res_float))
  expect_type (res_float, "double")
  expect_equal (ncol (res_float), 5)

  # ---- Relationship checks ----
  # Values should align to the floating grid positions at expected columns.
  expect_equal (res_float [1,1], 0.0)
  expect_equal (res_float [1,2], 0.5)
  expect_equal (res_float [1,3], 1.0)
  expect_equal (res_float [1,5], 2.0)

  # ---- Content checks ----
  # Full matrix should equal expected pattern and with floating value = tol
  expected_float <- matrix (NA_real_, nrow = 1, ncol = 5)
  expected_float [1, c (1,2,3,5)] <- c (0.0, 0.5, 1.0, 2.0)
  expect_equal (res_float, expected_float, tolerance = 1e-8)

  # ---- Error checks ----
  # Non-numeric inputs should raise an error.
  expect_error (SeqFill (c ("a","b"), Step = 0.5, Min = 0, Max = 2,
                       Filler = NA, Format = "Matrix"))
})







# Mode -------------------
test_that("Mode correctly identifies the most frequent value", {
  # Basic case with a clear mode
  x1 <- c (1, 2, 2, 3, 4)
  expect_equal (Mode (x1), 2)

  # Case with multiple modes (should return the first one)
  x2 <- c (1, 1, 2, 2, 3)
  expect_equal (Mode (x2), c(1,2))

  # Case with all unique values (should return all values)
  x3 <- c (1, 2, 3, 4, 5)
  expect_equal (Mode (x3), seq_len (5L))

  # Case with NA values (should ignore NA and return the mode of non-NA values)
  x4 <- c (1, NA, NA, NA, 2)
  expect_equal (Mode (x4, na.rm = FALSE), NA_real_)



  # Repeated Complex Sequence -----------------
  x <- FlyEast_adj$Hour
  res  <- Mode (x)
  res1 <- Mode (x, ties = "First")
  resN <- unique (x [!x %in% res])
  # ---- Structure checks ----
  # both are vectors of type double
  expect_true (is.vector (res))
  expect_type (res1, "double")

  # ---- Relationship checks ----
  # All vs. First when ties exist
  expect_true (length (res) > length(res1))

  # ---- Content checks ----
  expect_true (res1 == 0)
  expect_true (all (round (resN, 4) %in%
                 round (seq (12.11667, 14, by =  1/60), 4)))

})





# Padding ------------------
test_that ("Padding correctly add ", {
  x <- 1:10

  # Symmetrical window
  res  <- Padding (x, Window = 3, Pad = NA)
  res1 <- Padding (x, Window = 3, Pad = "Zero")
  res2 <- Padding (x, Window = 3, Pad = "Reflect")

  # Asymmetrical window
  res4 <- Padding (x, Window = c (2, 3), Pad = NA)
  res5 <- Padding (x, Window = c (2, 3), Pad = "Zero")
  res6 <- Padding (x, Window = c (2, 3), Pad = "Reflect")

  # String Asymmetrical window
  x2 <- c ("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
  res7 <- Padding (x2, Window = c (2, 3), Pad = NA)
  res8 <- Padding (x2, Window = c (2, 3), Pad = "Zero")
  res9 <- Padding (x2, Window = c (2, 3), Pad = "Reflect")

  # ---- Structure checks ----
  # Symmetric numeric results should be numeric vectors of
  # length original + 2*Window
  res12 <- c (res, res1, res2)
  expect_true (is.numeric (res12))
  expect_type (res12, "integer")
  expect_equal (length (res12), 3 * (length (x) + 2 * 3))


  # Asymmetrical numeric results should be numeric vectors of
  # length original + left + right
  res456 <- c (res4, res5, res6)
  expect_true (is.numeric (res456))
  expect_type (res456, "integer")
  expect_equal (length (res456), 3 * (length (x) + 5))


  # Character results should be character vectors of
  # expected length
  res789 <- c (res7, res8, res9)
  expect_true (is.character (res789))
  expect_type (res789, "character")
  expect_equal (length (res789), 3 * (length (x) + 5))

  # ---- Relationship checks ----
  # Extract left and right pads given total window sizes
  left_pad  <- function (v, left_n) v [seq_len (left_n)]
  right_pad <- function (v, right_n) tail (v, right_n)


  for (i in list (res, res4, res7)) {
    expect_true (all (is.na (right_pad (i, 3))))
  }

})
