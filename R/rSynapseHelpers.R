# A better query, cleans the column names
betterQuery <- function(q, ...) {
  qres <- synQuery(q, blockSize=100)
  res <- qres$collectAll()
  colnames(res) <- gsub(".*\\.(.*)", "\\1", colnames(res))
  res
}

# Find records with missing data - blank, empty string, or NA
findBlankRecords <- function(res, cols2check, ...) {
  badrecs <- apply(res[, cols2check], 1, function(x) any(is.na(x)) | any(x == "") | any(x == " "))
  res[badrecs, ]  
}