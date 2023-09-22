# reproducible example using "mtcars" dataset
data(mtcars)
y <- mtcars
y$cyl <- ifelse(y$cyl <= 8, "under 9 cyl", "over 9 cyl")
y$cyl <- factor(y$cyl)
lm(mpg ~ ., data=y)
# error occurs with the message:
# Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : contrasts can be applied only to factors with 2 or more levels

# error message interpretation:
# the error message typically occurs when the dataset "y" contains a factor variable (or column) with only one level "under 8 cyl."

# error solution:
# the following code removes all factor variables in the dataset "y" with only one level and stores the resulting dataset (cleaned) in "yy"
a <- which(grepl("factor", lapply(y, class))) # "a" contains the column indexes of all the factor variables in the dataset "y"
b <- lapply(y[a], function(x) length(unique(x))) # "b" (list) contains the number of unique values for each of the factor variables in the dataset "y"
b <- unlist(b, recursive = FALSE) # list "b" is flattened into a vector "b"
if (any(b == 1)) { # condition to check whether any elements in "b" are equal to 1
  c <- which(b == 1) # "c" contains the indexes of "b" elements which are equal to 1
  d <- paste0(names(c), collapse = "|") # "d" contains a single string of the names (same as column names of "y") of all the "b" elements which are equal to 1 separated by "|"
  e <- which(grepl(as.character(d), colnames(y))) # "e" contains the column indexes of all the factor variables (or columns) in the dataset "y" with only one level
  yy <- y[-e]} # "yy" contains all the variables (or columns) which are in "y," except the factor variables (or columns) with only one level
else {yy <- y} # "yy" contains all the variables (or columns) which are in "y," except the factor variables (or columns) with only one level

lm(mpg ~ ., data=yy)
# no error occurs
