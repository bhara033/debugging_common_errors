# simple solution using grepl
a <- which(grepl("mpg|disp", colnames(mtcars)))
b <- which(!grepl("mpg|disp", colnames(mtcars)))
ab <- setdiff(a,b) # additional check
as.formula(paste0(colnames(mtcars)[ab], collapse = " ~ ")) # using colnames(mtcars)[a] should yield the same results

# if the simpler solution also fails, try simplifying the variable names
# rewrite model input formula with simplified names (variable index number with prefix "var")
clean_formula <- function(data, ...) {
  formula_string <- paste0(...)
  
  if (!stringi::stri_detect(formula_string, fixed = " ~ ")) {
    formula_string <- paste0(" ~ ", formula_string)
  }
  
  a <- colnames(data)
  b <- paste0("var", 1:length(a))
  
  # Replace variable names in the formula
  formula_str <- stringi::stri_replace_all_regex(formula_string, paste0("\\b", a, "\\b"), b, vectorize_all = FALSE)
  
  # Remove words that do not begin with "var"
  cleaned_formula_str <- stringi::stri_replace_all_regex(formula_str, "\\b(?!var\\d | offset\\()\\b", "", vectorize_all = FALSE)
  
  # Remove trailing '+' or '~' characters
  cleaned_formula_str <- stringi::stri_replace_all_regex(cleaned_formula_str, "[+~]+$", "")
  
  as.formula(cleaned_formula_str)
}

# example usage with multiple input strings
data(mtcars)
s1 <- "mpg ~ cyl"
s2 <- "+ disp"
s3 <- "+ hp"

# the function automatically combines multiple input strings into a single formula
result <- clean_formula(mtcars, s1, s2, s3)
print(result)
