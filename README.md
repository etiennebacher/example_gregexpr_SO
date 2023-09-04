``` r
library(countrycode)

country.name <- codelist$country.name.en
regex <- codelist$country.name.en.regex
dest <- codelist$iso3c

country.name[1:10]
#>  [1] "Afghanistan"       "Albania"           "Algeria"          
#>  [4] "American Samoa"    "Andorra"           "Angola"           
#>  [7] "Anguilla"          "Antarctica"        "Antigua & Barbuda"
#> [10] "Argentina"
regex[1:10]
#>  [1] "afghan"               "albania"              "algeria"             
#>  [4] "^(?=.*americ).*samoa" "andorra"              "angola"              
#>  [7] "anguill?a"            "antarctica"           "antigua"             
#> [10] "argentin"
dest[1:10]
#>  [1] "AFG" "ALB" "DZA" "ASM" "AND" "AGO" "AIA" "ATA" "ATG" "ARG"

sourcefctr <- factor(country.name)

origin <- "country.name.en.regex"
destination <- "iso3c"

dictionary <- countrycode::codelist
dict <- stats::na.omit(dictionary[, c(origin, destination)])

# prefix a vector of regexes with random IDs
append_unique_id <- function(x) {
  for (i in seq_along(x)) {
    x[i] <- paste0("<", paste(sample(letters, 10), collapse = ""), ">", x[i])
  }
  x
}

# Split a vector into several chunks
# https://stackoverflow.com/questions/3318333
chunk <- function(x, n) split(x, factor(sort(rank(x) %% n)))


# Using groups + gregexpr() -------------------------------------------

system.time({
  regexes <- append_unique_id(dict[[origin]])
  regexes <- paste0("(?", regexes, ")")
  regex_chunks <- chunk(regexes, 5)
  
  # match levels of sourcefctr
  matches_gregexpr <- sapply(
    c(levels(sourcefctr), NA), # add NA so there's at least one item
    function(x) {
      matchidx <- list()
      
      # sometimes an error is triggered by encoding issues
      x <- tryCatch(trimws(x), error = function(e) x)
      for (i in seq_along(regex_chunks)) {
        
        tmp <- paste(regex_chunks[[i]], collapse = "|")
        out <- gregexpr(tmp, x, perl = TRUE, ignore.case = TRUE)[[1]]
        
        starts <- attr(out, "capture.start")
        matches <- starts[, nchar(dimnames(starts)[[2]]) > 0, drop = FALSE]
        if (length(matches) == 0) {
          matchidx[[i]] <- NULL
          next
        }
        matches <- which(colSums(matches) > 0)
        if (i > 1 && length(matches) > 0) {
          matches <- matches + sum(sapply(regex_chunks[1:(i-1)], length))
        }
        matchidx[[i]] <- matches
        
      }
      
      matchidx <- unlist(matchidx)
      dict[matchidx, destination]
    })
  
})
#>    user  system elapsed 
#>    0.34    0.06    0.41


# Using sapply + grepl() -------------------------------------------

system.time({
  matches_grepl <-
    sapply(c(levels(sourcefctr), NA), function(x) { 
      x <- tryCatch(trimws(x), error = function(e) x) 
      matchidx <- sapply(dict[[origin]], function(y) grepl(y, x, perl = TRUE, ignore.case = TRUE))
      dict[matchidx, destination]
    })
})
#>    user  system elapsed 
#>    7.57    0.05    7.66



# Check -------------------------------------------------------------

waldo::compare(matches_gregexpr, matches_grepl)
#> ✔ No differences
```
