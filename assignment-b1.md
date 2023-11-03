Assignment B-1: Making a function - Khanh Nguyen
================

**Total Points**: 100

This assignment covers making a function in R, documenting it, and
testing it.

## Exercise 1: Make a Function (25 points) and Exercise 2: Document your Function (20 points)

In this exercise, you’ll be making a function and fortifying it. The
function need not be complicated. The function need not be “serious”,
but shouldn’t be nonsense.

### Loading the library

First, we will load the packages: `tidyverse` and `testthat`.
`tidyverse` includes some functions used to create our new function, and
`testthat` was used to test our new function.

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null
    ## 
    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

In this assignment, I will create a function to to generate the grade
letter based on the grade scheme at UBC from
<https://students.ubc.ca/enrolment/courses/grades>. For example: A is
for score 80-100, B is for 70-79, C is for 60-69, D is for 50-59 and F
(fail) is for score less than 50.

However, to make the function become generic and avoid magic numbers, we
can set our own scale when using the function by setting the numbers for
lowest_D, lowest_C, lowest_B and lowest_A variables.

If the score is less than lowest_D, the grade letter will be “F”. If the
score is less than lowest_C, the grade letter will be “D”. If the score
is less than lowest_B, the grade letter will be “C”. If the score is
less than lowest_A, the grade letter will be “B”. Otherwise, the grade
letter will be “A”

lowest_D, lowest_C, lowest_B, lowest_A are the lowest score in the range
of D, C, B and A, respectively.

``` r
#' Title: Determine the grade letter based on score input
#'
#'This function will create a new column called 'grade_letter' which contains the grade letter based on the number of scores of students in the imported data. 
#'
#' @param data is a variable that represents the imported data. It contains information of the students and their scores. The information can be anything (student's ID, name, major or date of birth). The reason why I named this 'data' is because it is informative and it can tell the user that the input should be a data frame. 
#'
#' @param score_col is the name of the column which contains score variables (numeric variable). The reason why I named this 'score_col' is to remind the users to indicate which score column they want to use for converting to grade letters
#' 
#' @param lowest_D is the lowest score in the score range for grade letter D (numeric variable)
#' @param lowest_C is the lowest score in the score range for grade letter C (numeric variable)
#' @param lowest_B is the lowest score in the score range for grade letter B (numeric variable)
#' @param lowest_A is the lowest score in the score range for grade letter A (numeric variable) 
#'The reason why I named these variables is because it is also informative and directly tells the users what they should enter for these variables. 
#'
#' @return a data frame which contains all the previous variables in the original data plus one new variable called 'grade letter'.The 'grade_letter' column will contain the grade letter based on the number of score of students in the imported data.
#' @export
#'
#' @examples
#' grade_letter(student_grades,"Score",50,60,70,80)
#'
grade_letter <- function(data, score_col, lowest_D, lowest_C, lowest_B, lowest_A) {
  
  # check if the data imported is a data frame
  if (!is.data.frame(data)) {
    stop("The input must be a dataframe or tibble")
  }
  
  # Check if score_col exist in the data
  if (!score_col %in% colnames(data)) {
    stop("The score_col does not exist in the data")
  }
 
  # check if the score column is a numeric variable
  if (!is.numeric(data[[score_col]])) {
    stop("The score must be numeric")
  }
  
  # check if lowest_D, lowest_C, lowest_B, lowest_A are numeric variables
  if (!is.numeric(lowest_D) | !is.numeric(lowest_C) | !is.numeric(lowest_B)| !is.numeric(lowest_A)) {
    stop("The grades must be numeric")
  }
      
  # filter NA values
  data <- data[!is.na(data[[score_col]]), ]
  
  # create a new column 'grade_letter' based on the score in score column (score_col)
  results <- data %>% 
    mutate(grade_letter = if_else(.data[[score_col]] < lowest_D, "F",
                        if_else(.data[[score_col]] < lowest_C, "D",
                        if_else(.data[[score_col]] < lowest_B, "C",
                        if_else(.data[[score_col]] < lowest_A, "B", "A")))))
  return(results)
  
}
```

## Exercise 3: Include examples (15 points)

Demonstrate the usage of your function with a few examples. Use one or
more new code chunks, describing what you’re doing.

Note: If you want to deliberately show an error, you can use
`error = TRUE` in your code chunk option.

1.  First, we should create a data that contains information of student
    (StudentID and Student_Name) and one column for their scores. The
    data input can contain any information of students but it must have
    one column for the score.

``` r
student_grades <- data.frame(
  StudentID = c(1,2,3,4,5),
  Student_Name = c('John','Sarah','Mike','James','Thomas'),
  Score = c(89,75,63,85, 93)
)

print(student_grades)
```

    ##   StudentID Student_Name Score
    ## 1         1         John    89
    ## 2         2        Sarah    75
    ## 3         3         Mike    63
    ## 4         4        James    85
    ## 5         5       Thomas    93

2.  Now we will use the function to create a new column which contains
    grade letter based on the score. For example, I chose lowest_D = 50,
    lowest_C = 60, lowest_B =70 and lowest_A =80.

``` r
grade_letter(student_grades,"Score",50,60,70,80)
```

    ##   StudentID Student_Name Score grade_letter
    ## 1         1         John    89            A
    ## 2         2        Sarah    75            B
    ## 3         3         Mike    63            C
    ## 4         4        James    85            A
    ## 5         5       Thomas    93            A

3.  If we want to have a higher grade scale, let’s make lowest_A = 90,
    lowest_B = 80, lowest_C = 70 and lowest_D =60. Now we can see that
    only one student get an A.

``` r
grade_letter(student_grades,"Score",60,70,80,90)
```

    ##   StudentID Student_Name Score grade_letter
    ## 1         1         John    89            B
    ## 2         2        Sarah    75            C
    ## 3         3         Mike    63            D
    ## 4         4        James    85            B
    ## 5         5       Thomas    93            A

## Exercise 4: Test the Function (25 points)

Running examples is a good way of checking by-eye whether your function
is working as expected. But, having a formal “yes or no” check is useful
when you move on to other parts of your analysis.

Write formal tests for your function. You should use at least three
non-redundant uses of an `expect_()` function from the `testthat`
package, and they should be contained in a `test_that()` function (or
more than one). They should all pass.

Example of non-redundant inputs:

- Vector with no NA’s
- Vector that has NA’s
- Vector of a different type (if relevant)
- Vector of length 0, like `numeric(0)`.

Example of redundant inputs:

- Providing a different number (unless one of these numbers have some
  significance, like an extreme point – just tell us if that’s the case)

**Test for data with no NA’s**

``` r
Set1 <- data.frame(
  StudentID = c(1,2,3,4,5),
  Student_Name = c('John','Sarah','Mike','James','Thomas'),
  Score = c(89,75,63,85, 93)
)
```

``` r
Results1 <- grade_letter(Set1,"Score",50,60,70,80) 
test_that("Data with no NA's",{
  expect_is(Results1, "data.frame") #test if the result is a data frame
  expect_equal(Results1$grade_letter[1], "A") #test of the first grade letter for John is A
})
```

    ## Test passed 🥇

**Test for data with NA’s**

``` r
Set2 <- data.frame(
  StudentID = c(1,2,3,4,5),
  Student_Name = c('John','Sarah','Mike','James','Thomas'),
  Score = c(89,75,NA,85, 93)
)
```

``` r
Results2 <- grade_letter(Set2,"Score",50,60,70,80) 
test_that("Data with NA's",{
  expect_equal(nrow(Results2), 4) # test if the function filter the NA column. The result should only contain 4 columns
})
```

    ## Test passed 🥳

**Vector of a different type (if relevant)**

``` r
Set3 <- data.frame(
  StudentID = c(1,2,3,4,5),
  Student_Name = c('John','Sarah','Mike','James','Thomas'),
  Score = c("Good","Fair","Low","Good", "Good")
)
```

``` r
test_that("Data with different type",{
 expect_error(grade_letter(Set3,"Score",50,60,70,80),"The score must be numeric")
})
```

    ## Test passed 🥳

**Vector of length 0, like `numeric(0)`**

``` r
Set4 <- data.frame(
  StudentID = numeric(0),
  Student_Name =character(0),
  Score = numeric(0)
)
```

``` r
Results4 <-grade_letter(Set4,"Score",50,60,70,80) 

test_that("Data with vector of length 0", {
  expect_true(length(Results4$grade_type) == 0)
})
```

    ## Test passed 🎉

**Test for data with no score_column exist**

``` r
Set5 <- data.frame(
  StudentID = c(1,2,3,4,5),
  Student_Name = c('John','Sarah','Mike','James','Thomas')
)
```

``` r
test_that("Data with no score_column",{
 expect_error(grade_letter(Set5,"Score-s",50,60,70,80),"The score_col does not exist in the data")
})
```

    ## Test passed 🥇

We passed all the four tests above.
