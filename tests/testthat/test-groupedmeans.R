## Checking whether the groupedmeans() function works by comparing it to a manual calculation of mean life expectancy, by year, in the gapminder dataset

test_that("function returns equivalent output table as manually calculating grouped means in gapminder dataset", {
  expect_equal(assignmentB2MadelineWoodley::groupedmeans(data = gapminder::gapminder, var1 = year, var2 = lifeExp, removeNAsvar2 = TRUE), gapminder::gapminder %>%
                 group_by(year) %>%
                 filter(!is.na(year)) %>%
                 summarise(meanvar2 = mean(lifeExp, na.rm = TRUE)))
})

## Checking whether the groupedmeans() function works by comparing it to a manual calculation of mean bill length, by species, in the penguins dataset (i.e., same purpose as check #1 but for different data)

test_that("function returns equivalent output table as manually calculating grouped means in penguins dataset", {
  expect_equal(assignmentB2MadelineWoodley::groupedmeans(data = palmerpenguins::penguins, var1 = species, var2 = bill_length_mm, removeNAsvar2 = TRUE), palmerpenguins::penguins %>%
                 group_by(species) %>%
                 filter(!is.na(species)) %>%
                 summarise(meanvar2 = mean(bill_length_mm, na.rm = TRUE)))

})

## Checking that error is returned when var2 is assigned to a categorical variable (Error 1/2 in function)

test_that("function returns error when var 2 is assigned to categorical data", {
  expect_error(assignmentB2MadelineWoodley::groupedmeans(data = palmerpenguins::penguins, var1 = sex, var2 = species, removeNAsvar2 = TRUE), "var2 is not numeric")
})

## Checking that error is returned when NA values in var2 are not dropped (default value for removeNAsvar2 argument) (Error 2/2 in function)

test_that("function returns error when NAs in var2 are not dropped", {
  expect_error(assignmentB2MadelineWoodley::groupedmeans(data = palmerpenguins::penguins, var1 = species, var2 = bill_length_mm), "var2 contains NA values, cannot compute mean")
})

