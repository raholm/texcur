test_that("rm_numbers raises an error for invalid input", {
    .check_incorrect_input(rm_numbers)
})

test_that("rm_non_alphanumeric raises an error for invalid input", {
    .check_incorrect_input(rm_non_alphanumeric)
})

test_that("rm_emails raises an error for invalid input", {
    .check_incorrect_input(rm_emails)
})

test_that("rm_urls raises an error for invalid input", {
    .check_incorrect_input(rm_urls)
})

test_that("rm_whitespace raises an error for invalid input", {
    .check_incorrect_input(rm_whitespace)
})

test_that("rm_numbers removes numbers from corpus", {
    corpus <- dplyr::data_frame(text=c("123 hello 123", "123 h3ll0 321a"))

    actual <- rm_numbers(corpus)
    expected <- dplyr::data_frame(text=c("hello", "h3ll0 321a"))

    expect_equal(actual, expected)
})

test_that("rm_non_alphanumeric removes non-alphanumeric characters from corpus", {
    corpus <- dplyr::data_frame(text=c("hello{@\\[}}}^^.,-^'", "^^. there's a bu^gger!"))

    actual <- rm_non_alphanumeric(corpus)
    expected <- dplyr::data_frame(text=c("hello", "theres a bugger"))

    expect_equal(actual, expected)
})

test_that("rm_emails removes emails from corpus", {
    corpus <- dplyr::data_frame(text=c("the user.name@torka.se is great",
                                       "my email is user@example.com.",
                                       "user.name@example.com.org."))
    actual <- rm_emails(corpus)
    expected <- dplyr::data_frame(text=c("the  is great", "my email is .", "."))

    expect_equal(actual, expected)
})

test_that("rm_urls removes urls from corpus", {
    corpus <- dplyr::data_frame(text=c("test http://regexr.com/foo.html?q=bar test",
                                       "test https://regexr.com/foo.html?q=bar test",
                                       "test ftp://regexr.com/foo.html?q=bar test",
                                       "test regexr.com/foo.html?q=bar test",
                                       "test www.example.com test",
                                       "test example.com test"))

    actual <- rm_urls(corpus)
    expected <- dplyr::data_frame(text=rep("test  test", nrow(corpus)))

    expect_equal(actual, expected)
})

test_that("rm_whitespace removes extra whitespace from corpus", {
    corpus <- dplyr::data_frame(text=c("test  test ",
                                       "  test test",
                                       "test\t\ttest",
                                       "test\n\ntest",
                                       "test \n\ntest"))

    actual <- rm_whitespace(corpus)
    expected <- dplyr::data_frame(text=c("test test",
                                         "test test",
                                         "test\ttest",
                                         "test\ntest",
                                         "test \ntest"))

    expect_equal(actual, expected)
})