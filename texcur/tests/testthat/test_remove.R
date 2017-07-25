test_that("rm_numbers raises an error for invalid input", {
    .check_incorrect_corpus_input(rm_numbers)
})

test_that("rm_non_alphanumeric raises an error for invalid input", {
    .check_incorrect_corpus_input(rm_non_alphanumeric)
})

test_that("rm_regexp raises an error for invalid input", {
    .check_incorrect_corpus_input(rm_regexp, pattern="")
    expect_error(rm_regexp(dplyr::data_frame(text=c("hello", "world"), 123)))
    expect_error(rm_regexp(dplyr::data_frame(text=c("hello", "world"), NULL)))
})

test_that("rm_emails raises an error for invalid input", {
    .check_incorrect_corpus_input(rm_emails)
})

test_that("rm_urls raises an error for invalid input", {
    .check_incorrect_corpus_input(rm_urls)
})

test_that("rm_whitespace raises an error for invalid input", {
    .check_incorrect_corpus_input(rm_whitespace)
})

test_that("rm_words raises an error for invalid input", {
    .check_incorrect_corpus_input(rm_words)

    expect_error(rm_words(dplyr::data_frame(text=c("hello", "world")), common_word_limit=0))
    expect_error(rm_words(dplyr::data_frame(text=c("hello", "world")), common_word_limit=""))

    expect_error(rm_words(dplyr::data_frame(text=c("hello", "world")), words=123))
    expect_error(rm_words(dplyr::data_frame(text=c("hello", "world")), words=c(1, 2, 3)))

    expect_error(rm_words(dplyr::data_frame(text=c("hello", "world")), rare_word_limit=0))
    expect_error(rm_words(dplyr::data_frame(text=c("hello", "world")), rare_word_limit=""))

    expect_error(rm_words(dplyr::data_frame(text=c("hello", "world")), tokens=data.frame()))
    expect_error(rm_words(dplyr::data_frame(text=c("hello", "world")), tokens=dplyr::data_frame()))
    expect_error(rm_words(dplyr::data_frame(text=c("hello", "world")), tokens=dplyr::data_frame(text=c("hello"))))
    expect_error(rm_words(dplyr::data_frame(text=c("hello", "world")), tokens=dplyr::data_frame(token=c(1, 2, 3))))
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

test_that("rm_regexp removes based on pattern from corpus", {
    corpus <- dplyr::data_frame(text=c("hello my name is this and that haha",
                                       "what is your name"))
    pattern <- "\\b(hello|name|haha)\\s?\\b"

    actual <- rm_regexp(corpus, pattern)
    expected <- dplyr::data_frame(text=c("my is this and that",
                                         "what is your"))

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

test_that("rm_words removes rare words from corpus", {
    corpus <- dplyr::data_frame(id=c("1", "2"),
                         text=c("what the what is not the what is",
                                       "is that your name or what"))
    rare_word_limit <- 2

    actual <- rm_words(corpus, rare_word_limit=rare_word_limit)
    expected <- dplyr::data_frame(id=c("1", "2"),
                                  text=c("what what is what is", "is what"))

    expect_equal(actual, expected)

    tokens <- corpus %>% tf_tokenize(token="words")
    actual <- rm_words(corpus, tokens, rare_word_limit=rare_word_limit)

    expect_equal(actual, expected)
})

test_that("rm_words removes common words from corpus", {
    corpus <- dplyr::data_frame(id=c("1", "2"),
                                text=c("what the what is not the what is",
                                       "is that your name or what"))
    common_word_limit <- 2

    actual <- rm_words(corpus, common_word_limit=common_word_limit)
    expected <- dplyr::data_frame(id=c("1", "2"),
                                  text=c("not", "that your name or"))

    expect_equal(actual, expected)

    tokens <- corpus %>% tf_tokenize(token="words")
    actual <- rm_words(corpus, tokens, common_word_limit=common_word_limit)

    expect_equal(actual, expected)
})

test_that("rm_words removes specified words from corpus", {
    corpus <- dplyr::data_frame(id=c("1", "2"),
                                text=c("hello world", "test That this actually works"))
    words <- c("hello", "actually", "That")

    actual <- rm_words(corpus, words=words)
    expected <- dplyr::data_frame(id=c("1", "2"),
                                  text=c("world", "test this works"))

    expect_equal(actual, expected)

    tokens <- corpus %>% tf_tokenize(token="words")
    actual <- rm_words(corpus, tokens, words=words)

    expect_equal(actual, expected)
})
