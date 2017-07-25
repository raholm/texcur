test_that("tf_lowercase raises error for invalid input", {
    .check_incorrect_corpus_input(tf_lowercase)
})

test_that("tf_tokenize raises error for invalid input", {
    .check_incorrect_corpus_input(tf_tokenize)
})

test_that("tf_merge_tokens raises an error for invalid input", {
    .check_incorrect_tokens_input(tf_merge_tokens)
    expect_error(tf_merge_tokens(dplyr::data_frame(token=c("hello", "world"), NULL)))
    expect_error(tf_merge_tokens(dplyr::data_frame(token=c("hello", "world"), 123)))
})

test_that("tf_lowercase transforms corpus to lowercase", {
    corpus <- dplyr::data_frame(text=c("Hello world!", "HellO WorlD"))

    actual <- tf_lowercase(corpus)
    expected <- dplyr::data_frame(text=c("hello world!", "hello world"))

    expect_equal(actual, expected)
})

test_that("tf_tokenize tokenize corpus", {
    corpus <- dplyr::data_frame(text=c("Hello World!",
                                       "What are you doing",
                                       "What is the meaning of life?"))

    actual <- tf_tokenize(corpus)
    expected <- dplyr::data_frame(token=c("Hello", "World", "What", "are", "you",
                                          "doing", "What", "is", "the", "meaning", "of", "life"))

    expect_equal(actual, expected)

    actual <- tf_tokenize(corpus, stringr::str_split, pattern="( )+")
    expected <- dplyr::data_frame(token=c("Hello", "World!", "What", "are", "you",
                                          "doing", "What", "is", "the", "meaning", "of", "life?"))

    expect_equal(actual, expected)

    corpus <- dplyr::data_frame(id=c("1", "2", "3"),
                                text=c("Hello World!",
                                       "What are you doing",
                                       "What is the meaning of life?"))

    actual <- tf_tokenize(corpus)
    expected <- dplyr::data_frame(id=c(rep("1", 2), rep("2", 4), rep("3", 6)),
                                  token=c("Hello", "World",
                                          "What", "are", "you", "doing",
                                          "What", "is", "the", "meaning", "of", "life"))
    expect_equal(actual, expected)
})

test_that("tf_merge_tokens constructs a corpus", {
    tokens <- dplyr::data_frame(id=c("1", "1", "2", "2", "4"),
                                          token=c("hello", "world!", "new", "years", "monkey"))

    actual <- tf_merge_tokens(tokens)
    expected <- dplyr::data_frame(id=c("1", "2", "4"),
                                  text=c("hello world!", "new years", "monkey"))

    expect_equal(actual, expected)

    actual <- tf_merge_tokens(tokens, delim="|")
    expected <- dplyr::data_frame(id=c("1", "2", "4"),
                                  text=c("hello|world!", "new|years", "monkey"))

    expect_equal(actual, expected)
})
