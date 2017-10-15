#include "transformer.h"

#include <Rcpp.h>

#include "def.h"

namespace texcur {

  const WordIndex WordToIndexTransformer::unobserved_word_index= -1;
  const Word WordToIndexTransformer::unobserved_word = "<!!unknown!!>";

  WordToIndexTransformer::WordToIndexTransformer()
    : next_index_{0},
      words_{},
      indexes_{} {}

  void WordToIndexTransformer::update(const Word& word) {
    add_if_missing(word);
  }

  void WordToIndexTransformer::update(const WordVector& words) {
    for (auto const& word : words)
      update(Rcpp::as<Word>(word));
  }

  WordIndex WordToIndexTransformer::update_and_transform(const Word& word) {
    return add_if_missing_and_transform(word);
  }

  WordIndexVector WordToIndexTransformer::update_and_transform(const WordVector& words) {
    WordIndexVector indexes(words.size());

    for (unsigned i = 0; i < words.size(); ++i)
      indexes.at(i) = update_and_transform(Rcpp::as<Word>(words(i)));

    return indexes;
  }

  WordIndex WordToIndexTransformer::transform(const Word& word) const {
    return get_index_or_invalid_index(word);
  }

  WordIndexVector WordToIndexTransformer::transform(const WordVector& words) const {
    WordIndexVector indexes(words.size());

    for (unsigned i = 0; i < words.size(); ++i)
      indexes.at(i) = transform(Rcpp::as<Word>(words(i)));

    return indexes;
  }

  Word WordToIndexTransformer::revert(const WordIndex& index) const {
    return get_word_or_invalid_word(index);
  }

  WordVector WordToIndexTransformer::revert(const WordIndexVector& indexes) const {
    WordVector words(indexes.size());

    for (unsigned i = 0; i < words.size(); ++i)
      words(i) = revert(indexes.at(i));

    return words;
  }

  void WordToIndexTransformer::add_if_missing(const Word& word) {
    auto p = indexes_.insert(std::make_pair(word, next_index_));
    if (p.second) {
      words_.push_back(word);
      ++next_index_;
    }
  }

  WordIndex WordToIndexTransformer::add_if_missing_and_transform(const Word& word) {
    auto p = indexes_.insert(std::make_pair(word, next_index_));
    if (p.second) {
      words_.push_back(word);
      return next_index_++;
    }
    return p.first->second;
  }

  Word WordToIndexTransformer::get_word_or_invalid_word(const WordIndex& index) const {
    if (index < 0 || index >= next_index_) return unobserved_word;
    return words_.at(index);
  }

  WordIndex WordToIndexTransformer::get_index_or_invalid_index(const Word& word) const {
    auto it = indexes_.find(word);
    if (it == indexes_.end()) return unobserved_word_index;
    return it->second;
  }

} // namespace texcur
