#ifndef TEXCUR_WORD_TRANSFORMER_H_
#define TEXCUR_WORD_TRANSFORMER_H_

#include "def.h"

namespace texcur {

  class WordToIndexTransformer {
  public:
    static const WordIndex unobserved_word_index;
    static const Word unobserved_word;

  public:
    explicit WordToIndexTransformer();

    WordToIndexTransformer(const WordToIndexTransformer& other) = default;
    WordToIndexTransformer(WordToIndexTransformer&& other) = default;

    WordToIndexTransformer& operator=(const WordToIndexTransformer& other) = default;
    WordToIndexTransformer& operator=(WordToIndexTransformer&& other) = default;

    ~WordToIndexTransformer() = default;

    void update(const Word& word);
    void update(const WordVector& words);

    WordIndex update_and_transform(const Word& word);
    WordIndexVector update_and_transform(const WordVector& words);

    WordIndex transform(const Word& word) const;
    WordIndexVector transform(const WordVector& words) const;

    Word revert(const WordIndex& index) const;
    WordVector revert(const WordIndexVector& indexes) const;

  private:
    WordIndex next_index_;
    Vector<String> words_;
    Map<String, WordIndex> indexes_;

    void add_if_missing(const Word& word);
    WordIndex add_if_missing_and_transform(const Word& word);
    Word get_word_or_invalid_word(const WordIndex& index) const;
    WordIndex get_index_or_invalid_index(const Word& word) const;

  };

} // namespace texcur

#endif // TEXCUR_WORD_TRANSFORMER_H_
