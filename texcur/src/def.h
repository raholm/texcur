#ifndef TEXCUR_DEF_H_
#define TEXCUR_DEF_H_

#include <string>
#include <vector>
#include <map>
#include <set>
#include <memory>
#include <Rcpp.h>

namespace texcur {

  using String = std::string;
  using size_t = std::size_t;

  template<typename T>
  using Vector = std::vector<T>;

  template<typename Key, typename Value>
  using Map = std::map<Key, Value>;

  template<typename T>
  using Set = std::set<T>;

  template<typename First, typename Second>
  using Pair = std::pair<First, Second>;

  template<typename T>
  using Matrix = std::vector<std::vector<T>>;

  using StringVector = Rcpp::StringVector;

  using WordIndex = int;
  using WordIndexVector = Vector<WordIndex>;
  using WordIndexMatrix = Matrix<WordIndex>;

  using Word = String;
  using WordVector = StringVector;
  using Count = size_t;

} // namespace texcur

#endif // TEXCUR_DEF_H_
