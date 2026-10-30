#pragma once
#include <cstddef>

namespace eeg {

template <typename T, std::size_t Size>
class Ring {
 public:
  void push(T value) {
    items_[next_] = value;
    next_ = (next_ + 1) % Size;
  }

 private:
  T items_[Size] = {};
  std::size_t next_ = 0;
};

}  // namespace eeg
