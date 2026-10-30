#pragma once
#include <string>
#include <thread>
#include <vector>
#include "ring.h"

namespace eeg {

class Board {
 public:
  explicit Board(std::string port);
  std::vector<double> read(std::size_t count);
  void stream(std::size_t count);
  bool open() const { return !port_.empty(); }

 private:
  std::string port_;
  Ring<double, 8> recent_;
};

}  // namespace eeg
