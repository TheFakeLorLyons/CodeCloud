#include <cstdio>
#include "Board.hpp"
#include "filter.h"

namespace {

double smoothed(eeg::Board& board) {
  double total = 0.0;
  for (double sample : board.read(4)) {
    total += filter_sample(sample, 0.5);
  }
  return total;
}

}  // namespace

int main() {
  eeg::Board board("/dev/ttyUSB0");
  if (!board.open()) {
    return 1;
  }
  std::printf("%f\n", smoothed(board));
  return 0;
}
