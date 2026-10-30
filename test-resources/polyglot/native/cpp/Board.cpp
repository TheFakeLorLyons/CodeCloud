#include "Board.hpp"

namespace eeg {

Board::Board(std::string port) : port_(std::move(port)) {}

std::vector<double> Board::read(std::size_t count) {
  std::vector<double> samples(count, 0.0);
  for (std::size_t index = 0; index < count; ++index) {
    samples[index] = index % 2 == 0 ? 1.0 : -1.0;
  }
  return samples;
}

void Board::stream(std::size_t count) {
  std::thread worker([this, count] { read(count); });
  worker.detach();
}

}  // namespace eeg
