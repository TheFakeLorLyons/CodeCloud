#include <gtest/gtest.h>
#include "Board.hpp"

TEST(BoardTest, ReadsAsManySamplesAsAsked) {
  eeg::Board board("loopback");
  EXPECT_EQ(board.read(3).size(), 3u);
}
