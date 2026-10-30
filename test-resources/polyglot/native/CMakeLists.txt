cmake_minimum_required(VERSION 3.16)
project(eeg_native C CXX)

add_library(filter src/filter.c)
target_include_directories(filter PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

add_executable(board cpp/main.cpp cpp/Board.cpp)
target_link_libraries(board PRIVATE filter)
