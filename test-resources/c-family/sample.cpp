#include <vector>
#include "eeg/filter.hpp"
#define SQUARE(x) ((x) * (x))
#define LIMIT 8
// é before a brace: { stays in the comment
namespace eeg { namespace detail {
template <typename T, typename = std::enable_if_t<std::is_integral_v<T>>>
class Ring final : public Base<T> {
 public:
  explicit Ring(int size) : size_(size), items_{} {}
  ~Ring() override = default;
  T& operator[](std::size_t index) { return items_[index]; }
  operator bool() const noexcept { return size_ > 0; }
  bool operator()(int value) const { return value < size_; }
  struct Node { int value() const { return 1'000'000; } };
  static constexpr int kMax = 8;
 private:
  int size_;
};
template <typename T>
T Ring<T>::front() const { if (size_ > 0 && ok) { return items_[0]; } return T{}; }
}  // namespace detail
void (*get_handler(int signal))(int) { return nullptr; }
const char *name_of(int id) { return id ? "é" : R"json({"id": "}"})json"; }
std::vector<int> &cache() { static std::vector<int> values; return values; }
typedef struct { double x, y; } Point;
enum class Color : unsigned char { Red, Green };
using Samples = std::vector<double>;
static const int kRate = 250;
static int helper(int value) { return value * 2; }
namespace {
int hidden() { return helper(LIMIT); }
}
extern "C" {
int c_api(void) { return 0; }
}
#ifdef WITH_DEBUG
void debug_dump() { }
#else
void debug_dump() { /* nothing */ }
#endif
TEST(RingTest, Grows) { EXPECT_TRUE(true); }
}  // namespace eeg
