import geometry;
import std;

int main() {
  geometry::Vector a{0, 0};
  geometry::Vector b{3, 4};
  return static_cast<int>(geometry::distance(a, scaled(b, 2.0)));
}
