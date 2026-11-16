module;
#include <cmath>

export module geometry;

export import :vectors;
import <vector>;

export namespace geometry {

double hypotenuse(double a, double b) {
  return std::sqrt(a * a + b * b);
}

double distance(Vector p, Vector q);

}  // namespace geometry
