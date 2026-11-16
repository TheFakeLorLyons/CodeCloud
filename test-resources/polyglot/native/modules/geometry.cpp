module geometry;

namespace geometry {

double distance(Vector p, Vector q) {
  return hypotenuse(p.x - q.x, p.y - q.y);
}

}  // namespace geometry
