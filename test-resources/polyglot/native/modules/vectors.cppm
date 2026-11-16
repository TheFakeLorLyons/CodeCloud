export module geometry:vectors;

export struct Vector {
  double x;
  double y;
};

export {
  Vector scaled(Vector v, double factor) { return {v.x * factor, v.y * factor}; }
}
