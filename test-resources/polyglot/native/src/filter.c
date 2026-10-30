#include <math.h>
#include "filter.h"

static double clamped(double weight) {
  return CLAMP(weight, 0.0, 1.0);
}

double filter_sample(double sample, double weight) {
  if (clamped(weight) <= 0.0) {
    return sample;
  }
  return sample * exp(-weight);
}
