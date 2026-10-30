#ifndef FILTER_H
#define FILTER_H

#define FILTER_ORDER 4
#define CLAMP(value, low, high) ((value) < (low) ? (low) : (value) > (high) ? (high) : (value))

/* One weighted step of the low-pass filter. */
double filter_sample(double sample, double weight);

#endif
