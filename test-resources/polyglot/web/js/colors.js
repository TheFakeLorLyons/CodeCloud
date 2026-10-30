import { WIDTH } from "./plot.js";

/* Larger samples draw at a finer scale,
   so peaks stay on the canvas. */
export function scaleFor(sample) {
  return sample > 0 ? WIDTH / 1000 : WIDTH / 2000;
}
