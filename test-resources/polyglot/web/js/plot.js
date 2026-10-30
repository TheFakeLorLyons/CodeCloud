import { scaleFor } from "./colors.js";

export const WIDTH = 800;

export function plot(canvas, samples) {
  const context = canvas.getContext("2d");
  context.beginPath();
  samples.forEach((sample, index) => {
    const y = sample * scaleFor(sample);
    if (index === 0) {
      context.moveTo(index, y);
    } else {
      context.lineTo(index, y);
    }
  });
  context.stroke();
}
