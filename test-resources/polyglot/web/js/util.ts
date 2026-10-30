export interface Window {
  size: number;
}

export function smooth(samples: number[], size: number): number[] {
  const result: number[] = [];
  for (let index = 0; index < samples.length; index++) {
    const start = Math.max(0, index - size);
    const slice = samples.slice(start, index + 1);
    result.push(slice.reduce((total, value) => total + value, 0) / slice.length);
  }
  return result;
}
