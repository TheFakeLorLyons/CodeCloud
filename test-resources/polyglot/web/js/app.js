import { plot } from "./plot.js";
import { smooth } from "./util";
import * as THREE from "three";

const canvas = document.getElementById("chart");
const pattern = /"(\w+)"/g;

export async function start() {
  const response = await fetch("/samples.json");
  const samples = await response.json();
  plot(canvas, smooth(samples, 4));
  setTimeout(start, 1000 / 2);
  return new THREE.Clock();
}

start();
