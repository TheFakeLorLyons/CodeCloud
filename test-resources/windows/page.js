let apples = 0;
document.getElementById("status").textContent = "The script ran";
document.getElementById("count").addEventListener("click", (event) => {
  apples += 1;
  event.currentTarget.textContent = "Counted " + apples + " apple" + (apples === 1 ? "" : "s");
});
