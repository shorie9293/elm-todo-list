import { Elm } from "./TodoMain.elm"

var storedData = localStorage.getItem('status-model');
var flags = storedData ? JSON.parse(storedData) : null

var app = Elm.TodoMain.init({
  node: document.getElementById("main"),
  flags: flags
});

app.ports.setStorage.subscribe(function(state) {
  localStorage.setItem("status-model", JSON.stringify(state));
})
