import { Elm } from "./TodoMain.elm"
import { Dexie } from 'dexie';

var storedData = localStorage.getItem('status-model');
var flags = storedData ? JSON.parse(storedData) : null

var app = Elm.TodoMain.init({
  node: document.getElementById("main"),
  flags: flags
});

app.ports.setStorage.subscribe(function(state) {
  localStorage.setItem("status-model", JSON.stringify(state));
  setStatusToDB(state);
  console.log(JSON.stringify(state));
})

function setStatusToDB(state) {
  var db = new Dexie("TodoAppDatabase");
  
  db.version(1).stores({
    enemy: "id",
    actor: "id"
  });
  
  db.enemy.put(state.enemy);
  db.actor.put(state.actor);
}
