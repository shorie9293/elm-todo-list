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
})

var db = new Dexie("FriendDatabase");

// DB with single table "friends" with primary key "id" and
// indexes on properties "name" and "age"
db.version(1).stores({
  friends: `
    id,
    name,
    age`,
});

// Now add some values.
db.friends.bulkPut([
  { id: 1, name: "Josephine", age: 21 },
  { id: 2, name: "Per", age: 75 },
  { id: 3, name: "Simon", age: 5 },
  { id: 4, name: "Sara", age: 50, notIndexedProperty: 'foo' }
])