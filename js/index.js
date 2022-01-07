import { Elm } from "../src/TodoMain.elm"
import { Dexie } from 'dexie';

async function main() {

  const storedData = JSON.stringify(await getStatusFromDB());
  const flags = storedData ? JSON.parse(storedData) : null
  
  const app = Elm.TodoMain.init({
    node: document.getElementById("main"),
    flags: flags
  });
  
  app.ports.setStatusStorage.subscribe(function(state) {
    setStatusToDB(state);
  })

  app.ports.setTasksStorage.subscribe(function(state) {
    localStorage.setItem("todo-model", JSON.stringify(state));
    setTasksToDB(state);
  })


}

let db;

function openDB() {
  if (!db) {
    console.log("create new database");
    db = new Dexie("TodoAppDatabase");
    
    db.version(1).stores({
      enemy: "id",
      actor: "id"
    });

    db.version(2).stores({
      enemy: "id",
      actor: "id",
      todo: "id"
    });

    db.version(3).stores({
      enemy: "id",
      actor: "id",
      todo: "task"
    });

  }
  db.open();
}

async function setStatusToDB(state) {
  openDB();  
  await db.enemy.put(state.enemy);
  await db.actor.put(state.actor);
  db.close();
}

async function getStatusFromDB () {
  openDB();
  const enemyData = await db.enemy.where("id").equals(0).toArray();
  const actorData = await db.actor.where("id").equals(0).toArray();
  db.close();
  return {enemy : enemyData[0], actor: actorData[0]}
}

async function setTasksToDB(state) {
  openDB();
  console.log(state);
  await db.todo.put(state);
  db.close();
}


main();
