import { Elm } from "../src/TodoMain.elm"
import { Dexie } from 'dexie';

async function main() {

  // const statusData = JSON.stringify(await getStatusFromDB());
  // const todoData = JSON.stringify(await getTasksFromDB());
  const statusData = await getStatusFromDB();
  const todoData = await getTasksFromDB();

  const flag = JSON.stringify({
    "status": statusData.actor && statusData.enemy ? statusData : null, 
    "todos" : JSON.stringify(todoData) != "[]" ? todoData : null 
    })

  const flags = flag ? JSON.parse(flag) : null
  
  const app = Elm.TodoMain.init({
    node: document.getElementById("main"),
    flags: flags
  });
  
  app.ports.setStatusStorage.subscribe(function(state) {
    setStatusToDB(state);
  });

  app.ports.setTasksStorage.subscribe(function(state) {
    setTasksToDB(state);
  });

  app.ports.deleteTaskFromDb.subscribe(function(state) {
    deleteTaskFromDb(state);
  });

  app.ports.changeCheckedDB.subscribe(function(state) {
    changeCheck(state);
  });


}

let db;

function openDB() {
  if (!db) {
    console.log("create new database");
    db = new Dexie("TodoAppDatabase");
    
    db.version(1).stores({
      enemy: "id",
      actor: "id",
      todo: "id, task"
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
  const enemyData = await db.enemy.toArray();
  const actorData = await db.actor.toArray();
  db.close();
  return {enemy : enemyData[0], actor: actorData[0]}
}

async function setTasksToDB(state) {
  openDB();
  await db.todo.put(state);
  db.close();
}

async function getTasksFromDB () {
  openDB();
  const taskData = await db.todo.toArray();
  db.close;
  return taskData;
}

async function deleteTaskFromDb (state) {
  openDB();
  await db.todo.where("id").equals(state.id).delete();
  db.close;
}

async function changeCheck (state) {
  openDB();
  await db.todo.where("id").equals(state.id).modify({checked : !state.checked});
  db.close;
}

main();
