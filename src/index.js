import "./output.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";
import { Plugins } from "@capacitor/core";

const { StatusBar } = Plugins;

StatusBar.setStyle({ style: "LIGHT" });

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.register();

// Local Storage
const habitKeyV1 = "buildinghabits.habit";
const habitKeyV2 = "buildinghabits.habit-v2";
const habitLogKeyV1 = "buildinghabits.habit-log";
const habitLogKeyV2 = "buildinghabits.habit-log-v2";

const fetchHabitDataFromLocalStorage = () => {
  const habitV1 = JSON.parse(localStorage.getItem(habitKeyV1));
  const habitV2 = JSON.parse(localStorage.getItem(habitKeyV2));
  const habitLogV1 = JSON.parse(localStorage.getItem(habitLogKeyV1));
  const habitLogV2 = JSON.parse(localStorage.getItem(habitLogKeyV2));

  const habit = habitV2 ? habitV2 : { id: habitV1, title: habitV1 };
  const habitLog = habitLogV2 ? habitLogV2 : habitLogV1;

  return {
    habit: habit || "Log a habit right after lunch",
    habitLog: habitLog || [],
  };
};

const saveHabitLocally = (habit) => {
  localStorage.setItem(habitKeyV2, JSON.stringify(habit));
};

const saveHabitLogLocally = (habitLog) => {
  console.log("saving Habit");
  localStorage.setItem(habitLogKeyV2, JSON.stringify(habitLog));
};

const localHabitData = fetchHabitDataFromLocalStorage();

// App
const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: localHabitData,
});

app.ports.saveHabitLocally.subscribe((habit) => {
  saveHabitLocally(habit);
});

app.ports.saveHabitLogLocally.subscribe((habitLog) => {
  saveHabitLogLocally(habitLog);
});
