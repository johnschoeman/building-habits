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
const habitKey = "buildinghabits.habit";
const habitLogKey = "buildinghabits.habit-log";

const fetchHabitDataFromLocalStorage = () => {
  const habit = JSON.parse(localStorage.getItem(habitKey));
  const habitLog = JSON.parse(localStorage.getItem(habitLogKey));

  return {
    habit: habit || "Log a habit right after lunch",
    habitLog: habitLog || [],
  };
};

const saveHabitLocally = (habit) => {
  localStorage.setItem(habitKey, JSON.stringify(habit));
};

const saveHabitLogLocally = (habitLog) => {
  localStorage.setItem(habitLogKey, JSON.stringify(habitLog));
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
