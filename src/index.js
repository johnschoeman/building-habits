import "./output.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.register();

// Local Storage
const habitKey = "buildinghabits.habit";
const habitHistoryKey = "buildinghabits.habit-history";

const fetchHabitDataFromLocalStorage = () => {
  const habit = JSON.parse(localStorage.getItem(habitKey));
  const habitHistory = JSON.parse(localStorage.getItem(habitHistoryKey));

  return {
    habit: habit || "Log a habit right after lunch",
    habitHistory: habitHistory || [],
  };
};

const saveHabitLocally = (habit) => {
  localStorage.setItem(habitKey, JSON.stringify(habit));
};

const saveHabitHistoryLocally = (habitHistory) => {
  localStorage.setItem(habitHistoryKey, JSON.stringify(habitHistory));
};

const localHabitData = fetchHabitDataFromLocalStorage();

// App
const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: localHabitData,
});

app.ports.saveHabitLocally.subscribe(function (habit) {
  saveHabitLocally(habit);
});

app.ports.saveHabitHistoryLocally.subscribe(function (habitHistory) {
  saveHabitHistoryLocally(habitHistory);
});
