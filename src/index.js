import "./output.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.register();

// Local Storage
const localStorageKey = "buildinghabits.userData";

const fetchUserDataFromLocalStorage = () => {
  const userData = JSON.parse(localStorage.getItem(localStorageKey));
  return userData ? userData : "Log a habit right after lunch";
};

const saveUserDataLocally = (userData) => {
  localStorage.setItem(localStorageKey, JSON.stringify(userData));
};

const localHabit = fetchUserDataFromLocalStorage();

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: localHabit,
});

app.ports.saveUserDataLocally.subscribe(function (userData) {
  saveUserDataLocally(userData);
});
