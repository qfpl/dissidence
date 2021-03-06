import { Elm } from './Main.elm'

const sessionKey = "dissidence_player_session";

var oldSessionStr = localStorage.getItem(sessionKey)

console.log("STARTUP");

const app = Elm.Main.init({
  node: document.getElementById('elm'),
  flags: JSON.parse(oldSessionStr)
})

app.ports.putPlayerSessionValue.subscribe((d) => {
  if (d === null) {
    localStorage.removeItem(sessionKey);
  } else {
    localStorage.setItem(sessionKey, JSON.stringify(d));
  }

  setTimeout(function () { app.ports.onPlayerSessionValueChange.send(d); }, 0);
})


window.addEventListener('storage', (event) => {
  if (event.storageArea === localStorage && event.key === sessionKey) {
    app.ports.onPlayerSessionValueChange.send(JSON.parse(event.newValue));
  }
});