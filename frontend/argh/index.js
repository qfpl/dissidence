import { Elm } from './Main.elm'

const sessionKey = "dissidence_usersession";

var oldSessionStr = localStorage.getItem(sessionKey)

const app = Elm.Main.init({
})

app.ports.putUserSessionValue.subscribe((d) => {
  console.log("PUT SESSION", d);
  if (d === null) {
    localStorage.removeItem(sessionKey);
  } else {
    localStorage.setItem(sessionKey, JSON.stringify(d));
  }
  setTimeout(function () { app.ports.onUserSessionValueChange.send(d); }, 0);
})


window.addEventListener('storage', (event) => {
  console.log("STORAGE", event)
  if (event.storageArea === localStorage && event.key === sessionKey) {
    app.ports.onUserSessionValueChange.send(JSON.parse(event.newValue));
  }
}, false);

