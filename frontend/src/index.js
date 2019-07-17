import { Elm } from './Main.elm'

const app = Elm.Main.init({
  node: document.getElementById('elm')
})

app.ports.putUserSessionValue.subscribe((d) => {
  console.log("PUT USER SESSION", d);
})