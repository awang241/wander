import Vue from 'vue'
import Vuex from'vuex'
import App from './App'
import VueRouter from 'vue-router'
import router from "./router.js";
import Buefy from 'buefy'
import api from './Api'
import store from './store'

Vue.use(Buefy);

Vue.use(VueRouter)
Vue.config.productionTip = false

Vue.use(Vuex)
//Check if a token is expired or null, if so it will redirect a user to the homepage
if (localStorage.getItem('authToken') != null) {
    console.log("MAIN")
    api.verifyToken(localStorage.getItem('authToken'))
      .then(r => {
        let payload = {'token': localStorage.getItem('authToken'), 'userId': localStorage.getItem('userId')}
        store.dispatch('validateByTokenAndUserId', payload).then()
        return r
      })
      .catch((error) => {
          let payload = {'token': null, 'userId': null, 'authenticationStatus': false, 'authenticationLevel': 5};
        store.dispatch('resetUserData', payload).then();
        localStorage.clear()
          console.log(error)
          return error
      })
}

import VueLogger from 'vuejs-logger';

const options = {
  isEnabled: true,
  logLevel :'debug',
  stringifyArguments : false,
  showLogLevel : true,
  showMethodName : false,
  separator: '|',
  showConsoleColors: true
};

Vue.use(VueLogger, options);

/* eslint-disable no-new */
new Vue({
  el: '#app',
  template: '<App/>',
  components: { App },
  router
});