import Vue from 'vue'
import App from './App'
import VueRouter from 'vue-router'
import router from "./router.js";
import Buefy from 'buefy'

Vue.use(Buefy)

Vue.use(VueRouter)
Vue.config.productionTip = false

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