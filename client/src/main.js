import Vue from 'vue'
import Vuex from'vuex'
import App from './App'
import VueRouter from 'vue-router'
import router from "./router.js";
import Buefy from 'buefy'
import { library } from '@fortawesome/fontawesome-svg-core';
// import store from './store'
// internal icons
import { faCheck, faCheckCircle, faInfoCircle, faExclamationTriangle, faExclamationCircle,
  faArrowUp, faAngleRight, faAngleLeft, faAngleDown,
  faEye, faEyeSlash, faCaretDown, faCaretUp, faUpload } from "@fortawesome/free-solid-svg-icons";
import { FontAwesomeIcon } from "@fortawesome/vue-fontawesome";

library.add(faCheck, faCheckCircle, faInfoCircle, faExclamationTriangle, faExclamationCircle,
    faArrowUp, faAngleRight, faAngleLeft, faAngleDown,
    faEye, faEyeSlash, faCaretDown, faCaretUp, faUpload);
Vue.component('vue-fontawesome', FontAwesomeIcon);


Vue.use(Buefy, {
  defaultIconComponent: 'vue-fontawesome',
  defaultIconPack: 'fas',
});

Vue.use(VueRouter)
Vue.config.productionTip = false


Vue.use(Vuex)
// if (localStorage.getItem('testToken') != 'undefined') {
//   store.dispatch('validateToken', localStorage.getItem('auth_token'))
// }

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