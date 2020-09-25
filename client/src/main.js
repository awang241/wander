import Vue from 'vue'
import Vuex from 'vuex'
import App from './App'
import VueRouter from 'vue-router'
import router from "./router.js";
import Buefy from 'buefy'
import api from './Api'
import store from './store'
import './veeValidateErrorMessages'

import {library} from '@fortawesome/fontawesome-svg-core'
import {faEllipsisV, faPlus, faMinus, faEnvelope, faSearch, faUserShield, faRunning, faUser, faSignOutAlt, faKey, faHome, faPlusCircle, faUserEdit, faUniversalAccess, faPassport, faHeartbeat, faMapMarkerAlt, faChevronLeft, faSignInAlt, faGlobeAsia} from '@fortawesome/free-solid-svg-icons'
import {FontAwesomeIcon} from '@fortawesome/vue-fontawesome'

library.add(faEllipsisV, faPlus, faMinus, faEnvelope, faSearch, faUserShield, faRunning, faUser, faSignOutAlt, faKey, faHome, faPlusCircle, faUserEdit, faUniversalAccess, faPassport, faHeartbeat, faMapMarkerAlt, faChevronLeft, faSignInAlt, faGlobeAsia);
Vue.component('vue-fontawesome', FontAwesomeIcon);

Vue.use(Buefy, {
    defaultIconComponent: 'vue-fontawesome',
    defaultIconPack: 'fas',
});
Vue.use(VueRouter);
Vue.config.productionTip = false;

Vue.use(Vuex);

//Check if a token is expired or null, if so it will redirect a user to the homepage
if (localStorage.getItem('authToken') != null) {
    let payload = {'token': localStorage.getItem('authToken'), 'userId': parseInt(localStorage.getItem('userId'))};
    store.dispatch('validateByTokenAndUserId', payload).then();
    api.verifyToken(localStorage.getItem('authToken'))
        .then(res => {
            return res
        })
        .catch((error) => {
            let emptyPayload = {'token': null, 'userId': null, 'authenticationStatus': false, 'authenticationLevel': 5};
            store.dispatch('resetUserData', emptyPayload).then();
            localStorage.clear();
            this.warningToast("An error occurred while verifying token.");
            return error
        })
}

import VueLogger from 'vuejs-logger';

const options = {
    isEnabled: true,
    logLevel: 'debug',
    stringifyArguments: false,
    showLogLevel: true,
    showMethodName: false,
    separator: '|',
    showConsoleColors: true
};

Vue.use(VueLogger, options);

export const eventBus = new Vue({});

/* eslint-disable no-new */
new Vue({
    el: '#app',
    template: '<App/>',
    components: {App},
    router,
    store
});
