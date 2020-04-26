import Vue from 'vue';
import Vuex from 'vuex';
import api from "../Api";
import authenticationStore from "./authenticationStore";

Vue.use(Vuex);

export default new Vuex.Store({
    namespaced: true,
    state: {
        token: null
    },
    mutations: {
        SET_TOKEN(state, token) {
            state.token = token
        }
    },
    actions: {
        async validateToken({commit}, local_token) {
            console.log("ran attempt function")
            console.log(local_token)

            if (local_token == 'tempToken'){
                console.log('working')
                api.login({
                    email: 'test1@gmail.com',
                    password: '12345679',
                }).then((response => {
                    authenticationStore.methods.setUserId(response.data.userId)
                    authenticationStore.methods.setSessionId(response.data.token)
                    authenticationStore.methods.setAuthenticated(true)
                }))
                    .catch(error => this.displayError(error.response.status))
            }
            commit('SET_TOKEN', local_token)
        }
    }
})