import Vue from 'vue';
import Vuex from 'vuex';
import api from "../Api";
import authenticationStore from "./authenticationStore";
import profileStore from "../store/profileStore";

Vue.use(Vuex);

export default new Vuex.Store({
    namespaced: true,
    state: {
        token: null,
        userId: null
    },
    mutations: {
        SET_SESSION(state, token, userId) {
            state.token = token
            state.userId = userId
        }
    },
    actions: {
        async validateByTokenAndUserId({commit}, payload) {
            const token = payload.token
            const userId = payload.userId
            console.log("Validating using below credentials")
            console.log(token)
            console.log(userId)
            try {
                api.getProfile(userId, token).then((response => {
                    authenticationStore.methods.setUserId(userId)
                    authenticationStore.methods.setSessionId(token)
                    authenticationStore.methods.setAuthenticated(true)
                    profileStore.methods.setProfile(response.data)
                }))
            } catch (err) {
                console.log(err)
                return;
            }
            commit('SET_SESSION', token, userId)
        },
        resetTokenAndUserId({commit}, payload) {
            console.log('Resetting token and userId state to null')
            const token = payload.token
            const userId = payload.userId
            commit('SET_SESSION', token, userId)
            return;
        }
    }
})