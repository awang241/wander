import Vue from 'vue';
import Vuex from 'vuex';
import api from "../Api";


Vue.use(Vuex);

export default new Vuex.Store({
    namespaced: true,
    state: {
        token: null,
        userId: null,
        authenticationStatus: false
    },
    mutations: {
        SET_TOKEN(state, token) {
            state.token = token
        },
        SET_USER_ID(state, userId) {
            state.userId = userId
        },
        SET_AUTHENTICATION(state,  authenticationStatus){
            state.authenticationStatus =  authenticationStatus
        }
    },
    getters: {
        getAuthenticationStatus: state => {
            return state.authenticationStatus
        },
        getUserId: state => {
            return state.userId
        }
    },
    actions: {
        async validateByTokenAndUserId({commit}, payload) {
            const token = payload.token
            const userId = payload.userId
            // try {
            //     api.getProfile(userId, token).then((response => {
            //         profileStore.methods.setProfile(response.data)
            //     }))
            // } catch (err) {
            //     console.log(err)
            //     return;
            // }
            let authenticationStatus = true
            commit('SET_TOKEN', token)
            commit('SET_USER_ID', userId)
            commit('SET_AUTHENTICATION', authenticationStatus)
        },
        resetUserData({commit}, payload) {
            console.log('Resetting token and userId state to null')
            const token = payload.token
            const userId = payload.userId
            const authenticationStatus = payload.authenticationStatus
            commit('SET_TOKEN', token)
            commit('SET_USER_ID', userId)
            commit('SET_AUTHENTICATION', authenticationStatus)
            return;
        }
    }
})