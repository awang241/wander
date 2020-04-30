import Vue from 'vue';
import Vuex from 'vuex';
import jwt_decode from 'jwt-decode';

Vue.use(Vuex);

export default new Vuex.Store({
    namespaced: true,
    state: {
        token: null,
        userId: null,
        authenticationStatus: false,
        authenticationLevel: 5
    },
    mutations: {
        SET_TOKEN(state, token) {
            state.token = token
        },
        SET_USER_ID(state, userId) {
            state.userId = userId
        },
        SET_AUTHENTICATION_STATUS(state,  authenticationStatus){
            state.authenticationStatus =  authenticationStatus
        },
        SET_AUTHENTICATION_LEVEL(state, authenticationLevel){
            state.authenticationLevel = authenticationLevel
        }
    },
    getters: {
        getAuthenticationStatus: state => {
            return state.authenticationStatus
        },
        getUserId: state => {
            return state.userId
        },
        getAuthenticationLevel: state => {
            return state.authenticationLevel
        }
    },
    actions: {
        async validateByTokenAndUserId({commit}, payload) {
            const token = payload.token
            const userId = payload.userId
            const authenticationLevel = jwt_decode(token);
            let authenticationStatus = true
            commit('SET_TOKEN', token)
            commit('SET_USER_ID', userId)
            commit('SET_AUTHENTICATION_STATUS', authenticationStatus)
            commit('SET_AUTHENTICATION_LEVEL', authenticationLevel)
        },
        resetUserData({commit}, payload) {
            console.log('Resetting token and userId state to null')
            const token = payload.token
            const userId = payload.userId
            const authenticationStatus = payload.authenticationStatus
            const authenticationLevel = payload.authenticationLevel
            commit('SET_TOKEN', token)
            commit('SET_USER_ID', userId)
            commit('SET_AUTHENTICATION_STATUS', authenticationStatus)
            commit('SET_AUTHENTICATION_LEVEL', authenticationLevel)
            return;
        }
    }
})