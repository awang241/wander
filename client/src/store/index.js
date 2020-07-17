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
        authenticationLevel: 5,
        searchName: "",
        searchEmail: "",
        searchActivityType: "all",
        searchProfiles: [],
        searchPossibleActivityTypes: [],
        searchFilteredActivityTypes: [],
        searchObserver: null,
        searchStartIndex: 0,
        searchChosenActivityTypes: [],
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
        },
        SET_SEARCH_NAME(state, searchName){
            state.searchName = searchName
        },
        SET_SEARCH_EMAIL(state, searchEmail){
            state.searchEmail = searchEmail
        },
        SET_SEARCH_ACTIVITY_TYPES(state, searchActivityTypes){
            state.searchActivityTypes = searchActivityTypes
        },
        SET_SEARCH_PROFILES(state, searchProfiles){
            state.searchProfiles = searchProfiles
        },
        SET_SEARCH_FILTERED_ACTIVITY_TYPES(state, searchFilteredActivityTypes){
            state.searchFilteredActivityTypes = searchFilteredActivityTypes
        },
        SET_SEARCH_OBSERVER(state, searchObserver){
            state.searchObserver = searchObserver
        },
        SET_SEARCH_START_INDEX(state, searchStartIndex){
            state.searchStartIndex = searchStartIndex
        },
        SET_SEARCH_CHOSEN_ACTIVITY_TYPES(state, searchChosenActivityTypes){
            state.searchChosenActivityTypes = searchChosenActivityTypes
        },
        SET_SEARCH_POSSIBLE_ACTIVITY_TYPES(state, searchPossibleActivityTypes){
            state.searchPossibleActivityTypes = searchPossibleActivityTypes
        },

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
        },
        getSearchName: state => {
            return state.searchName
        },
        getSearchEmail: state => {
            return state.searchEmail
        },
        getSearchActivityTypes: state => {
            return state.searchActivityTypes
        },
        getSearchProfiles: state => {
            return state.searchProfiles
        },
        getSearchFilteredActivityTypes: state => {
            return state.searchFilteredActivityTypes
        },
        getSearchObserver: state => {
            return state.searchObserver
        },
        getSearchStartIndex: state => {
            return state.searchStartIndex
        },
        getSearchChosenActivityTypes: state => {
            return state.searchChosenActivityTypes
        },
        getSearchPossibleActivityTypes: state => {
            return state.searchPossibleActivityTypes
        },

    },
    actions: {
        async validateByTokenAndUserId({commit}, payload) {
            const token = payload.token
            const userId = payload.userId
            const decoded = jwt_decode(token)
            const authenticationLevel = decoded.authLevel
            const authenticationStatus = true
            commit('SET_TOKEN', token)
            commit('SET_USER_ID', userId)
            commit('SET_AUTHENTICATION_STATUS', authenticationStatus)
            commit('SET_AUTHENTICATION_LEVEL', authenticationLevel)
            return;
            },
        resetUserData({commit}, payload) {
            const token = payload.token
            const userId = payload.userId
            const authenticationStatus = payload.authenticationStatus
            const authenticationLevel = payload.authenticationLevel
            commit('SET_TOKEN', token)
            commit('SET_USER_ID', userId)
            commit('SET_AUTHENTICATION_STATUS', authenticationStatus)
            commit('SET_AUTHENTICATION_LEVEL', authenticationLevel)
            return;
        },
        resetSearchFields({commit}, payload) {
            const searchName = payload.searchName
            const searchEmail = payload.searchEmail
            const searchActivityTypes = payload.searchActivityTypes
            const searchProfiles = payload.searchProfiles
            const searchFilteredActivityTypes = payload.searchFilteredActivityTypes
            const searchObserver = payload.searchObserver
            const searchStartIndex = payload.searchStartIndex
            const searchChosenActivityTypes = payload.searchChosenActivityTypes
            const searchPossibleActivityTypes = payload.searchPossibleActivityTypes
            commit('SET_SEARCH_NAME', searchName)
            commit('SET_SEARCH_EMAIL', searchEmail)
            commit('SET_SEARCH_ACTIVITY_TYPES', searchActivityTypes)
            commit('SET_SEARCH_PROFILES', searchProfiles)
            commit('SET_SEARCH_FILTERED_ACTIVITY_TYPES', searchFilteredActivityTypes)
            commit('SET_SEARCH_OBSERVER', searchObserver)
            commit('SET_SEARCH_START_INDEX', searchStartIndex)
            commit('SET_SEARCH_CHOSEN_ACTIVITY_TYPES', searchChosenActivityTypes)
            commit('SET_SEARCH_POSSIBLE_ACTIVITY_TYPES', searchPossibleActivityTypes)
            return;

        },
    }
})