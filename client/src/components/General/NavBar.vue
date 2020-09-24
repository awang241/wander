<template>
    <b-navbar>
        <template slot="brand">
            <div v-if="!store.getters.getAuthenticationStatus" @click="goToMainPage">
                <img class="crop-banner" src="../../../images/WANDER-day-navbar.png" alt="Wander Logo in Navigation Bar"/>
            </div>
            <div v-if="store.getters.getAuthenticationStatus" @click="goToHomeFeed">
                <img class="crop-banner" src="../../../images/WANDER-day-navbar.png" alt="Wander Logo in Navigation Bar"/>
            </div>
        </template>

        <template slot="end">
            <b-navbar-item tag="div">
                <div class="buttons">
                    <b-button v-if="!store.getters.getAuthenticationStatus"
                              tag="router-link"
                              to="/Login"
                              type="is-primary">
                        <strong>Login</strong>
                    </b-button>
                    <b-button v-if="!store.getters.getAuthenticationStatus"
                              tag="router-link"
                              to="/Registration"
                              type="is-light"
                              id="registrationButton">
                        Register
                    </b-button>
                    <b-button  @click="goToSearch"
                               v-if="store.getters.getAuthenticationLevel > 1 && store.getters.getAuthenticationStatus"
                               class="navbarButton">
                        <i class="fas fa-search" style="font-size: 1.5em; color: #38eeff"></i>
                        Search
                    </b-button>
                    <b-button  @click="goToAdminDashboard"
                               v-if="store.getters.getAuthenticationLevel <= 1"
                               class="navbarButton">
                        Admin Dashboard
                    </b-button>
                    <b-button  @click="goToActivities"
                               v-if="store.getters.getAuthenticationStatus"
                               class="navbarButton">
                        <i class="fas fa-running" style="font-size: 1.5em; color: #38eeff"></i>
                        Activities
                    </b-button>
                    <b-button  @click="goToProfile"
                               v-if="store.getters.getAuthenticationStatus && store.getters.getAuthenticationLevel > 0"
                               class="navbarButton">
                        <i class="fas fa-user" style="font-size: 1.5em; color: #38eeff"></i>
                        Profile
                    </b-button>
                    <b-button  @click="logout"
                               v-if="store.getters.getAuthenticationStatus"
                               class="navbarButton">
                        <i class="fas fa-sign-out-alt" style="font-size: 1.5em; color: #38eeff"></i>
                        Logout
                    </b-button>
                </div>
            </b-navbar-item>
        </template>
    </b-navbar>
</template>


<script>
    import router from "../../router";
    import store from '../../store';
    import Vuex from 'vuex';
    import Vue from "vue";
    Vue.use(Vuex);

    export default {
        name: "NavBar",
        data: () => {
            return {
                store: store
            }
        },
        methods: {
            logout(){
                localStorage.clear();
                let payload = {'token': null, 'userId': null, 'authenticationStatus': false, 'authenticationLevel': 5}
                store.dispatch('resetUserData', payload, {root:true});
                router.push({path: '/Login'});
            },
            goToProfile(){
                router.push({path: '/Profile/' + store.getters.getUserId})
            },
            goToAdminDashboard(){
                router.push({path: '/AdminDashboard'});
            },
            goToActivities(){
                router.push({path: '/Activities'});
            },
            goToSearch() {
                router.push({path: '/Search'})
            },
            goToHomeFeed() {
                router.push({path: '/Home'})
            }
        }
    }
</script>

<style>
    img { cursor: pointer; }

    .crop-banner {
        width: 200px;
        height: 50px;
        overflow: hidden;
        margin-left: auto;
        margin-right: auto;
        display: block;

    }
    .crop-banner img {
        width: 200px;
        height: 50px;
        margin-left: auto;
        margin-right: auto;
        display: block;
    }
</style>