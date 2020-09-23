<template>
    <b-navbar>
        <template slot="start">
            <div class="buttons">
                <b-button v-if="!store.getters.getAuthenticationStatus"
                          tag="router-link"
                          to="/"
                          type="is-light">
                  Home
                </b-button>
                <b-button @click=goToHomeFeed
                          v-if="store.getters.getAuthenticationStatus"
                          type="is-light">
                  Home Feed
                </b-button>
            </div>

        </template>

        <template slot="brand">
            <img class="crop-banner" src="../../../images/WANDER-day-navbar.png" alt="Wander Logo in Navigation Bar"/>
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
                               v-if="store.getters.getAuthenticationLevel > 1"
                               type="is-light">
                        Search
                    </b-button>
                    <b-button  @click="goToAdminDashboard"
                               v-if="store.getters.getAuthenticationLevel <= 1"
                               type="is-light">
                        Admin Dashboard
                    </b-button>
                    <b-button  @click="goToActivities"
                               v-if="store.getters.getAuthenticationStatus"
                               type="is-light">
                        Activities
                    </b-button>
                    <b-button  @click="goToProfile"
                               v-if="store.getters.getAuthenticationStatus && store.getters.getAuthenticationLevel > 0"
                               type="is-light">
                        Profile
                    </b-button>
                    <b-button  @click="logout"
                               v-if="store.getters.getAuthenticationStatus"
                               type="is-light">
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
                localStorage.clear()
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
    #main-navbar{
        display: flex;
        flex-wrap: wrap;
        height: 30px;
        padding: 1rem;
        justify-content: space-between;
    }

    .container{
        width: 100%;
    }

    buttons{
        padding: 10px;
    }

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