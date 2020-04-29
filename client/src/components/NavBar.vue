<template>
    <b-navbar>
        <template slot="brand">
            <b-navbar-item tag="router-link"
                           to="/Mainpage"
                           href="#">
                Home
            </b-navbar-item>
        </template>

        <template slot="end">
            <b-navbar-item tag="div">
                <div class="buttons">
                    <b-button v-if="!authenticationStore.authenticated"
                              tag="router-link"
                              to="/Login"
                              type="is-primary">
                        <strong>Login</strong>
                    </b-button>
                    <b-button v-if="!authenticationStore.authenticated"
                              tag="router-link"
                              to="/Registration"
                              type="is-light">
                        Register
                    </b-button>
                    <b-button  @click="goToAdminDashboard"
                               v-if="authenticationStore.authenticated && (profileStore.authLevel == 0 || profileStore.authLevel == 1)"
                               type="is-light">
                        Admin Dashboard
                    </b-button>
                    <b-button  @click="goToProfile"
                               v-if="authenticationStore.authenticated"
                               type="is-light">
                        Profile
                    </b-button>
                    <b-button  @click="logout"
                               v-if="authenticationStore.authenticated"
                               type="is-light">
                        Logout
                    </b-button>

                </div>
            </b-navbar-item>
        </template>
    </b-navbar>
</template>


<script>
    import authenticationStore  from "../store/authenticationStore";
    import router from "../router";
    import api from "../Api";
    import store from '../store';
    import Vuex from 'vuex';
    import Vue from "vue";
    Vue.use(Vuex)

    export default {
        name: "NavBar",
        data: () => {
            return {
                authenticationStore: authenticationStore.data,
            }
        },
        methods: {
            logout(){
                api.logout({userId: authenticationStore.methods.getUserId()}, authenticationStore.methods.getSessionId())
                    .catch(error => console.log(error))

                //User is now logged out in authentication store
                authenticationStore.methods.setSessionId(0)
                authenticationStore.methods.setUserId(0)
                authenticationStore.methods.setAuthenticated(false)
                localStorage.removeItem('authToken')
                localStorage.removeItem('userId')
                let payload = {'token': null, 'userId': null}
                store.dispatch('resetTokenAndUserId', payload, {root:true});
                router.go(-1)
                router.replace('Login')
            },
            goToProfile(){
                router.go(-1)
                router.push('Profile')
            },
            goToAdminDashboard(){
                router.push('AdminDashboard')
            },

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
</style>