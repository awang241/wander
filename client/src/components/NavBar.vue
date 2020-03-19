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
                    .catch(error => window.alert(error.response.data))

                //User is now logged out in authentication store
                authenticationStore.methods.setSessionId(0)
                authenticationStore.methods.setUserId(0)
                authenticationStore.methods.setAuthenticated(false)
                router.push('Login')
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
</style>