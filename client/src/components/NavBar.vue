<template>
    <b-navbar>
        <template slot="brand">
            <b-navbar-item href="#">
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
                        Registration
                    </b-button>
                    <b-button v-if="authenticationStore.authenticated"
                              tag="router-link"
                              to="/Login"
                              type="is-light">
                        Logout
                    </b-button>
                </div>
            </b-navbar-item>
        </template>
    </b-navbar>
</template>


<script>
    import authenticationStore  from "../store/authentication";

    export default {
        name: "NavBar",
        data: () => {
            return {
                authenticationStore: authenticationStore.data
            }
        },
        methods: {
            logout(){
                authenticationStore.methods.setAuthenticated(false)
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