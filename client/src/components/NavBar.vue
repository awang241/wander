<template>
    <b-navbar>
        <template slot="start">
            <b-navbar-item href="#">
                Home
            </b-navbar-item>
        </template>

        <template slot="end">
            <div class="buttons">
                <b-button v-if="!authenticationStore.authenticated"
                          tag="router-link"
                          to="/Login"
                          type="is-primary">
                    Login
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

</style>