<template>
    <b-navbar>
        <template slot="start">
            <b-navbar-item href="#">
                Home
            </b-navbar-item>
        </template>
        <template slot="end">
            <b-navbar-item tag="div">
                <router-link v-if="!authenticationStore.authenticated" to="/Registration">
                    <a class="button is-primary">
                        <strong>Register</strong>
                    </a>
                </router-link>

                <router-link v-if="!authenticationStore.authenticated" to="/Login">
                    <a class="button is-light">
                        Login
                    </a>
                </router-link>

                <router-link v-if="authenticationStore.authenticated" to="/Login" replace>
                    <a class="button is-light">
                        <strong>Logout</strong>
                    </a>
                </router-link>

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

</style>