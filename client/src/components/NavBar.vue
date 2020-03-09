<template>
    <div class="container">
        <nav id="main-navbar">
            <div class="nav-left">
                <button class="btn-light-navbar">Home</button>
            </div>
            <div class="nav-right">

                <router-link v-if="!authenticationStore.authenticated" to="/Registration">
                    <button class="btn-light-navbar">Register</button>
                </router-link>

                <router-link v-if="!authenticationStore.authenticated" to="/Login">
                    <button class="btn-light-navbar">Login</button>
                </router-link>

                <router-link v-if="authenticationStore.authenticated" to="/Login" replace>
                    <button class="btn-light-navbar" @click="logout" >Logout</button>
                </router-link>
            </div>
        </nav>
    </div>
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