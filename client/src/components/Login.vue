<template>
    <div class="card">
        <div class="form-container sign-in-container">
            <h1>Login</h1>
            <form action="" method="post" class="form-register">
                <input type="text" name="email" v-model="email" placeholder="Email">
                <input type="password" name="password" v-model="password" placeholder="Password"/>
                <button class="btn-light" type="button" v-on:click="login()">Login</button>
            </form>
        </div>
    </div>
</template>

<script>
    import api from '../Api';
    import router from "../router";
    import authenticationStore from "../store/authentication";

    export default {
        name: 'Login',
        data() {
            return {
                email: "",
                password: "",
            }
        },
        methods: {
            login() {
                api.login({
                    email: this.email,
                    password: this.password,
                }).then((response => {
                    console.log(response)
                    console.log(response.data)
                    authenticationStore.methods.setAuthenticated(true)
                    router.push('Profile')
                }))
                .catch(error => window.alert(error.response.data))
            }
        }
    }
</script>

<style scoped>

    h1 {
        padding: 1.5rem 0;
        font-weight: bold;
        margin: 0;
        text-align: center;
    }

    .card {
        min-height: 300px;
    }

    .sign-in-container {
        left: 0;
        width: 100%;
    }

    form div {
        width: 100%;
    }

    form {
        background-color: #FFFFFF;
        display: flex;
        align-items: center;
        justify-content: center;
        flex-direction: column;
        padding: 0 50px;
        text-align: left;
    }


</style>
