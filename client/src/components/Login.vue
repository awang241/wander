<template>
    <div class="card">
        <div class="form-container sign-in-container">
            <h1>Login</h1>
            <form action="" method="post" class="form-register">
                <input type="text" name="email" v-model="input.email" placeholder="Email">
                <input type="password" name="password" v-model="input.password" placeholder="Password"/>
                <button class="btn-light" type="button" v-on:click="login()">Login</button>
            </form>
        </div>
    </div>
</template>

<script>
    import axios from "axios";
    import authenticationStore  from "../store/authentication";
    export default {
        name: 'Login',
        data() {
            return {
                input: {
                    email: "",
                    password: "",
                    error: false,
                    allUsers: null
                }
            }
        },
        mounted() {
            axios.get("https://f91246de-53d1-425e-9b1b-5524c2b62a0e.mock.pstmn.io/getusers")
                .then(response => this.allUsers = response.data)
                .catch(error => console.log(error));
        },
        methods: {
            login() {
                let logged = false;
                if (this.input.email != "" && this.input.password != "") {
                    for (const user of this.allUsers.users) {
                        console.log(user.email);
                        if (this.input.email == user.email && this.input.password == user.password) {
                            logged = true;
                            authenticationStore.methods.setAuthenticated(true)
                            this.$router.replace({ name: "profile" });
                            break;
                        }
                    }
                    if (logged == false) {
                        this.error = true;
                        window.alert("Incorrect email or password");
                    }
                    console.log();
                } else {
                    window.alert("A email and password must be present");
                }
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


</style>
