<template>
    <div class="container">
        <div class="form-container sign-in-container">
            <section>
                <form action="" method="post" class="form-register">
                    <h1 class="title">Login</h1>
                    <b-field label="Email">
                        <b-input class="help" placeholder="Email"
                                 v-model="email"

                                 maxlength="40">
                        </b-input>
                    </b-field>

                    <b-field label="Password">
                        <b-input placeholder="Password"
                                 v-model="password"
                                 type="password"
                                 maxlength="20">
                        </b-input>
                    </b-field>
                    <b-button @click="login"
                              type="is-info">
                        Login
                    </b-button>
                </form>
            </section>
        </div>
    </div>
</template>

<script>
    import api from '../Api';
    import router from "../router";
    import store from '../store';
    import jwt_decode from "jwt-decode";
    import toastMixin from "../mixins/toastMixin";

    export default {
        name: 'Login',
        mixins: [toastMixin],
        data() {
            return {
                email: "",
                password: ""
            }
        },
        methods: {
            login() {
                api.login({
                    email: this.email,
                    password: this.password,
                }).then((response => {

                    localStorage.setItem('authToken', response.data.token)
                    localStorage.setItem('userId', response.data.userId)

                    let payload = {'token': response.data.token, 'userId': response.data.userId}
                    store.dispatch('validateByTokenAndUserId', payload).then()
                    const decoded = jwt_decode(response.data.token)
                    const authenticationLevel = decoded.authLevel
                    if (authenticationLevel == 0) {
                        router.push({path: '/AdminDashboard'});
                    }
                    else{
                        router.push('Profile')
                    }
                }))
                    .catch(error => this.warningToast(this.getErrorMessageFromStatusCode(error.response.status)))
            },

            getErrorMessageFromStatusCode(statusCode){
                let message = "Incorrect email or password"
                if(statusCode === 401){
                    message = "Incorrect email or password"
                }
                return message;
            }
        }
    }
</script>

<style scoped>
    .container {
        width: 500px;
    }

    @media only screen and (max-width: 600px) {
        .container {
            width: 100%;
        }
    }

</style>
