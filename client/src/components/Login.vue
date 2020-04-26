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
    import authenticationStore from "../store/authenticationStore";

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
                    authenticationStore.methods.setUserId(response.data.userId)
                    authenticationStore.methods.setSessionId(response.data.token)
                    authenticationStore.methods.setAuthenticated(true)
                    localStorage.setItem('testToken', 'tempToken')

                    router.push('Profile')
                }))
                    .catch(error => this.displayError(error.response.status))
            },

            displayError(errorStatusCode){
                const message = this.getErrorMessageFromStatusCode(errorStatusCode)
                this.$buefy.toast.open({
                    duration:5500,
                    message: message,
                    type: 'is-danger',
                    position: 'is-top'
                })
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
