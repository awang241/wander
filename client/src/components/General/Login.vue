<template>
    <div class="container">
        <div class="form-container sign-in-container">
            <section>

                <ValidationObserver v-slot="{ handleSubmit }">

                <form action="" method="post" class="form-register" @submit.prevent="handleSubmit(login)">
                    <h1 class="title">Login</h1>
                    <ValidationProvider rules="required|email" name="Email" v-slot="{ errors, valid }" slim>
                        <b-field label="Email"
                                 :type="{ 'is-danger': errors[0], 'is-success' : valid}"
                                 :message="errors">
                            <template slot="label">
                                <i class="fas fa-envelope" style="font-size: 1em; color: #38eeff"></i>
                                Email <span>*</span></template>
                            <b-input class="help"
                                     placeholder="Email"
                                     v-model="email">
                            </b-input>
                        </b-field>
                    </ValidationProvider>

                    <ValidationProvider rules="required|minPassword" name="Password" v-slot="{ errors, valid }">
                        <b-field label="Password"
                                 :type="{ 'is-danger': errors[0], 'is-success': valid}"
                                 :message="errors"
                                 expanded>
                            <template slot="label">
                                <i class="fas fa-key" style="font-size: 1em; color: #38eeff"></i>
                                Password <span>*</span></template>
                            <b-input placeholder="Password"
                                     v-model="password"
                                     type="password">
                            </b-input>
                        </b-field>
                    </ValidationProvider>

                    <br>

                    <b-button native-type="submit"
                              type="is-primary" style="float:right">
                        Login
                    </b-button>
                    <br>
                </form>
                </ValidationObserver>
            </section>
        </div>
    </div>
</template>

<script>
    import api from '../../Api';
    import router from "../../router";
    import store from '../../store';
    import jwt_decode from "jwt-decode";
    import toastMixin from "../../mixins/toastMixin";
    import {ValidationObserver, ValidationProvider} from "vee-validate";

    export default {
        name: 'Login',
        mixins: [toastMixin],
        components: {
            ValidationProvider,
            ValidationObserver
        },
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

                    localStorage.setItem('authToken', response.data.token);
                    localStorage.setItem('userId', response.data.userId);

                    let payload = {'token': response.data.token, 'userId': response.data.userId};
                    store.dispatch('validateByTokenAndUserId', payload).then();
                    const decoded = jwt_decode(response.data.token);
                    const authenticationLevel = decoded.authLevel;
                    if (authenticationLevel === 0) {
                        router.push({path: '/AdminDashboard'});
                    }
                    else{
                        router.push({path: '/Home'})
                    }
                }))
                    .catch(error => this.warningToast(this.getErrorMessageFromStatusCode(error.response.status)))
            },

            getErrorMessageFromStatusCode(statusCode){
                let message = "Incorrect email or password";
                if (statusCode === 401){
                    message = "Incorrect email or password";
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

    span {
        color: red;
    }

</style>
