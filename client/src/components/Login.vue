<template>
    <div class="container">
        <div class="form-container sign-in-container">
            <section>
                <form action="" method="post" class="form-register">
                    <h1 class="title">Login</h1>
                    <b-field label="Email">
                             <b-input placeholder="Email"
                                      v-model="email"
                                      type="email"
                                      maxlength="20">
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
                              type="is-danger">
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

    /*h1 {*/
    /*    padding: 1.5rem 0;*/
    /*    font-weight: bold;*/
    /*    margin: 0;*/
    /*    text-align: center;*/
    /*}*/

    /*.card {*/
    /*    min-height: 300px;*/
    /*}*/

    /*.sign-in-container {*/
    /*    left: 0;*/
    /*    width: 100%;*/
    /*}*/

    /*form div {*/
    /*    width: 100%;*/
    /*}*/

    /*form {*/
    /*    background-color: #FFFFFF;*/
    /*    display: flex;*/
    /*    align-items: center;*/
    /*    justify-content: center;*/
    /*    flex-direction: column;*/
    /*    padding: 40px;*/
    /*    !*margin: 100px;*!*/
    /*    margin-left: 300px;*/
    /*    margin-right: 300px;*/
    /*    text-align: left;*/
    /*    margin-top: 60px;*/
    /*    border-radius: 10px;*/
    /*    position: center;*/

    /*}*/

</style>
