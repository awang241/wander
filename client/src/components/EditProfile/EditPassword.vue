<template>

    <div class="container">

        <h1 class="title is-5">Change Password</h1>

        <ValidationObserver v-slot="{ handleSubmit }">
            <form @submit.prevent="handleSubmit(updatePassword)">
                <ValidationProvider :rules="!checkLoggedInUserIsAdmin()" name="Current Password" v-slot="{ errors, valid }">
                    <b-field v-if="store.getters.getAuthenticationLevel > 1"
                             :type="{ 'is-danger': errors[0], 'is-success': valid }"
                             :message="errors"
                             expanded>
                        <template slot="label">Current Password <span>*</span></template>
                        <b-input v-model="currentPassword" type="password" placeholder="Current Password"></b-input>
                    </b-field>
                </ValidationProvider>

                <ValidationProvider rules="required|minPassword" name="New Password" v-slot="{ errors, valid }" vid="password">
                    <b-field :type="{ 'is-danger': errors[0], 'is-success': valid }"
                             :message="errors"
                             expanded>
                        <template slot="label">New Password <span>*</span></template>
                        <b-input v-model="password" type="password" placeholder="New Password"></b-input>
                    </b-field>
                </ValidationProvider>
                <ValidationProvider rules="requiredConfirm|confirmed:password" name="Confirm Password" v-slot="{ errors, valid }">
                    <b-field :type="{ 'is-danger': errors[0], 'is-success': valid }"
                             :message="errors"
                             expanded>
                        <template slot="label">Confirm Password <span>*</span></template>
                        <b-input v-model="confPassword" type="password" placeholder="Confirm Password" ></b-input>
                    </b-field>
                </ValidationProvider>
                <br>
                <b-button style="float:right" type="is-primary" native-type="submit" :disabled="isDisabled">Save</b-button>
                <br>
            </form>
        </ValidationObserver>
    </div>

</template>

<script>
    import api from "../../Api";
    import store from "../../store";
    import toastMixin from "../../mixins/toastMixin";
    import {ValidationProvider, ValidationObserver} from 'vee-validate';

    export default {
        name: "EditPassword",
        mixins: [toastMixin],
        props: ["profile"],
        components: {
            ValidationProvider,
            ValidationObserver
        },
        data() {
            return {
                currentPassword: "",
                password: "",
                confPassword: "",
                store: store
            }
        },

        computed: {
            isDisabled() {
                return (this.password !== this.confPassword);
            }
        },
        methods: {
            updatePassword() {

                if (this.password === this.currentPassword) {
                    this.warningToast("No changes made")
                } else if(this.password.length < 8) {
                    this.warningToast("Password must be at least 8 characters long")
                } else if (this.confPassword !== this.password) {
                    this.warningToast("Passwords do not match!")
                } else {
                    const passwordDetails = {
                        "currentPassword": this.currentPassword,
                        "newPassword": this.password,
                        "confPassword": this.confPassword
                    }
                    let userId = this.$route.params.id;
                    if (userId === undefined) {
                        userId = this.profile.id;
                    }
                    api.editPassword(passwordDetails, userId, localStorage.getItem('authToken'))
                        .catch(error => this.warningToast(this.getErrorMessageFromStatusCode(error.response.status)))
                        .then(response => this.successToast(this.getErrorMessageFromStatusCode(response.status)))
                }
            },
            getErrorMessageFromStatusCode(statusCode){
                let message = ""
                if(statusCode == 200) {
                    message = "Password changed successfully"
                } else if(statusCode == 400 || statusCode == 403 || statusCode == 401) {
                    message = "Incorrect details entered"
                }
                return message;
            },
            checkLoggedInUserIsAdmin() {
                if (!store.getters.getAuthenticationLevel == 0 ||
                    !store.getters.getAuthenticationLevel == 1) {
                    return null;
                } else {
                    return "required|minPassword";
                }
            }
        }
    }


</script>

<style scoped>
    .container {
        background-color: #F7F8F9;
        margin-top: 0px;
        padding: 0px;
    }

    span {
        color: red;
    }

</style>