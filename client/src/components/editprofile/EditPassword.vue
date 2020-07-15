<template>

    <div class="container">

        <h1 class="title is-5">Change Password</h1>

        <form @submit.prevent="updatePassword">
            <b-field v-if="store.getters.getAuthenticationLevel > 1"
                    label="Current Password" expanded >
                <b-input v-model="currentPassword" type="password" placeholder="Current Password" required></b-input>
            </b-field>
            <b-field label="New Password" expanded >
            <b-input v-model="password" type="password" placeholder="New Password" required></b-input>
            </b-field>
            <b-field label="Confirm Password" :message="[{'Passwords do not match':isDisabled}]" expanded >
                <b-input v-model="confPassword" type="password" placeholder="Confirm Password" required></b-input>
            </b-field>
            <b-field>
                <b-button type="is-info" native-type="submit" :disabled="isDisabled">Save</b-button>
            </b-field>
        </form>
    </div>

</template>

<script>
    import api from "../../Api";
    import store from "../../store";
    import toastMixin from "../../mixins/toastMixin";

    export default {
        name: "EditPassword",
        mixins: [toastMixin],
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
                return (this.password != this.confPassword);
            }
        },
        methods: {
            updatePassword() {
                if(this.password.length < 8) {
                    this.warningToast("Password must be 8 characters long")
                } else {
                    const passwordDetails = {
                        "currentPassword": this.currentPassword,
                        "newPassword": this.password,
                        "confPassword": this.confPassword
                    }
                    api.editPassword(passwordDetails, this.$route.params.id, localStorage.getItem('authToken'))
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

</style>