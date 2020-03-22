<template>

    <div class="container">

        <h1 class="title is-5">Change Password</h1>

        <form @submit.prevent="updatePassword">
            <b-field label="Current Password" expanded >
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
    import authenticationStore from "../../store/authenticationStore";

    export default {
        name: "EditPassword",
        data() {
            return {
                currentPassword: "",
                password: "",
                confPassword: "",
            }
        },

        computed: {
            isDisabled() {
                return !(this.password == this.confPassword);
            }
        },
        methods: {
            updatePassword() {
                if(this.password.length < 8) {
                    this.showMessage("Password must be 8 characters long")
                } else {
                    const passwordDetails = {
                        "currentPassword": this.currentPassword,
                        "newPassword": this.password,
                        "confPassword": this.confPassword
                    }
                    api.editPassword(passwordDetails, authenticationStore.methods.getUserId(), authenticationStore.methods.getSessionId())
                        .catch(error => this.showError(this.displayError(error.response.status)))
                        .then(response => this.showMessage(this.displayError(response.status)))
                    // this.showMessage(this.displayError(status))
                }
            },
            showMessage(message) {
                this.$buefy.toast.open({
                    duration: 5500,
                    message: message,
                    type: 'is-success',
                    position: 'is-top'
                })
            },
            showError(message) {
                this.$buefy.toast.open({
                    duration: 5500,
                    message: message,
                    type: 'is-danger',
                    position: 'is-top'
                })
            },
            displayError(statusCode){
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