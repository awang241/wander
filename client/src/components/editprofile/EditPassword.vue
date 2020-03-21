<template>

    <div class="container">

        <h1 class="Title">Change Password</h1>

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
                const passwordDetails = {
                    "currentPassword": this.currentPassword,
                    "newPassword": this.password,
                    "confPassword": this.confPassword
                }
                api.editPassword(passwordDetails, authenticationStore.methods.getUserId(), authenticationStore.methods.getSessionId())
            }
        }
    }


</script>

<style scoped>
    .container {
        background-color: #F7F8F9;
    }

</style>