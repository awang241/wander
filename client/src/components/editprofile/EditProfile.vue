<!--Container that holds all other edit components and allows switching between them-->

<template>
    <div class="container containerColor">
        <h3 class="title is-4">Profile Settings</h3>
        <div class="tabs is-centered">

            <ul>
                <li><a v-on:click="changeToPersonal">Basic Info</a></li>
                <li><a v-on:click="changeToPassword">Change Password</a></li>
                <li><a v-on:click="changeToCountries">Passport Countries</a></li>
                <li><a v-on:click="changeToActivityTypes">Activity Types</a></li>
                <li><a v-on:click="changeToEmail">Emails</a></li>
            </ul>
            <div>
                <a @click="changeToProfile">Back to Profile</a>

            </div>
        </div>

        <div>
            <component v-bind:is="component" />
        </div>
    </div>
</template>

<script>
    import editPersonal from "./EditPersonal";
    import editPassword from "./EditPassword";
    import editCountries from "./EditCountries";
    import editActivityTypes from "./EditActivityTypes";
    import editEmails from "./EditEmails";
    import router from "../../router";

    export default {
        name: "EditProfile",
        components: {
            editPersonal
        },
        data() {
            return {
                component: "editPersonal"
            }
        },
        // These methods are used to dynamically swap between components on click
        methods: {
            changeToPassword() {
                this.component = editPassword
            },
            changeToPersonal() {
                this.component = editPersonal
            },
            changeToCountries() {
                this.component = editCountries
            },
            changeToActivityTypes() {
                this.component = editActivityTypes
            },
            changeToEmail() {
                this.component = editEmails
            },
            changeToProfile() {
                router.push('Profile');
            }
        }
    }
</script>

<style scoped>
    .containerColor {
        background-color: #F7F8F9;
    }

</style>