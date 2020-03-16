<template>
    <div class="container containerColor">
        <div class="tabs is-centered">
            <ul>
                <li><a v-on:click="changeToPersonal">Edit Profile</a></li>
                <li><a v-on:click="changeToPassword">Change Password</a></li>
                <li><a v-on:click="changeToCountries">Countries</a></li>
                <li><a v-on:click="changeToAcitivites">Activities</a></li>
                <li><a v-on:click="changeToEmail">Email</a></li>
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
    import editPersonal from "./editPersonal";
    import changePassword from "./changePassword";
    import countries from "./countries";
    import activities from "./activities";
    import email from "./email";
    import router from "../../router";
    export default {
        name: "editProfile",
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
                this.component = changePassword
            },
            changeToPersonal() {
                this.component = editPersonal
            },
            changeToCountries() {
                this.component = countries
            },
            changeToAcitivites() {
                this.component = activities
            },
            changeToEmail() {
                this.component = email
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