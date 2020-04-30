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
            <component v-bind:is="component"  v-bind:profile="profile"/>

        </div>
    </div>
</template>

<script>
    import editPersonal from "./EditPersonal";
    import editPassword from "./EditPassword";
    import editCountries from "./EditCountries";
    import editActivityTypes from "./EditActivityTypes";
    import editEmails from "./EditEmails";
    import api from '../../Api';
    import authenticationStore from "../../store/authenticationStore";
    import router from "../../router";

    export default {
        name: "EditProfile",
        data() {
            return {
                component: "editPersonal",
                profileId: this.$route.params.id,
                profile: {}
            }
        },
         mounted(){
            this.getProfile()
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
                //Goes to the previous component on the stack
                router.go(-1)
            },
            getProfile(){
                api.getProfile(authenticationStore.methods.getUserId(), authenticationStore.methods.getSessionId())
                    .then(response => this.profile = response.data)
            },

            updateCountries(newCountries){
                this.profile.passports = newCountries
                api.editProfile(authenticationStore.methods.getUserId(), this.profile, authenticationStore.methods.getSessionId())
            },

            updateActivityTypes(newActivities){
                this.profile.activities = newActivities
                api.editProfile(authenticationStore.methods.getUserId(), this.profile, authenticationStore.methods.getSessionId())
            },

            updateEmails(primaryEmail, optionalEmails){
                this.profile.primary_email = primaryEmail
                this.profile.optional_email = optionalEmails
                api.editEmail({"primary_email" : primaryEmail,
                                        "additional_email": optionalEmails}, authenticationStore.methods.getUserId(), authenticationStore.methods.getSessionId())

            },

            updatePersonal(personalDetails){
                this.profile.firstname = personalDetails.firstname
                this.profile.lastname = personalDetails.lastname
                this.profile.middlename = personalDetails.middlename
                this.profile.nickname = personalDetails.nickname
                this.profile.bio = personalDetails.bio
                this.profile.date_of_birth = personalDetails.date_of_birth
                this.profile.gender = personalDetails.gender
                this.profile.fitness = personalDetails.fitness
                api.editProfile(authenticationStore.methods.getUserId(), this.profile, authenticationStore.methods.getSessionId())

            }
        }
    }
</script>

<style scoped>
    .containerColor {
        background-color: #F7F8F9;
    }

</style>