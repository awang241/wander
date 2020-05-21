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
                <li><a v-on:click="changeToLocation">Location</a></li>

            </ul>
            <div>
                <a v-if="store.getters.getAuthenticationLevel === 0 || store.getters.getAuthenticationLevel === 1"
                   @click="changeToDashboard">Back To Admin Dashboard</a>
            </div>
            <div>
                <a v-if="store.getters.getAuthenticationLevel > 0"
                   @click="changeToProfile">Back to Profile</a>
            </div>

        </div>

        <div>
            <component v-bind:is="component" v-bind:profile="profile"/>
        </div>
    </div>
</template>

<script>
    import editPersonal from "./EditPersonal";
    import editPassword from "./EditPassword";
    import editCountries from "./EditCountries";
    import editActivityTypes from "./EditActivityTypes";
    import editEmails from "./EditEmails";
    import editLocation from "./EditLocation";
    import api from '../../Api';
    import router from "../../router";
    import store from "../../store";

    export default {
        name: "EditProfile",

        data() {
            return {
                component: "editPersonal",
                profileId: this.$route.params.id,
                profile: {},
                store: store
            }
        },
        mounted() {
            // this.URLAuthorization(),
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
            changeToLocation() {
                this.component = editLocation
            },
            changeToProfile() {
                router.push({path: '/Profile'});
            },
            changeToDashboard() {
                router.push({path: '/AdminDashboard'});
            },

            getProfile() {
                if (!(this.$route.params.id == store.getters.getUserId || store.getters.getAuthenticationLevel < 2)) {
                    router.push({path: '/EditProfile/' + store.getters.getUserId})

                }
                api.getProfile(this.$route.params.id, localStorage.getItem('authToken'))
                    .then(response => this.profile = response.data)
                    .catch((error) => {
                        console.log(error)
                        router.go(-1)
                    })
            },
            updateCountries(newCountries) {
                this.profile.passports = newCountries
                api.editProfile(this.$route.params.id, this.profile, localStorage.getItem('authToken'))
            },
            updateActivityTypes(newActivities) {
                this.profile.activities = newActivities
                api.editProfile(this.$route.params.id, this.profile, localStorage.getItem('authToken'))
            },
            updateEmails(primaryEmail, optionalEmails) {
                this.profile.primary_email = primaryEmail
                this.profile.optional_email = optionalEmails
                api.editEmail({
                    "primary_email": primaryEmail,
                    "additional_email": optionalEmails
                }, this.$route.params.id, localStorage.getItem('authToken'))
            },
            updateLocation(country, city, state) {
                this.profile.country = country
                this.profile.city = city
                this.profile.state = state
                api.editProfile(this.$route.params.id, this.profile, localStorage.getItem('authToken'))
            },
            clearLocation() {
                api.deleteLocation(this.$route.params.id, localStorage.getItem('authToken'));
            },
            updatePersonal(personalDetails) {
                this.profile.firstname = personalDetails.firstname
                this.profile.lastname = personalDetails.lastname
                this.profile.middlename = personalDetails.middlename
                this.profile.nickname = personalDetails.nickname
                this.profile.bio = personalDetails.bio
                this.profile.date_of_birth = personalDetails.date_of_birth
                this.profile.gender = personalDetails.gender
                this.profile.fitness = personalDetails.fitness
                api.editProfile(this.$route.params.id, this.profile, localStorage.getItem('authToken'))
            }
        }
    }
</script>

<style scoped>
    .containerColor {
        background-color: #F7F8F9;
    }

</style>