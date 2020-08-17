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
                <li>
                    <a v-if="store.getters.getAuthenticationLevel > 0 && !editingThroughDashboard"
                   @click="changeToProfile">Back to Profile</a>
                </li>
            </ul>
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
    import {eventBus} from '../../main';

    export default {
        name: "EditProfile",
        props: ['id'],
        data() {
            return {
                component: "editPersonal",
                profile: {},
                store: store
            }
        },
        computed: {
            editingThroughDashboard: function () {
                return this.$route.params.id == null
            }
        },
        mounted() {
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
            changeToLocation() {
                this.component = editLocation
            },
            changeToEmail() {
                this.component = editEmails
            },
            changeToProfile() {
                router.push({path: '/Profile/' + store.getters.getUserId});
            },
            changeToDashboard() {
                router.push({path: '/AdminDashboard'});
            },

            getProfile() {
                if (!(this.id === store.getters.getUserId || store.getters.getAuthenticationLevel < 2)) {
                    router.push({path: '/EditProfile/' + store.getters.getUserId})

                }
                api.getProfile(this.id, localStorage.getItem('authToken'))
                    .then(response => this.profile = response.data)
                    .catch(() => {
                        this.warningToast("Error occurred while getting Profile details.");
                        router.go(-1)
                    })
            },
            updateCountries(newCountries) {
                this.profile.passports = newCountries
                api.editProfile(this.id, this.profile, localStorage.getItem('authToken'))
            },
            updateActivityTypes(newActivities) {
                this.profile.activities = newActivities
                eventBus.$emit('profileWasEdited', this.profile)
                api.editProfile(this.id, this.profile, localStorage.getItem('authToken'))
            },
            updateEmails(primaryEmail, optionalEmails) {
                this.profile.primary_email = primaryEmail
                this.profile.optional_email = optionalEmails
                eventBus.$emit('profileWasEdited', this.profile)
                api.editEmail({
                    "primary_email": primaryEmail,
                    "additional_email": optionalEmails
                }, this.id, localStorage.getItem('authToken'))
            },
            updateLocation(location) {
                this.profile.location = location
                api.editProfileLocation(this.id, location, localStorage.getItem('authToken'))
                    .then(() => {
                        this.successToast("Location updated!")
                    })
                    .catch(error => this.warningToast(error.response.data))
            },
            clearLocation() {
                api.deleteLocation(this.id, localStorage.getItem('authToken'));
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
                eventBus.$emit('profileWasEdited', this.profile)
                api.editProfile(this.id, this.profile, localStorage.getItem('authToken'))
            },
            getId() {
                return this.id;
            }
        }
    }
</script>

<style scoped>
    .containerColor {
        background-color: #F7F8F9;
    }

</style>