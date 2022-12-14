<!--Container that holds all other edit components and allows switching between them-->

<template>
  <div class="container containerColor">
    <h3 class="title is-4">Profile Settings</h3>
    <div class="tabs is-centered">
      <ul>
        <li :class="{'is-active': tabIndex === 0}"><a v-on:click="setTabIndex(0)"><b-icon icon="user" style="color: #38C7FF"></b-icon>Basic Info</a></li>
        <li :class="{'is-active': tabIndex === 1}"><a v-on:click="setTabIndex(1)"><b-icon icon="key" style="color: #38C7FF"></b-icon>Change Password</a></li>
        <li :class="{'is-active': tabIndex === 2}"><a v-on:click="setTabIndex(2)"><b-icon icon="passport" style="color: #38C7FF"></b-icon>Passport Countries</a></li>
        <li :class="{'is-active': tabIndex === 3}"><a v-on:click="setTabIndex(3)"><b-icon icon="heartbeat" style="color: #38C7FF"></b-icon>Activity Types</a></li>
        <li :class="{'is-active': tabIndex === 4}"><a v-on:click="setTabIndex(4)"><b-icon icon="envelope" style="color: #38C7FF"></b-icon>Emails</a></li>
        <li :class="{'is-active': tabIndex === 5}"><a v-on:click="setTabIndex(5)"><b-icon icon="map-marker-alt" style="color: #38C7FF"></b-icon>Location</a></li>
        <li v-if="$store.getters.getAuthenticationLevel > 0 && !editingThroughDashboard">
            <a @click="changeToProfile">
              <b-icon icon="chevron-left" style="color: #38C7FF"></b-icon>
                Back to Profile
            </a>
        </li>
      </ul>
    </div>
    <div>
      <component v-bind:is="componentMap[tabIndex]"
                 v-bind:profile="profile"/>
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
import api from '../../../Api';
import router from "../../../router";
import {eventBus} from '../../../main';
import toastMixin from "../../../mixins/toastMixin";

const COMPONENT_MAP = {
    0: editPersonal,
    1: editPassword,
    2: editCountries,
    3: editActivityTypes,
    4: editEmails,
    5: editLocation
};
Object.freeze(COMPONENT_MAP);

export default {

    name: "EditProfile",
    mixins: [toastMixin],

    props: ['id'],
    data() {
        return {
            tabIndex: 0,
            profile: {},
        }
    },
    computed: {
        editingThroughDashboard: function () {
          return this.$route.params.id == null
        }
    },
    created() {
        this.componentMap = COMPONENT_MAP
    },
    mounted() {
        this.getProfile();
    },
    methods: {
        setTabIndex(index) {
            this.tabIndex = index
        },
        changeToProfile() {
            router.push({path: '/Profile/' + this.$store.getters.getUserId});
        },
        changeToDashboard() {
            router.push({path: '/AdminDashboard'});
        },
        getProfile() {
            if (!(this.id === this.$store.getters.getUserId || this.$store.getters.getAuthenticationLevel < 2)) {
                router.push({path: '/EditProfile/' + this.$store.getters.getUserId})
            }
            api.getProfile(this.id, localStorage.getItem('authToken'))
                .then(response => this.profile = response.data)
                .catch(() => {
                    this.warningToast("Error occurred while getting Profile details.");
                    router.go(-1)
                })
        },
        updateCountries(newCountries) {
            this.profile.passports = newCountries;
            api.editProfile(this.id, this.profile, localStorage.getItem('authToken'))
        },
        updateActivityTypes(newActivities) {
            this.profile.activities = newActivities;
            eventBus.$emit('profileWasEdited', this.profile);
            api.editProfile(this.id, this.profile, localStorage.getItem('authToken'))
        },
        updateEmails(primaryEmail, optionalEmails) {
            this.profile.primary_email = primaryEmail;
            this.profile.optional_email = optionalEmails;
            eventBus.$emit('profileWasEdited', this.profile);
            const emails = {
                "primary_email": primaryEmail,
                "additional_email": optionalEmails
            };
            api.editEmail(emails, this.id, localStorage.getItem('authToken'))
        },
        updateLocation(location) {
            this.profile.location = location;
            api.editProfileLocation(this.id, this.profile.location, localStorage.getItem('authToken'))
                .then(() => {
                    this.successToast("Location updated")
                })
                .catch(error => this.warningToast(error.response.data))
        },
        clearLocation() {
            api.deleteLocation(this.id, localStorage.getItem('authToken'))
                .then(() => {
                    this.successToast("Location removed");
                })
                .catch(error => this.warningToast(error.response.data))
        },
        updatePersonal(personalDetails) {
            this.profile.firstname = personalDetails.firstname;
            this.profile.lastname = personalDetails.lastname;
            this.profile.middlename = personalDetails.middlename;
            this.profile.nickname = personalDetails.nickname;
            this.profile.bio = personalDetails.bio;
            this.profile.date_of_birth = personalDetails.date_of_birth;
            this.profile.gender = personalDetails.gender;
            this.profile.fitness = personalDetails.fitness;
            eventBus.$emit('profileWasEdited', this.profile);
            api.editProfile(this.id, this.profile, localStorage.getItem('authToken'))
        }
    }
}
</script>

<style scoped>
.containerColor {
  background-color: #F7F8F9;
}

</style>