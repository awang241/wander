<!--Container that holds all other edit components and allows switching between them-->

<template>
  <div class="container containerColor">
    <h3 class="title is-4">Profile Settings</h3>
    <div class="tabs is-centered">
      <ul>
        <li :class="{'is-active': tabIndex === 0}"><a v-on:click="changeToPersonal">Basic Info</a></li>
        <li :class="{'is-active': tabIndex === 1}"><a v-on:click="changeToPassword">Change Password</a></li>
        <li :class="{'is-active': tabIndex === 2}"><a v-on:click="changeToCountries">Passport Countries</a></li>
        <li :class="{'is-active': tabIndex === 3}"><a v-on:click="changeToActivityTypes">Activity Types</a></li>
        <li :class="{'is-active': tabIndex === 4}"><a v-on:click="changeToEmail">Emails</a></li>
        <li :class="{'is-active': tabIndex === 5}"><a v-on:click="changeToLocation">Location</a></li>
        <li>
          <a v-if="store.getters.getAuthenticationLevel > 0 && !editingThroughDashboard"
             @click="changeToProfile">Back to Profile</a>
        </li>
      </ul>
    </div>
    <div>
      <component v-bind:is="component"
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
import api from '../../Api';
import router from "../../router";
import store from "../../store";
import {eventBus} from '../../main';
import toastMixin from "../../mixins/toastMixin";


export default {
  name: "EditProfile",
  mixins: [toastMixin],

  props: ['id'],
  data() {
    return {
        tabIndex: 0,
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
    this.getProfile();
    this.changeToPersonal();
  },
  // These methods are used to dynamically swap between components on click
  methods: {
    changeToPassword() {
      this.component = editPassword;
        this.tabIndex = 1;
    },
    changeToPersonal() {
        this.component = editPersonal;
        this.tabIndex = 0;
    },
    changeToCountries() {
      this.component = editCountries;
        this.tabIndex = 2;
    },
    changeToActivityTypes() {
      this.component = editActivityTypes;
        this.tabIndex = 3;
    },
    changeToLocation() {
      this.component = editLocation;
        this.tabIndex = 5;
    },
    changeToEmail() {
      this.component = editEmails;
        this.tabIndex = 4;
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
      api.editEmail({
        "primary_email": primaryEmail,
        "additional_email": optionalEmails
      }, this.id, localStorage.getItem('authToken'))
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