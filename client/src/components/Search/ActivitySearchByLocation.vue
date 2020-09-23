<template>
  <div class="container">
    <h1 class="title">Activity Search</h1>

    <ValidationObserver v-slot="{ handleSubmit }">
      <form ref="form" @submit.prevent="handleSubmit(search)">

        <b-field group-multiline grouped>
            <b-field label="Enter a location" expanded>
              <AutoCompleteLocation v-on:locationStringChanged="updateMapLocationFromAutoComplete" v-on:updateMap="updateLocation" v-bind:profileLocation="profile.location" ref="autocomplete"></AutoCompleteLocation>
            </b-field>
          <ValidationProvider rules="required|integer|maxDistanceInRange" name="Max distance (km)" v-slot="{ errors, valid }" slim>
          <b-field label="Max distance (km)"

                   :type="{ 'is-danger': errors[0], 'is-success': valid }"
                   :message="errors">
            <template slot="label">Max distance (km)<span>*</span></template>
            <b-input id="maxDistanceInput" v-model="maxDistance" type="is-primary" ></b-input>
          </b-field>
          </ValidationProvider>
        </b-field>
        <ActivityTypesField v-on:updateSearchMethod="newSearchMethod => activitySearchType = newSearchMethod"
                            v-on:updateChosenActivityTypes="newActivityTypes => chosenActivityTypes = newActivityTypes"
                            :chosenActivityTypes="chosenActivityTypes"
                            :activitySearchType="activitySearchType"></ActivityTypesField>
        <br>
        <div class="columns is-desktop">
          <div class="column">
            <MapPane ref="map" marker-label="Search Location" :location-choice-coordinates="profileLocationLatLong" v-bind:address="profile.location.address"
                     v-on:locationChoiceChanged="updateLocation"
                     :info-window-content="informationWindowData" :default_width="500" :default_height="500"></MapPane>
          </div>
          <div class="column">
          <div id="results" v-if="activityResults.length">
              <h1><b>Activities returned from Search:</b></h1>
              <br>
              <div style="overflow-y: auto; overflow-x: hidden; height: 450px;">
                <div
                        v-for="activity in activityResults"
                        :key="activity.id">
                  <ActivitySummary :activity="activity">
                  </ActivitySummary>
                  <br>
                </div>
              </div>

            </div>
            <div v-else id="noMatches">
              <h1><b>{{searchResultString}}</b></h1>
            </div>
          </div>
        </div>
        <br>
        <div class="row">
          <b-field style="float:right;">
            <b-button native-type="submit" type="is-primary">Search</b-button>
          </b-field>
        </div>
        <br/>
      </form>
    </ValidationObserver>
  </div>
</template>

<script>
    import ActivitySummary from '../Summaries/ActivitySummary';
    import googleMapsInit from '../../utils/googlemaps';
    import MapPane from "../Location/MapPane";
    import ActivityTypesField from "./SearchReusables/ActivityTypesField";
    import Api from "../../Api";
    import store from "../../store";
    import toastMixin from "../../mixins/toastMixin";
    import AutoCompleteLocation from "../Location/AutoCompleteLocation";
    import {ValidationProvider, ValidationObserver} from 'vee-validate'

export default {
  name: "ActivitySearch",
  components: {
    MapPane, ActivityTypesField, AutoCompleteLocation, ActivitySummary, ValidationObserver, ValidationProvider
  },
  mixins: [toastMixin],
  data() {
    return {
      geocoder: null,
      profile: {},
      maxDistance: 50,
      activitySearchType: "all",
      chosenActivityTypes: [],
      activityResults: [],
      store: store,
      profileLocationLatLong: null,
      locationString: "",
      informationWindowData: "",
      searchResultString: "Please click the 'Search' button below!"

    }
  },
  methods: {
    clearLocation() {
      // also clear autocomplete field when merged in
      this.maxDistance = 50;
      this.chosenActivityTypes = []
    },
    search() {
      // remove old pins
      this.$refs.map.clearAdditionalMarkers();
      const searchParameters = this.getSearchParameters();
      Api.getActivitiesByLocation(localStorage.getItem('authToken'), searchParameters).then(response => {
        if (response.data.length) {
          this.activityResults = response.data;
          // add new pins
          for (let i = 0; i < this.activityResults.length; i++) {
              let activityLatLong = {lat: this.activityResults[i].latitude, lng: this.activityResults[i].longitude};
              this.$refs.map.createSingleMarker({position: activityLatLong, id: this.activityResults[i].id});
              this.$refs.map.setZoomWithMarkers();
          }
        } else {
          this.activityResults = [];
          this.searchResultString = "Sorry, your search didn't return any activities in the specified range."
        }
      })
    },
    getSearchParameters() {
      const M_TO_KM = 1000;
      const searchParameters = {};
      searchParameters.distance = this.maxDistance * M_TO_KM
      searchParameters.latitude = this.profileLocationLatLong.lat
      searchParameters.longitude = this.profileLocationLatLong.lng
      if (this.chosenActivityTypes.length > 0) {
        searchParameters.activityTypes = this.chosenActivityTypes.join(",")
        searchParameters.searchMethod = this.activitySearchType
      }
      return searchParameters
    },

    setDefaultProfileLocation() {
      Api.getProfile(this.store.getters.getUserId, localStorage.getItem('authToken'))
              .then((response) => {
                this.profile = response.data;
                if (this.profile.location) {
                  this.profileLocationLatLong = {lat: this.profile.location.latitude, lng: this.profile.location.longitude};
                } else {
                  this.profile.location = {address: "", latitude: null, longitude: null}
                }
              })
              .catch(() => {
                this.warningToast("Error occurred while getting your location details.");
              })
    },
    updateLocation(location) {
      this.geocoder.geocode({'location': {lat: location.lat(), lng: location.lng()}}, (results, status) => {
        if (status === 'OK') {
          document.getElementById("autocompleteLocation").value = results[0].formatted_address
          this.locationString = results[0].formatted_address
          this.profileLocationLatLong = {lat: location.lat(), lng: location.lng()}

        }
      })
    },
    updateMapLocationFromAutoComplete(location) {
      this.profileLocationLatLong = {lat: location.latitude, lng: location.longitude}
      this.locationString = location.address
    },
    /**
     * Method to format the details of an activity for the information pop up window
     * At the moment it has dummy data
     * Need to put in a variable (activityDetails) into this method
     */
    formatActivityDetails() {

      //This variable is dummy data
      let activityDetails = {
        activityName: "Doing happy tings",
        location: "a happy place",
        lat: 68.174270,
        lng: 16.329620,
        activityTypes: ["happy stuff", "really happy stuff"]
      };
      const activityTypesString = this.formatActivityTypesString(activityDetails.activityTypes);

      //Had to use inline styling because of scope :(
      const informationWindowText =
              `<div style="width: 100vh; height: 100vh;">` +
              `<h1 style="font-size: 22px">${activityDetails.activityName}</h1>` +
              `<br>` +
              `<h1 class="infoWindowHeader">Location: <span>${activityDetails.location}</span></h1>` +
              `<br>` +
              `<h1 class="infoWindowHeader">Latitude: <span>${activityDetails.lat}</span></h1>` +
              `<br>` +
              `<h1 class="infoWindowHeader">Longitude: <span>${activityDetails.lng}</span></h1>` +
              `<br>` +
              `${activityTypesString}` +
              `</div>`
      return informationWindowText
    },

    formatActivityTypesString(activityTypes) {
      let formattedActivityTypes =
              `<h1 style="font-size: 16px"> Activity Types:</h1>` +
              `<br>`
      let typesString = "";
      for (let i = 0; i < activityTypes.length; i++) {
        typesString = typesString + `*<span>${activityTypes[i]}</span>` + `<br>`
      }
      return formattedActivityTypes + typesString
    }
  },
  async mounted() {
    this.google = await googleMapsInit();
    this.setDefaultProfileLocation();
    this.geocoder = new this.google.maps.Geocoder;
  }
}

</script>

<style scoped>
  .container {
    margin-top: 0px;
  }

</style>