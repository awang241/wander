<template>
  <div class="container">
    <h1 class="title">Activity Search By Location</h1>

    <ValidationObserver v-slot="{ handleSubmit }">
      <form ref="form" @submit.prevent="handleSubmit(search)">
        <b-field group-multiline grouped>
          <b-field expanded>
            <template slot="label">Enter a location <span class="red-star">*</span></template>
            <AutoCompleteLocation v-on:locationStringChanged="updateMapLocationFromAutoComplete" v-on:updateMap="updateLocation" v-bind:profileLocation="profile.location" ref="autocomplete"></AutoCompleteLocation>
          </b-field>
          <ValidationProvider rules="required|integer|maxDistanceInRange" name="Max distance (km)" v-slot="{ errors, valid }" slim>
            <b-field label="Max distance (km)"

                     :type="{ 'is-danger': errors[0], 'is-success': valid }"
                     :message="errors">
              <template slot="label">Max distance (km) <span class="red-star">*</span></template>
              <b-input id="maxDistanceInput" v-model="maxDistance" type="is-primary" ></b-input>
            </b-field>
          </ValidationProvider>
        </b-field>
        <ActivityTypesField v-on:updateSearchMethod="newSearchMethod => activitySearchType = newSearchMethod"
                          v-on:updateChosenActivityTypes="newActivityTypes => chosenActivityTypes = newActivityTypes"
                          :chosenActivityTypes="chosenActivityTypes"
                          :activitySearchType="activitySearchType"></ActivityTypesField>
        <br>
        <div class="row">
          <b-field style="float:right;">
            <b-button type="is-primary" native-type="submit">Search</b-button>
          </b-field>
        </div>
      </form>
    </ValidationObserver>
        <br>
        <br>

        <div class="columns is-desktop">
          <div class="column">
            <MapPane ref="map" marker-label="Search Location" :location-choice-coordinates="profileLocationLatLong" v-bind:address="profile.location.address"
                     v-on:locationChoiceChanged="updateLocation"
                     :info-window-content="informationWindowData" :default_width="500" :default_height="500"></MapPane>
          </div>
          <div class="column">
            <div id="results" v-if="activityResults.length">
              <h1><strong>Activities returned from Search:</strong></h1>
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
              <h1><strong>{{searchResultString}}</strong></h1>
            </div>
          </div>
        </div>
        <br>
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
    import dateTimeMixin from "../../mixins/dateTimeMixin";

export default {
  name: "ActivitySearch",
  components: {
    MapPane, ActivityTypesField, AutoCompleteLocation, ActivitySummary, ValidationObserver, ValidationProvider
  },
  mixins: [toastMixin, dateTimeMixin],
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
      searchResultString: "Please click the 'Search' button!"

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
              let contentInformation = this.formatActivityDetails(this.activityResults[i]);
              this.$refs.map.createSingleMarker({position: activityLatLong, text: this.activityResults[i].activityName, id: this.activityResults[i].id}, contentInformation);
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
     */
    formatActivityDetails(activityDetails) {
      const activityTypesString = this.formatActivityTypesString(activityDetails.activityTypes);
      let informationWindowText = ""

      if (activityDetails.startTime && activityDetails.endTime) {
        const formattedStartTime = this.dateTimeFormat(activityDetails.startTime)
        const formattedEndTime = this.dateTimeFormat(activityDetails.endTime)
        informationWindowText =
                `<div>` +
                `<h1 style="font-size: 22px; font-weight: bold; font-style: italic">${activityDetails.activityName}</h1>` +
                `<h1 style="font-weight: bold">${activityDetails.location}</h1>` +
                `<br>` +
                `<h1 style="font-weight: bold">Start date and time: <span>${formattedStartTime}</span></h1>` +
                `<br>` +
                `<h1 style="font-weight: bold">End date and time: <span>${formattedEndTime}</span></h1>` +
                `<br>` +
                `${activityTypesString}` +
                `</div>`
      } else {
        informationWindowText =
                `<div>` +
                `<h1 style="font-size: 22px; font-weight: bold; font-style: italic">${activityDetails.activityName}</h1>` +
                `<h1 style="font-weight: bold">${activityDetails.location}</h1>` +
                `<br>` +
                `<h1 style="font-weight: bold">Start date: Now!</h1>` +
                `<br>` +
                `<h1 style="font-weight: bold">End date: "${activityDetails.activityName}" is continuous!</h1>` +
                `<br>` +
                `${activityTypesString}` +
                `</div>`
      }

      return informationWindowText
    },

    formatActivityTypesString(activityTypes) {
      let formattedActivityTypes = `<h1 style="font-size: 16px; font-weight: bold"> Activity Types:</h1>`
      let typesString = "";
      for (let i = 0; i < activityTypes.length; i++) {
        typesString = typesString + `<span style="color:red; font-weight: bold">*</span><span> ${activityTypes[i]} </span>`
        if (i % 5 === 0 && i !== 0) {
          typesString = typesString + `<br>`
        }
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

  .red-star {
    color: red;
  }

</style>